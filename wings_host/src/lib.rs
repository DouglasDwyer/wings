#![allow(warnings)]

use const_list::*;
use fxhash::*;
use geese::*;
use private::*;
use ::serde::*;
use ::serde::de::*;
use std::collections::*;
use std::marker::*;
use std::mem::*;
use std::sync::*;
use std::sync::atomic::*;
use topological_sort::*;
use wasm_runtime_layer::*;
pub use wings::*;
use wings::marshal::*;

pub struct Events<H: Host> {
    event_handlers: geese::EventHandlers<WingsHost<H>>,
    event_raisers: ConstList<'static, HostEventRaiser<H>>
}

impl<H: Host> Events<H> {
    pub const fn with<T: HostEvent>(&'static self) -> Self {
        Self {
            event_handlers: self.event_handlers.with(WingsHost::dispatch_event::<T>),
            event_raisers: self.event_raisers.push(HostEventRaiser { ty: T::TYPE, raise: Self::raise::<T> })
        }
    }

    fn raise<T: HostEvent>(ctx: &GeeseContextHandle<WingsHost<H>>, buffer: &[u8]) -> Result<(), WingsError> {
        ctx.raise_event(bincode::deserialize::<T>(buffer).map_err(WingsError::Serialization)?);
        Ok(())
    }
}

pub const fn events<H: Host>() -> Events<H> {
    Events {
        event_handlers: geese::event_handlers(),
        event_raisers: ConstList::new()
    }
}

pub struct Systems<H: Host> {
    dependencies: geese::Dependencies,
    traits: ConstList<'static, ConstList<'static, HostSystemTrait<H>>>
}

impl<H: Host> Systems<H> {
    pub const fn with<S: GeeseSystem>(&'static self, traits: Traits<H, S>) -> Self {
        Self {
            dependencies: self.dependencies.with::<Mut<S>>(),
            traits: self.traits.push(traits.inner)
        }
    }

}

pub const fn systems<H: Host>() -> Systems<H> {
    Systems {
        dependencies: geese::dependencies(),
        traits: ConstList::new()
    }
}

pub struct Traits<H: Host, S: GeeseSystem> {
    inner: ConstList<'static, HostSystemTrait<H>>,
    marker: PhantomData<fn(S)>
}

impl<H: Host, S: GeeseSystem> Traits<H, S> {
    pub const fn with<T: Proxyable + SystemTrait + ?Sized>(&'static self) -> Self where S: AsMut<T> {
        Self {
            inner: self.inner.push(HostSystemTrait { ty: T::TYPE, invoke: Self::invoke::<T> }),
            marker: PhantomData
        }
    }

    unsafe fn invoke<T: Proxyable + SystemTrait + ?Sized>(ctx: &mut GeeseContextHandle<WingsHost<H>>, func_index: u32, buffer: *mut Vec<u8>) -> Result<(), WingsError> where S: AsMut<T> {
        ctx.get_mut::<S>().as_mut().invoke(func_index, buffer)
    }
}

pub const fn traits<H: Host, S: GeeseSystem>() -> Traits<H, S> {
    Traits {
        inner: ConstList::new(),
        marker: PhantomData
    }
}

#[derive(Clone, Debug, Default)]
pub struct WingsImage {
    modules: Vec<WingsModule>,
    systems: FxHashMap<DisjointExportedType, SystemEntry>,
    top_level_systems: FxHashSet<ExportedType>
}

impl WingsImage {
    pub fn add<T: wings::ExportType + ?Sized>(&mut self, module: &WingsModule) {
        let module_id = self.add_or_get_module(module);
        self.add_top_level_systems::<T>(module_id, module);
    }

    fn add_or_get_module(&mut self, module: &WingsModule) -> u32 {
        if let Some(position) = self.modules.iter().position(|x| x == module) {
            position as u32
        }
        else {
            let result = self.modules.len() as u32;
            self.modules.push(module.clone());
            self.add_systems(module, result);
            result
        }
    }

    fn add_systems(&mut self, module: &WingsModule, module_id: u32) {
        for (index, system) in module.0.metadata.system_descriptors.iter().enumerate() {
            let entry = SystemEntry {
                module_id,
                system_id: index as u32,
                version: system.ty.version
            };

            match self.systems.entry(DisjointExportedType::from(&system.ty)) {
                std::collections::hash_map::Entry::Occupied(mut v) => if v.get().version < entry.version {
                    v.insert(entry);
                },
                std::collections::hash_map::Entry::Vacant(v) => { v.insert(entry); },
            }
        }
    }

    fn add_top_level_systems<T: wings::ExportType + ?Sized>(&mut self, module_id: u32, module: &WingsModule) {
        let group_ty = ExportedType::from(T::TYPE);
        if let Some(group) = module.0.metadata.group_instantiates.iter().find(|x| x.group_ty == group_ty) {
            self.top_level_systems.extend(group.systems.iter().cloned());
        }
    }
}

#[derive(Clone, Debug)]
pub struct WingsModule(Arc<WingsModuleInner>);

impl WingsModule {
    pub fn groups(&self) -> impl '_ + Iterator<Item = &ExportedType> {
        self.0.metadata.group_instantiates.iter().map(|x| &x.group_ty)
    }

    pub fn systems(&self) -> impl '_ + Iterator<Item = &ExportedType> {
        self.0.metadata.system_descriptors.iter().map(|x| &x.ty)
    }
}

impl PartialEq for WingsModule {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for WingsModule {}

pub struct WingsHost<H: Host> {
    engine: Engine<H::Engine>,
    id: u64,
    store: Option<wasm_runtime_layer::Store<WingsHostInner<H>, H::Engine>>
}

impl<H: Host> WingsHost<H> {
    pub fn load(&self, package: impl std::io::Read) -> Result<WingsModule, WingsError> {
        let module = Module::new(&self.engine, package).map_err(WingsError::from_invalid_module)?;
        let mut metadata = self.load_module_metadata(&module)?;

        self.collate_group_instantiates(&mut metadata.group_instantiates);
        self.check_group_instantiates_duplicates(&metadata.group_instantiates)?;

        Ok(WingsModule(Arc::new(WingsModuleInner {
            host_id: self.id,
            metadata,
            module,
        })))
    }

    pub fn instantiate(&mut self, image: &WingsImage) {
        self.clear_image();
        if let Err(error) = self.try_instantiate(image) {
            self.handle_error(error);
        }
    }

    fn check_group_instantiates_duplicates(&self, group_instantiates: &[InstantiateGroup]) -> Result<(), WingsError> {
        for group in group_instantiates {
            for i in 1..group.systems.len() {
                for j in 0..i {
                    if group.systems[i] == group.systems[j] {
                        return Err(WingsError::from_invalid_module(format_args!("Duplicate instantiated systems in group {}", group.group_ty.name)));
                    }
                }
            }
        }

        Ok(())
    }

    fn collate_group_instantiates(&self, group_instantiates: &mut Vec<InstantiateGroup>) {
        let mut i = 1;
        while i < group_instantiates.len() {
            let current = &group_instantiates[i];
            if let Some(index) = group_instantiates[0..i].iter().position(|x| x.group_ty == current.group_ty) {
                let old = group_instantiates.swap_remove(index);
                group_instantiates[index].systems.extend(old.systems);
            }
            else {
                i += 1;
            }
        }
    }

    fn clear_image(&mut self) -> Result<(), WingsError> {
        let result = self.drop_systems();
        self.reset_store();
        result
    }

    fn drop_systems(&mut self) -> Result<(), WingsError> {
        let mut store = self.store.as_mut().expect("Failed to get store");

        for i in (0..store.data().systems.len()).rev() {
            let system = store.data().systems[i];
            let invoke_func = store.data().instance_funcs[system.instance as usize].invoke_func_1.clone();
            invoke_func.call(&mut store, &[Value::I32(system.drop_fn.into()), Value::I32(system.pointer.into())], &mut [Value::I32(0)]).map_err(WingsError::from_trap)?;
        }
        
        Ok(())
    }

    fn load_module_metadata(&self, module: &Module) -> Result<ModuleMetadata, WingsError> {
        let mut store = wasm_runtime_layer::Store::new(&self.engine, ());
        let dummy_imports = self.create_dummy_imports(&mut store, module)?;

        let instance = Instance::new(&mut store, module, &dummy_imports).map_err(WingsError::from_invalid_module)?;
        let metadata = self.metadata_functions(&mut store, &instance);

        let Some(Extern::Memory(memory)) = instance.get_export(&mut store, "memory")
            else { return Err(WingsError::from_invalid_module("Module missing memory")) };

        let mut system_descriptors = Vec::with_capacity(metadata.describes.len());
        let mut group_instantiates = Vec::with_capacity(metadata.instantiates.len());
        let mut buffer = Vec::new();           

        for func in metadata.instantiates {
            self.read_marshal_buffer(&mut store, &func, &memory, &mut buffer)?;
            group_instantiates.push(bincode::deserialize(&buffer).map_err(WingsError::from_invalid_module)?);
        }

        for func in metadata.describes {
            self.read_marshal_buffer(&mut store, &func, &memory, &mut buffer)?;
            system_descriptors.push(bincode::deserialize(&buffer).map_err(WingsError::from_invalid_module)?);
        }

        Ok(ModuleMetadata {
            group_instantiates,
            system_descriptors
        })
    }

    fn create_dummy_imports<C: AsContextMut>(&self, mut ctx: C, module: &Module) -> Result<Imports, WingsError> {
        let mut imports = Imports::new();
        for import in module.imports(&self.engine) {
            let func = match import.ty {
                ExternType::Func(func_ty) => Func::new(&mut ctx, func_ty, |_, _, _| anyhow::bail!("Module called stub method")),
                _ => return Err(WingsError::InvalidModule("Module imported unexpected object".to_string()))
            };
            imports.define(import.module, import.name, Extern::Func(func));
        }
        Ok(imports)
    }

    fn fill_event_handlers(&mut self, image: &WingsImage, types: &[DisjointExportedType]) {
        let mut store = self.store.as_mut().expect("Failed to get store");
        let data = store.data_mut();

        for (id, ty) in types.iter().enumerate() {
            let entry = image.systems[ty];
            let descriptor = &image.modules[entry.module_id as usize].0.metadata.system_descriptors[entry.system_id as usize];
            for event_handler in &descriptor.event_handlers {
                let event_entry = data.guest_event_handlers.entry(event_handler.ty.clone()).or_default();
                if !event_entry.instances.contains(&entry.module_id) {
                    event_entry.instances.push(entry.module_id);
                }
                event_entry.event_handlers.push(EventHandlerDescriptor { system: id as u32, event_func: event_handler.event_func, invoke_func: event_handler.invoke_func });
            }
        }
    }

    fn fill_system_traits(&mut self, image: &WingsImage, types: &[DisjointExportedType]) -> Result<FxHashMap<DisjointExportedType, u32>, WingsError> {
        let mut store = self.store.as_mut().expect("Failed to get store");
        let data = store.data_mut();

        let capacity = types.len() + data.invokers.len();
        let mut trait_map = HashMap::with_capacity_and_hasher(capacity, FxBuildHasher::default());
        data.system_traits.reserve(capacity);

        for invoker in &data.invokers {
            let id = data.system_traits.len() as u32;
            data.system_traits.push(SystemTraitHolder::Host { invoker: id });
            if trait_map.insert(ExportedType::from(invoker.ty).into(), id).is_some() {
                panic!("Duplicate host dependency {:?}", invoker.ty);
            }
        }

        for ty in types {
            let entry = image.systems[ty];
            let descriptor = &image.modules[entry.module_id as usize].0.metadata.system_descriptors[entry.system_id as usize];
            for system_trait in &descriptor.traits {
                let id = data.system_traits.len() as u32;
                data.system_traits.push(SystemTraitHolder::Guest { invoke: system_trait.invoke, system: entry.module_id, v_table: system_trait.v_table });
                if trait_map.insert(DisjointExportedType::from(&system_trait.ty), id).is_some() {
                    return Err(WingsError::from_invalid_module("Duplicate trait implementations"));
                }
            }
        }

        Ok(trait_map)
    }

    fn instantiate_modules(&mut self, image: &WingsImage) -> Result<(), WingsError> {
        let mut store = self.store.as_mut().expect("Failed to get store");
        store.data_mut().instance_funcs.reserve(image.modules.len());
        
        for (index, module) in image.modules.iter().enumerate() {
            let imports = Self::create_host_imports(&mut store, index);
            let instance = Instance::new(&mut store, &module.0.module, &imports).map_err(WingsError::from_invalid_module)?;
            let Some(Extern::Memory(memory)) = instance.get_export(&mut store, "memory") else { unreachable!() };
            store.data_mut().memories.push(memory);
            let instance_funcs = Self::get_instance_funcs(&mut store, &instance)?;
            store.data_mut().instance_funcs.push(instance_funcs);
        }

        Ok(())
    }

    fn instantiate_systems(&mut self, image: &WingsImage, order: &[DisjointExportedType], trait_map: &FxHashMap<DisjointExportedType, u32>) -> Result<(), WingsError> {
        let mut store = self.store.as_mut().expect("Failed to get store");
        for ty in order {
            let entry = image.systems[ty];
            let descriptor = &image.modules[entry.module_id as usize].0.metadata.system_descriptors[entry.system_id as usize];
            let mut dependencies = Vec::with_capacity(descriptor.dependencies.len());
            for dependency in &descriptor.dependencies {
                let Some(index) = trait_map.get(&dependency.into()) else { return Err(WingsError::from_invalid_module("Missing dependency")) };
                dependencies.push(match store.data().system_traits[*index as usize] {
                    SystemTraitHolder::Guest { system, v_table, .. } => {
                        let system = &store.data().systems[system as usize];
                        if system.instance == entry.module_id {
                            DependencyReference::Local(FatGuestPointer::new(system.pointer, v_table))
                        }
                        else {
                            DependencyReference::Remote(*index)
                        }
                    },
                    SystemTraitHolder::Host { invoker } => DependencyReference::Remote(*index)
                })
            }
            
            let instance_funcs = store.data().instance_funcs[entry.module_id as usize].clone();
            let memory = store.data().memories[entry.module_id as usize].clone();

            let dependency_data = bincode::serialize(&dependencies).map_err(WingsError::Serialization)?;
            let mut result = [Value::I32(0)];
            instance_funcs.alloc_marshal_buffer.call(&mut store, &[Value::I32(dependency_data.len() as i32)], &mut result).map_err(WingsError::from_invalid_module)?;
            let [Value::I32(pointer)] = result else { unreachable!() };
            memory.write(&mut store, pointer as usize, &dependency_data).map_err(WingsError::from_invalid_module)?;
            instance_funcs.invoke_func_1.call(&mut store, &[Value::I32(descriptor.new_func.into()), Value::I32(pointer)], &mut result).map_err(WingsError::from_trap)?;
            let [Value::I32(pointer)] = result else { unreachable!() };
            
            store.data_mut().systems.push(SystemHolder {
                instance: entry.module_id,
                pointer: GuestPointer::new(pointer as u32),
                drop_fn: descriptor.drop_func,
                dropped: false
            });
        }

        Ok(())
    }

    fn metadata_functions<C: AsContextMut>(&self, mut ctx: C, instance: &Instance) -> MetadataFunctions {
        let mut describes = Vec::new();
        let mut instantiates = Vec::new();
        for export in instance.exports(&mut ctx) {
            let Extern::Func(func) = export.value else { continue };
            if export.name.starts_with("__wings_describe_") {
                describes.push(func);
            }
            else if export.name.starts_with("__wings_instantiate_") {
                instantiates.push(func);
            }
        }
        
        MetadataFunctions {
            describes,
            instantiates
        }
    }

    fn read_marshal_buffer<C: AsContextMut>(&self, mut ctx: C, func: &Func, memory: &Memory, buffer: &mut Vec<u8>) -> Result<(), WingsError> {
        unsafe {
            let mut result_pointer_value = [Value::I32(0)];
            func.call(&mut ctx, &[], &mut result_pointer_value).map_err(WingsError::from_invalid_module)?;
    
            let Value::I32(result_pointer) = result_pointer_value[0] else { return Err(WingsError::from_invalid_module("Describe function returned wrong type")) };
            let result_offset = result_pointer as usize;
            let mut total_len = [0; size_of::<u32>()];
            memory.read(&mut ctx, result_offset, &mut total_len).map_err(WingsError::from_invalid_module)?;
    
            let data_len = u32::from_le_bytes(total_len) as usize;
            buffer.clear();
            buffer.reserve(data_len);
            buffer.set_len(data_len);
            memory.read(&mut ctx, result_offset + total_len.len(), buffer).map_err(WingsError::from_invalid_module)
        }
    }

    fn reset_store(&mut self) {
        let mut data = take(&mut self.store).expect("Failed to get store").into_data();
        data.ctx.as_ref().expect("Failed to get context").raise_event(on::ImageReloaded);
        data.guest_event_handlers.clear();
        data.memories.clear();
        data.systems.clear();
        data.system_traits.clear();
        data.instance_funcs.clear();
        self.store = Some(wasm_runtime_layer::Store::new(&self.engine, data));
    }

    fn generate_system_trait_map(&self, image: &WingsImage) -> FxHashMap<DisjointExportedType, ExportedType> {
        let mut result: FxHashMap<DisjointExportedType, ExportedType> = HashMap::with_capacity_and_hasher(image.systems.len(), FxBuildHasher::default());
        for (ty, system) in &image.systems {
            let descriptor = &image.modules[system.module_id as usize].0.metadata.system_descriptors[system.system_id as usize];
            for system_trait in &descriptor.traits {
                match result.entry(DisjointExportedType::from(&system_trait.ty)) {
                    hash_map::Entry::Occupied(mut v) => if v.get().version < descriptor.ty.version {
                        v.insert(descriptor.ty.clone());
                    },
                    hash_map::Entry::Vacant(v) => { v.insert(descriptor.ty.clone()); },
                }
            }
        }
        result
    }

    fn sort_image_dependencies(&self, image: &WingsImage) -> Result<Vec<DisjointExportedType>, WingsError> {
        let trait_map = self.generate_system_trait_map(image);
        let mut sort = TopologicalSort::new();
        
        let mut to_process = image.top_level_systems.iter().map(DisjointExportedType::from).filter(|x| image.systems.contains_key(x)).collect::<Vec<_>>();
        let mut result = HashSet::with_capacity_and_hasher(image.systems.len(), FxBuildHasher::default());
        while let Some(next) = to_process.pop() {
            let system = image.systems[&next];
            if result.insert(next.clone()) {
                let descriptor = &image.modules[system.module_id as usize].0.metadata.system_descriptors[system.system_id as usize];
                for dependency_ty in descriptor.dependencies.iter().map(DisjointExportedType::from)
                    .filter_map(|x| trait_map.get(&x).cloned().map(DisjointExportedType::from))
                    .filter(|x| image.systems.contains_key(&x)) {
                    to_process.push(dependency_ty.clone());
                    sort.add_dependency(dependency_ty, next.clone());
                }
                sort.insert(next);
            }
        }

        let mut result = Vec::with_capacity(sort.len());
        while let Some(item) = sort.pop() {
            result.push(item);
        }

        if sort.is_empty() {
            Ok(result)
        }
        else {
            Err(WingsError::CyclicDependency())
        }
    }

    fn create_host_imports<C: AsContextMut<UserState = WingsHostInner<H>, Engine = H::Engine>>(mut ctx: C, index: usize) -> Imports {
        let mut ctx_handle = take(&mut ctx.as_context_mut().data_mut().ctx).expect("Failed to get context");
        let mut imports = H::create_imports(&mut ctx_handle, &mut ctx);
        ctx.as_context_mut().data_mut().ctx = Some(ctx_handle);

        imports.define("env", "__wings_invoke_proxy_function", Extern::Func(Self::create_invoke_proxy_func(&mut ctx, index)));
        imports.define("env", "__wings_raise_event", Extern::Func(Self::create_raise_event_func(&mut ctx, index)));

        imports.define("env", "__wings_dbg", Extern::Func(Func::new(&mut ctx, FuncType::new([ValueType::I32], []),
            |_, params, _| { println!("DBG: {:?}", params[0]); Ok(()) })));

        imports
    }

    fn create_host_inner(ctx: GeeseContextHandle<Self>) -> WingsHostInner<H> {
        let event_raisers = Self::get_event_raisers();
        let invokers = Self::get_invokers();

        let guest_event_handlers = FxHashMap::default();
        let instance_funcs = Vec::new();
        let memories = Vec::new();
        let systems = Vec::new();
        let system_traits = Vec::new();

        WingsHostInner {
            ctx: Some(ctx),
            event_raisers,
            guest_event_handlers,
            instance_funcs,
            invokers,
            systems,
            system_traits,
            memories
        }
    }

    fn create_invoke_proxy_func<C: AsContextMut<UserState = WingsHostInner<H>, Engine = H::Engine>>(mut ctx: C, index: usize) -> Func {
        Func::new(&mut ctx, FuncType::new([ValueType::I32; 4], []), move |mut ctx, params, _| unsafe {
            let [Value::I32(id), Value::I32(func_index), Value::I32(pointer), Value::I32(size)] = params else { unreachable!() };
            let len = *size as usize;
            let mut buffer = Vec::with_capacity(len);
            buffer.set_len(len);
            let memory = ctx.data().memories[index].clone();
            memory.read(&mut ctx, *pointer as usize, &mut buffer)?;

            let system_index = *id as usize;
            let data = ctx.data_mut();
            match data.system_traits.get(*id as usize).copied() {
                Some(SystemTraitHolder::Host { invoker }) => (data.invokers[invoker as usize].invoke)(data.ctx.as_mut().expect("Failed to get context"), *func_index as u32, &mut buffer)?,
                Some(SystemTraitHolder::Guest { invoke, system, v_table }) => Self::invoke_guest_proxy(&mut ctx, *func_index as u32, system, invoke, v_table, &mut buffer)?,
                None => anyhow::bail!("Invalid system trait index"),
            }

            let mut result = [Value::I32(0)];
            let alloc_marshal_buffer = ctx.data().instance_funcs[index].alloc_marshal_buffer.clone();
            alloc_marshal_buffer.call(&mut ctx, &[Value::I32(buffer.len() as i32)], &mut result)?;
            let [Value::I32(pointer)] = result else { unreachable!() };
            memory.write(&mut ctx, pointer as usize, &buffer)?;

            Ok(())
        })
    }

    fn create_raise_event_func<C: AsContextMut<UserState = WingsHostInner<H>>>(mut ctx: C, index: usize) -> Func {
        Func::new(&mut ctx, FuncType::new([ValueType::I32; 2], []), move |mut ctx, params, _| unsafe {
            let [Value::I32(pointer), Value::I32(size)] = params else { unreachable!() };
            let len = *size as usize;
            let mut buffer = Vec::with_capacity(len);
            buffer.set_len(len);
            let memory = ctx.data().memories[index].clone();
            memory.read(&mut ctx, *pointer as usize, &mut buffer)?;

            let mut section_reader = SectionedBufferReader::new(&buffer);
            let ty = DisjointExportedType::from(bincode::deserialize::<ExportedType>(section_reader.section()?)?);
            let inner = ctx.data_mut();
            if let Some(raiser) = inner.event_raisers.get(&ty) {
                raiser(inner.ctx.as_mut().expect("Failed to get context"), section_reader.section()?)?;
            }
            else {
                drop(section_reader);
                inner.ctx.as_ref().expect("Failed to get context").raise_event(on::GuestEvent { buffer, ty });
            }

            Ok(())
        })
    }

    fn get_instance_funcs<C: AsContextMut>(mut ctx: C, instance: &Instance) -> Result<InstanceFuncs, WingsError> {
        let alloc_marshal_buffer = Self::get_instance_func(
            &mut ctx, instance, "__wings_alloc_marshal_buffer", FuncType::new([ValueType::I32], [ValueType::I32]))?;
        let copy_event_object = Self::get_instance_func(
            &mut ctx, instance, "__wings_copy_event_object", FuncType::new([], []))?;
        let invoke_func_1 = Self::get_instance_func(
            &mut ctx, instance, "__wings_invoke_func_1", FuncType::new([ValueType::I32; 2], [ValueType::I32]))?;
        let invoke_func_2 = Self::get_instance_func(
            &mut ctx, instance, "__wings_invoke_func_2", FuncType::new([ValueType::I32; 3], [ValueType::I32]))?;
        let invoke_proxy_func = Self::get_instance_func(
            &mut ctx, instance, "__wings_invoke_proxy_func", FuncType::new([ValueType::I32; 4], [ValueType::I32]))?;

        Ok(InstanceFuncs {
            alloc_marshal_buffer,
            copy_event_object,
            invoke_func_1,
            invoke_func_2,
            invoke_proxy_func
        })
    }

    fn get_instance_func<C: AsContextMut>(mut ctx: C, instance: &Instance, name: &str, ty: FuncType) -> Result<Func, WingsError> {
        let Some(Extern::Func(func)) = instance.get_export(&mut ctx, name)
            else { return Err(WingsError::from_invalid_module(format_args!("Module missing intrinsic {name}"))) };

        if func.ty(&mut ctx) == ty {
            Ok(func)
        }
        else {
            Err(WingsError::from_invalid_module(format_args!("Module intrinsic {name} had invalid signature")))
        }
    }

    fn handle_error(&mut self, error: WingsError) {
        self.store.as_ref().expect("Failed to get store").data().ctx.as_ref().expect("Failed to get context").raise_event(on::Error { error });
        self.reset_store();
    }

    fn invoke_guest_proxy(mut ctx: &mut StoreContextMut<WingsHostInner<H>, H::Engine>, func_index: u32, system: u32, invoke: GuestPointer, v_table: GuestPointer, buffer: &mut Vec<u8>) -> Result<(), WingsError> {
        unsafe {
            let Some(system) = ctx.data().systems.get(system as usize).copied() else { return Err(WingsError::from_trap("Invalid system ID")) };
            let mut result = [Value::I32(0)];
    
            let index = system.instance as usize;
            let instance_funcs = ctx.data().instance_funcs[index].clone();
            let memory = ctx.data().memories[index].clone();
    
            let mut result = [Value::I32(0)];
            instance_funcs.alloc_marshal_buffer.call(&mut ctx, &[Value::I32(buffer.len() as i32)], &mut result).map_err(WingsError::from_trap)?;
            let [Value::I32(pointer)] = result else { unreachable!() };
            memory.write(&mut ctx, pointer as usize, &buffer).map_err(WingsError::from_trap)?;
    
            instance_funcs.invoke_proxy_func.call(&mut ctx, &[Value::I32(invoke.into()), Value::I32(system.pointer.into()), Value::I32(v_table.into()), Value::I32(func_index as i32)], &mut result).map_err(WingsError::from_trap)?;
            let [Value::I32(size)] = result else { unreachable!() };
    
            instance_funcs.alloc_marshal_buffer.call(&mut ctx, &[Value::I32(size)], &mut result).map_err(WingsError::from_trap)?;
            let [Value::I32(pointer)] = result else { unreachable!() };
    
            let len: usize = size as usize;
            buffer.clear();
            buffer.reserve(len);
            buffer.set_len(len);
            memory.read(&mut ctx, pointer as usize, buffer).map_err(WingsError::from_trap)?;
    
            Ok(())
        }
    }

    fn try_instantiate(&mut self, image: &WingsImage) -> Result<(), WingsError> {
        let sorted_dependencies = self.sort_image_dependencies(image)?;
        let trait_map = self.fill_system_traits(image, &sorted_dependencies)?;
        self.fill_event_handlers(image, &sorted_dependencies);
        self.instantiate_modules(image)?;
        self.instantiate_systems(image, &sorted_dependencies, &trait_map)
    }

    fn dispatch_event<T: wings::ExportType + Serialize + DeserializeOwned>(&mut self, event: &T) {
        if let Err(error) = bincode::serialize(&event).map_err(WingsError::Serialization).and_then(|x| self.dispatch_raw_event(&ExportedType::from(T::TYPE).into(), &x)) {
            self.handle_error(error);
        }
    }

    fn dispatch_guest_event(&mut self, event: &on::GuestEvent) {
        if let Err(error) = self.dispatch_raw_event(&event.ty, &event.buffer) {
            self.handle_error(error);
        }
    }

    fn dispatch_raw_event(&mut self, ty: &DisjointExportedType, buffer: &[u8]) -> Result<(), WingsError> {
        if let Some(handler_list) = self.store.as_ref().expect("Failed to get store").data().guest_event_handlers.get(ty).cloned() {
            self.lower_raw_event_buffer(&handler_list, buffer)?;
            self.invoke_event_handlers(&handler_list)?;
        }

        Ok(())
    }

    fn lower_raw_event_buffer(&mut self, handler_list: &EventHandlerList, buffer: &[u8]) -> Result<(), WingsError> {
        let mut store = self.store.as_mut().expect("Failed to get store");

        for instance in handler_list.instances.iter().copied() {
            let instance_funcs = &store.data().instance_funcs[instance as usize];
            let alloc_marshal_buffer = instance_funcs.alloc_marshal_buffer.clone();
            let copy_event_object = instance_funcs.copy_event_object.clone();

            let mut result = [Value::I32(0)];
            alloc_marshal_buffer.call(&mut store, &[Value::I32(buffer.len() as i32)], &mut result).map_err(WingsError::from_trap)?;
            let [Value::I32(pointer)] = result else { unreachable!() };

            let memory = store.data().memories[instance as usize].clone();
            memory.write(&mut store, pointer as usize, buffer).map_err(WingsError::from_trap)?;
            
            copy_event_object.call(&mut store, &[], &mut []).map_err(WingsError::from_trap)?;
        }

        Ok(())
    }

    fn invoke_event_handlers(&mut self, handler_list: &EventHandlerList) -> Result<(), WingsError> {
        let mut store = self.store.as_mut().expect("Failed to get store");
        
        for handler in &handler_list.event_handlers {
            let system = store.data().systems[handler.system as usize];
            let invoke_func = store.data().instance_funcs[system.instance as usize].invoke_func_2.clone();
            invoke_func.call(&mut store, &[Value::I32(handler.invoke_func.into()), Value::I32(system.pointer.into()), Value::I32(handler.event_func.into())], &mut [Value::I32(0)]).map_err(WingsError::from_trap)?;
        }

        Ok(())
    }

    fn get_event_raisers() -> FxHashMap<DisjointExportedType, HostEventRaiserFunc<H>> {
        FxHashMap::from_iter(H::EVENTS.event_raisers.iter().map(|x| (ExportedType::from(x.ty).into(), x.raise)))
    }

    fn get_invokers() -> Vec<HostSystemTrait<H>> {
        Vec::from_iter(H::SYSTEMS.traits.iter().flat_map(|x| x.iter()).copied())
    }
}

impl<H: Host> Drop for WingsHost<H> {
    fn drop(&mut self) {
        if let Err(error) = self.clear_image() {
            self.handle_error(error);
        }
    }
}

impl<H: Host> GeeseSystem for WingsHost<H> {
    const DEPENDENCIES: geese::Dependencies = H::SYSTEMS.dependencies;

    const EVENT_HANDLERS: geese::EventHandlers<Self> = H::EVENTS.event_handlers
        .with(Self::dispatch_guest_event);

    fn new(mut ctx: GeeseContextHandle<Self>) -> Self {
        const UNIQUE_ID: AtomicU64 = AtomicU64::new(0);

        let engine = Engine::new(H::create_engine(&mut ctx));
        let id = UNIQUE_ID.fetch_add(1, Ordering::Relaxed);
        let store = Some(wasm_runtime_layer::Store::new(&engine, Self::create_host_inner(ctx)));

        Self {
            engine,
            id,
            store
        }
    }
}

pub trait Host: 'static + Sized {
    const EVENTS: Events<Self> = events();
    const SYSTEMS: Systems<Self> = systems();

    type Engine: wasm_runtime_layer::backend::WasmEngine;

    fn create_engine(ctx: &mut GeeseContextHandle<WingsHost<Self>>) -> Self::Engine;

    #[allow(unused)]
    fn create_imports(ctx: &mut GeeseContextHandle<WingsHost<Self>>, store: impl AsContextMut) -> Imports {
        Imports::new()
    }
}

#[derive(Clone, Debug)]
struct HostFuncs {
    invoke_proxy: Func,
    raise_event: Func
}

type HostEventRaiserFunc<H> = fn(&GeeseContextHandle<WingsHost<H>>, &[u8]) -> Result<(), WingsError>;

struct HostEventRaiser<H: Host> {
    pub ty: StaticExportedType,
    pub raise: HostEventRaiserFunc<H>
}

type HostInvokerFunc<H> = unsafe fn(&mut GeeseContextHandle<WingsHost<H>>, u32, *mut Vec<u8>) -> Result<(), WingsError>;

struct HostSystemTrait<H: Host> {
    pub ty: StaticExportedType,
    pub invoke: HostInvokerFunc<H>
}

impl<H: Host> Copy for HostSystemTrait<H> {}

impl<H: Host> Clone for HostSystemTrait<H> {
    fn clone(&self) -> Self {
        *self
    }
}

#[derive(Clone, Debug)]
struct EventHandlerDescriptor {
    pub system: u32,
    pub event_func: GuestPointer,
    pub invoke_func: GuestPointer,
}

#[derive(Clone, Debug, Default)]
struct EventHandlerList {
    pub instances: Vec<u32>,
    pub event_handlers: Vec<EventHandlerDescriptor>
}

#[derive(Clone, Debug)]
struct InstanceFuncs {
    pub alloc_marshal_buffer: Func,
    pub copy_event_object: Func,
    pub invoke_func_1: Func,
    pub invoke_func_2: Func,
    pub invoke_proxy_func: Func
}

struct MetadataFunctions {
    pub describes: Vec<Func>,
    pub instantiates: Vec<Func>
}

#[derive(Debug)]
struct ModuleMetadata {
    pub group_instantiates: Vec<InstantiateGroup>,
    pub system_descriptors: Vec<SystemDescriptor>
}

#[derive(Copy, Clone, Debug)]
struct SystemEntry {
    pub module_id: u32,
    pub system_id: u32,
    pub version: Version
}

#[derive(Copy, Clone, Debug)]
struct SystemHolder {
    pub instance: u32,
    pub pointer: GuestPointer,
    pub drop_fn: GuestPointer,
    pub dropped: bool
}

#[derive(Copy, Clone, Debug)]
enum SystemTraitHolder {
    Guest {
        system: u32,
        invoke: GuestPointer,
        v_table: GuestPointer
    },
    Host {
        invoker: u32
    }
}

struct WingsHostInner<H: Host> {
    ctx: Option<GeeseContextHandle<WingsHost<H>>>,
    event_raisers: FxHashMap<DisjointExportedType, HostEventRaiserFunc<H>>,
    guest_event_handlers: FxHashMap<DisjointExportedType, EventHandlerList>,
    instance_funcs: Vec<InstanceFuncs>,
    invokers: Vec<HostSystemTrait<H>>,
    memories: Vec<Memory>,
    systems: Vec<SystemHolder>,
    system_traits: Vec<SystemTraitHolder>,
}

#[derive(Debug)]
struct WingsModuleInner {
    pub host_id: u64,
    pub module: Module,
    pub metadata: ModuleMetadata
}

#[no_mangle]
extern "C" fn __wings_invoke_proxy_function(id: u32, func_index: u32, pointer: GuestPointer, size: u32) {
    unreachable!()
}

#[no_mangle]
extern "C" fn __wings_raise_event(pointer: GuestPointer, size: u32) {
    unreachable!()
}

#[no_mangle]
extern "C" fn __wings_dbg(x: u32) {
    unreachable!()
}

pub mod on {
    use super::*;

    pub struct Error {
        pub error: WingsError
    }

    pub struct ImageReloaded;

    #[derive(Debug)]
    pub(crate) struct GuestEvent {
        pub buffer: Vec<u8>,
        pub ty: DisjointExportedType
    }
}

mod private {
    use super::*;

    pub trait HostEvent: wings::ExportType + Serialize + DeserializeOwned + Send + Sync {}
    impl<T: wings::ExportType + Serialize + DeserializeOwned + Send + Sync> HostEvent for T {}
}