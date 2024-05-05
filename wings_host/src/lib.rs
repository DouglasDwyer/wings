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

    pub fn instantiate(&mut self, image: &WingsImage) -> Result<(), WingsError> {
        self.reset_store();
        let sorted_dependencies = self.sort_image_dependencies(image)?;
        self.instantiate_modules(image)?;
        println!("SORTED {sorted_dependencies:?} and had {:?}", self.store.as_ref().unwrap().data().event_raisers);
        Ok(())
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

    fn instantiate_systems(&mut self, image: &WingsImage, order: &[DisjointExportedType]) -> Result<(), WingsError> {
        let mut store = self.store.as_mut().expect("Failed to get store");
        for ty in order {
            // todo: make sure no hanging deps
            let entry = image.systems[ty];
            let descriptor = &image.modules[entry.module_id as usize].0.metadata.system_descriptors[entry.system_id as usize];

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
        data.memories.clear();
        data.systems.clear();
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

    fn create_host_imports<C: AsContextMut<UserState = WingsHostInner<H>>>(mut ctx: C, index: usize) -> Imports {
        let mut imports = Imports::new();
        imports.define("env", "__wings_invoke_proxy_function", Extern::Func(Self::create_invoke_proxy_func(&mut ctx, index)));
        imports.define("env", "__wings_raise_event", Extern::Func(Self::create_raise_event_func(&mut ctx, index)));

        imports.define("env", "__wings_dbg", Extern::Func(Func::new(&mut ctx, FuncType::new([ValueType::I32], []),
            |_, params, _| { println!("DBG: {params:?}"); Ok(()) })));

        imports
    }

    fn create_host_inner(ctx: GeeseContextHandle<Self>) -> WingsHostInner<H> {
        let event_raisers = Self::get_event_raisers();
        let invokers = Self::get_invokers();

        let instance_funcs = Vec::new();
        let memories = Vec::new();
        let systems = Vec::new();

        WingsHostInner {
            ctx,
            event_raisers,
            instance_funcs,
            invokers,
            systems,
            memories
        }
    }

    fn create_invoke_proxy_func<C: AsContextMut<UserState = WingsHostInner<H>>>(mut ctx: C, index: usize) -> Func {
        Func::new(&mut ctx, FuncType::new([ValueType::I32; 4], []), move |mut ctx, params, _| unsafe {
            let [Value::I32(id), Value::I32(func_index), Value::I32(pointer), Value::I32(size)] = params else { unreachable!() };
            let len = *size as usize;
            let mut buffer = Vec::with_capacity(len);
            let memory = ctx.data().memories[index].clone();
            memory.read(&mut ctx, *pointer as usize, &mut buffer)?;

            let system_index = *id as usize;
            let data = ctx.data_mut();
            if let Some(trait_holder) = data.invokers.get(system_index) {
                (trait_holder.invoke)(&mut data.ctx, *func_index as u32, &mut buffer)?;
            }
            else {
                todo!()
                //let Some(holder) = data.systems.get(system_index - data.invokers.len()) else { anyhow::bail!("System index out-of-range") };
                //let func = data.instance_funcs[holder.instance as usize].invoke_func.clone();
                //func.call(&mut ctx, &[])
            }

            Ok(())
        })
    }

    fn create_raise_event_func<C: AsContextMut<UserState = WingsHostInner<H>>>(mut ctx: C, index: usize) -> Func {
        Func::new(&mut ctx, FuncType::new([ValueType::I32; 2], []), move |mut ctx, params, _| {
            let [Value::I32(pointer), Value::I32(size)] = params else { unreachable!() };
            let len = *size as usize;
            let mut buffer = Vec::with_capacity(len);
            let memory = ctx.data().memories[index].clone();
            memory.read(&mut ctx, *pointer as usize, &mut buffer)?;

            let mut section_reader = SectionedBufferReader::new(&buffer);
            let ty = bincode::deserialize::<ExportedType>(section_reader.section()?)?;
            
            let inner = ctx.data_mut();
            if let Some(raiser) = inner.event_raisers.get(&ty.into()) {
                raiser(&mut inner.ctx, section_reader.section()?)?;
            }
            else {
                drop(section_reader);
                inner.ctx.raise_event(on::GuestEvent { data: buffer });
            }

            Ok(())
        })
    }

    fn get_instance_funcs<C: AsContextMut>(mut ctx: C, instance: &Instance) -> Result<InstanceFuncs, WingsError> {
        let alloc_marshal_buffer = Self::get_instance_func(
            &mut ctx, instance, "__wings_alloc_marshal_buffer", FuncType::new([ValueType::I32], [ValueType::I32]))?;
        let invoke_func = Self::get_instance_func(
            &mut ctx, instance, "__wings_invoke_func", FuncType::new([ValueType::I32, ValueType::I32], [ValueType::I32]))?;

        Ok(InstanceFuncs {
            alloc_marshal_buffer,
            invoke_func
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

    fn dispatch_event<T: wings::ExportType + Serialize + DeserializeOwned>(&mut self, event: &T) {
        todo!("Dispatch it")
    }

    fn dispatch_guest_event(&mut self, event: &on::GuestEvent) {

    }

    fn get_event_raisers() -> FxHashMap<DisjointExportedType, HostEventRaiserFunc<H>> {
        FxHashMap::from_iter(H::EVENTS.event_raisers.iter().map(|x| (ExportedType::from(x.ty).into(), x.raise)))
    }

    fn get_invokers() -> Vec<HostSystemTrait<H>> {
        Vec::from_iter(H::SYSTEMS.traits.iter().flat_map(|x| x.iter()).copied())
    }
}

impl<H: Host> GeeseSystem for WingsHost<H> {
    const DEPENDENCIES: geese::Dependencies = H::SYSTEMS.dependencies;

    fn new(ctx: GeeseContextHandle<Self>) -> Self {
        const UNIQUE_ID: AtomicU64 = AtomicU64::new(0);

        let engine = Engine::new(H::create_engine());
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

    fn create_engine() -> Self::Engine;
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
struct InstanceFuncs {
    pub alloc_marshal_buffer: Func,
    pub invoke_func: Func
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

#[derive(Clone, Debug)]
struct SystemHolder {
    instance: u32,
    pointer: FatGuestPointer,
    drop_fn: GuestPointer
}

struct WingsHostInner<H: Host> {
    ctx: GeeseContextHandle<WingsHost<H>>,
    event_raisers: FxHashMap<DisjointExportedType, HostEventRaiserFunc<H>>,
    instance_funcs: Vec<InstanceFuncs>,
    invokers: Vec<HostSystemTrait<H>>,
    systems: Vec<SystemHolder>,
    memories: Vec<Memory>
}

#[derive(Debug)]
struct WingsModuleInner {
    pub host_id: u64,
    pub module: Module,
    pub metadata: ModuleMetadata
}

mod on {
    #[derive(Debug)]
    pub struct GuestEvent {
        pub data: Vec<u8>
    }
}

mod private {
    use super::*;

    pub trait HostEvent: wings::ExportType + Serialize + DeserializeOwned + Send + Sync {}
    impl<T: wings::ExportType + Serialize + DeserializeOwned + Send + Sync> HostEvent for T {}
}