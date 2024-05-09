#![allow(clippy::uninit_vec)]
#![deny(missing_docs)]
#![deny(clippy::missing_docs_in_private_items)]

//! Defines the host system and events for the [`wings`](https://github.com/DouglasDwyer/wings) plugin system.

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
use wings_marshal::*;
use wings_marshal::exported_type::*;

/// Denotes a set of events that the host may send and receive from WASM plugins.
pub struct Events<H: Host> {
    /// The event handlers for sending the events to WASM.
    event_handlers: geese::EventHandlers<WingsHost<H>>,
    /// The event raisers for receiving the event from WASM.
    event_raisers: ConstList<'static, HostEventRaiser<H>>
}

impl<H: Host> Events<H> {
    /// Adds the given event to the list.
    pub const fn with<T: HostEvent>(&'static self) -> Self {
        Self {
            event_handlers: self.event_handlers.with(WingsHost::dispatch_event::<T>),
            event_raisers: self.event_raisers.push(HostEventRaiser { ty: T::TYPE, raise: Self::raise::<T> })
        }
    }

    /// Deserializes the provided buffer and raises it as an event of the provided type.
    fn raise<T: HostEvent>(ctx: &GeeseContextHandle<WingsHost<H>>, buffer: &[u8]) -> Result<(), WingsError> {
        ctx.raise_event(bincode::deserialize::<T>(buffer).map_err(WingsError::Serialization)?);
        Ok(())
    }
}

impl<H: Host> Copy for Events<H> {}

impl<H: Host> Clone for Events<H> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<H: Host> std::fmt::Debug for Events<H> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Events").finish()
    }
}

/// Creates a new, empty set of host events.
pub const fn events<H: Host>() -> Events<H> {
    Events {
        event_handlers: geese::event_handlers(),
        event_raisers: ConstList::new()
    }
}

/// Denotes a set of systems that the host will expose to WASM plugins.
pub struct Systems<H: Host> {
    /// The set of dependencies required by the `WingsHost` to access these systems.
    dependencies: geese::Dependencies,
    /// The traits through which the WASM plugins will access the host.
    traits: ConstList<'static, ConstList<'static, HostSystemTrait<H>>>
}

impl<H: Host> Systems<H> {
    /// Adds the given system to the list.
    pub const fn with<S: GeeseSystem>(&'static self, traits: Traits<H, S>) -> Self {
        Self {
            dependencies: self.dependencies.with::<Mut<S>>(),
            traits: self.traits.push(traits.inner)
        }
    }
}

impl<H: Host> Copy for Systems<H> {}

impl<H: Host> Clone for Systems<H> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<H: Host> std::fmt::Debug for Systems<H> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Systems").finish()
    }
}

/// Creates a new, empty set of host systems.
pub const fn systems<H: Host>() -> Systems<H> {
    Systems {
        dependencies: geese::dependencies(),
        traits: ConstList::new()
    }
}

/// Defines the traits through which a system is exposed to WASM.
pub struct Traits<H: Host, S: GeeseSystem> {
    /// The inner list of trait data.
    inner: ConstList<'static, HostSystemTrait<H>>,
    /// Generic marker.
    marker: PhantomData<fn(S)>
}

impl<H: Host, S: GeeseSystem> Traits<H, S> {
    /// Exposes the system under the given trait.
    pub const fn with<T: Proxyable + SystemTrait + ?Sized>(&'static self) -> Self where S: AsMut<T> {
        Self {
            inner: self.inner.push(HostSystemTrait { ty: T::TYPE, invoke: Self::invoke::<T> }),
            marker: PhantomData
        }
    }

    /// Performs a proxy invocation on the system via the given trait.
    unsafe fn invoke<T: Proxyable + SystemTrait + ?Sized>(ctx: &mut GeeseContextHandle<WingsHost<H>>, func_index: u32, buffer: *mut Vec<u8>) -> Result<(), WingsError> where S: AsMut<T> {
        ctx.get_mut::<S>().as_mut().invoke(func_index, buffer)
    }
}

impl<H: Host, S: GeeseSystem> Copy for Traits<H, S> {}

impl<H: Host, S: GeeseSystem> Clone for Traits<H, S> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<H: Host, S: GeeseSystem> std::fmt::Debug for Traits<H, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Traits").finish()
    }
}

/// Creates a new, empty set of host traits.
pub const fn traits<H: Host, S: GeeseSystem>() -> Traits<H, S> {
    Traits {
        inner: ConstList::new(),
        marker: PhantomData
    }
}

/// Represents a group of WASM plugins that should be instantiated together.
#[derive(Clone, Debug, Default)]
pub struct WingsImage {
    /// The underlying WASM modules.
    modules: Vec<WingsModule>,
    /// A mapping from system types to the entries that should be used when instantiating them.
    systems: FxHashMap<DisjointExportedType, SystemEntry>,
    /// The top-level systems that should be created when this image is instantiated.
    top_level_systems: FxHashSet<ExportedType>
}

impl WingsImage {
    /// Adds all of the systems in the given group to this image.
    /// All associated systems will be created when this image is instantiated.
    pub fn add<T: wings_marshal::exported_type::ExportType + ?Sized>(&mut self, module: &WingsModule) {
        self.add_new_module(module);
        self.add_top_level_systems::<T>(module);
    }

    /// Adds a module to the image if it does not already exist.
    fn add_new_module(&mut self, module: &WingsModule) {
        if !self.modules.contains(module) {
            let result = self.modules.len() as u32;
            self.modules.push(module.clone());
            self.add_systems(module, result);
        }
    }

    /// Adds all systems from the given module to this image.
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

    /// Adds all top-level systems for the provided group to the top-level list.
    fn add_top_level_systems<T: wings_marshal::exported_type::ExportType + ?Sized>(&mut self, module: &WingsModule) {
        let group_ty = ExportedType::from(T::TYPE);
        if let Some(group) = module.0.metadata.group_instantiates.iter().find(|x| x.group_ty == group_ty) {
            self.top_level_systems.extend(group.systems.iter().cloned());
        }
    }
}

/// Represents a parsed, loaded WASM plugin.
#[derive(Clone, Debug)]
pub struct WingsModule(Arc<WingsModuleInner>);

impl WingsModule {
    /// Gets an iterator over all of the instantiation groups declared in this module.
    pub fn groups(&self) -> impl '_ + Iterator<Item = &ExportedType> {
        self.0.metadata.group_instantiates.iter().map(|x| &x.group_ty)
    }

    /// Gets an iterator over all of the systems declared in this module.
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

/// Holds a set of WASM plugins, managing their creation, destruction, and interop between them.
pub struct WingsHost<H: Host> {
    /// The WASM engine.
    engine: Engine<H::Engine>,
    /// The ID of this host.
    id: u64,
    /// A store for holding all WASM objects.
    store: Option<wasm_runtime_layer::Store<WingsHostInner<H>, H::Engine>>
}

impl<H: Host> WingsHost<H> {
    /// Parses and loads a WASM module from bytes.
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

    /// Unloads the existing image and replaces it with the provided one.
    /// All top-level systems in the given image will be instantiated.
    pub fn instantiate(&mut self, image: &WingsImage) {
        assert!(image.modules.iter().all(|x| self.id == x.0.host_id), "Image contained modules from another host");
        if let Err(error) = self.clear_image() {
            self.handle_error(error);
        }
        if let Err(error) = self.try_instantiate(image) {
            self.handle_error(error);
        }
    }

    /// Checks to ensure that no top-level systems are instantiated more than once.
    fn check_group_instantiates_duplicates(&self, group_instantiates: &[InstantiateGroup]) -> Result<(), WingsError> {
        for group in group_instantiates {
            for i in 1..group.systems.len() {
                for j in 0..i {
                    if group.systems[i] == group.systems[j] {
                        return Err(WingsError::from_invalid_module(format!("Duplicate instantiated systems in group {}", group.group_ty.name)));
                    }
                }
            }
        }

        Ok(())
    }

    /// Gathers all instantiation groups together into the given list.
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

    /// Attempts to unload all systems. If a panic occurs during unload,
    /// the remaining systems will be forgotten without being dropped.
    fn clear_image(&mut self) -> Result<(), WingsError> {
        let result = self.drop_systems();
        self.reset_store();
        result
    }

    /// Invokes the drop procedure on all systems in reverse topological order.
    fn drop_systems(&mut self) -> Result<(), WingsError> {
        let mut store = self.store.as_mut().expect("Failed to get store");

        for i in (0..store.data().systems.len()).rev() {
            let system = store.data().systems[i];
            let invoke_func = store.data().instance_funcs[system.instance as usize].invoke_func_1.clone();
            invoke_func.call(&mut store, &[Value::I32(system.drop_fn.into()), Value::I32(system.pointer.into())], &mut [Value::I32(0)]).map_err(WingsError::from_trap)?;
        }
        
        Ok(())
    }

    /// Loads metadata about all systems and groups within the given module.
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

    /// Creates a set of dummy imports for inspecting a module's metadata without executing its contents.
    fn create_dummy_imports<C: AsContextMut>(&self, mut ctx: C, module: &Module) -> Result<Imports, WingsError> {
        let mut imports = Imports::new();
        for import in module.imports(&self.engine) {
            let func = match import.ty {
                ExternType::Func(func_ty) => Func::new(&mut ctx, func_ty, |_, _, _| Err(WingsError::from_invalid_module("Module called stub method").into())),
                _ => return Err(WingsError::from_invalid_module("Module imported unexpected object"))
            };
            imports.define(import.module, import.name, Extern::Func(func));
        }
        Ok(imports)
    }

    /// Gathers all of the event handlers from the image into the store.
    fn fill_event_handlers(&mut self, image: &WingsImage, types: &[DisjointExportedType]) {
        let store = self.store.as_mut().expect("Failed to get store");
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

    /// Gathers all of the system traits from the image into the store.
    fn fill_system_traits(&mut self, image: &WingsImage, types: &[DisjointExportedType]) -> Result<FxHashMap<DisjointExportedType, u32>, WingsError> {
        let store = self.store.as_mut().expect("Failed to get store");
        let data = store.data_mut();

        let capacity = types.len() + data.invokers.len();
        let mut trait_map = HashMap::with_capacity_and_hasher(capacity, FxBuildHasher::default());
        data.system_traits.reserve(capacity);

        for invoker in &data.invokers {
            let ty = DisjointExportedType::from(ExportedType::from(invoker.ty));
            let id = data.system_traits.len() as u32;
            data.system_traits.push(SystemTraitHolder::Host { invoker: id });
            data.system_trait_map.insert(ty.clone(), id);
            if trait_map.insert(ty, id).is_some() {
                panic!("Duplicate host dependency {:?}", invoker.ty);
            }
        }

        for ty in types {
            let entry = image.systems[ty];
            let descriptor = &image.modules[entry.module_id as usize].0.metadata.system_descriptors[entry.system_id as usize];
            for system_trait in &descriptor.traits {
                let ty = DisjointExportedType::from(&system_trait.ty);
                let id = data.system_traits.len() as u32;
                data.system_traits.push(SystemTraitHolder::Guest { invoke: system_trait.invoke, system: entry.module_id, v_table: system_trait.v_table });
                data.system_trait_map.insert(ty.clone(), id);
                if trait_map.insert(ty, id).is_some() {
                    return Err(WingsError::from_invalid_module("Duplicate trait implementations"));
                }
            }
        }

        Ok(trait_map)
    }

    /// Instantiates the modules from the image with the current store.
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

    /// Instantiates the systems from the image in the provided order.
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
                    SystemTraitHolder::Host { invoker } => DependencyReference::Remote(invoker)
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
                drop_fn: descriptor.drop_func
            });
        }

        Ok(())
    }

    /// Gathers references to all metadata functions contained in the provided instance.
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

    /// Reads length-prefixed data from the guest memory into the provided buffer.
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

    /// Resets the WASM store, forgetting all modules.
    fn reset_store(&mut self) {
        let mut data = take(&mut self.store).expect("Failed to get store").into_data();
        data.ctx.as_ref().expect("Failed to get context").raise_event(on::ImageReloaded);
        data.guest_event_handlers.clear();
        data.instance_funcs.clear();
        data.memories.clear();
        data.systems.clear();
        data.system_traits.clear();
        data.system_trait_map.clear();
        self.store = Some(wasm_runtime_layer::Store::new(&self.engine, data));
    }

    /// Generates a trait map for converting between system trait types and the underlying
    /// concrete implementations.
    fn generate_system_trait_map(&self, image: &WingsImage) -> FxHashMap<DisjointExportedType, ExportedType> {
        let mut result: FxHashMap<DisjointExportedType, ExportedType> = HashMap::with_capacity_and_hasher(image.systems.len(), FxBuildHasher::default());
        for system in image.systems.values() {
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

    /// Topologically sorts all required systems in the given image to determine
    /// their instantiation order.
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
                    .filter(|x| image.systems.contains_key(x)) {
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

    /// Creates the set of imports that the host must provide to the guest.
    fn create_host_imports<C: AsContextMut<UserState = WingsHostInner<H>, Engine = H::Engine>>(mut ctx: C, index: usize) -> Imports {
        let mut ctx_handle = take(&mut ctx.as_context_mut().data_mut().ctx).expect("Failed to get context");
        let mut imports = H::create_imports(&mut ctx_handle, &mut ctx);
        ctx.as_context_mut().data_mut().ctx = Some(ctx_handle);

        imports.define("env", "__wings_invoke_proxy_function", Extern::Func(Self::create_invoke_proxy_func(&mut ctx, index)));
        imports.define("env", "__wings_proxy_index", Extern::Func(Self::create_proxy_index_func(&mut ctx, index)));
        imports.define("env", "__wings_raise_event", Extern::Func(Self::create_raise_event_func(&mut ctx, index)));

        imports
    }

    /// Creates the inner store data for this host.
    fn create_host_inner(ctx: GeeseContextHandle<Self>) -> WingsHostInner<H> {
        let event_raisers = Self::get_event_raisers();
        let invokers = Self::get_invokers();

        let guest_event_handlers = FxHashMap::default();
        let instance_funcs = Vec::new();
        let memories = Vec::new();
        let systems = Vec::new();
        let system_traits = Vec::new();
        let system_trait_map = FxHashMap::default();

        WingsHostInner {
            ctx: Some(ctx),
            event_raisers,
            guest_event_handlers,
            instance_funcs,
            invokers,
            systems,
            system_traits,
            system_trait_map,
            memories
        }
    }

    /// Creates the proxy invocation function for the host.
    fn create_invoke_proxy_func<C: AsContextMut<UserState = WingsHostInner<H>, Engine = H::Engine>>(mut ctx: C, index: usize) -> Func {
        Func::new(&mut ctx, FuncType::new([ValueType::I32; 4], []), move |mut ctx, params, _| unsafe {
            let [Value::I32(id), Value::I32(func_index), Value::I32(pointer), Value::I32(size)] = params else { unreachable!() };
            let len = *size as usize;
            let mut buffer = Vec::with_capacity(len);
            buffer.set_len(len);
            let memory = ctx.data().memories[index].clone();
            memory.read(&mut ctx, *pointer as usize, &mut buffer)?;

            let data = ctx.data_mut();
            match data.system_traits.get(*id as usize).copied() {
                Some(SystemTraitHolder::Host { invoker }) => (data.invokers[invoker as usize].invoke)(data.ctx.as_mut().expect("Failed to get context"), *func_index as u32, &mut buffer)?,
                Some(SystemTraitHolder::Guest { invoke, system, v_table }) => Self::invoke_guest_proxy(&mut ctx, *func_index as u32, system, invoke, v_table, &mut buffer)?,
                None => return Err(WingsError::from_trap("Invalid system trait index").into()),
            }

            let mut result = [Value::I32(0)];
            let alloc_marshal_buffer = ctx.data().instance_funcs[index].alloc_marshal_buffer.clone();
            alloc_marshal_buffer.call(&mut ctx, &[Value::I32(buffer.len() as i32)], &mut result)?;
            let [Value::I32(pointer)] = result else { unreachable!() };
            memory.write(&mut ctx, pointer as usize, &buffer)?;

            Ok(())
        })
    }

    /// Creates the proxy invocation function for the host.
    fn create_proxy_index_func<C: AsContextMut<UserState = WingsHostInner<H>, Engine = H::Engine>>(mut ctx: C, index: usize) -> Func {
        Func::new(&mut ctx, FuncType::new([ValueType::I32; 2], [ValueType::I32]), move |mut ctx, params, results| unsafe {
            let [Value::I32(pointer), Value::I32(size)] = params else { unreachable!() };
            let len = *size as usize;
            let mut buffer = Vec::with_capacity(len);
            buffer.set_len(len);
            let memory = ctx.data().memories[index].clone();
            memory.read(&mut ctx, *pointer as usize, &mut buffer)?;

            let ty = DisjointExportedType::from(bincode::deserialize::<ExportedType>(&buffer)?);

            let data = ctx.data();
            results[0] = Value::I32(*data.system_trait_map.get(&ty).ok_or_else(|| WingsError::from_trap("Invalid proxy type"))? as i32);

            Ok(())
        })
    }

    /// Creates the event invocation function for the host.
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
                inner.ctx.as_ref().expect("Failed to get context").raise_event(on::GuestEvent { buffer, ty });
            }

            Ok(())
        })
    }

    /// Gets references to all internal Wings functions for the given instance.
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

    /// Gets a WASM function exported from the instance with the provided name and type.
    fn get_instance_func<C: AsContextMut>(mut ctx: C, instance: &Instance, name: &str, ty: FuncType) -> Result<Func, WingsError> {
        let Some(Extern::Func(func)) = instance.get_export(&mut ctx, name)
            else { return Err(WingsError::from_invalid_module(format!("Module missing intrinsic {name}"))) };

        if func.ty(&mut ctx) == ty {
            Ok(func)
        }
        else {
            Err(WingsError::from_invalid_module(format!("Module intrinsic {name} had invalid signature")))
        }
    }

    /// Raises an error that occurred during WASM processing.
    fn handle_error(&mut self, error: WingsError) {
        self.store.as_ref().expect("Failed to get store").data().ctx.as_ref().expect("Failed to get context").raise_event(on::Error { error });
        self.reset_store();
    }

    /// Invokes a proxy function within a guest instance.
    fn invoke_guest_proxy(mut ctx: &mut StoreContextMut<WingsHostInner<H>, H::Engine>, func_index: u32, system: u32, invoke: GuestPointer, v_table: GuestPointer, buffer: &mut Vec<u8>) -> Result<(), WingsError> {
        unsafe {
            let Some(system) = ctx.data().systems.get(system as usize).copied() else { return Err(WingsError::from_trap("Invalid system ID")) };
    
            let index = system.instance as usize;
            let instance_funcs = ctx.data().instance_funcs[index].clone();
            let memory = ctx.data().memories[index].clone();
    
            let mut result = [Value::I32(0)];
            instance_funcs.alloc_marshal_buffer.call(&mut ctx, &[Value::I32(buffer.len() as i32)], &mut result).map_err(WingsError::from_trap)?;
            let [Value::I32(pointer)] = result else { unreachable!() };
            memory.write(&mut ctx, pointer as usize, buffer).map_err(WingsError::from_trap)?;
    
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

    /// Attempts to instantiate the given image.
    fn try_instantiate(&mut self, image: &WingsImage) -> Result<(), WingsError> {
        let sorted_dependencies = self.sort_image_dependencies(image)?;
        let trait_map = self.fill_system_traits(image, &sorted_dependencies)?;
        self.fill_event_handlers(image, &sorted_dependencies);
        self.instantiate_modules(image)?;
        self.instantiate_systems(image, &sorted_dependencies, &trait_map)
    }

    /// Dispatches a strongly-typed event to all guest modules.
    fn dispatch_event<T: wings_marshal::exported_type::ExportType + Serialize + DeserializeOwned>(&mut self, event: &T) {
        if let Err(error) = bincode::serialize(&event).map_err(WingsError::Serialization).and_then(|x| self.dispatch_raw_event(&ExportedType::from(T::TYPE).into(), &x)) {
            self.handle_error(error);
        }
    }

    /// Dispatches a weakly-typed event to all guest modules.
    fn dispatch_guest_event(&mut self, event: &on::GuestEvent) {
        if let Err(error) = self.dispatch_raw_event(&event.ty, &event.buffer) {
            self.handle_error(error);
        }
    }

    /// Dispatches a serialized event to all guest modules.
    fn dispatch_raw_event(&mut self, ty: &DisjointExportedType, buffer: &[u8]) -> Result<(), WingsError> {
        if let Some(handler_list) = self.store.as_ref().expect("Failed to get store").data().guest_event_handlers.get(ty).cloned() {
            self.lower_raw_event_buffer(&handler_list, buffer)?;
            self.invoke_event_handlers(&handler_list)?;
        }

        Ok(())
    }

    /// Stores the provided event data into every relevant instance's event object buffer.
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

    /// Invokes all event handlers in the provided list.
    fn invoke_event_handlers(&mut self, handler_list: &EventHandlerList) -> Result<(), WingsError> {
        let mut store = self.store.as_mut().expect("Failed to get store");
        
        for handler in &handler_list.event_handlers {
            let system = store.data().systems[handler.system as usize];
            let invoke_func = store.data().instance_funcs[system.instance as usize].invoke_func_2.clone();
            invoke_func.call(&mut store, &[Value::I32(handler.invoke_func.into()), Value::I32(system.pointer.into()), Value::I32(handler.event_func.into())], &mut [Value::I32(0)]).map_err(WingsError::from_trap)?;
        }

        Ok(())
    }

    /// Gets a mapping from exported type to event raiser for this host.
    fn get_event_raisers() -> FxHashMap<DisjointExportedType, HostEventRaiserFunc<H>> {
        FxHashMap::from_iter(H::EVENTS.event_raisers.iter().map(|x| (ExportedType::from(x.ty).into(), x.raise)))
    }

    /// Gets a list of all exported system traits for this host.
    fn get_invokers() -> Vec<HostSystemTrait<H>> {
        Vec::from_iter(H::SYSTEMS.traits.iter().flat_map(|x| x.iter()).copied())
    }
}

impl<H: Host> std::fmt::Debug for WingsHost<H> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("WingsHost").field(&self.id).finish()
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
        /// A monotonic counter used to distinguish between hosts.
        static UNIQUE_ID: AtomicU64 = AtomicU64::new(0);

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

/// Defines the types and functionality associated with a Wings host.
pub trait Host: 'static + Sized {
    /// The set of events that should be passed across the WASM boundary.
    const EVENTS: Events<Self> = events();

    /// The set of systems that should be accessible across the WASM boundary.
    const SYSTEMS: Systems<Self> = systems();

    /// The WASM runtime that should be used.
    type Engine: wasm_runtime_layer::backend::WasmEngine;

    /// Creates the WASM runtime to use.
    fn create_engine(ctx: &mut GeeseContextHandle<WingsHost<Self>>) -> Self::Engine;

    /// Creates the set of imports that every guest module should receive.
    #[allow(unused)]
    fn create_imports(ctx: &mut GeeseContextHandle<WingsHost<Self>>, store: impl AsContextMut) -> Imports {
        Imports::new()
    }
}

/// Defines the function type used to raise an event on the host from the guest.
type HostEventRaiserFunc<H> = fn(&GeeseContextHandle<WingsHost<H>>, &[u8]) -> Result<(), WingsError>;

/// Describes an event which guests should be able to pass to hosts.
struct HostEventRaiser<H: Host> {
    /// The type of the host-visible event.
    pub ty: StaticExportedType,
    /// The function to use when raising the event to the host.
    pub raise: HostEventRaiserFunc<H>
}

impl<H: Host> Copy for HostEventRaiser<H> {}

impl<H: Host> Clone for HostEventRaiser<H> {
    fn clone(&self) -> Self {
        *self
    }
}

/// Defines the function type used to call a proxy method on a host system from the guest.
type HostInvokerFunc<H> = unsafe fn(&mut GeeseContextHandle<WingsHost<H>>, u32, *mut Vec<u8>) -> Result<(), WingsError>;

/// Describes a host trait object that guests should be able to call.
struct HostSystemTrait<H: Host> {
    /// The type of the trait to use.
    pub ty: StaticExportedType,
    /// The function to use when invoking proxy types.
    pub invoke: HostInvokerFunc<H>
}

impl<H: Host> Copy for HostSystemTrait<H> {}

impl<H: Host> Clone for HostSystemTrait<H> {
    fn clone(&self) -> Self {
        *self
    }
}

/// Describes an event handler on the guest.
#[derive(Clone, Debug)]
struct EventHandlerDescriptor {
    /// The index of the guest system.
    pub system: u32,
    /// A WASM pointer to the actual event handler function.
    pub event_func: GuestPointer,
    /// A WASM pointer to the function used to deserialize and invoke the event handler.
    pub invoke_func: GuestPointer,
}

/// A list of guest-side event handlers for a single event type.
#[derive(Clone, Debug, Default)]
struct EventHandlerList {
    /// The instances that should receive this event.
    pub instances: Vec<u32>,
    /// The handlers that should be invoked for this event.
    pub event_handlers: Vec<EventHandlerDescriptor>
}

/// A set of all internal functions exposed by an instance.
#[derive(Clone, Debug)]
struct InstanceFuncs {
    /// Allocates a certain amount of space for guest-host communication.
    pub alloc_marshal_buffer: Func,
    /// Copies an event object out of the marshal buffer.
    pub copy_event_object: Func,
    /// Indirectly invokes a function with one argument.
    pub invoke_func_1: Func,
    /// Indirectly invokes a function with two arguments.
    pub invoke_func_2: Func,
    /// Indirectly invokes a proxy function.
    pub invoke_proxy_func: Func
}

/// Holds the set of functions that describe the systems in a module.
struct MetadataFunctions {
    /// The functions that return system descriptors.
    pub describes: Vec<Func>,
    /// The functions that return instantiation groups.
    pub instantiates: Vec<Func>
}

/// Describes the systems and groups declared in a module.
#[derive(Debug)]
struct ModuleMetadata {
    /// Groups containing systems to instantiate.
    pub group_instantiates: Vec<InstantiateGroup>,
    /// A list of all systems contained in the module.
    pub system_descriptors: Vec<SystemDescriptor>
}

/// References a system declared within a module.
#[derive(Copy, Clone, Debug)]
struct SystemEntry {
    /// The ID of the module that has the system.
    pub module_id: u32,
    /// The ID of the system within the module.
    pub system_id: u32,
    /// The version of the module containing the system.
    pub version: Version
}

/// Holds an active system from an instance.
#[derive(Copy, Clone, Debug)]
struct SystemHolder {
    /// The instance in which the system resides.
    pub instance: u32,
    /// A pointer to the system in guest memory.
    pub pointer: GuestPointer,
    /// A pointer to the system's drop function in guest memory.
    pub drop_fn: GuestPointer
}

/// Describes how a proxy function call should occur.
#[derive(Copy, Clone, Debug)]
enum SystemTraitHolder {
    /// The call should invoke a guest function.
    Guest {
        /// The ID of the system to invoke.
        system: u32,
        /// The proxy function to invoke.
        invoke: GuestPointer,
        /// The V-table associated with the system trait object.
        v_table: GuestPointer
    },
    /// The call should invoke a host function.
    Host {
        /// The ID of the host trait on which to invoke the function.
        invoker: u32
    }
}

/// Stores the inner state for a host.
struct WingsHostInner<H: Host> {
    /// The context handle.
    ctx: Option<GeeseContextHandle<WingsHost<H>>>,
    /// The event raisers.
    event_raisers: FxHashMap<DisjointExportedType, HostEventRaiserFunc<H>>,
    /// The guest event handlers.
    guest_event_handlers: FxHashMap<DisjointExportedType, EventHandlerList>,
    /// The instance-specific functions.
    instance_funcs: Vec<InstanceFuncs>,
    /// The host trait objects.
    invokers: Vec<HostSystemTrait<H>>,
    /// The instance memories.
    memories: Vec<Memory>,
    /// The instantiated systems.
    systems: Vec<SystemHolder>,
    /// The system trait mappings.
    system_traits: Vec<SystemTraitHolder>,
    /// A mapping from system trait type to index.
    system_trait_map: FxHashMap<DisjointExportedType, u32>
}

/// Holds the inner state for a parsed module.
#[derive(Debug)]
struct WingsModuleInner {
    /// The ID of the host.
    pub host_id: u64,
    /// The underlying WASM module.
    pub module: Module,
    /// The module metadata.
    pub metadata: ModuleMetadata
}

/// Ensures that the linker finds an implementation for the associated guest function.
#[no_mangle]
extern "C" fn __wings_invoke_proxy_function(_: u32, _: u32, _: GuestPointer, _: u32) {
    unreachable!()
}

/// Ensures that the linker finds an implementation for the associated guest function.
#[no_mangle]
extern "C" fn __wings_proxy_index(_: GuestPointer, _: u32) -> u32 {
    unreachable!()
}

/// Ensures that the linker finds an implementation for the associated guest function.
#[no_mangle]
extern "C" fn __wings_raise_event(_: GuestPointer, _: u32) {
    unreachable!()
}

/// Events which this module raises.
pub mod on {
    use super::*;

    /// Indicates that an error happened within a WASM plugin.
    pub struct Error {
        /// The error that occurred.
        pub error: WingsError
    }

    /// Raised whenever a new WASM image is loaded.
    pub struct ImageReloaded;

    /// Stores a weakly-typed guest event, which should be raised to all
    /// guests at the appropriate time.
    #[derive(Debug)]
    pub(crate) struct GuestEvent {
        /// The buffer holding the event data.
        pub buffer: Vec<u8>,
        /// The type of event.
        pub ty: DisjointExportedType
    }
}

/// Private module for hiding implementation details.
mod private {
    use super::*;

    /// Marks an event type that can cross the WASM boundary.
    pub trait HostEvent: wings_marshal::exported_type::ExportType + Serialize + DeserializeOwned + Send + Sync {}

    impl<T: wings_marshal::exported_type::ExportType + Serialize + DeserializeOwned + Send + Sync> HostEvent for T {}
}