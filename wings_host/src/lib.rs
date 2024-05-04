#![cfg_attr(unstable, feature(unsize))]

use const_list::*;
use geese::*;
use private::*;
use serde::*;
use serde::de::*;
use std::mem::*;
use std::sync::*;
use std::sync::atomic::*;
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
    inner: ConstList<'static, HostSystem<H>>
}

#[cfg(unstable)]
impl<H: Host> Systems<H> {
    pub const fn with<S: GeeseSystem + std::marker::Unsize<T>, T: Proxyable + SystemTrait + ?Sized>(&'static self) -> Self {
        let dependency = Dependency::new::<S>();
        let invoke = Self::invoke::<S, T>;

        Self {
            inner: self.inner.push(HostSystem { dependency, invoke })
        }
    }

    fn invoke<S: GeeseSystem + std::marker::Unsize<T>, T: Proxyable + SystemTrait + ?Sized>(ctx: &mut GeeseContextHandle<WingsHost<H>>, func_index: u32, buffer: &mut Vec<u8>) -> Result<(), WingsError> {
        (&mut *ctx.get_mut::<S>() as &mut T).invoke(func_index, buffer)
    }
}

#[cfg(not(unstable))]
impl<H: Host> Systems<H> {
    pub const fn with<S: GeeseSystem + AsMut<T>, T: Proxyable + SystemTrait + ?Sized>(&'static self) -> Self {
        let dependency = Dependency::new::<S>();
        let invoke = Self::invoke::<S, T>;

        Self {
            inner: self.inner.push(HostSystem { dependency, invoke })
        }
    }

    fn invoke<S: GeeseSystem + AsMut<T>, T: Proxyable + SystemTrait + ?Sized>(ctx: &mut GeeseContextHandle<WingsHost<H>>, func_index: u32, buffer: &mut Vec<u8>) -> Result<(), WingsError> {
        ctx.get_mut::<S>().as_mut().invoke(func_index, buffer)
    }
}

pub const fn systems<H: Host>() -> Systems<H> {
    Systems {
        inner: ConstList::new()
    }
}

#[derive(Clone, Debug)]
pub struct WingsModule(Arc<WingsModuleInner>);

impl WingsModule {

}

pub struct WingsHost<H: Host> {
    engine: Engine<H::Engine>,
    id: u64,
    store: wasm_runtime_layer::Store<WingsHostInner<H>, H::Engine>
}

impl<H: Host> WingsHost<H> {
    pub fn load(&self, package: impl std::io::Read) -> Result<WingsModule, WingsError> {
        let module = Module::new(&self.engine, package).map_err(WingsError::from_invalid_module)?;
        let systems = self.load_system_descriptors(&module)?;

        Ok(WingsModule(Arc::new(WingsModuleInner {
            host_id: self.id,
            module,
            systems
        })))
    }

    fn load_system_descriptors(&self, module: &Module) -> Result<Vec<SystemDescriptor>, WingsError> {
        unsafe {
            let mut store = wasm_runtime_layer::Store::new(&self.engine, ());
            let dummy_imports = self.create_dummy_imports(&mut store, module)?;
    
            let instance = Instance::new(&mut store, module, &dummy_imports).map_err(WingsError::from_invalid_module)?;
            let describe_functions = self.describe_functions(&mut store, &instance);

            let Extern::Memory(memory) = instance.get_export(&mut store, "memory")
                .ok_or_else(|| WingsError::from_invalid_module("Module missing memory"))?
                else { return Err(WingsError::from_invalid_module("Memory was not of correct extern type")) };
    
            let mut result = Vec::with_capacity(describe_functions.len());
            for func in describe_functions {
                let mut result_pointer_value = [Value::I32(0)];
                func.call(&mut store, &[], &mut result_pointer_value).map_err(WingsError::from_invalid_module)?;
                let Value::I32(result_pointer) = result_pointer_value[0] else { return Err(WingsError::from_invalid_module("Describe function returned wrong type")) };
                let result_offset = result_pointer as usize;
                let mut total_len = [0; size_of::<u32>()];
                memory.read(&mut store, result_offset, &mut total_len).map_err(WingsError::from_invalid_module)?;
    
                let data_len = u32::from_le_bytes(total_len) as usize;
                let mut data = Vec::with_capacity(data_len);
                data.set_len(data_len);
                memory.read(&mut store, result_offset + total_len.len(), &mut data).map_err(WingsError::from_invalid_module)?;

                result.push(bincode::deserialize(&data).map_err(WingsError::from_invalid_module)?);
            }

            Ok(result)
        }
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

    fn describe_functions<C: AsContextMut>(&self, mut ctx: C, instance: &Instance) -> Vec<Func> {
        let mut result = Vec::new();
        for export in instance.exports(&mut ctx) {
            let Extern::Func(func) = export.value else { continue };
            if export.name.starts_with("__wings_describe_") {
                result.push(func);
            }
        }
        result
    }

    fn dispatch_event<T: wings::ExportType + Serialize + DeserializeOwned>(&mut self, event: &T) {
        todo!("Dispatch it")
    }
}

impl<H: Host> GeeseSystem for WingsHost<H> {
    fn new(ctx: GeeseContextHandle<Self>) -> Self {
        const UNIQUE_ID: AtomicU64 = AtomicU64::new(0);

        let engine = Engine::new(H::create_engine());
        let id = UNIQUE_ID.fetch_add(1, Ordering::Relaxed);
        let store = wasm_runtime_layer::Store::new(&engine, WingsHostInner {
            ctx
        });

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

struct HostEventRaiser<H: Host> {
    pub ty: StaticExportedType,
    pub raise: fn(&GeeseContextHandle<WingsHost<H>>, &[u8]) -> Result<(), WingsError>
}

struct HostSystem<H: Host> {
    pub dependency: Dependency,
    pub invoke: fn(&mut GeeseContextHandle<WingsHost<H>>, u32, &mut Vec<u8>) -> Result<(), WingsError>
}

struct WingsHostInner<H: Host> {
    ctx: GeeseContextHandle<WingsHost<H>>
}

#[derive(Debug)]
struct WingsModuleInner {
    pub host_id: u64,
    pub module: Module,
    pub systems: Vec<SystemDescriptor>
}

mod private {
    use super::*;

    pub trait HostEvent: wings::ExportType + Serialize + DeserializeOwned + Send + Sync {}
    impl<T: wings::ExportType + Serialize + DeserializeOwned + Send + Sync> HostEvent for T {}
}