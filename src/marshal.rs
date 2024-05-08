pub use bincode;
pub use serde;

use crate::*;
pub use wings_macro::crate_version;
pub use wings_marshal::{GuestPointer, InstantiateGroup, MarshalAs, Proxyable, SectionedBufferReader, SectionedBufferWriter, write_to_marshal_buffer};
pub use wings_marshal::exported_type::{ExportType, StaticExportedType, SystemTrait, Version};

pub fn system_descriptor_for<S: WingsSystem>() -> SystemDescriptor {
    let ty = S::TYPE.into();
    let new_fn = (create_system::<S> as *const ()).into();
    let drop_fn = (drop_system::<S> as *const()).into();
    let dependencies = S::DEPENDENCIES.inner.into_iter().map(|x| x.system_trait.into()).collect();
    let event_handlers = S::EVENT_HANDLERS.inner.into_iter().map(EventHandler::from).collect();
    let traits = Vec::new();

    SystemDescriptor {
        ty,
        new_func: new_fn,
        drop_func: drop_fn,
        dependencies,
        event_handlers,
        traits
    }
}

pub fn add_system_descriptor_trait<S: WingsSystem, W: SystemTrait + ?Sized>(descriptor: &mut SystemDescriptor, v_table: GuestPointer, invoke: unsafe fn(FatGuestPointer, u32, *mut Vec<u8>)) {
    descriptor.traits.push(SystemTraitDescriptor {
        invoke: GuestPointer::from(invoke as *const ()),
        ty: W::TYPE.into(),
        v_table
    });
}

unsafe fn create_system<S: WingsSystem>(_: *const ()) -> *mut RefCell<S> {
    let raw_dependencies = bincode::deserialize::<Vec<DependencyReference>>(&*std::ptr::addr_of!(MARSHAL_BUFFER))
        .expect("Failed to deserialize dependencies");

    assert!(raw_dependencies.len() == S::DEPENDENCIES.inner.iter().count(), "Dependencies were of incorrect length");
    let mut dependencies = Vec::with_capacity(raw_dependencies.len());
    for (raw, dependency) in raw_dependencies.into_iter().zip(S::DEPENDENCIES.inner.iter()) {
        dependencies.push(((dependency.ty)(), match raw {
            DependencyReference::Local(pointer) => DependencyHolder::Local(pointer),
            DependencyReference::Remote(id) => DependencyHolder::Remote((dependency.proxy_func)(id))
        }));
    }

    Box::leak(Box::new(RefCell::new(S::new(WingsContextHandle {
        dependencies,
        marker: PhantomData
    }))))
}

unsafe fn drop_system<S: WingsSystem>(pointer: *mut RefCell<S>) -> GuestPointer {
    drop(Box::from_raw(pointer));
    GuestPointer::default()
}

impl From<&StaticEventHandler> for EventHandler {
    fn from(value: &StaticEventHandler) -> Self {
        Self {
            ty: ExportedType::from(value.ty).into(),
            event_func: (value.event_func as *const ()).into(),
            invoke_func: (value.invoke_func as *const ()).into()
        }
    }
}

extern "C" {
    pub fn __wings_invoke_proxy_function(id: u32, func_index: u32, pointer: GuestPointer, size: u32);
    pub(crate) fn __wings_raise_event(pointer: GuestPointer, size: u32);
}