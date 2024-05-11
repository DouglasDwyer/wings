pub use bincode;
pub use serde;

use crate::*;
pub use wings_macro::crate_version;
pub use wings_marshal::exported_type::{ExportType, StaticExportedType, SystemTrait, Version};
pub use wings_marshal::{
    write_to_marshal_buffer, GuestPointer, InstantiateGroup, Marshal, Proxyable,
    SectionedBufferReader, SectionedBufferWriter,
};

/// Creates a system descriptor for the provided system.
pub fn system_descriptor_for<S: WingsSystem>() -> SystemDescriptor {
    let ty = S::TYPE.into();
    let new_fn = (create_system::<S> as *const ()).into();
    let drop_fn = (drop_system::<S> as *const ()).into();
    let dependencies = S::DEPENDENCIES
        .inner
        .into_iter()
        .map(|x| x.system_trait.into())
        .collect();
    let event_handlers = S::EVENT_HANDLERS
        .inner
        .into_iter()
        .map(EventHandler::from)
        .collect();
    let traits = Vec::new();

    SystemDescriptor {
        ty,
        new_func: new_fn,
        drop_func: drop_fn,
        dependencies,
        event_handlers,
        traits,
    }
}

/// Adds the given system trait to the provided descriptor.
pub fn add_system_descriptor_trait<S: WingsSystem, W: SystemTrait + ?Sized>(
    descriptor: &mut SystemDescriptor,
    v_table: GuestPointer,
    invoke: unsafe fn(FatGuestPointer, u32, *mut Vec<u8>),
) {
    descriptor.traits.push(SystemTraitDescriptor {
        invoke: GuestPointer::from(invoke as *const ()),
        ty: W::TYPE.into(),
        v_table,
    });
}

/// Allocates space for the given system type, creates it, and then
/// returns a pointer to it. The system dependencies are read from the
/// marshal buffer.
///
/// # Safety
///
/// For this function call to be sound, no other references may exist to
/// the marshal buffer.
unsafe fn create_system<S: WingsSystem>(_: *const ()) -> *mut RefCell<S> {
    let raw_dependencies =
        bincode::deserialize::<Vec<DependencyReference>>(&*std::ptr::addr_of!(MARSHAL_BUFFER))
            .expect("Failed to deserialize dependencies");

    assert!(
        raw_dependencies.len() == S::DEPENDENCIES.inner.iter().count(),
        "Dependencies were of incorrect length"
    );
    let mut dependencies = Vec::with_capacity(raw_dependencies.len());
    for (raw, dependency) in raw_dependencies
        .into_iter()
        .zip(S::DEPENDENCIES.inner.iter())
    {
        dependencies.push((
            (dependency.ty)(),
            match raw {
                DependencyReference::Local(pointer) => DependencyHolder::Local(pointer),
                DependencyReference::Remote(id) => {
                    DependencyHolder::Remote((dependency.proxy_func)(id))
                }
            },
        ));
    }

    Box::leak(Box::new(RefCell::new(S::new(WingsContextHandle {
        dependencies,
        marker: PhantomData,
    }))))
}

/// Drops the system at the given pointer and deallocates it.
///
/// # Safety
///
/// For this function call to be sound, the provided pointer
/// must be valid and must not be referenced anywhere else.
unsafe fn drop_system<S: WingsSystem>(pointer: *mut RefCell<S>) -> GuestPointer {
    drop(Box::from_raw(pointer));
    GuestPointer::default()
}

impl From<&StaticEventHandler> for EventHandler {
    fn from(value: &StaticEventHandler) -> Self {
        Self {
            ty: ExportedType::from(value.ty).into(),
            event_func: (value.event_func as *const ()).into(),
            invoke_func: (value.invoke_func as *const ()).into(),
        }
    }
}

/// Gets the proxy index of the system from the host. Assumes that the proxy is loaded at this point.
///
/// # Safety
///
/// For this function call to be sound, the marshal buffer currently
/// must not be referenced anywhere else.
pub unsafe fn proxy_index<T: SystemTrait + ?Sized>() -> u32 {
    let (pointer, size) = {
        let mut buffer = &mut *std::ptr::addr_of_mut!(MARSHAL_BUFFER);
        buffer.clear();
        bincode::serialize_into(&mut buffer, &T::TYPE).expect("Failed to serialize type");
        (buffer.as_ptr().into(), buffer.len() as u32)
    };
    __wings_proxy_index(pointer, size)
}

extern "C" {
    /// Instructs the host to invoke a remote proxy function.
    pub fn __wings_invoke_proxy_function(
        id: u32,
        func_index: u32,
        pointer: GuestPointer,
        size: u32,
    );

    /// Gets the index of a remote proxy.
    pub fn __wings_proxy_index(pointer: GuestPointer, size: u32) -> u32;

    /// Instructs the host to raise an event.
    pub(crate) fn __wings_raise_event(pointer: GuestPointer, size: u32);
}
