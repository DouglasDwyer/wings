#![deny(missing_docs)]
#![deny(clippy::missing_docs_in_private_items)]

//! # wings
//!
//! Wings is a WASM plugin system for Rust. It integrates directly with the [Geese event library](https://github.com/DouglasDwyer/geese), allowing
//! plugins to seamlessly communicate with one another and the host using events and systems. The following features are supported:
//!
//! - *Sending events to the host*: Guests can send and receive strongly-typed events to the host, which will be injected into the host's Geese context.
//! - *Sending events to other guest systems*: Events will propagate between guest systems and from the host's Geese context into WASM modules.
//! - *Accessing systems of the host*: Hosts can expose Geese systems as trait objects, which can be called directly from the guest. Function arguments and return types are serialized across the WASM boundary.
//! - *Accessing systems of other plugins*: Plugins can export their systems as trait objects, which other plugins can call. Just like regular Geese, Wings guarantees that every system is a singleton - if separate plugins are loaded that share a dependency, both plugins will access the same dependency instance.
//! - *Automatically resolving plugin dependencies/versions*: Plugins are built as Rust crates with all transitive dependencies included, so there's no need to worry about creating a dependency resolver. Whenever Wings loads a system, it will choose the newest Semver-compatible version from the set of loaded plugins.
//!
//! ### Example
//!
//! The following is an abridged example of how to use Wings. The complete code may be found in the [`wings_example` folder](/wings_example/).
//!
//! Wings allows for defining traits like the following, which can be shared between the code of hosts and between plugins:
//!
//! ```rust
//! // Define a system that the host will expose.
//! // This can also be used to expose guest systems to other guest systems.
//! #[system_trait(host)]
//! pub trait ExampleSystem: 'static {
//!     // Prints a value to the console.
//!     fn print(&self, value: &str);
//! }
//! ```
//!
//! Then, the system may be referenced as a trait object from WASM:
//!
//! ```rust
//! // Define a Wings system that will run within WASM
//! #[export_system]
//! pub struct PluginSystem;
//!
//! impl WingsSystem for PluginSystem {
//!     // Declare a dependency on the exported host system
//!     const DEPENDENCIES: Dependencies = dependencies()
//!         .with::<dyn ExampleSystem>();
//!
//!     // Invoked when the plugin is created
//!     fn new(mut ctx: WingsContextHandle<Self>) -> Self {
//!         // Get the system from WASM and invoke its function
//!         ctx.get::<dyn ExampleSystem>().print(&format!("Hello from WASM!"));
//!         Self
//!     }
//! }
//! ```
//!
//! Finally, the system may be implemented on the host and exposed to plugins:
//!
//! ```rust
//! // Define a host type
//! pub struct TestHost;
//!
//! impl Host for TestHost {
//!     // Declare the systems that should be exported to WASM,
//!     // and the traits under which they should be exported
//!     const SYSTEMS: Systems<Self> = systems()
//!         .with::<ExampleSystemImpl>(traits()
//!             .with::<dyn example_host_system::ExampleSystem>());
//!
//!     ...
//! }
//!
//! // Declare an implementation for the WASM-exported system
//! pub struct ExampleSystemImpl;
//!
//! impl ExampleSystem for ExampleSystemImpl {
//!     fn print(&self, value: &str) {
//!         println!("Plugin says '{value}'");
//!     }
//! }
//! ```
//!
//! In general, anything possible with vanilla Geese is also possible with Wings. See the [example](/wings_example/) for demonstration of more functionality.

use crate::marshal::*;
use crate::private::*;
use const_list::*;
use serde::de::*;
use serde::*;
use std::any::*;
use std::cell::*;
use std::marker::*;
use std::mem::*;
use std::ops::*;
pub use wings_macro::{export_system, export_type, instantiate_systems, system_trait};
use wings_marshal::exported_type::*;
pub use wings_marshal::exported_type::{ExportedType, StaticExportedType};
pub use wings_marshal::WingsError;
use wings_marshal::*;

/// Re-exports types from [`wings_marshal`] for use by the system macros.
#[doc(hidden)]
pub mod marshal;

/// Denotes a list of system dependencies.
#[derive(Copy, Clone, Debug)]
pub struct Dependencies {
    /// The inner list of dependencies.
    inner: ConstList<'static, DependencyType>,
}

impl Dependencies {
    /// Creates a new, empty list of dependencies.
    #[inline(always)]
    const fn new() -> Self {
        Self {
            inner: ConstList::new(),
        }
    }

    /// Adds the given type to the dependency list, returning the modified list.
    pub const fn with<S: SystemTrait + ?Sized>(&'static self) -> Self {
        Self {
            inner: self.inner.push(DependencyType::new::<S>()),
        }
    }
}

/// Creates a new, empty list of dependencies.
#[inline(always)]
pub const fn dependencies() -> Dependencies {
    Dependencies::new()
}

/// Denotes a list of system methods that respond to events.
pub struct EventHandlers<S: WingsSystem> {
    /// The inner list of event handlers.
    inner: ConstList<'static, StaticEventHandler>,
    /// Phantom data to mark the system as used.
    data: PhantomData<fn(S)>,
}

impl<S: WingsSystem> EventHandlers<S> {
    /// Creates a new, empty list of event handlers.
    #[inline(always)]
    const fn new() -> Self {
        Self {
            inner: ConstList::new(),
            data: PhantomData,
        }
    }

    /// Adds the given event handler to the list, returning the modified list.
    pub const fn with<Q: MutableRef<S>, T: ExportEvent>(&'static self, handler: fn(Q, &T)) -> Self {
        unsafe {
            Self {
                inner: self.inner.push(StaticEventHandler {
                    ty: T::TYPE,
                    invoke_func: transmute(Self::invoke_handler::<T> as *const ()),
                    event_func: transmute(handler),
                }),
                data: PhantomData,
            }
        }
    }

    /// Invokes handler on the provided system reference, reading the event object
    /// from the event object static.
    ///
    /// # Safety
    ///
    /// For this function call to be sound, no other references to the event object
    /// static may exist.
    unsafe fn invoke_handler<T: ExportEvent>(
        system: &mut RefCell<S>,
        handler: fn(&mut S, &T),
    ) -> GuestPointer {
        let event_object = &mut *std::ptr::addr_of_mut!(EVENT_OBJECT);

        let object = if let Some(object) = event_object.objects.iter().find(|x| x.is::<T>()) {
            object
        } else {
            event_object.objects.push(Box::new(
                bincode::deserialize::<T>(&event_object.buffer)
                    .expect("Failed to deserialize event object"),
            ));
            event_object
                .objects
                .last()
                .expect("Failed to get last object in event objects list")
        };

        handler(
            &mut system.borrow_mut(),
            object.downcast_ref().unwrap_unchecked(),
        );
        GuestPointer::default()
    }
}

impl<S: WingsSystem> Copy for EventHandlers<S> {}

impl<S: WingsSystem> Clone for EventHandlers<S> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<S: WingsSystem> std::fmt::Debug for EventHandlers<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EventHandlers").finish()
    }
}

/// Creates a new, empty list of event handlers.
#[inline(always)]
pub const fn event_handlers<S: WingsSystem>() -> EventHandlers<S> {
    EventHandlers::new()
}

/// Represents a system-specific handle to a Wings context.
pub struct WingsContextHandle<S: WingsSystem> {
    /// A list of references to all system dependencies.
    dependencies: Vec<(TypeId, DependencyHolder)>,
    /// Phantom data to mark the system as used.
    marker: PhantomData<fn(S)>,
}

impl<S: WingsSystem> WingsContextHandle<S> {
    /// Raises the specified event.
    #[inline(always)]
    pub fn raise_event<T: ExportEvent>(&self, event: T) {
        unsafe {
            let mut section_writer = SectionedBufferWriter::from_marshal_buffer();
            bincode::serialize_into(section_writer.section(), &T::TYPE)
                .expect("Failed to serialize event type.");
            bincode::serialize_into(section_writer.section(), &event)
                .expect("Failed to serialize event.");
            let buffer = section_writer.into_inner();
            __wings_raise_event(buffer.as_ptr().into(), buffer.len() as u32);
        }
    }

    /// Obtains the specified system dependency.
    #[inline(always)]
    pub fn get<T: SystemTrait + ?Sized>(&self) -> SystemRef<T> {
        unsafe {
            let inner = match &self
                .dependencies
                .iter()
                .find(|(x, _)| *x == TypeId::of::<T>())
                .unwrap_or_else(|| {
                    panic!(
                        "System {} was not a dependency of {}",
                        type_name::<T>(),
                        type_name::<S>()
                    )
                })
                .1
            {
                DependencyHolder::Local(pointer) => (*pointer.cast::<RefCell<T>>()).borrow(),
                DependencyHolder::Remote(proxy) => Ref::map(
                    proxy
                        .downcast_ref::<RefCell<T::Proxy>>()
                        .unwrap_unchecked()
                        .borrow(),
                    |x| &**x,
                ),
            };

            SystemRef { inner }
        }
    }

    /// Mutably obtains the specified system dependency.
    #[inline(always)]
    pub fn get_mut<T: SystemTrait + ?Sized>(&mut self) -> SystemRefMut<T> {
        unsafe {
            let inner = match &self
                .dependencies
                .iter()
                .find(|(x, _)| *x == TypeId::of::<T>())
                .unwrap_or_else(|| {
                    panic!(
                        "System {} was not a dependency of {}",
                        type_name::<T>(),
                        type_name::<S>()
                    )
                })
                .1
            {
                DependencyHolder::Local(pointer) => (*pointer.cast::<RefCell<T>>()).borrow_mut(),
                DependencyHolder::Remote(proxy) => RefMut::map(
                    proxy
                        .downcast_ref::<RefCell<T::Proxy>>()
                        .unwrap_unchecked()
                        .borrow_mut(),
                    |x| &mut **x,
                ),
            };

            SystemRefMut { inner }
        }
    }
}

impl<S: WingsSystem> std::fmt::Debug for WingsContextHandle<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("WingsContextHandle").finish()
    }
}

/// Represents a collection of event handlers with internal state.
pub trait WingsSystem: ExportType + Sized {
    /// The set of dependencies that this system has.
    const DEPENDENCIES: Dependencies = dependencies();

    /// The set of events to which this system responds.
    const EVENT_HANDLERS: EventHandlers<Self> = event_handlers();

    /// Creates a new instance of the system for the given system handle.
    fn new(ctx: WingsContextHandle<Self>) -> Self;
}

/// Represents an immutable reference to a system.
#[derive(Debug)]
pub struct SystemRef<'a, S: ?Sized> {
    /// The inner reference.
    inner: Ref<'a, S>,
}

impl<'a, S: ?Sized> Deref for SystemRef<'a, S> {
    type Target = S;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// Represents a mutable reference to a system.
#[derive(Debug)]
pub struct SystemRefMut<'a, S: ?Sized> {
    /// The inner reference.
    inner: RefMut<'a, S>,
}

impl<'a, S: ?Sized> Deref for SystemRefMut<'a, S> {
    type Target = S;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a, S: ?Sized> DerefMut for SystemRefMut<'a, S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

/// Holds a reference to a dependency trait object.
enum DependencyHolder {
    /// The object exists locally within the WASM instance.
    Local(FatGuestPointer),
    /// The object exists remotely and should be accessed via the given proxy.
    Remote(Box<dyn Any>),
}

/// Holds information about the dependency of a system.
#[derive(Copy, Clone, Debug)]
struct DependencyType {
    /// The type of the trait being accessed.
    pub system_trait: StaticExportedType,
    /// The type ID of the trait.
    pub ty: fn() -> TypeId,
    /// A function which creates a proxy object from the provided ID.
    pub proxy_func: fn(u32) -> Box<dyn Any>,
}

impl DependencyType {
    /// Creates a new `DependencyType` for the provided system trait.
    pub const fn new<T: SystemTrait + ?Sized>() -> Self {
        Self {
            system_trait: T::TYPE,
            ty: TypeId::of::<T>,
            proxy_func: |x| Box::new(RefCell::new(T::create_proxy(x))),
        }
    }
}

/// Holds deserialized versions of the most recent event.
static mut EVENT_OBJECT: EventObject = EventObject::new();

/// Holds the data for an event, as well as cached deserialized copies.
struct EventObject {
    /// The serialized event data.
    buffer: Vec<u8>,
    /// The deserialized copies.
    objects: Vec<Box<dyn Any>>,
}

impl EventObject {
    /// Creates a new, empty event object.
    pub const fn new() -> Self {
        Self {
            buffer: Vec::new(),
            objects: Vec::new(),
        }
    }
}

/// Describes an event handler that should be registered with the host.
#[derive(Copy, Clone, Debug)]
struct StaticEventHandler {
    /// The type of the event to process.
    pub ty: StaticExportedType,
    /// The actual event handler function.
    pub event_func: fn(*mut (), *const ()),
    /// A function that should be used to deserialize the event
    /// and invoke the handler.
    pub invoke_func: unsafe fn(*mut (), fn(*mut (), *const ())),
}

/// Resizes the marshal buffer to guarantee it reaches a certain minimum size.
/// Returns a pointer to the beginning of the buffer.
#[allow(clippy::uninit_vec)]
#[no_mangle]
unsafe extern "C" fn __wings_alloc_marshal_buffer(size: u32) -> GuestPointer {
    let buffer = &mut *std::ptr::addr_of_mut!(MARSHAL_BUFFER);
    let to_reserve = size as usize;
    buffer.clear();
    buffer.reserve(to_reserve);
    buffer.set_len(to_reserve);
    buffer.as_mut_ptr().into()
}

/// Copies the contents of the marshal buffer into the event object structure.
#[allow(clippy::uninit_vec)]
#[no_mangle]
unsafe extern "C" fn __wings_copy_event_object() {
    let event_object = &mut *std::ptr::addr_of_mut!(EVENT_OBJECT);
    let marshal_buffer = &*std::ptr::addr_of!(MARSHAL_BUFFER);
    event_object.buffer.clear();
    event_object.objects.clear();

    event_object.buffer.reserve(marshal_buffer.len());
    event_object.buffer.set_len(marshal_buffer.len());
    event_object.buffer.copy_from_slice(marshal_buffer);
}

/// Invokes the provided function with the given argument.
#[allow(improper_ctypes_definitions)]
#[no_mangle]
unsafe extern "C" fn __wings_invoke_func_1(
    func: fn(GuestPointer) -> GuestPointer,
    arg: GuestPointer,
) -> GuestPointer {
    func(arg)
}

/// Invokes the provided function with the given two arguments.
#[allow(improper_ctypes_definitions)]
#[no_mangle]
unsafe extern "C" fn __wings_invoke_func_2(
    func: fn(GuestPointer, GuestPointer) -> GuestPointer,
    arg_0: GuestPointer,
    arg_1: GuestPointer,
) -> GuestPointer {
    func(arg_0, arg_1)
}

/// Invokes a proxy function, passing in the given arguments as well as a reference to the marshal buffer.
#[allow(improper_ctypes_definitions)]
#[no_mangle]
unsafe extern "C" fn __wings_invoke_proxy_func(
    func: unsafe fn(FatGuestPointer, u32, *mut Vec<u8>),
    pointer: GuestPointer,
    metadata: GuestPointer,
    func_index: u32,
) -> u32 {
    func(
        FatGuestPointer::new(pointer, metadata),
        func_index,
        std::ptr::addr_of_mut!(MARSHAL_BUFFER),
    );
    (*std::ptr::addr_of_mut!(MARSHAL_BUFFER)).len() as u32
}

/// Hides traits from being externally visible.
mod private {
    use super::*;

    /// Trait that marks a type as a mutable reference. This is used to
    /// hide mutable references from `const` functions, so that they may
    /// be manipulated in a `const` context.
    pub trait MutableRef<T> {}

    impl<'a, T> MutableRef<T> for &'a mut T {}

    /// Represents an exported type that may be used as an event.
    pub trait ExportEvent: ExportType + Serialize + DeserializeOwned {}

    impl<T: ExportType + Serialize + DeserializeOwned> ExportEvent for T {}
}
