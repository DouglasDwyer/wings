use const_list::*;
pub use crate::exported_type::*;
use crate::marshal::*;
use crate::private::*;
use serde::*;
use serde::de::*;
use std::any::*;
use std::cell::*;
use std::marker::*;
use std::mem::*;
use std::ops::*;
use std::rc::*;
use thiserror::*;
pub use wings_macro::*;

mod exported_type;
pub mod marshal;

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

pub struct EventHandlers<S: WingsSystem> {
    // The inner list of event handlers.
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
                inner: self.inner.push(StaticEventHandler { ty: T::TYPE, invoke_func: transmute(Self::invoke_handler::<T> as *const ()), event_func: transmute(handler) }),
                data: PhantomData
            }
        }
    }

    unsafe fn invoke_handler<T: ExportEvent>(system: &mut S, handler: fn(&mut S, &T)) -> GuestPointer {
        let event_object = &mut *std::ptr::addr_of_mut!(EVENT_OBJECT);

        let object = if let Some(object) = event_object.objects.iter().find(|x| x.is::<T>()) {
            object
        }
        else {
            event_object.objects.push(Box::new(bincode::deserialize::<T>(&event_object.buffer).expect("Failed to deserialize event object")));
            event_object.objects.last().expect("Failed to get last object in event objects list")
        };

        handler(system, object.downcast_ref().unwrap());
        GuestPointer::default()
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
    marker: PhantomData<fn(S)>
}

impl<S: WingsSystem> WingsContextHandle<S> {
    /// Raises the specified event.
    #[inline(always)]
    pub fn raise_event<T: ExportEvent>(&self, event: T) {
        unsafe {
            let mut section_writer = SectionedBufferWriter::from_marshal_buffer();
            bincode::serialize_into(section_writer.section(), &T::TYPE).expect("Failed to serialize event type.");
            bincode::serialize_into(section_writer.section(), &event).expect("Failed to serialize event.");
            let buffer = section_writer.into_inner();
            __wings_raise_event(buffer.as_ptr().into(), buffer.len() as u32);
        }
    }

    /// Obtains the specified system dependency.
    #[inline(always)]
    pub fn get<T: SystemTrait + ?Sized>(&self) -> SystemRef<T> {
        unsafe {
            let inner = match &self.dependencies.iter().find(|(x, _)| *x == TypeId::of::<T>())
            .unwrap_or_else(|| panic!("System {} was not a dependency of {}", type_name::<T>(), type_name::<S>())).1 {
                DependencyHolder::Local(pointer) => (*pointer.cast::<RefCell<T>>()).borrow(),
                DependencyHolder::Remote(proxy) => Ref::map(proxy.downcast_ref::<RefCell<T::Proxy>>().unwrap_unchecked().borrow(), |x| &**x),
            };

            SystemRef {
                inner
            }
        }
    }

    /// Mutably obtains the specified system dependency.
    #[inline(always)]
    pub fn get_mut<T: SystemTrait + ?Sized>(&mut self) -> SystemRefMut<T> {
        unsafe {
            let inner = match &self.dependencies.iter().find(|(x, _)| *x == TypeId::of::<T>())
            .unwrap_or_else(|| panic!("System {} was not a dependency of {}", type_name::<T>(), type_name::<S>())).1 {
                DependencyHolder::Local(pointer) => (*pointer.cast::<RefCell<T>>()).borrow_mut(),
                DependencyHolder::Remote(proxy) => RefMut::map(proxy.downcast_ref::<RefCell<T::Proxy>>().unwrap_unchecked().borrow_mut(), |x| &mut **x),
            };

            SystemRefMut {
                inner
            }
        }
    }
}

pub trait WingsSystem: ExportType + Sized {
    /// The set of dependencies that this system has.
    const DEPENDENCIES: Dependencies = dependencies();

    /// The set of events to which this system responds.
    const EVENT_HANDLERS: EventHandlers<Self> = event_handlers();

    /// Creates a new instance of the system for the given system handle.
    fn new(ctx: WingsContextHandle<Self>) -> Self;
}

pub struct SystemRef<'a, S: ?Sized> {
    inner: Ref<'a, S>
}

impl<'a, S: ?Sized> Deref for SystemRef<'a, S> {
    type Target = S;

    fn deref(&self) -> &Self::Target {
        &*self.inner
    }
}

pub struct SystemRefMut<'a, S: ?Sized> {
    inner: RefMut<'a, S>
}

impl<'a, S: ?Sized> Deref for SystemRefMut<'a, S> {
    type Target = S;

    fn deref(&self) -> &Self::Target {
        &*self.inner
    }
}

impl<'a, S: ?Sized> DerefMut for SystemRefMut<'a, S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.inner
    }
}

enum DependencyHolder {
    Local(FatGuestPointer),
    Remote(Box<dyn Any>)
}

#[derive(Copy, Clone, Debug)]
struct DependencyType {
    pub exported_system: StaticExportedType,
    pub system_trait: StaticExportedType,
    pub ty: fn() -> TypeId,
    pub proxy_func: fn(u32) -> Box<dyn Any>
}

impl DependencyType {
    pub const fn new<T: SystemTrait + ?Sized>() -> Self {
        Self {
            exported_system: T::SYSTEM_TYPE,
            system_trait: T::TYPE,
            ty: TypeId::of::<T>,
            proxy_func: |x| Box::new(RefCell::new(T::create_proxy(x)))
        }
    }
}

static mut EVENT_OBJECT: EventObject = EventObject::new();

struct EventObject {
    buffer: Vec<u8>,
    objects: Vec<Box<dyn Any>>
}

impl EventObject {
    pub const fn new() -> Self {
        Self {
            buffer: Vec::new(),
            objects: Vec::new()
        }
    }
}

struct StaticEventHandler {
    pub ty: StaticExportedType,
    pub event_func: fn(*mut (), *const ()),
    pub invoke_func: unsafe fn(*mut (), fn(*mut (), *const ())),
}

#[derive(Debug, Error)]
pub enum WingsError {
    #[error("There was a cyclic dependency between systems")]
    CyclicDependency(),
    #[error("A function call was invalid")]
    InvalidFunction(),
    #[error("The module was invalid: {0}")]
    InvalidModule(String),
    #[error("{0}")]
    Serialization(bincode::Error),
    #[error("{0}")]
    Trap(String)
}

impl WingsError {
    pub fn from_invalid_module(x: impl std::fmt::Display) -> Self {
        Self::InvalidModule(x.to_string())
    }

    pub fn from_trap(x: impl std::fmt::Display) -> Self {
        Self::Trap(x.to_string())
    }
}

/// Hides traits from being externally visible.
mod private {
    use super::*;

    /// Trait that marks a type as a mutable reference. This is used to
    /// hide mutable references from `const` functions, so that they may
    /// be manipulated in a `const` context.
    pub trait MutableRef<T> {}

    impl<'a, T> MutableRef<T> for &'a mut T {}

    pub trait ExportEvent: ExportType + Serialize + DeserializeOwned {}

    impl<T: ExportType + Serialize + DeserializeOwned> ExportEvent for T {}
}