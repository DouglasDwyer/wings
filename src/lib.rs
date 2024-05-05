use const_list::*;
pub use crate::exported_type::*;
use crate::marshal::*;
use crate::private::*;
use serde::*;
use serde::de::*;
use std::any::*;
use std::marker::*;
use sync_rw_cell::*;
use thiserror::*;
pub use wings_macro::*;

mod exported_type;
pub mod marshal;

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
    inner: ConstList<'static, ()>,
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
        todo!()
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
    dependencies: Vec<DependencyReference>,
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
    pub fn get<T: SystemTrait + ?Sized>(&self) -> &T {
        unsafe {
            &*self.get_dependency_ptr()
        }
    }

    /// Mutably obtains the specified system dependency.
    #[inline(always)]
    pub fn get_mut<T: SystemTrait + ?Sized>(&mut self) -> &mut T {
        unsafe {
            &mut *self.get_dependency_ptr()
        }
    }

    /// Gets the pointer which references the given system dependency.
    #[inline(always)]
    fn get_dependency_ptr<T: SystemTrait + ?Sized>(&self) -> *mut T {
        unsafe {
            *self.dependencies.iter().find(|x| x.id == TypeId::of::<T>())
                    .unwrap_or_else(|| panic!("System {} was not a dependency of {}", type_name::<T>(), type_name::<S>()))
                    .reference.downcast_ref().unwrap_unchecked()
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

/*
pub struct SystemRef<'a, M: Mutability, S: ExportSystem + ?Sized> {
    inner: 
} */

#[derive(Debug, Error)]
pub enum WingsError {
    #[error("There was a cyclic dependency between systems")]
    CyclicDependency(),
    #[error("A function call was invalid")]
    InvalidFunction(),
    #[error("The module was invalid: {0}")]
    InvalidModule(String),
    #[error("An error occurred during boundary serialization: {0}")]
    Serialization(bincode::Error)
}

impl WingsError {
    pub fn from_invalid_module(x: impl std::fmt::Display) -> Self {
        Self::InvalidModule(x.to_string())
    }
}

struct DependencyReference {
    pub id: TypeId,
    pub reference: Box<dyn Any>
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