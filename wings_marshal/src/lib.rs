#![deny(missing_docs)]
#![deny(clippy::missing_docs_in_private_items)]

//! Defines types to communicate between guests and the host in the [`wings`](https://github.com/DouglasDwyer/wings) plugin system.

use crate::exported_type::*;
use serde::de::*;
use serde::*;
use std::mem::*;
use std::ops::*;
use thiserror::*;

/// Defines structs for unique identification of types.
pub mod exported_type;

/// Represents a thin, untyped pointer in 32-bit WASM.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[repr(transparent)]
pub struct GuestPointer(u32);

impl GuestPointer {
    /// Creates a new guest pointer from the given integer.
    pub fn new(pointer: u32) -> Self {
        Self(pointer)
    }

    /// Casts this value to a typed pointer.
    pub fn cast<T>(self) -> *const T {
        self.0 as *const T
    }

    /// Casts this value to a mutable typed pointer.
    pub fn cast_mut<T>(self) -> *mut T {
        self.0 as *mut T
    }
}

impl<T> From<*const T> for GuestPointer {
    fn from(value: *const T) -> Self {
        Self(value as u32)
    }
}

impl<T> From<*mut T> for GuestPointer {
    fn from(value: *mut T) -> Self {
        Self(value as u32)
    }
}

impl From<GuestPointer> for i32 {
    fn from(value: GuestPointer) -> Self {
        value.0 as i32
    }
}

impl From<GuestPointer> for u32 {
    fn from(value: GuestPointer) -> Self {
        value.0
    }
}

/// Represents a fat, untyped pointer in 32-bit WASM. This is used
/// to store pointers to values and their V-tables.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[repr(C)]
pub struct FatGuestPointer([u32; 2]);

impl FatGuestPointer {
    /// Creates a new fat pointer with the given object address
    /// and metadata value.
    pub fn new(pointer: GuestPointer, metadata: GuestPointer) -> Self {
        Self([pointer.0, metadata.0])
    }

    /// Casts this value to a typed pointer.
    pub fn cast<T: ?Sized>(self) -> *const T {
        unsafe { (&self.0 as *const _ as *const *const T).read() }
    }

    /// Casts this value to a mutable typed pointer.
    pub fn cast_mut<T: ?Sized>(self) -> *mut T {
        unsafe { (&self.0 as *const _ as *const *mut T).read() }
    }
}

impl<T: ?Sized> From<*const T> for FatGuestPointer {
    fn from(value: *const T) -> Self {
        unsafe {
            let value_array = [value, value];
            Self((&value_array as *const _ as *const [u32; 2]).read())
        }
    }
}

impl<T: ?Sized> From<*mut T> for FatGuestPointer {
    fn from(value: *mut T) -> Self {
        unsafe {
            let value_array = [value, value];
            Self((&value_array as *const _ as *const [u32; 2]).read())
        }
    }
}

/// Describes a guest event handler.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct EventHandler {
    /// The type of the event handler.
    pub ty: DisjointExportedType,
    /// The actual event handler function.
    pub event_func: GuestPointer,
    /// The function to call in order to indirectly invoke the event function.
    pub invoke_func: GuestPointer,
}

/// Represents a group of systems that a plugin would like to instantiate.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InstantiateGroup {
    /// The type denoting which kind of instantiation group this is.
    pub group_ty: ExportedType,
    /// The set of systems to instantiate.
    pub systems: Vec<ExportedType>,
}

/// Describes a guest system.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SystemDescriptor {
    /// The type of the guest system.
    pub ty: ExportedType,
    /// The function to use when creating new instances of the system.
    pub new_func: GuestPointer,
    /// The function to use when dropping instances of the system.
    pub drop_func: GuestPointer,
    /// The dependencies of this system.
    pub dependencies: Vec<ExportedType>,
    /// The event handlers for this system.
    pub event_handlers: Vec<EventHandler>,
    /// The set of traits through which this system is exported.
    pub traits: Vec<SystemTraitDescriptor>,
}

/// Identifies a dependency.
#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum DependencyReference {
    /// The dependency is in the same instance and accessible via the given pointer.
    Local(FatGuestPointer),
    /// The dependency is in another instance or on the host.
    Remote(u32),
}

/// Describes a system trait.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SystemTraitDescriptor {
    /// The proxy invocation function for this trait.
    pub invoke: GuestPointer,
    /// The type of the system trait.
    pub ty: ExportedType,
    /// The V-table pointer associated with the concrete trait implementation.
    pub v_table: GuestPointer,
}

/// Represents an object that may be called by proxy.
pub trait Proxyable {
    /// The type to use as a proxy for this object.
    type Proxy: Deref<Target = Self> + DerefMut<Target = Self>;

    /// Creates a proxy with the given remote ID.
    fn create_proxy(id: u32) -> Self::Proxy;

    /// Invokes a function that was called remotely.
    ///
    /// # Safety
    ///
    /// For this function call to be sound, the buffer pointer must not
    /// be dereferenced when proxy calls are made recursively.
    unsafe fn invoke(&mut self, func_index: u32, buffer: *mut Vec<u8>) -> Result<(), WingsError>;
}

/// Determines how a type will be cloned across the WASM-native barrier.
pub trait Marshal<'a> {
    /// The type that will be created on the stack to hold this value during a marshalled call.
    type Temporary;

    /// Serializes this object into the buffer.
    fn lower_argument(&self, buffer: SectionedBufferWrite) -> Result<(), WingsError>;

    /// Deserializes this object from the buffer.
    fn lift_argument(buffer: &[u8]) -> Result<Self::Temporary, WingsError>;

    /// Creates a temporary copy of this type from the underlying value.
    fn make_temporary(value: &'a mut Self::Temporary) -> Self;

    /// Serializes the result object into the buffer.
    fn lower_result(
        value: &Self::Temporary,
        buffer: SectionedBufferWrite,
    ) -> Result<(), WingsError>;

    /// Deserializes the result object from the buffer.
    fn lift_result(&mut self, buffer: &[u8]) -> Result<(), WingsError>;
}

impl<'a, T: Serialize + DeserializeOwned> Marshal<'a> for &'a T {
    type Temporary = T;

    fn lower_argument(&self, buffer: SectionedBufferWrite) -> Result<(), WingsError> {
        bincode::serialize_into(buffer, self).map_err(WingsError::Serialization)
    }

    fn lift_argument(buffer: &[u8]) -> Result<Self::Temporary, WingsError> {
        bincode::deserialize(buffer).map_err(WingsError::Serialization)
    }

    fn make_temporary(value: &'a mut Self::Temporary) -> Self {
        value
    }

    fn lower_result(_: &Self::Temporary, _: SectionedBufferWrite) -> Result<(), WingsError> {
        Ok(())
    }

    fn lift_result(&mut self, _: &[u8]) -> Result<(), WingsError> {
        Ok(())
    }
}

impl<'a, T: Serialize + DeserializeOwned> Marshal<'a> for &'a mut T {
    type Temporary = T;

    fn lower_argument(&self, buffer: SectionedBufferWrite) -> Result<(), WingsError> {
        bincode::serialize_into(buffer, self).map_err(WingsError::Serialization)
    }

    fn lift_argument(buffer: &[u8]) -> Result<T, WingsError> {
        bincode::deserialize(buffer).map_err(WingsError::Serialization)
    }

    fn make_temporary(value: &'a mut T) -> Self {
        value
    }

    fn lower_result(value: &T, buffer: SectionedBufferWrite) -> Result<(), WingsError> {
        bincode::serialize_into(buffer, value).map_err(WingsError::Serialization)
    }

    fn lift_result(&mut self, buffer: &[u8]) -> Result<(), WingsError> {
        **self = Self::lift_argument(buffer)?;
        Ok(())
    }
}

impl<'a> Marshal<'a> for &'a str {
    type Temporary = String;

    fn lower_argument(&self, buffer: SectionedBufferWrite) -> Result<(), WingsError> {
        bincode::serialize_into(buffer, self).map_err(WingsError::Serialization)
    }

    fn lift_argument(buffer: &[u8]) -> Result<Self::Temporary, WingsError> {
        bincode::deserialize(buffer).map_err(WingsError::Serialization)
    }

    fn make_temporary(value: &'a mut Self::Temporary) -> Self {
        &*value
    }

    fn lower_result(_: &Self::Temporary, _: SectionedBufferWrite) -> Result<(), WingsError> {
        Ok(())
    }

    fn lift_result(&mut self, _: &[u8]) -> Result<(), WingsError> {
        Ok(())
    }
}

impl<'a, T: Serialize + DeserializeOwned> Marshal<'a> for &'a [T] {
    type Temporary = Vec<T>;

    fn lower_argument(&self, buffer: SectionedBufferWrite) -> Result<(), WingsError> {
        bincode::serialize_into(buffer, self).map_err(WingsError::Serialization)
    }

    fn lift_argument(buffer: &[u8]) -> Result<Self::Temporary, WingsError> {
        bincode::deserialize(buffer).map_err(WingsError::Serialization)
    }

    fn make_temporary(value: &'a mut Self::Temporary) -> Self {
        &*value
    }

    fn lower_result(_: &Self::Temporary, _: SectionedBufferWrite) -> Result<(), WingsError> {
        Ok(())
    }

    fn lift_result(&mut self, _: &[u8]) -> Result<(), WingsError> {
        Ok(())
    }
}

impl<'a, T: Serialize + DeserializeOwned> Marshal<'a> for &'a mut [T] {
    type Temporary = Vec<T>;

    fn lower_argument(&self, buffer: SectionedBufferWrite) -> Result<(), WingsError> {
        bincode::serialize_into(buffer, self).map_err(WingsError::Serialization)
    }

    fn lift_argument(buffer: &[u8]) -> Result<Self::Temporary, WingsError> {
        bincode::deserialize(buffer).map_err(WingsError::Serialization)
    }

    fn make_temporary(value: &'a mut Self::Temporary) -> Self {
        &mut *value
    }

    fn lower_result(
        value: &Self::Temporary,
        buffer: SectionedBufferWrite,
    ) -> Result<(), WingsError> {
        bincode::serialize_into(buffer, value).map_err(WingsError::Serialization)
    }

    fn lift_result(&mut self, buffer: &[u8]) -> Result<(), WingsError> {
        let values = Self::lift_argument(buffer)?;
        if self.len() == values.len() {
            for (old, new) in self.iter_mut().zip(values) {
                *old = new;
            }
            Ok(())
        } else {
            Err(WingsError::Serialization(bincode::Error::new(
                bincode::ErrorKind::Custom("Slice length mismatch.".to_string()),
            )))
        }
    }
}

/// Holds the serialized data for communication between guest and host.
pub static mut MARSHAL_BUFFER: Vec<u8> = Vec::new();

/// Writes to a buffer in length-delineated sections.
pub struct SectionedBufferWriter<'a> {
    /// The underlying buffer.
    buffer: &'a mut Vec<u8>,
}

impl SectionedBufferWriter<'static> {
    /// Creates a new writer for the marshal buffer.
    ///
    /// # Safety
    ///
    /// For this call to be sound, the marshal buffer must not
    /// currently be borrowed anywhere else.
    pub unsafe fn from_marshal_buffer() -> Self {
        Self::new(&mut *std::ptr::addr_of_mut!(MARSHAL_BUFFER))
    }
}

impl<'a> SectionedBufferWriter<'a> {
    /// Creates a new writer.
    pub fn new(buffer: &'a mut Vec<u8>) -> Self {
        buffer.clear();
        Self { buffer }
    }

    /// Adds a section to the buffer.
    pub fn section(&mut self) -> SectionedBufferWrite {
        SectionedBufferWrite::new(self.buffer)
    }

    /// Converts the writer to the underlying mutable buffer.
    pub fn into_inner(self) -> &'a mut Vec<u8> {
        self.buffer
    }
}

/// Allows for writing to a section of a buffer.
pub struct SectionedBufferWrite<'a> {
    /// The underlying buffer.
    buffer: &'a mut Vec<u8>,
    /// The position at which the length should be written in the buffer
    /// when this write completes.
    start_position: usize,
}

impl<'a> SectionedBufferWrite<'a> {
    /// Creates a new section within the given buffer.
    fn new(buffer: &'a mut Vec<u8>) -> Self {
        let start_position = buffer.len();
        buffer.extend_from_slice(&0u32.to_le_bytes());

        Self {
            buffer,
            start_position,
        }
    }
}

impl<'a> Drop for SectionedBufferWrite<'a> {
    fn drop(&mut self) {
        let data_begin = self.start_position + size_of::<u32>();
        let data_len = (self.buffer.len() - data_begin) as u32;
        self.buffer[self.start_position..data_begin].copy_from_slice(&data_len.to_le_bytes());
    }
}

impl<'a> std::io::Write for SectionedBufferWrite<'a> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        std::io::Write::write(self.buffer, buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        std::io::Write::flush(self.buffer)
    }

    fn write_vectored(&mut self, bufs: &[std::io::IoSlice<'_>]) -> std::io::Result<usize> {
        std::io::Write::write_vectored(self.buffer, bufs)
    }

    fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()> {
        std::io::Write::write_all(self.buffer, buf)
    }

    fn write_fmt(&mut self, fmt: std::fmt::Arguments<'_>) -> std::io::Result<()> {
        std::io::Write::write_fmt(self.buffer, fmt)
    }

    fn by_ref(&mut self) -> &mut Self
    where
        Self: Sized,
    {
        self
    }
}

/// Reads from a buffer in length-delineated sections.
pub struct SectionedBufferReader<'a> {
    /// The underlying buffer.
    buffer: &'a [u8],
}

impl SectionedBufferReader<'static> {
    /// Creates a new reader for the marshal buffer.
    ///
    /// # Safety
    ///
    /// For this call to be sound, the marshal buffer must not
    /// currently be borrowed anywhere else.
    pub unsafe fn from_marshal_buffer() -> Self {
        Self::new(&*std::ptr::addr_of_mut!(MARSHAL_BUFFER))
    }
}

impl<'a> SectionedBufferReader<'a> {
    /// Creates a new reader for the given buffer.
    pub fn new(buffer: &'a [u8]) -> Self {
        Self { buffer }
    }

    /// Gets the next section in the buffer, or returns an error
    /// if the buffer was not formatted correctly.
    pub fn section(&mut self) -> Result<&[u8], WingsError> {
        if self.buffer.len() < size_of::<u32>() {
            Err(WingsError::Serialization(bincode::Error::new(
                bincode::ErrorKind::Custom("Sectioned buffer incomplete".to_string()),
            )))
        } else {
            let mut len_bytes = [0; size_of::<u32>()];
            len_bytes.copy_from_slice(&self.buffer[0..size_of::<u32>()]);
            let len = u32::from_le_bytes(len_bytes) as usize;
            if self.buffer.len() < size_of::<u32>() + len {
                Err(WingsError::Serialization(bincode::Error::new(
                    bincode::ErrorKind::Custom("Sectioned buffer incomplete".to_string()),
                )))
            } else {
                let (beginning, rest) = self.buffer[size_of::<u32>()..].split_at(len);
                self.buffer = rest;
                Ok(beginning)
            }
        }
    }
}

/// Describes an error that occurred during WASM instantiation or execution.
#[derive(Debug, Error)]
pub enum WingsError {
    /// There was a cyclic dependency between systems.
    #[error("There was a cyclic dependency between systems")]
    CyclicDependency(),
    /// A function call was invalid.
    #[error("A function call was invalid")]
    InvalidFunction(),
    /// The module was malformatted.
    #[error("The module was invalid: {0}")]
    InvalidModule(Box<dyn std::error::Error + Send + Sync>),
    /// A serialization error occurred.
    #[error("{0}")]
    Serialization(bincode::Error),
    /// A WASM trap occurred.
    #[error("{0}")]
    Trap(Box<dyn std::error::Error + Send + Sync>),
}

impl WingsError {
    /// Wraps the given error as an `InvalidModule`.
    pub fn from_invalid_module(x: impl Into<Box<dyn std::error::Error + Send + Sync>>) -> Self {
        Self::InvalidModule(x.into())
    }

    /// Wraps the given error as a `Trap`.
    pub fn from_trap(x: impl Into<Box<dyn std::error::Error + Send + Sync>>) -> Self {
        Self::Trap(x.into())
    }
}

/// Serializes the given object as a single section in the marshal buffer.
///
/// # Safety
///
/// For this function call to be sound, no references may currently
/// exist to the marshal buffer.
pub unsafe fn write_to_marshal_buffer(x: &impl Serialize) -> GuestPointer {
    let buffer = &mut *std::ptr::addr_of_mut!(MARSHAL_BUFFER);
    buffer.clear();
    buffer.extend(0u32.to_le_bytes());
    bincode::serialize_into(&mut *buffer, x).expect("Failed to serialize buffer descriptor.");
    let total_len = (buffer.len() - std::mem::size_of::<u32>()) as u32;
    buffer[0..4].copy_from_slice(&total_len.to_le_bytes());
    buffer.as_mut_ptr().into()
}
