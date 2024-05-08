use crate::exported_type::*;
use serde::*;
use serde::de::*;
use std::mem::*;
use std::ops::*;
use thiserror::*;

pub mod exported_type;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[repr(transparent)]
pub struct GuestPointer(u32);

impl GuestPointer {
    pub fn new(pointer: u32) -> Self {
        Self(pointer)
    }

    pub fn cast<T>(self) -> *const T {
        self.0 as *const T
    }

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
        value.0 as u32
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[repr(C)]
pub struct FatGuestPointer([u32; 2]);

impl FatGuestPointer {
    pub fn new(pointer: GuestPointer, metadata: GuestPointer) -> Self {
        Self([pointer.0, metadata.0])
    }

    pub fn cast<T: ?Sized>(self) -> *const T {
        unsafe {
            (&self.0 as *const _ as *const *const T).read()
        }
    }

    pub fn cast_mut<T: ?Sized>(self) -> *mut T {
        unsafe {
            (&self.0 as *const _ as *const *mut T).read()
        }
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

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct EventHandler {
    pub ty: DisjointExportedType,
    pub event_func: GuestPointer,
    pub invoke_func: GuestPointer,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InstantiateGroup {
    pub group_ty: ExportedType,
    pub systems: Vec<ExportedType>
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SystemDescriptor {
    pub ty: ExportedType,
    pub new_func: GuestPointer,
    pub drop_func: GuestPointer,
    pub dependencies: Vec<ExportedType>,
    pub event_handlers: Vec<EventHandler>,
    pub traits: Vec<SystemTraitDescriptor>
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum DependencyReference {
    Local(FatGuestPointer),
    Remote(u32)
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SystemTraitDescriptor {
    pub invoke: GuestPointer,
    pub ty: ExportedType,
    pub v_table: GuestPointer
}

pub trait Proxyable {
    type Proxy: Deref<Target = Self> + DerefMut<Target = Self>;

    fn create_proxy(id: u32) -> Self::Proxy;
    unsafe fn invoke(&mut self, func_index: u32, buffer: *mut Vec<u8>) -> Result<(), WingsError>;
}

pub trait MarshalAs<'a, T> {
    fn lower_argument(&self, buffer: SectionedBufferWrite) -> Result<(), WingsError>;
    fn lift_argument(buffer: &[u8]) -> Result<T, WingsError>;
    fn make_temporary(value: &'a mut T) -> Self;
    fn lower_result(value: &T, buffer: SectionedBufferWrite) -> Result<(), WingsError>;
    fn lift_result(&mut self, buffer: &[u8]) -> Result<(), WingsError>;
}

impl<'a, T: Serialize + DeserializeOwned> MarshalAs<'a, T> for &'a T {
    fn lower_argument(&self, buffer: SectionedBufferWrite) -> Result<(), WingsError> {
        bincode::serialize_into(buffer, self).map_err(WingsError::Serialization)
    }

    fn lift_argument(buffer: &[u8]) -> Result<T, WingsError> {
        bincode::deserialize(buffer).map_err(WingsError::Serialization)
    }

    fn make_temporary(value: &'a mut T) -> Self {
        value
    }

    fn lower_result(_: &T, _: SectionedBufferWrite) -> Result<(), WingsError> {
        Ok(())
    }

    fn lift_result(&mut self, _: &[u8]) -> Result<(), WingsError> {
        Ok(())
    }
}

impl<'a, T: Serialize + DeserializeOwned> MarshalAs<'a, T> for &'a mut T {
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

impl<'a> MarshalAs<'a, String> for &'a str {
    fn lower_argument(&self, buffer: SectionedBufferWrite) -> Result<(), WingsError> {
        bincode::serialize_into(buffer, self).map_err(WingsError::Serialization)
    }

    fn lift_argument(buffer: &[u8]) -> Result<String, WingsError> {
        bincode::deserialize(buffer).map_err(WingsError::Serialization)
    }

    fn make_temporary(value: &'a mut String) -> Self {
        &**value
    }

    fn lower_result(_: &String, _: SectionedBufferWrite) -> Result<(), WingsError> {
        Ok(())
    }

    fn lift_result(&mut self, _: &[u8]) -> Result<(), WingsError> {
        Ok(())
    }
}

impl<'a, T: Serialize + DeserializeOwned> MarshalAs<'a, Vec<T>> for &'a [T] {
    fn lower_argument(&self, buffer: SectionedBufferWrite) -> Result<(), WingsError> {
        bincode::serialize_into(buffer, self).map_err(WingsError::Serialization)
    }

    fn lift_argument(buffer: &[u8]) -> Result<Vec<T>, WingsError> {
        bincode::deserialize(buffer).map_err(WingsError::Serialization)
    }

    fn make_temporary(value: &'a mut Vec<T>) -> Self {
        &*value
    }

    fn lower_result(_: &Vec<T>, _: SectionedBufferWrite) -> Result<(), WingsError> {
        Ok(())
    }

    fn lift_result(&mut self, _: &[u8]) -> Result<(), WingsError> {
        Ok(())
    }
}

impl<'a, T: Serialize + DeserializeOwned> MarshalAs<'a, Vec<T>> for &'a mut [T] {
    fn lower_argument(&self, buffer: SectionedBufferWrite) -> Result<(), WingsError> {
        bincode::serialize_into(buffer, self).map_err(WingsError::Serialization)
    }

    fn lift_argument(buffer: &[u8]) -> Result<Vec<T>, WingsError> {
        bincode::deserialize(buffer).map_err(WingsError::Serialization)
    }

    fn make_temporary(value: &'a mut Vec<T>) -> Self {
        &mut *value
    }

    fn lower_result(value: &Vec<T>, buffer: SectionedBufferWrite) -> Result<(), WingsError> {
        bincode::serialize_into(buffer, value).map_err(WingsError::Serialization)
    }

    fn lift_result(&mut self, buffer: &[u8]) -> Result<(), WingsError> {
        let values = Self::lift_argument(buffer)?;
        if self.len() == values.len() {
            for (old, new) in self.iter_mut().zip(values) {
                *old = new;
            }
            Ok(())
        }
        else {
            Err(WingsError::Serialization(bincode::Error::new(bincode::ErrorKind::Custom("Slice length mismatch.".to_string()))))
        }
    }
}

pub static mut MARSHAL_BUFFER: Vec<u8> = Vec::new();

pub struct SectionedBufferWriter<'a> {
    buffer: &'a mut Vec<u8>
}

impl SectionedBufferWriter<'static> {
    pub unsafe fn from_marshal_buffer() -> Self {
        Self::new(&mut *std::ptr::addr_of_mut!(MARSHAL_BUFFER))
    }
}

impl<'a> SectionedBufferWriter<'a> {
    pub fn new(buffer: &'a mut Vec<u8>) -> Self {
        buffer.clear();
        Self {
            buffer
        }
    }

    pub fn section(&mut self) -> SectionedBufferWrite {
        SectionedBufferWrite::new(self.buffer)
    }

    pub fn into_inner(self) -> &'a mut Vec<u8> {
        self.buffer
    }
}

pub struct SectionedBufferWrite<'a> {
    buffer: &'a mut Vec<u8>,
    start_position: usize
}

impl<'a> SectionedBufferWrite<'a> {
    fn new(buffer: &'a mut Vec<u8>) -> Self {
        let start_position = buffer.len();
        buffer.extend_from_slice(&0u32.to_le_bytes());

        Self {
            buffer,
            start_position
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

pub struct SectionedBufferReader<'a> {
    buffer: &'a [u8]
}

impl SectionedBufferReader<'static> {
    pub unsafe fn from_marshal_buffer() -> Self {
        Self::new(&*std::ptr::addr_of_mut!(MARSHAL_BUFFER))
    }
}

impl<'a> SectionedBufferReader<'a> {
    pub fn new(buffer: &'a [u8]) -> Self {
        Self {
            buffer
        }
    }

    pub fn section(&mut self) -> Result<&[u8], WingsError> {
        if self.buffer.len() < size_of::<u32>() {
            Err(WingsError::Serialization(bincode::Error::new(bincode::ErrorKind::Custom("Sectioned buffer incomplete".to_string()))))
        }
        else {
            let mut len_bytes = [0; size_of::<u32>()];
            len_bytes.copy_from_slice(&self.buffer[0..size_of::<u32>()]);
            let len = u32::from_le_bytes(len_bytes) as usize;
            if self.buffer.len() < size_of::<u32>() + len {
                Err(WingsError::Serialization(bincode::Error::new(bincode::ErrorKind::Custom("Sectioned buffer incomplete".to_string()))))
            }
            else {
                let (beginning, rest) = self.buffer[size_of::<u32>()..].split_at(len);
                self.buffer = rest;
                Ok(beginning)
            }
        }
    }
}

#[derive(Debug, Error)]
pub enum WingsError {
    #[error("There was a cyclic dependency between systems")]
    CyclicDependency(),
    #[error("A function call was invalid")]
    InvalidFunction(),
    #[error("The module was invalid: {0}")]
    InvalidModule(Box<dyn std::error::Error + Send + Sync>),
    #[error("{0}")]
    Serialization(bincode::Error),
    #[error("{0}")]
    Trap(Box<dyn std::error::Error + Send + Sync>)
}

impl WingsError {
    pub fn from_invalid_module(x: impl Into<Box<dyn std::error::Error + Send + Sync>>) -> Self {
        Self::InvalidModule(x.into())
    }

    pub fn from_trap(x: impl Into<Box<dyn std::error::Error + Send + Sync>>) -> Self {
        Self::Trap(x.into())
    }
}

pub unsafe fn write_to_marshal_buffer(x: &impl Serialize) -> GuestPointer {
    let buffer = &mut *std::ptr::addr_of_mut!(MARSHAL_BUFFER);
    buffer.clear();
    buffer.extend(0u32.to_le_bytes());
    bincode::serialize_into(&mut *buffer, x).expect("Failed to serialize buffer descriptor.");
    let total_len = (buffer.len() - std::mem::size_of::<u32>()) as usize;
    buffer[0..4].copy_from_slice(&total_len.to_le_bytes());
    buffer.as_mut_ptr().into()
}