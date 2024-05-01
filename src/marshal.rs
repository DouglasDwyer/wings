pub use bincode;
use crate::*;
use serde::*;
use serde::de::*;
use std::mem::*;
use std::ops::*;

pub trait Proxyable {
    type Proxy: Deref<Target = Self>;

    fn create_proxy(id: u32) -> Self::Proxy;
    fn invoke(&mut self, func_index: u32, buffer: &mut Vec<u8>) -> Result<(), WingsError>;
}

pub trait MarshalAs<'a, T> {
    fn lower_argument(&self, buffer: SectionedBufferWrite) -> Result<(), WingsError>;
    fn lift_argument(buffer: &[u8]) -> Result<T, WingsError>;
    fn make_temporary(value: &'a mut T) -> Self;
    fn lower_result(value: &T, buffer: SectionedBufferWrite) -> Result<(), WingsError>;
    fn lift_result(&mut self, buffer: &[u8]) -> Result<(), WingsError>;
}

impl<'a, T: Serialize + DeserializeOwned> MarshalAs<'a, Option<T>> for T {
    fn lower_argument(&self, buffer: SectionedBufferWrite) -> Result<(), WingsError> {
        bincode::serialize_into(buffer, self).map_err(WingsError::Serialization)
    }

    fn lift_argument(buffer: &[u8]) -> Result<Option<T>, WingsError> {
        bincode::deserialize(buffer).map(Some).map_err(WingsError::Serialization)
    }

    fn make_temporary(value: &'a mut Option<T>) -> Self {
        take(value).expect("Attempted to make temporary from value twice.")
    }

    fn lower_result(_: &Option<T>, _: SectionedBufferWrite) -> Result<(), WingsError> {
        Ok(())
    }

    fn lift_result(&mut self, _: &[u8]) -> Result<(), WingsError> {
        Ok(())
    }
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
        println!("Lower it");
        bincode::serialize_into(buffer, value).map_err(WingsError::Serialization)
    }

    fn lift_result(&mut self, buffer: &[u8]) -> Result<(), WingsError> {
        println!("Raise it");
        **self = Self::lift_argument(buffer)?;
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

#[no_mangle]
unsafe extern "C" fn __wings_alloc_marshal_buffer(size: u32) -> u32 {
    let buffer = &mut *std::ptr::addr_of_mut!(MARSHAL_BUFFER);
    let to_reserve = size as usize;
    buffer.clear();
    buffer.reserve(to_reserve);
    buffer.set_len(to_reserve);
    buffer.as_mut_ptr() as u32
}

extern "C" {
    pub fn __wings_invoke_host(pointer: u32, size: u32) -> u32;
}