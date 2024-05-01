pub use wings_macro::*;

pub mod marshal;

#[derive(Debug)]
pub enum WingsError {
    InvalidFunction(),
    Serialization(bincode::Error)
}