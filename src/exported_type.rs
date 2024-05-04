use serde::*;

pub trait SystemTrait: ExportType {
    const SYSTEM_TYPE: StaticExportedType;
}

pub trait ExportType: 'static {
    const TYPE: StaticExportedType;
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct ExportedType {
    pub name: String,
    pub version: Version
}

impl From<StaticExportedType> for ExportedType {
    fn from(value: StaticExportedType) -> Self {
        Self {
            name: value.name.to_string(),
            version: value.version
        }
    }
}

#[derive(Copy, Clone, Debug, Default, Serialize)]
pub struct StaticExportedType {
    pub name: &'static str,
    pub version: Version
}

#[derive(Copy, Clone, Debug, Default, Serialize, Deserialize)]
pub struct Version {
    pub major: u32,
    pub minor: u32,
    pub patch: u32
}

pub struct DependencyType {
    pub exported_system: StaticExportedType,
    pub system_trait: StaticExportedType,
}

impl DependencyType {
    pub const fn new<T: SystemTrait + ?Sized>() -> Self {
        Self {
            exported_system: T::SYSTEM_TYPE,
            system_trait: T::TYPE
        }
    }
}