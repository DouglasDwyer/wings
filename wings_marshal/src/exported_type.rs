use crate::*;

pub trait SystemTrait: ExportType + Proxyable {
    const SYSTEM_TYPE: StaticExportedType;
}

pub trait ExportType: 'static {
    const TYPE: StaticExportedType;
}

#[derive(Clone, Debug, Default, Serialize, Deserialize, Hash, PartialEq, Eq)]
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

#[derive(Clone, Debug, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct DisjointExportedType {
    pub name: String,
    pub version: DisjointVersion
}

impl From<ExportedType> for DisjointExportedType {
    fn from(value: ExportedType) -> Self {
        Self {
            name: value.name,
            version: value.version.into()
        }
    }
}

impl From<&ExportedType> for DisjointExportedType {
    fn from(value: &ExportedType) -> Self {
        Self {
            name: value.name.clone(),
            version: value.version.into()
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct DisjointVersion {
    pub major: u32,
    pub minor: u32
}

impl From<Version> for DisjointVersion {
    fn from(value: Version) -> Self {
        Self {
            major: value.major,
            minor: value.minor
        }
    }
}

#[derive(Copy, Clone, Debug, Default, Serialize, Hash, PartialEq, Eq)]
pub struct StaticExportedType {
    pub name: &'static str,
    pub version: Version
}

#[derive(Copy, Clone, Debug, Default, Serialize, Deserialize, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Version {
    pub major: u32,
    pub minor: u32,
    pub patch: u32
}