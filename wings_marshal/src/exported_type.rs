use crate::*;

/// Marks an exported trait that can be used to access systems.
pub trait SystemTrait: ExportType + Proxyable {
    /// The underlying system type descriptor.
    const SYSTEM_TYPE: StaticExportedType;
}

/// Exports a type that can cross plugin boundaries
/// with identification.
pub trait ExportType: 'static {
    /// The type descriptor.
    const TYPE: StaticExportedType;
}

/// Uniquely identifies a type.
#[derive(Clone, Debug, Default, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub struct ExportedType {
    /// The name of the type.
    pub name: String,
    /// The version of the type.
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

/// Identifies all compatible versions of a single type.
#[derive(Clone, Debug, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct DisjointExportedType {
    /// The name of the type.
    pub name: String,
    /// The range of versions for the type.
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

/// Identifies a range of compatible versions for a type.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct DisjointVersion {
    /// The major version.
    pub major: u32,
    /// The minor version.
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

/// Uniquely identifies a type with a `'static` name.
#[derive(Copy, Clone, Debug, Default, Serialize, Hash, PartialEq, Eq)]
pub struct StaticExportedType {
    /// The name of the type.
    pub name: &'static str,
    /// The version of the type.
    pub version: Version
}

/// Identifies the crate version in which a type was published.
#[derive(Copy, Clone, Debug, Default, Serialize, Deserialize, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Version {
    /// The major version.
    pub major: u32,
    /// The minor version.
    pub minor: u32,
    /// The patch version. Instances of a type are considered the same across patch versions.
    pub patch: u32
}