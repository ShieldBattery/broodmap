use thiserror::Error;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum FormatVersion {
    /// StarCraft 1.00
    OriginalRetail = 59,
    /// StarCraft 1.04
    OriginalHybrid = 63,
    /// StarCraft 1.21+
    OriginalRemastered = 64,
    /// Brood War 1.04
    BroodWar = 205,
    /// Brood War 1.21+
    BroodWarRemastered = 206,
}

impl TryFrom<u16> for FormatVersion {
    type Error = ();

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        match value {
            59 => Ok(FormatVersion::OriginalRetail),
            63 => Ok(FormatVersion::OriginalHybrid),
            64 => Ok(FormatVersion::OriginalRemastered),
            205 => Ok(FormatVersion::BroodWar),
            206 => Ok(FormatVersion::BroodWarRemastered),
            _ => Err(()),
        }
    }
}

#[derive(Error, Debug)]
pub enum FormatVersionError {
    #[error("No version specified")]
    NoVersion,
    #[error("Invalid data length")]
    InvalidDataLength,
    #[error("Unrecognized version: {0}")]
    UnrecognizedVersion(u16),
}

/// Reads the format version from the given `b"VER "` chunks.
pub fn read_format_version(data: Option<&[u8]>) -> Result<FormatVersion, FormatVersionError> {
    let data = data.ok_or(FormatVersionError::NoVersion)?;
    if data.len() != 2 {
        return Err(FormatVersionError::InvalidDataLength);
    }

    let version = u16::from_le_bytes(data.try_into().unwrap());
    version
        .try_into()
        .map_err(|_| FormatVersionError::UnrecognizedVersion(version))
}
