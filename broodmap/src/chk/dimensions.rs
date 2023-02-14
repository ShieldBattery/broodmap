use thiserror::Error;

/// The DIM chunk of a CHK file (Map Dimensions).
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct MapDimensions {
    /// The width of the map in 32x32 tiles.
    pub width: u16,
    /// The height of the map in 32x32 tiles.
    pub height: u16,
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum DimensionsError {
    #[error("Chunk missing")]
    ChunkMissing,
    #[error("Invalid data length")]
    InvalidDataLength,
}

pub fn read_dimensions(data: &[u8]) -> Result<MapDimensions, DimensionsError> {
    if data.len() != 4 {
        return Err(DimensionsError::InvalidDataLength);
    }

    let width = u16::from_le_bytes(data[0..2].try_into().unwrap());
    let height = u16::from_le_bytes(data[2..4].try_into().unwrap());

    Ok(MapDimensions { width, height })
}
