use thiserror::Error;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Tileset {
    Badlands = 0,
    SpacePlatform = 1,
    Installation = 2,
    Ashworld = 3,
    Jungle = 4,
    Desert = 5,
    Arctic = 6,
    Twilight = 7,
}

impl From<u16> for Tileset {
    fn from(value: u16) -> Self {
        // Why'd you make it a u16 then, Blizzard? Huh?
        let value = value & 0x7;
        match value {
            0 => Tileset::Badlands,
            1 => Tileset::SpacePlatform,
            2 => Tileset::Installation,
            3 => Tileset::Ashworld,
            4 => Tileset::Jungle,
            5 => Tileset::Desert,
            6 => Tileset::Arctic,
            7 => Tileset::Twilight,
            _ => unreachable!(),
        }
    }
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum TilesetError {
    #[error("Invalid data length")]
    InvalidDataLength,
}

pub fn read_tileset(data: &[u8]) -> Result<Tileset, TilesetError> {
    if data.len() != 2 {
        return Err(TilesetError::InvalidDataLength);
    }

    // TODO(tec27): How does BW handle a non-BW map using a BW tileset?
    Ok(u16::from_le_bytes(data.try_into().unwrap()).into())
}
