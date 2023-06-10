use bitflags::bitflags;
use thiserror::Error;

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct SpriteFlags: u8 {
        /// If set, this is a pure sprite, otherwise this is a unit sprite.
        const DRAW_AS_SPRITE = 0x10;
        /// This sprite is disabled. Only valid if `DRAW_AS_SPRITE` is not set.
        const DISABLED = 0x80;
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Sprite {
    /// The unit/sprite ID. Note that this is not guaranteed to refer to a valid unit.
    pub id: u16,
    /// The x coordinate of the sprite.
    pub x: u16,
    /// The y coordinate of the sprite.
    pub y: u16,
    pub owner: u8,
    pub flags: SpriteFlags,
}

impl Sprite {
    pub fn is_disabled(&self) -> bool {
        // TODO(tec27): Figure out the right behavior here, seems to be some conflicting info about
        // this flag
        self.flags.contains(SpriteFlags::DISABLED)
            && !self.flags.contains(SpriteFlags::DRAW_AS_SPRITE)
    }
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum SpriteError {
    #[error("Chunk missing")]
    ChunkMissing,
}

/// Reads the sprites chunk of a CHK file (aka the THG2 chunk).
pub fn read_sprites(data: &[u8]) -> Result<Vec<Sprite>, SpriteError> {
    let result = data
        .chunks_exact(10)
        .map(|chunk| {
            let id = u16::from_le_bytes(chunk[0..2].try_into().unwrap());
            let x = u16::from_le_bytes(chunk[2..4].try_into().unwrap());
            let y = u16::from_le_bytes(chunk[4..6].try_into().unwrap());
            let owner = chunk[6];
            // bytes 7 and 8 are unused
            let flags = SpriteFlags::from_bits_truncate(chunk[9]);

            Sprite {
                id,
                x,
                y,
                owner,
                flags,
            }
        })
        .collect();

    Ok(result)
}
