use bitflags::bitflags;

/// The maximum number of mega-tile entries a VF4 file can address. Mega-tile IDs are stored as
/// `u16`, so this is the largest count that could ever be validly indexed.
const MAX_MEGA_TILES: usize = 0x10000;

/// The size in bytes of a single VF4 entry (16 minitile flag `u16`s).
const ENTRY_SIZE: usize = 32;

bitflags! {
    /// Flags for a single minitile (a 8x8 pixel cell within a mega-tile's 4x4 grid).
    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    pub struct MiniTileFlags: u16 {
        const WALKABLE = 0x0001;
        const LEVEL_MID = 0x0002;
        const LEVEL_HIGH = 0x0004;
        const BLOCKS_VISION = 0x0008;
        const RAMP = 0x0010;
    }
}

/// A parsed VF4 file: minitile flags for every mega-tile in a tileset.
#[derive(Debug, Clone, Default)]
pub struct Vf4 {
    pub mega_tiles: Vec<[MiniTileFlags; 16]>,
}

impl Vf4 {
    /// Returns the minitile flags for the given mega-tile ID, or `None` if it's out of range.
    pub fn mega_tile(&self, mega_tile_id: u16) -> Option<&[MiniTileFlags; 16]> {
        self.mega_tiles.get(mega_tile_id as usize)
    }
}

/// Parses a VF4 file. Parsing is permissive: any trailing bytes that don't form a full 32-byte
/// entry are ignored, and entries beyond `0x10000` (the range addressable by a `u16` mega-tile ID)
/// are silently dropped.
pub fn parse_vf4(data: &[u8]) -> Vf4 {
    let mega_tiles = data
        .chunks_exact(ENTRY_SIZE)
        .take(MAX_MEGA_TILES)
        .map(|entry| {
            let mut flags = [MiniTileFlags::empty(); 16];
            for (i, flag) in flags.iter_mut().enumerate() {
                let offset = i * 2;
                let raw = u16::from_le_bytes(entry[offset..offset + 2].try_into().unwrap());
                *flag = MiniTileFlags::from_bits_retain(raw);
            }
            flags
        })
        .collect();

    Vf4 { mega_tiles }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_entry(flags: [u16; 16]) -> Vec<u8> {
        let mut entry = Vec::with_capacity(ENTRY_SIZE);
        for flag in flags {
            entry.extend_from_slice(&flag.to_le_bytes());
        }
        assert_eq!(entry.len(), ENTRY_SIZE);
        entry
    }

    #[test]
    fn parses_basic_entries() {
        let mut raw_a = [0u16; 16];
        for (i, v) in raw_a.iter_mut().enumerate() {
            *v = i as u16;
        }
        raw_a[0] = MiniTileFlags::WALKABLE.bits();
        raw_a[1] = (MiniTileFlags::LEVEL_HIGH | MiniTileFlags::RAMP).bits();

        let mut data = Vec::new();
        data.extend(make_entry(raw_a));
        data.extend(make_entry([0u16; 16]));

        let vf4 = parse_vf4(&data);
        assert_eq!(vf4.mega_tiles.len(), 2);

        let tile0 = vf4.mega_tile(0).unwrap();
        assert_eq!(tile0[0], MiniTileFlags::WALKABLE);
        assert_eq!(tile0[1], MiniTileFlags::LEVEL_HIGH | MiniTileFlags::RAMP);

        let tile1 = vf4.mega_tile(1).unwrap();
        assert_eq!(tile1, &[MiniTileFlags::empty(); 16]);

        assert!(vf4.mega_tile(2).is_none());
    }

    #[test]
    fn trailing_garbage_is_ignored() {
        let mut data = make_entry([0u16; 16]);
        data.extend_from_slice(&[0xAB; 5]); // partial trailing entry

        let vf4 = parse_vf4(&data);
        assert_eq!(vf4.mega_tiles.len(), 1);
    }

    #[test]
    fn unknown_flag_bits_are_retained() {
        let data = make_entry([0xFFFF; 16]);
        let vf4 = parse_vf4(&data);
        assert_eq!(vf4.mega_tiles[0][0].bits(), 0xFFFF);
    }

    #[test]
    fn empty_input_yields_no_entries() {
        let vf4 = parse_vf4(&[]);
        assert!(vf4.mega_tiles.is_empty());
        assert!(vf4.mega_tile(0).is_none());
    }
}
