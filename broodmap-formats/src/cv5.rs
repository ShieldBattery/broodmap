use bitflags::bitflags;

/// The maximum number of tile groups a CV5 file can address. Group IDs are 11 bits
/// (see `TileId::group_id` in broodmap), so at most this many groups are ever referenced.
const MAX_GROUPS: usize = 0x800;

/// The size in bytes of a single CV5 entry.
const ENTRY_SIZE: usize = 52;

bitflags! {
    /// Flags for a tile group from a CV5 file. Some of these are recomputed by the game from
    /// minitile flags at load time.
    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    pub struct TileGroupFlags: u16 {
        const WALKABLE = 0x0001;
        const UNWALKABLE = 0x0004;
        const PROVIDES_COVER = 0x0010;
        const HAS_CREEP = 0x0040;
        const UNBUILDABLE = 0x0080;
        const BLOCKS_VISION = 0x0100;
        const LEVEL_MID = 0x0200;
        const LEVEL_HIGH = 0x0400;
        const OCCUPIED = 0x0800;
        const RECEDING_CREEP = 0x1000;
        const PARTIALLY_WALKABLE = 0x2000;
        const TEMPORARY_CREEP = 0x4000;
        const ALLOW_BEACONS_AND_START_LOCATIONS = 0x8000;
    }
}

/// A single tile group entry from a CV5 file, describing one entry in a tileset's group table.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TileGroup {
    /// The group type, kept for completeness but unused by the game.
    pub group_type: u16,
    /// Flags describing how this tile group behaves (walkability, creep, etc).
    pub flags: TileGroupFlags,
    /// The mega-tile IDs belonging to this group, indexable via `TileId::tile_index()`.
    pub mega_tiles: [u16; 16],
}

/// A parsed CV5 file: the tile-group table for a tileset.
#[derive(Debug, Clone, Default)]
pub struct Cv5 {
    pub groups: Vec<TileGroup>,
}

impl Cv5 {
    /// Looks up a tile group by group ID (the value from broodmap's `TileId::group_id()`).
    pub fn group(&self, group_id: u16) -> Option<&TileGroup> {
        self.groups.get(group_id as usize)
    }
}

/// Parses a CV5 file. Parsing is permissive: any trailing bytes that don't form a full 52-byte
/// entry are ignored, and entries beyond the addressable group ID range (0x800) are silently
/// dropped.
pub fn parse_cv5(data: &[u8]) -> Cv5 {
    let groups = data
        .chunks_exact(ENTRY_SIZE)
        .take(MAX_GROUPS)
        .map(|entry| {
            let group_type = u16::from_le_bytes(entry[0..2].try_into().unwrap());
            let flags = TileGroupFlags::from_bits_retain(u16::from_le_bytes(
                entry[2..4].try_into().unwrap(),
            ));

            let mut mega_tiles = [0u16; 16];
            for (i, mega_tile) in mega_tiles.iter_mut().enumerate() {
                let offset = 20 + i * 2;
                *mega_tile = u16::from_le_bytes(entry[offset..offset + 2].try_into().unwrap());
            }

            TileGroup {
                group_type,
                flags,
                mega_tiles,
            }
        })
        .collect();

    Cv5 { groups }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_entry(group_type: u16, flags: u16, mega_tiles: [u16; 16]) -> Vec<u8> {
        let mut entry = Vec::with_capacity(ENTRY_SIZE);
        entry.extend_from_slice(&group_type.to_le_bytes());
        entry.extend_from_slice(&flags.to_le_bytes());
        entry.extend_from_slice(&[0u8; 16]); // unused rects
        for tile in mega_tiles {
            entry.extend_from_slice(&tile.to_le_bytes());
        }
        assert_eq!(entry.len(), ENTRY_SIZE);
        entry
    }

    #[test]
    fn parses_basic_entries() {
        let mut mega_tiles_a = [0u16; 16];
        for (i, tile) in mega_tiles_a.iter_mut().enumerate() {
            *tile = i as u16;
        }
        let mut mega_tiles_b = [0u16; 16];
        for (i, tile) in mega_tiles_b.iter_mut().enumerate() {
            *tile = 100 + i as u16;
        }

        let mut data = Vec::new();
        data.extend(make_entry(
            1,
            (TileGroupFlags::WALKABLE | TileGroupFlags::HAS_CREEP).bits(),
            mega_tiles_a,
        ));
        data.extend(make_entry(
            2,
            TileGroupFlags::UNWALKABLE.bits(),
            mega_tiles_b,
        ));

        let cv5 = parse_cv5(&data);
        assert_eq!(cv5.groups.len(), 2);

        let group0 = cv5.group(0).unwrap();
        assert_eq!(group0.group_type, 1);
        assert_eq!(
            group0.flags,
            TileGroupFlags::WALKABLE | TileGroupFlags::HAS_CREEP
        );
        assert_eq!(group0.mega_tiles, mega_tiles_a);

        let group1 = cv5.group(1).unwrap();
        assert_eq!(group1.group_type, 2);
        assert_eq!(group1.flags, TileGroupFlags::UNWALKABLE);
        assert_eq!(group1.mega_tiles, mega_tiles_b);

        assert!(cv5.group(2).is_none());
    }

    #[test]
    fn trailing_garbage_is_ignored() {
        let mut data = make_entry(1, 0, [0; 16]);
        data.extend_from_slice(&[0xAB; 10]); // partial trailing entry

        let cv5 = parse_cv5(&data);
        assert_eq!(cv5.groups.len(), 1);
    }

    #[test]
    fn unknown_flag_bits_are_retained() {
        // Bit 0x0002 isn't in the documented set; from_bits_retain must keep it.
        let data = make_entry(0, 0xFFFF, [0; 16]);
        let cv5 = parse_cv5(&data);
        assert_eq!(cv5.groups[0].flags.bits(), 0xFFFF);
    }

    #[test]
    fn caps_at_max_groups() {
        let mut data = Vec::new();
        for _ in 0..(MAX_GROUPS + 5) {
            data.extend(make_entry(0, 0, [0; 16]));
        }

        let cv5 = parse_cv5(&data);
        assert_eq!(cv5.groups.len(), MAX_GROUPS);
    }

    #[test]
    fn empty_input_yields_no_groups() {
        let cv5 = parse_cv5(&[]);
        assert!(cv5.groups.is_empty());
        assert!(cv5.group(0).is_none());
    }
}
