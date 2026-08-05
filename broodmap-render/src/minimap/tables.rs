//! The 8 committed, per-tileset minimap color tables, baked at dev time (see
//! `crate::minimap::build_minimap_table` and `broodmap-cli`'s hidden `gen-minimap-tables`
//! subcommand) and embedded via `include_bytes!` so runtime minimap rendering needs no game
//! assets at all — just these small blobs, compiled into the crate.
//!
//! Blob layout (little-endian), matching `build_minimap_table`'s output exactly:
//!
//! ```text
//! [0..2)          u16   num_tile_ids            (= cv5 group count * 16)
//! [2..2+n)        u8    palette index per CHK *unified* tile id   (n = num_tile_ids)
//! [2+n..2+n+13)   u8    13 creep palette indices (creep group cv5[1], megatile slots 0..13)
//! [2+n+13..+768)  u8    256 x [r,g,b] palette (from the tileset's .wpe)
//! ```
//!
//! [`table`] parses (and validates the lengths of) one of the embedded blobs; [`parse_table`] is
//! the no-panic core so a malformed/truncated blob (which shouldn't happen for the committed
//! files, but this stays permissive per `AGENTS.md`) degrades to `None` — `crate::minimap` then
//! renders black terrain for that tileset rather than failing the whole render.

use broodmap::chk::tileset::Tileset;

const BADLANDS: &[u8] = include_bytes!("tables/badlands.bin");
const PLATFORM: &[u8] = include_bytes!("tables/platform.bin");
const INSTALL: &[u8] = include_bytes!("tables/install.bin");
const ASHWORLD: &[u8] = include_bytes!("tables/ashworld.bin");
const JUNGLE: &[u8] = include_bytes!("tables/jungle.bin");
const DESERT: &[u8] = include_bytes!("tables/desert.bin");
const ICE: &[u8] = include_bytes!("tables/ice.bin");
const TWILIGHT: &[u8] = include_bytes!("tables/twilight.bin");

/// Number of creep palette-index slots in a table blob (`cv5[1]`'s first 13 megatile slots).
const CREEP_SLOTS: usize = 13;
/// Byte size of the trailing 256-entry `[r, g, b]` palette.
const PALETTE_BYTES: usize = 768;

/// A parsed minimap color table: a tile id -> palette index map, 13 creep palette indices, and
/// the 256-entry RGB palette they index into. Borrows from the embedded blob (or, in tests, a
/// synthetic one), so it's cheap to look up per-tileset each render.
#[derive(Debug, Clone, Copy)]
pub(crate) struct MinimapTable<'a> {
    indices: &'a [u8],
    creep: &'a [u8; CREEP_SLOTS],
    palette: &'a [u8],
}

impl<'a> MinimapTable<'a> {
    /// Resolves a palette index to its `[r, g, b]` color. Out-of-range (shouldn't happen — the
    /// palette always has all 256 entries once a table parses at all) reads as black rather than
    /// panicking.
    pub(crate) fn color(&self, palette_index: u8) -> [u8; 3] {
        let offset = palette_index as usize * 3;
        match self.palette.get(offset..offset + 3) {
            Some(rgb) => [rgb[0], rgb[1], rgb[2]],
            None => [0, 0, 0],
        }
    }

    /// The minimap color for a CHK *unified* tile id (creep flag already stripped — see
    /// `broodmap::chk::terrain::TileId::id`). An id past the table's range (a tileset whose CV5
    /// declares fewer groups than the map's tiles reference) resolves to palette index 0's color,
    /// per `docs/render-design.md`'s degrade-to-index-0 rule.
    pub(crate) fn tile_color(&self, unified_tile_id: u16) -> [u8; 3] {
        let index = self
            .indices
            .get(unified_tile_id as usize)
            .copied()
            .unwrap_or(0);
        self.color(index)
    }

    /// The minimap color for creep slot `slot` (0..13, see `crate::minimap`'s creep hash). Out of
    /// range degrades to palette index 0's color, same as [`Self::tile_color`].
    pub(crate) fn creep_color(&self, slot: usize) -> [u8; 3] {
        let index = self.creep.get(slot).copied().unwrap_or(0);
        self.color(index)
    }
}

/// The committed minimap color table for `tileset`, or `None` if its embedded blob is malformed
/// (shorter than its own declared length, or too short to hold a header/creep/palette at all).
pub(crate) fn table(tileset: Tileset) -> Option<MinimapTable<'static>> {
    let bytes = match tileset {
        Tileset::Badlands => BADLANDS,
        Tileset::SpacePlatform => PLATFORM,
        Tileset::Installation => INSTALL,
        Tileset::Ashworld => ASHWORLD,
        Tileset::Jungle => JUNGLE,
        Tileset::Desert => DESERT,
        Tileset::Arctic => ICE,
        Tileset::Twilight => TWILIGHT,
    };
    parse_table(bytes)
}

/// Parses a table blob (see the module docs for the layout). `None` for anything shorter than its
/// own declared length demands — never a panic, matching every other parser in this workspace.
pub(crate) fn parse_table(bytes: &[u8]) -> Option<MinimapTable<'_>> {
    let num_tile_ids = u16::from_le_bytes(bytes.get(0..2)?.try_into().ok()?) as usize;
    let indices_end = 2usize.checked_add(num_tile_ids)?;
    let creep_end = indices_end.checked_add(CREEP_SLOTS)?;
    let palette_end = creep_end.checked_add(PALETTE_BYTES)?;

    let indices = bytes.get(2..indices_end)?;
    let creep: &[u8; CREEP_SLOTS] = bytes.get(indices_end..creep_end)?.try_into().ok()?;
    let palette = bytes.get(creep_end..palette_end)?;

    Some(MinimapTable {
        indices,
        creep,
        palette,
    })
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    /// Builds a synthetic table blob for tests: `num_tile_ids` indices (all set to `fill_index`),
    /// 13 creep indices (all `creep_index`), and a 256-entry palette where palette index `i`
    /// resolves to `[i, i, i]` (so a test can assert on a specific gray level to prove which
    /// index actually got sampled).
    pub(crate) fn synthetic_table_bytes(
        num_tile_ids: u16,
        fill_index: u8,
        creep_index: u8,
    ) -> Vec<u8> {
        let mut data = Vec::new();
        data.extend_from_slice(&num_tile_ids.to_le_bytes());
        data.extend(std::iter::repeat_n(fill_index, num_tile_ids as usize));
        data.extend([creep_index; CREEP_SLOTS]);
        for i in 0..=u8::MAX {
            data.extend([i, i, i]);
        }
        data
    }

    #[test]
    fn parses_a_well_formed_blob() {
        let bytes = synthetic_table_bytes(4, 7, 9);
        let table = parse_table(&bytes).unwrap();
        assert_eq!(table.tile_color(0), [7, 7, 7]);
        assert_eq!(table.tile_color(3), [7, 7, 7]);
        assert_eq!(table.creep_color(0), [9, 9, 9]);
        assert_eq!(table.color(200), [200, 200, 200]);
    }

    #[test]
    fn out_of_range_tile_id_degrades_to_index_0() {
        let bytes = synthetic_table_bytes(4, 7, 9);
        let table = parse_table(&bytes).unwrap();
        // Index 0's palette entry is [0, 0, 0] by `synthetic_table_bytes`'s construction.
        assert_eq!(table.tile_color(999), [0, 0, 0]);
        assert_eq!(table.creep_color(999), [0, 0, 0]);
    }

    #[test]
    fn truncated_blob_is_none() {
        assert!(parse_table(&[]).is_none());
        assert!(parse_table(&[4, 0]).is_none()); // claims 4 tile ids, has none
        let mut bytes = synthetic_table_bytes(4, 7, 9);
        bytes.truncate(bytes.len() - 1);
        assert!(parse_table(&bytes).is_none());
    }

    #[test]
    fn empty_placeholder_blob_is_none() {
        // Before `gen-minimap-tables` has been run against a real install, the committed .bin
        // files are empty placeholders -- must degrade to `None`, never panic.
        assert!(parse_table(&[]).is_none());
    }

    /// All 8 committed tables must parse and have a sane (nonzero) tile count once
    /// `gen-minimap-tables` has populated them -- a cheap regression guard against a table
    /// silently going stale/empty. Not gated on `BROODMAP_TEST_SCR_DIR`: the committed files
    /// themselves are checked in, not fetched.
    #[test]
    fn all_committed_tables_parse_with_a_sane_tile_count() {
        for tileset in [
            Tileset::Badlands,
            Tileset::SpacePlatform,
            Tileset::Installation,
            Tileset::Ashworld,
            Tileset::Jungle,
            Tileset::Desert,
            Tileset::Arctic,
            Tileset::Twilight,
        ] {
            let Some(table) = table(tileset) else {
                panic!(
                    "{tileset:?}: committed minimap table is missing/malformed -- run \
                     `cargo run -p broodmap-cli -- gen-minimap-tables`"
                );
            };
            assert!(
                !table.indices.is_empty(),
                "{tileset:?}: committed minimap table has zero tile ids"
            );
        }
    }
}
