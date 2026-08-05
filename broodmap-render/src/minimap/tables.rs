//! The 8 committed, per-tileset minimap color tables, baked at dev time (see
//! `crate::minimap::build_minimap_table` and `broodmap-cli`'s hidden `gen-minimap-tables`
//! subcommand) and embedded via `include_bytes!` so runtime minimap rendering needs no game
//! assets at all — just these small blobs, compiled into the crate.
//!
//! The embedded `.bin` files are DEFLATE-compressed (the raw blobs are mostly repeated palette
//! indices, so this shrinks them roughly ninefold). [`table`] inflates a tileset's blob on first
//! use and caches the result, so only the tilesets a program actually renders are ever
//! decompressed. The inflated bytes have this layout (little-endian):
//!
//! ```text
//! [0..2)          u16   num_tile_ids                 (= cv5 group count * 16)
//! [2..2+4n)       u8    FOUR quadrant palette indices per tile id, in TL,TR,BL,BR order
//!                       (minitiles 0,1,4,5, byte 55, flip ignored; missing lookup -> 0)
//! [2+4n..+768)    u8    256 x [r,g,b] (the tileset's .wpe palette)
//! ```
//!
//! [`parse_table`] is the no-panic core reading that layout, so a malformed blob (a bad DEFLATE
//! stream or a length shorter than the header demands) degrades to `None` — `crate::minimap` then
//! renders black terrain for that tileset rather than failing the whole render.

use std::sync::OnceLock;

use broodmap::chk::tileset::Tileset;

use crate::minimap::decompress_minimap_table;

const BADLANDS: &[u8] = include_bytes!("tables/badlands.bin");
const PLATFORM: &[u8] = include_bytes!("tables/platform.bin");
const INSTALL: &[u8] = include_bytes!("tables/install.bin");
const ASHWORLD: &[u8] = include_bytes!("tables/ashworld.bin");
const JUNGLE: &[u8] = include_bytes!("tables/jungle.bin");
const DESERT: &[u8] = include_bytes!("tables/desert.bin");
const ICE: &[u8] = include_bytes!("tables/ice.bin");
const TWILIGHT: &[u8] = include_bytes!("tables/twilight.bin");

/// Quadrant palette indices stored per tile id: TL, TR, BL, BR.
const QUADRANTS_PER_TILE: usize = 4;
/// Byte size of the trailing 256-entry `[r, g, b]` palette.
const PALETTE_BYTES: usize = 768;

/// A parsed minimap color table: a tile id -> four quadrant palette indices map, plus the
/// 256-entry RGB palette they index into. Borrows from the embedded blob (or, in tests, a
/// synthetic one), so it's cheap to look up per-tileset each render.
#[derive(Debug, Clone, Copy)]
pub(crate) struct MinimapTable<'a> {
    /// `QUADRANTS_PER_TILE` (4) palette-index bytes per unified tile id, TL/TR/BL/BR order.
    quadrants: &'a [u8],
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

    /// The minimap color for one quadrant (`0` = TL, `1` = TR, `2` = BL, `3` = BR — see
    /// `crate::minimap`'s module docs for the sampling rule) of a CHK *unified* tile id (creep
    /// flag already stripped — see `broodmap::chk::terrain::TileId::id`). An out-of-range tile id
    /// (a tileset whose CV5 declares fewer groups than the map's tiles reference) resolves to
    /// palette index 0's color, per `docs/render-design.md`'s degrade-to-index-0 rule; so does an
    /// out-of-range `quadrant` (never reached by this crate's own callers, which only ever pass
    /// `0..4`, but kept safe rather than panicking).
    pub(crate) fn quadrant_color(&self, unified_tile_id: u16, quadrant: usize) -> [u8; 3] {
        let base = unified_tile_id as usize * QUADRANTS_PER_TILE;
        let index = self.quadrants.get(base + quadrant).copied().unwrap_or(0);
        self.color(index)
    }
}

/// The committed minimap color table for `tileset`, or `None` if its embedded blob is malformed
/// (an invalid DEFLATE stream, or an inflated length too short to hold the header/palette). The
/// blob is inflated on first use per tileset and the result cached, so repeat renders of the same
/// tileset pay the decompression cost once.
pub(crate) fn table(tileset: Tileset) -> Option<MinimapTable<'static>> {
    let (slot, compressed) = compressed_blob(tileset);
    let inflated = INFLATED[slot]
        .get_or_init(|| decompress_minimap_table(compressed))
        .as_deref()?;
    parse_table(inflated)
}

/// The compressed blob and its [`INFLATED`] cache slot for each tileset. The slot indices are
/// private to this pairing (they only key the decompression cache), so they need no relation to
/// `Tileset`'s own discriminants.
fn compressed_blob(tileset: Tileset) -> (usize, &'static [u8]) {
    match tileset {
        Tileset::Badlands => (0, BADLANDS),
        Tileset::SpacePlatform => (1, PLATFORM),
        Tileset::Installation => (2, INSTALL),
        Tileset::Ashworld => (3, ASHWORLD),
        Tileset::Jungle => (4, JUNGLE),
        Tileset::Desert => (5, DESERT),
        Tileset::Arctic => (6, ICE),
        Tileset::Twilight => (7, TWILIGHT),
    }
}

/// Per-tileset cache of inflated table bytes, filled lazily by [`table`]. `None` in a slot means
/// that tileset's embedded blob failed to inflate.
static INFLATED: [OnceLock<Option<Vec<u8>>>; 8] = [const { OnceLock::new() }; 8];

/// Parses a table blob (see the module docs for the layout). `None` for anything shorter than its
/// own declared length demands — never a panic, matching every other parser in this workspace.
pub(crate) fn parse_table(bytes: &[u8]) -> Option<MinimapTable<'_>> {
    let num_tile_ids = u16::from_le_bytes(bytes.get(0..2)?.try_into().ok()?) as usize;
    let quadrants_len = num_tile_ids.checked_mul(QUADRANTS_PER_TILE)?;
    let quadrants_end = 2usize.checked_add(quadrants_len)?;
    let palette_end = quadrants_end.checked_add(PALETTE_BYTES)?;

    let quadrants = bytes.get(2..quadrants_end)?;
    let palette = bytes.get(quadrants_end..palette_end)?;

    Some(MinimapTable { quadrants, palette })
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    /// Builds a synthetic table blob from explicit per-tile quadrant indices (TL, TR, BL, BR),
    /// with an identity palette (palette index `i` resolves to `[i, i, i]`) so a test can assert
    /// on a specific gray level to prove which index actually got sampled.
    pub(crate) fn synthetic_table_bytes_with_quadrants(entries: &[[u8; 4]]) -> Vec<u8> {
        let num_tile_ids = entries.len() as u16;
        let mut data = Vec::new();
        data.extend_from_slice(&num_tile_ids.to_le_bytes());
        for q in entries {
            data.extend_from_slice(q);
        }
        for i in 0..=u8::MAX {
            data.extend([i, i, i]);
        }
        data
    }

    /// [`synthetic_table_bytes_with_quadrants`], with every quadrant of every tile id set to the
    /// same `fill_index` — the common case for tests that don't care about quadrant-vs-quadrant
    /// differences.
    pub(crate) fn synthetic_table_bytes(num_tile_ids: u16, fill_index: u8) -> Vec<u8> {
        synthetic_table_bytes_with_quadrants(&vec![[fill_index; 4]; num_tile_ids as usize])
    }

    #[test]
    fn parses_a_well_formed_blob() {
        let bytes = synthetic_table_bytes(4, 7);
        let table = parse_table(&bytes).unwrap();
        assert_eq!(table.quadrant_color(0, 0), [7, 7, 7]);
        assert_eq!(table.quadrant_color(3, 3), [7, 7, 7]);
        assert_eq!(table.color(200), [200, 200, 200]);
    }

    #[test]
    fn quadrants_can_differ_within_a_tile() {
        let bytes = synthetic_table_bytes_with_quadrants(&[[1, 2, 3, 4]]);
        let table = parse_table(&bytes).unwrap();
        assert_eq!(table.quadrant_color(0, 0), [1, 1, 1]);
        assert_eq!(table.quadrant_color(0, 1), [2, 2, 2]);
        assert_eq!(table.quadrant_color(0, 2), [3, 3, 3]);
        assert_eq!(table.quadrant_color(0, 3), [4, 4, 4]);
    }

    #[test]
    fn out_of_range_tile_id_degrades_to_index_0() {
        let bytes = synthetic_table_bytes(4, 7);
        let table = parse_table(&bytes).unwrap();
        // Index 0's palette entry is [0, 0, 0] by `synthetic_table_bytes`'s construction.
        assert_eq!(table.quadrant_color(999, 0), [0, 0, 0]);
    }

    #[test]
    fn truncated_blob_is_none() {
        assert!(parse_table(&[]).is_none());
        assert!(parse_table(&[4, 0]).is_none()); // claims 4 tile ids, has none
        let mut bytes = synthetic_table_bytes(4, 7);
        bytes.truncate(bytes.len() - 1);
        assert!(parse_table(&bytes).is_none());
    }

    #[test]
    fn empty_placeholder_blob_is_none() {
        // Before `gen-minimap-tables` has been run against a real install, the committed .bin
        // files are empty placeholders -- must degrade to `None`, never panic.
        assert!(parse_table(&[]).is_none());
    }

    #[test]
    fn blob_with_fewer_than_four_bytes_per_tile_is_none() {
        // A blob that only stores 1 byte per tile id (plus 13 unrelated trailing bytes and the
        // palette) instead of the required 4: shorter than the header's declared `num_tile_ids`
        // demands, so it must fail to parse rather than reading past its own quadrant section.
        // (At `num_tile_ids <= 4` the size difference from a real table is small enough that
        // this kind of blob could coincidentally still satisfy the length check; this uses a
        // tile count large enough that can never happen, matching every real committed table.)
        let num_tile_ids: u16 = 100;
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&num_tile_ids.to_le_bytes());
        bytes.extend(std::iter::repeat_n(7u8, num_tile_ids as usize)); // 1 byte per tile id
        bytes.extend([9u8; 13]); // unrelated trailing bytes
        for i in 0..=u8::MAX {
            bytes.extend([i, i, i]);
        }
        assert!(parse_table(&bytes).is_none());
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
                     `cargo run -p broodmap-cli -- gen-minimap-tables` against a real install"
                );
            };
            assert!(
                !table.quadrants.is_empty(),
                "{tileset:?}: committed minimap table has zero tile ids"
            );
        }
    }
}
