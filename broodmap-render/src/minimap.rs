//! The zero-asset minimap renderer: a native, low-resolution "texture" sampled straight from the
//! terrain's baked colors (see `tables`) plus unit/resource/start-location dots, magnified by an
//! integer nearest-neighbor upscale — matching how StarCraft: Remastered's own minimap actually
//! works, not a downsampled preview. See `docs/render-design.md`'s "Minimap" section for the full
//! design (pinned and authoritative); this module implements it exactly.
//!
//! Unlike [`crate::render_terrain`]/[`crate::render_preview`], nothing here ever calls a
//! [`crate::TilesetDataSource`]: [`render_minimap`]/[`render_chk_minimap`] take a `Chk` (or its
//! pieces) and an optional, already-loaded [`GameData`] and produce an image directly. This is
//! the "usable on a server or in WASM with nothing fetched at all" fast path the design doc
//! promises — `data: None` must still render a complete, correct minimap; it only loses the
//! `units.dat`-derived refinements documented on [`MinimapOptions`] and below.
//!
//! # BW's minimap algorithm
//!
//! Reverse-engineered from the real `StarCraft.1.23.10` binary and empirically validated against
//! the real `(2)Space Madness.scm` minimap. The game does **not** render one sample per map tile
//! at output resolution; it builds a small native "texture" (at most 128px per side) and the UI
//! magnifies that. The native size depends on `M = max(map_w, map_h)` in tiles:
//!
//! Each tile carries four quadrant samples — minitiles `[0]`, `[1]`, `[4]`, `[5]` of its 4x4
//! minitile grid (the grid's top-left 2x2), each sampled at byte 55 (see below). How those four
//! feed the native pixels depends on the map's size:
//!
//! - **`M <= 64`**: `native_ppt = 2` — every tile becomes a 2x2 block of native pixels showing
//!   the four quadrants *distinctly* (TL/TR/BL/BR), so a small map renders at full sample detail.
//! - **`65 <= M <= 128`**: `native_ppt = 1` — one native pixel per tile, colored by the **mean**
//!   of the tile's four quadrants.
//! - **`M > 128`**: subsampled — one native pixel per 2x2 *tile* block, colored by the mean over
//!   every quadrant of the (up to four) tiles in the block. The native image is always `<= 128`
//!   px per side.
//!
//! Averaging once a tile collapses to a single native pixel is a deliberate departure from the
//! game, which samples a single quadrant there: at the game's own ~128px minimap size a lone dark
//! quadrant texel (a crevice or edge pixel in the source art) is one invisible pixel, but under
//! this crate's magnification it would paint a whole tile dark — the mean keeps that from
//! happening while landing at the same per-tile color density the game shows.
//!
//! Per sampled minitile: look it up in VX4(EX) (the horizontal-flip bit is **ignored** — the
//! game's minimap path deliberately doesn't apply it), then sample byte 55 (row 6, column 7) of
//! that minitile's 8x8 VR4 bitmap; the resulting palette index, resolved through WPE, is the
//! sample's color. The real game also runs the resolved palette index through a small runtime
//! remap LUT that could not be recovered (it's built at runtime, not present in static data);
//! empirically, using the palette index directly (an identity LUT) matches the real minimap
//! closely, so that's what this crate does — a documented, minor, known divergence.
//!
//! Creep needs no special handling: BW masks the creep flag (`tile & 0x7FFF`,
//! [`broodmap::chk::terrain::TileId::id`]) and samples the resulting tile id exactly like any
//! other — creep megatiles are already baked into a map's tile ids by the editor/game, so the
//! ordinary per-tile-id table covers them with no separate creep-group table or per-cell RNG.
//!
//! The native image (terrain *and* dots together) is then magnified by [`MinimapOptions::scale`],
//! an integer nearest-neighbor upscale — reproducing "a small fixed texture displayed larger"
//! exactly, rather than rendering at output resolution directly.
//!
//! [`build_minimap_table`] bakes the terrain-sampling half of this into small per-tileset tables
//! (dev-time only, run via `broodmap-cli`'s hidden `gen-minimap-tables` subcommand against a real
//! install); `tables` embeds the 8 committed outputs via `include_bytes!` so render time never
//! touches VX4EX/VR4/WPE (or even a [`crate::TilesetDataSource`]) at all.

mod tables;

use broodmap::chk::Chk;
use broodmap::chk::placed_units::{PlacedUnit, UnitState};
use broodmap::chk::player_colors::PlayerColors;
use broodmap::chk::sprites::{Sprite, SpriteFlags};
use broodmap::chk::terrain::TerrainTileIds;
use broodmap::chk::tileset::Tileset;
use broodmap_formats::{Cv5, Vr4, Vx4, parse_cv5, parse_vr4, parse_vx4ex, parse_wpe};

use crate::error::RenderError;
use crate::gamedata::{GameData, UNIT_ID_START_LOCATION, is_resource};
use crate::image::RgbaImage;
use crate::options::{StartLocations, UnitFilter};
use crate::overlay::{
    MAX_START_LOCATION_BOX_LOGICAL_PX, Preview, hq_clear_bounds, overlaps_any_start_area,
    owner_color, start_location_box,
};
use crate::token::draw_player_token;
use tables::MinimapTable;

/// The CHK parser's own documented invariant on map dimensions — see `crate::terrain`'s identical
/// constant for why this is re-clamped here rather than trusted from `TerrainTileIds`.
const MAX_TERRAIN_DIM: usize = 256;

/// Upper bound [`MinimapOptions::scale`] is clamped to at render time.
const MAX_SCALE: u32 = 16;

/// Which minitile of a megatile's 4x4 grid each native-pixel "quadrant" samples, in TL/TR/BL/BR
/// order — the top-left 2x2 quadrant of the grid (see the module docs's "BW's minimap algorithm"
/// section). Shared by [`build_minimap_table`] (which bakes all four) and [`draw_terrain`] (which
/// picks a subset of them depending on `native_ppt`).
const QUADRANT_MINITILE_INDEX: [usize; 4] = [0, 1, 4, 5];

/// The color resources are highlighted in when [`MinimapOptions::highlight_resources`] is on,
/// instead of their owner's color (mineral fields and geysers are conventionally neutral anyway).
/// The same cyan SC:R uses for neutral/unowned art elsewhere in this crate (see
/// `crate::overlay::NEUTRAL_COLOR`) — reused here under its own name because the *meaning* is
/// different (a deliberate highlight, not "no owner"), even though the value coincides.
pub const RESOURCE_MINIMAP_COLOR: [u8; 3] = crate::overlay::NEUTRAL_COLOR;

// -------------------------------------------------------------------------------------------
// Table generation (dev-time)
// -------------------------------------------------------------------------------------------

/// Builds one tileset's baked minimap color table from its classic `.cv5`/`.vx4ex`/`.vr4`/`.wpe`
/// bytes, per `docs/render-design.md`'s "Minimap" section (the algorithm there is pinned and
/// authoritative — this is a direct implementation of it, not a reinterpretation).
///
/// **Dev-time only.** This is never called at render time (see the module docs): it's used by
/// `broodmap-cli`'s hidden `gen-minimap-tables` subcommand to produce the committed blobs under
/// `minimap/tables/*.bin`, and by the `casc`-gated drift test in `tests/real_assets.rs` that
/// re-derives `jungle.bin` from a real install and byte-compares it against the committed copy.
/// It's `pub` (rather than `pub(crate)`) so both of those callers, which live outside this crate's
/// `src/` tree, can reach it.
///
/// Output layout (little-endian), matching `tables.rs`'s parser exactly:
///
/// ```text
/// [0..2)          u16   num_tile_ids                 (= cv5 group count * 16)
/// [2..2+4n)       u8    FOUR quadrant palette indices per tile id, TL/TR/BL/BR order
/// [2+4n..+768)    u8    256 x [r, g, b] palette (from the tileset's .wpe)
/// ```
///
/// Per tile id `t` and quadrant (TL/TR/BL/BR, i.e. minitile index 0/1/4/5): group = `t >> 4`,
/// index = `t & 15`; megatile = `cv5.group(group)?.mega_tiles[index]`; that quadrant's minitile
/// (from `.vx4ex`, ignoring its flip bit — deliberate, see the module docs); the palette index is
/// byte 55 of that minitile's 64-byte `.vr4` bitmap. Any lookup failure along that chain (missing
/// group, an out-of-range megatile/minitile id) degrades to palette index 0, never an error —
/// only a too-short `.wpe` (the one truly load-bearing input; without it there's no palette to
/// resolve *any* index through) fails the whole call.
pub fn build_minimap_table(
    cv5: &[u8],
    vx4ex: &[u8],
    vr4: &[u8],
    wpe: &[u8],
) -> Result<Vec<u8>, RenderError> {
    let cv5 = parse_cv5(cv5);
    let vx4ex = parse_vx4ex(vx4ex);
    let vr4 = parse_vr4(vr4);
    let wpe = parse_wpe(wpe)?;

    let num_tile_ids = u16::try_from(cv5.groups.len().saturating_mul(16)).unwrap_or(u16::MAX);
    let mut quadrants = Vec::with_capacity(num_tile_ids as usize * QUADRANT_MINITILE_INDEX.len());
    for unified_tile_id in 0..num_tile_ids {
        quadrants.extend_from_slice(&sample_tile_quadrants(&cv5, &vx4ex, &vr4, unified_tile_id));
    }

    let mut out = Vec::with_capacity(2 + quadrants.len() + 768);
    out.extend_from_slice(&num_tile_ids.to_le_bytes());
    out.extend_from_slice(&quadrants);
    for i in 0..=u8::MAX {
        out.extend_from_slice(&wpe.color(i));
    }

    Ok(out)
}

/// The DEFLATE level used to pack the committed table blobs. Highest level for the smallest
/// committed size; decompression speed doesn't depend on it, and the level is fixed so the
/// compressed bytes stay reproducible (the drift guard compares them byte-for-byte).
const TABLE_DEFLATE_LEVEL: u8 = 10;

/// DEFLATE-compresses a raw table blob (as [`build_minimap_table`] returns) into the form stored
/// on disk under `src/minimap/tables/*.bin` and embedded via `include_bytes!`. The blobs are
/// mostly repeated palette indices, so this shrinks them roughly ninefold. Used by the
/// `gen-minimap-tables` tool to write the committed files and by the drift guard to check them;
/// [`decompress_minimap_table`] is the inverse applied at load.
pub fn compress_minimap_table(raw: &[u8]) -> Vec<u8> {
    miniz_oxide::deflate::compress_to_vec(raw, TABLE_DEFLATE_LEVEL)
}

/// Inflates a committed table blob back to the raw layout [`tables::parse_table`] reads. `None`
/// if the bytes aren't valid DEFLATE (a corrupted embedded file — treated like any other
/// malformed table, degrading to no minimap colors rather than panicking).
pub(crate) fn decompress_minimap_table(compressed: &[u8]) -> Option<Vec<u8>> {
    miniz_oxide::inflate::decompress_to_vec(compressed).ok()
}

/// Resolves a CHK unified tile id to its four quadrant (TL/TR/BL/BR) minimap palette indices:
/// `t >> 4` picks the CV5 group, `t & 15` the megatile within it, then
/// [`sample_megatile_palette_index`] for each of [`QUADRANT_MINITILE_INDEX`]. A missing group (an
/// id the tileset's CV5 doesn't define) degrades every quadrant to index 0.
fn sample_tile_quadrants(
    cv5: &Cv5,
    vx4ex: &Vx4<'_>,
    vr4: &Vr4<'_>,
    unified_tile_id: u16,
) -> [u8; 4] {
    let group_id = unified_tile_id >> 4;
    let index = (unified_tile_id & 0xF) as usize;
    let Some(group) = cv5.group(group_id) else {
        return [0; 4];
    };
    let megatile_id = group.mega_tiles[index];
    let mut out = [0u8; 4];
    for (slot, &minitile_index) in QUADRANT_MINITILE_INDEX.iter().enumerate() {
        out[slot] = sample_megatile_palette_index(vx4ex, vr4, megatile_id, minitile_index);
    }
    out
}

/// Samples one minitile of a megatile's 4x4 grid for its minimap palette index: byte 55 of that
/// minitile's VR4 bitmap (flip bit ignored — see the module docs). Any missing lookup (an
/// out-of-range megatile or minitile id) degrades to index 0.
fn sample_megatile_palette_index(
    vx4ex: &Vx4<'_>,
    vr4: &Vr4<'_>,
    megatile_id: u16,
    minitile_index: usize,
) -> u8 {
    let Some(minitile) = vx4ex.minitile(megatile_id as usize, minitile_index) else {
        return 0;
    };
    let Some(bitmap) = vr4.bitmap(minitile.vr4_index as usize) else {
        return 0;
    };
    bitmap[55]
}

// -------------------------------------------------------------------------------------------
// Options
// -------------------------------------------------------------------------------------------

/// Options controlling how a minimap is rendered. Deliberately smaller than [`RenderOptions`]
/// (`crate::RenderOptions`): the minimap has no art style/tier to pick (it's zero-asset by
/// construction) and no separate critter/neutral-building/doodad toggles — see the field docs and
/// [`render_minimap`]'s module-level docs for how filtering is simplified accordingly.
#[derive(Debug, Clone)]
pub struct MinimapOptions {
    /// Integer nearest-neighbor upscale applied to the native minimap image (see the module
    /// docs's "BW's minimap algorithm" section — the native image is always `<= 128` px per
    /// side). Clamped to `1..=16` at render time (not at construction, so a caller can freely
    /// set/read any value). `1` (no upscale — BW's own native texture size) is the default.
    pub scale: u32,
    /// Draw ordinary (non-resource) unit/THG2-unit-sprite dots. Default `true`. There is no
    /// separate critter/neutral-building toggle here (unlike [`RenderOptions`]): this single flag
    /// covers everything [`Self::show_resources`] doesn't.
    pub show_units: bool,
    /// Draw mineral field / vespene geyser dots. Default `true`.
    pub show_resources: bool,
    /// Draw resource dots in [`RESOURCE_MINIMAP_COLOR`] instead of their owner's color. Default
    /// `true`.
    pub highlight_resources: bool,
    /// How start locations are drawn. [`StartLocations::Sprite`] behaves exactly like
    /// [`StartLocations::ColorBlock`] here — there is no art to draw in a zero-asset render, so
    /// the "actual in-game graphic" mode falls back to the same token used for the default. See
    /// `crate::RenderOptions::start_locations` for the shared enum's full docs.
    pub start_locations: StartLocations,
    /// Which placed units survive into the render. See `crate::RenderOptions::unit_filter`.
    pub unit_filter: UnitFilter,
}

impl Default for MinimapOptions {
    fn default() -> Self {
        Self {
            scale: 1,
            show_units: true,
            show_resources: true,
            highlight_resources: true,
            start_locations: StartLocations::default(),
            unit_filter: UnitFilter::default(),
        }
    }
}

// -------------------------------------------------------------------------------------------
// Native-resolution layout
// -------------------------------------------------------------------------------------------

/// Which of BW's three map-size-dependent terrain-sampling modes applies (see the module docs's
/// "BW's minimap algorithm" section).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NativeMode {
    /// `M <= 64`: every tile becomes a 2x2 native block sampling all four quadrants distinctly.
    Quad,
    /// `65 <= M <= 128`: one native pixel per tile, colored by the mean of the tile's four
    /// quadrants (so a single dark quadrant texel can't dominate a whole tile once magnified).
    Single,
    /// `M > 128`: one native pixel per 2x2 *tile* block, colored by the mean over every quadrant
    /// of the (up to four) tiles in that block.
    Subsample,
}

/// Resolves `(map_w, map_h)` (in tiles) to the native sampling mode, the native pixels-per-tile
/// scalar used for dot/border positioning math (`2.0`, `1.0`, or `0.5` — see the module docs),
/// and the native base image's dimensions (always `<= 128` px per side).
fn native_layout(map_w: u32, map_h: u32) -> (NativeMode, f32, u32, u32) {
    let m = map_w.max(map_h);
    if m <= 64 {
        (NativeMode::Quad, 2.0, map_w * 2, map_h * 2)
    } else if m <= 128 {
        (NativeMode::Single, 1.0, map_w, map_h)
    } else {
        (
            NativeMode::Subsample,
            0.5,
            map_w.div_ceil(2),
            map_h.div_ceil(2),
        )
    }
}

// -------------------------------------------------------------------------------------------
// Rendering
// -------------------------------------------------------------------------------------------

/// Renders a zero-asset minimap: BW's native low-resolution terrain texture plus unit/resource/
/// start-location dots, magnified by [`MinimapOptions::scale`]. Needs no [`crate::TilesetDataSource`]
/// at all — see the module docs.
///
/// `data` is an already-loaded [`GameData`] (from [`GameData::load`]), used only to size unit/
/// resource dots from `units.dat` (building vs. non-building) and to apply melee's start-area
/// clearing; `None` still renders a complete, correct minimap (the zero-asset promise), just with
/// every dot at the fixed 2-native-px non-building size and no start-area clearing (documented on
/// [`MinimapOptions`]'s fields and below).
pub fn render_minimap(
    terrain: &TerrainTileIds,
    tileset: Tileset,
    units: &[PlacedUnit],
    sprites: &[Sprite],
    player_colors: &PlayerColors,
    data: Option<&GameData>,
    options: &MinimapOptions,
) -> RgbaImage {
    render_minimap_with_table(
        terrain,
        tables::table(tileset),
        units,
        sprites,
        player_colors,
        data,
        options,
    )
}

/// [`render_minimap`], taking an already-resolved [`MinimapTable`] instead of a [`Tileset`] —
/// factored out so tests can inject a small synthetic table instead of depending on the (real-
/// install-derived, and large) committed tables. [`render_minimap`] is the thin public wrapper
/// that resolves `tileset` through [`tables::table`].
fn render_minimap_with_table(
    terrain: &TerrainTileIds,
    table: Option<MinimapTable<'_>>,
    units: &[PlacedUnit],
    sprites: &[Sprite],
    player_colors: &PlayerColors,
    data: Option<&GameData>,
    options: &MinimapOptions,
) -> RgbaImage {
    let map_w = terrain.width.min(MAX_TERRAIN_DIM) as u32;
    let map_h = terrain.height.min(MAX_TERRAIN_DIM) as u32;
    if map_w == 0 || map_h == 0 {
        return RgbaImage {
            width: 0,
            height: 0,
            data: Vec::new(),
        };
    }

    let (mode, native_ppt, base_w, base_h) = native_layout(map_w, map_h);
    let mut base = RgbaImage {
        width: base_w,
        height: base_h,
        data: vec![0u8; base_w as usize * base_h as usize * 4],
    };

    draw_terrain(&mut base, terrain, map_w, map_h, mode, &table);

    let dots = collect_dots(units, sprites, data, options);
    // Paint order: resources, then ordinary units, then start locations (always on top) — see
    // the module docs. All three passes draw into the native base image; the whole thing (terrain
    // and dots alike) is upscaled together afterward.
    draw_dots(
        &mut base,
        &dots,
        true,
        player_colors,
        data,
        options,
        native_ppt,
    );
    draw_dots(
        &mut base,
        &dots,
        false,
        player_colors,
        data,
        options,
        native_ppt,
    );
    draw_start_locations(&mut base, units, player_colors, data, options, native_ppt);

    let scale = options.scale.clamp(1, MAX_SCALE);
    if scale <= 1 {
        base
    } else {
        nearest_upscale(&base, scale)
    }
}

/// Renders a minimap straight from a parsed [`Chk`]. Mirrors [`crate::render_chk_preview`]'s
/// empty-image/warning fallback for unreadable terrain; a map with no `UNIT`/`THG2` chunk simply
/// has no units/sprites (not an error).
pub fn render_chk_minimap(chk: &Chk, data: Option<&GameData>, options: &MinimapOptions) -> Preview {
    let terrain = match chk.terrain() {
        Ok(terrain) => terrain,
        Err(_) => {
            return Preview {
                image: RgbaImage {
                    width: 0,
                    height: 0,
                    data: Vec::new(),
                },
                warnings: vec!["map has no readable terrain".to_string()],
            };
        }
    };

    static NO_UNITS: &[PlacedUnit] = &[];
    static NO_SPRITES: &[Sprite] = &[];
    let units = chk.placed_units().map(Vec::as_slice).unwrap_or(NO_UNITS);
    let sprites = chk.sprites().map(Vec::as_slice).unwrap_or(NO_SPRITES);

    let image = render_minimap(
        terrain,
        chk.tileset(),
        units,
        sprites,
        chk.player_colors(),
        data,
        options,
    );
    Preview {
        image,
        warnings: Vec::new(),
    }
}

// -------------------------------------------------------------------------------------------
// Terrain pass (native resolution)
// -------------------------------------------------------------------------------------------

/// Draws the native-resolution terrain image per [`NativeMode`] — see the module docs's "BW's
/// minimap algorithm" section for exactly which minitile each native pixel samples.
fn draw_terrain(
    base: &mut RgbaImage,
    terrain: &TerrainTileIds,
    map_w: u32,
    map_h: u32,
    mode: NativeMode,
    table: &Option<MinimapTable<'_>>,
) {
    match mode {
        NativeMode::Quad => {
            // Each map tile becomes a 2x2 native block; the four pixels sample four distinct
            // quadrants: (dx, dy, quadrant) = TL=(0,0,0), TR=(1,0,1), BL=(0,1,2), BR=(1,1,3).
            const OFFSETS: [(u32, u32, usize); 4] = [(0, 0, 0), (1, 0, 1), (0, 1, 2), (1, 1, 3)];
            for y in 0..map_h {
                for x in 0..map_w {
                    let tile_id = tile_id_at(terrain, x, y);
                    for &(dx, dy, quadrant) in &OFFSETS {
                        let color = quadrant_pixel_color(table, tile_id, quadrant);
                        set_native_pixel(base, x * 2 + dx, y * 2 + dy, color);
                    }
                }
            }
        }
        NativeMode::Single => {
            for y in 0..map_h {
                for x in 0..map_w {
                    let color = averaged_tile_color(table, tile_id_at(terrain, x, y));
                    set_native_pixel(base, x, y, color);
                }
            }
        }
        NativeMode::Subsample => {
            for by in 0..base.height {
                for bx in 0..base.width {
                    let mut acc = [0u32; 3];
                    let mut count = 0u32;
                    for (tx, ty) in [
                        (bx * 2, by * 2),
                        (bx * 2 + 1, by * 2),
                        (bx * 2, by * 2 + 1),
                        (bx * 2 + 1, by * 2 + 1),
                    ] {
                        if tx < map_w && ty < map_h {
                            let c = averaged_tile_color(table, tile_id_at(terrain, tx, ty));
                            for k in 0..3 {
                                acc[k] += c[k] as u32;
                            }
                            count += 1;
                        }
                    }
                    let color = [
                        acc[0].checked_div(count).unwrap_or(0) as u8,
                        acc[1].checked_div(count).unwrap_or(0) as u8,
                        acc[2].checked_div(count).unwrap_or(0) as u8,
                    ];
                    set_native_pixel(base, bx, by, color);
                }
            }
        }
    }
}

/// The mean of a tile's four quadrant colors — the representative color for a tile that collapses
/// to a single native pixel. Averaging keeps one dark quadrant texel (a crevice or edge pixel in
/// the source art) from painting a whole tile dark once the native image is magnified, which a
/// single-quadrant pick can't avoid.
fn averaged_tile_color(table: &Option<MinimapTable<'_>>, tile_id: u16) -> [u8; 3] {
    let mut acc = [0u32; 3];
    for quadrant in 0..QUADRANT_MINITILE_INDEX.len() {
        let c = quadrant_pixel_color(table, tile_id, quadrant);
        for k in 0..3 {
            acc[k] += c[k] as u32;
        }
    }
    let n = QUADRANT_MINITILE_INDEX.len() as u32;
    [(acc[0] / n) as u8, (acc[1] / n) as u8, (acc[2] / n) as u8]
}

/// The unified tile id (creep flag already masked off, see [`broodmap::chk::terrain::TileId::id`])
/// at map cell `(x, y)`, or `0` for an out-of-range cell (shouldn't happen — callers only ever
/// pass cells within `terrain.width`/`height` — but permissive rather than panicking).
fn tile_id_at(terrain: &TerrainTileIds, x: u32, y: u32) -> u16 {
    terrain
        .tiles
        .get(y as usize * terrain.width + x as usize)
        .copied()
        .unwrap_or_default()
        .id()
}

/// One quadrant's color for a unified tile id: a tileset with no (or a malformed) committed table
/// renders solid black — documented degradation, never a panic or an error.
fn quadrant_pixel_color(
    table: &Option<MinimapTable<'_>>,
    unified_tile_id: u16,
    quadrant: usize,
) -> [u8; 3] {
    match table {
        Some(table) => table.quadrant_color(unified_tile_id, quadrant),
        None => [0, 0, 0],
    }
}

/// Sets one native-resolution pixel to a flat, opaque color. Bounds-checked rather than panicking
/// (native image dimensions are always in range for the loops above, but this stays defensive).
fn set_native_pixel(image: &mut RgbaImage, x: u32, y: u32, color: [u8; 3]) {
    if x >= image.width || y >= image.height {
        return;
    }
    let offset = (y as usize * image.width as usize + x as usize) * 4;
    if let Some(texel) = image.data.get_mut(offset..offset + 4) {
        texel[0] = color[0];
        texel[1] = color[1];
        texel[2] = color[2];
        texel[3] = 255;
    }
}

// -------------------------------------------------------------------------------------------
// Nearest-neighbor upscale
// -------------------------------------------------------------------------------------------

/// Magnifies `base` by integer `scale` (`>= 2`; `scale <= 1` is handled by the caller without
/// calling this at all), replicating each native pixel into a `scale`x`scale` block — reproducing
/// "a small fixed texture displayed larger" exactly, terrain and dots alike, matching the real
/// game's own minimap magnification.
fn nearest_upscale(base: &RgbaImage, scale: u32) -> RgbaImage {
    let out_w = base.width * scale;
    let out_h = base.height * scale;
    let mut data = vec![0u8; out_w as usize * out_h as usize * 4];

    for y in 0..base.height {
        let src_row = (y as usize * base.width as usize) * 4;
        let Some(src_row_bytes) = base.data.get(src_row..src_row + base.width as usize * 4) else {
            continue;
        };
        for dy in 0..scale {
            let out_y = y * scale + dy;
            let out_row_start = (out_y as usize * out_w as usize) * 4;
            for x in 0..base.width {
                let texel_start = x as usize * 4;
                let Some(texel) = src_row_bytes.get(texel_start..texel_start + 4) else {
                    continue;
                };
                for dx in 0..scale {
                    let out_x = x * scale + dx;
                    let o = out_row_start + out_x as usize * 4;
                    if let Some(dst) = data.get_mut(o..o + 4) {
                        dst.copy_from_slice(texel);
                    }
                }
            }
        }
    }

    RgbaImage {
        width: out_w,
        height: out_h,
        data,
    }
}

// -------------------------------------------------------------------------------------------
// Unit/resource dots (native resolution)
// -------------------------------------------------------------------------------------------

/// One unit/resource dot to draw: resolved position, owner and whether it's a resource (which
/// picks both its color rule and its `MinimapOptions` toggle).
struct Dot {
    x: i32,
    y: i32,
    unit_id: u16,
    owner: u8,
    is_resource: bool,
}

/// Resolves the map's units and THG2 unit-sprites into the dots the minimap will draw. THG2
/// *doodad* sprites (`DRAW_AS_SPRITE`) are never drawn (see the module docs); the start location
/// unit id is never a dot (handled separately by [`draw_start_locations`]).
///
/// Filtering intentionally reuses the melee/hallucinated-drop machinery from `crate::overlay`
/// (`hq_clear_bounds`/`overlaps_any_start_area`, only reachable with `data`) but *not* its
/// critter/neutral-building toggles: [`MinimapOptions`] exposes just [`MinimapOptions::show_units`]
/// (everything that isn't a resource) and [`MinimapOptions::show_resources`] (mineral fields and
/// geysers), so those two flags are the entire class-filter story here.
fn collect_dots(
    units: &[PlacedUnit],
    sprites: &[Sprite],
    data: Option<&GameData>,
    options: &MinimapOptions,
) -> Vec<Dot> {
    let mut dots = Vec::new();

    // Melee start-area clearing needs `units.dat` (the HQ buildings' collision bounds) — skipped
    // entirely without `data`, per `MinimapOptions`'s docs.
    let start_clear_rects: Vec<(i32, i32, i32, i32)> = if options.unit_filter == UnitFilter::Melee {
        match data {
            Some(data) => {
                let (l, t, r, b) = hq_clear_bounds(data);
                units
                    .iter()
                    .filter(|u| u.unit_id == UNIT_ID_START_LOCATION)
                    .map(|u| {
                        let (x, y) = (u.x as i32, u.y as i32);
                        (x - l, y - t, x + r, y + b)
                    })
                    .collect()
            }
            None => Vec::new(),
        }
    } else {
        Vec::new()
    };

    for unit in units {
        if unit.unit_id == UNIT_ID_START_LOCATION {
            continue;
        }
        // Hallucinated units expire moments into a real game — never drawn, matching
        // `crate::overlay::unit_passes_filters`.
        if unit.state.contains(UnitState::HALLUCINATED) {
            continue;
        }
        let is_neutral = !matches!(unit.owner, Some(owner) if (owner as usize) < 8);
        if options.unit_filter == UnitFilter::Melee && !is_neutral {
            continue;
        }
        if let Some(data) = data
            && !start_clear_rects.is_empty()
            && overlaps_any_start_area(unit, data, &start_clear_rects)
        {
            continue;
        }

        let is_res = is_resource(unit.unit_id);
        if !dot_class_visible(is_res, options) {
            continue;
        }
        dots.push(Dot {
            x: unit.x as i32,
            y: unit.y as i32,
            unit_id: unit.unit_id,
            owner: unit.owner.unwrap_or(u8::MAX),
            is_resource: is_res,
        });
    }

    for sprite in sprites {
        if sprite.is_disabled() {
            continue;
        }
        if sprite.flags.contains(SpriteFlags::DRAW_AS_SPRITE) {
            // A pure THG2 sprite (map doodad) -- never drawn on the minimap.
            continue;
        }
        // A THG2 "unit sprite": its `id` is a unit id (see `crate::overlay`'s module docs on the
        // THG2 flag split). Melee ownership rules don't apply to THG2 entries in the preview
        // renderer either, so they're not applied here.
        let is_res = is_resource(sprite.id);
        if !dot_class_visible(is_res, options) {
            continue;
        }
        dots.push(Dot {
            x: sprite.x as i32,
            y: sprite.y as i32,
            unit_id: sprite.id,
            owner: sprite.owner,
            is_resource: is_res,
        });
    }

    dots
}

/// Whether a dot's class (resource vs. everything else) survives [`MinimapOptions`]'s two class
/// toggles.
fn dot_class_visible(is_resource: bool, options: &MinimapOptions) -> bool {
    if is_resource {
        options.show_resources
    } else {
        options.show_units
    }
}

/// Draws every dot whose `is_resource` matches `draw_resources` (used to paint the "resources,
/// then units" order from [`render_minimap_with_table`]) into the native-resolution `base` image.
fn draw_dots(
    base: &mut RgbaImage,
    dots: &[Dot],
    draw_resources: bool,
    player_colors: &PlayerColors,
    data: Option<&GameData>,
    options: &MinimapOptions,
    native_ppt: f32,
) {
    let border = dot_border_native_px(native_ppt);
    for dot in dots.iter().filter(|d| d.is_resource == draw_resources) {
        let color = if dot.is_resource && options.highlight_resources {
            RESOURCE_MINIMAP_COLOR
        } else {
            owner_color(dot.owner, player_colors)
        };
        let (box_w, box_h) = dot_native_size(dot.unit_id, native_ppt, data);
        let cx = logical_to_native(dot.x, native_ppt);
        let cy = logical_to_native(dot.y, native_ppt);
        draw_dot(base, cx, cy, box_w, box_h, border, color);
    }
}

/// Converts a logical-pixel coordinate (32 per tile) to native pixels: `logical * native_ppt /
/// 32` (see the module docs).
fn logical_to_native(logical: i32, native_ppt: f32) -> f32 {
    logical as f32 * native_ppt / 32.0
}

/// A dot's border thickness in native pixels (RE'd from `sub_729ae0`): `round(0.5 * native_ppt)`,
/// with a minimum of 1 so dots are always outlined — a slight, documented deviation from the
/// game's exact formula, which can yield 0 at `native_ppt == 1`.
fn dot_border_native_px(native_ppt: f32) -> u32 {
    ((0.5 * native_ppt).round() as i64).max(1) as u32
}

/// A dot's fill box size in native pixels (RE'd from `sub_72a090`): non-building units (mobile
/// units *and* resources — mineral fields and vespene geysers are both non-buildings, so both
/// size identically regardless of their differing `units.dat` placeboxes) are always exactly 2
/// native px. Buildings scale their placebox to native px, clamped to `2..=4`. Without
/// `GameData`, everything degrades to the fixed 2-native-px non-building size (documented on
/// [`MinimapOptions`]).
fn dot_native_size(unit_id: u16, native_ppt: f32, data: Option<&GameData>) -> (u32, u32) {
    const NON_BUILDING_PX: u32 = 2;
    let Some(data) = data else {
        return (NON_BUILDING_PX, NON_BUILDING_PX);
    };
    if !data.is_building(unit_id) {
        return (NON_BUILDING_PX, NON_BUILDING_PX);
    }
    let (w, h) = data
        .units_dat()
        .entry(unit_id)
        .map(|entry| entry.placebox)
        .unwrap_or((0, 0));
    (
        native_px_from_logical(w, native_ppt).clamp(2, 4),
        native_px_from_logical(h, native_ppt).clamp(2, 4),
    )
}

/// Scales a logical-pixel length (e.g. one axis of a `units.dat` placebox) to native pixels:
/// `round(logical * native_ppt / 32)`, floored at 0 (a negative/corrupted placebox axis never
/// produces a negative size).
fn native_px_from_logical(logical: i16, native_ppt: f32) -> u32 {
    ((logical.max(0) as f32) * native_ppt / 32.0)
        .round()
        .max(0.0) as u32
}

/// Draws one dot centred at native-pixel `(cx, cy)`: a black border-expanded rect (RE'd from
/// `sub_729ae0` — filled everywhere it isn't already this dot's own fill color), then the inner
/// `box_w`x`box_h` rect in `color` on top.
fn draw_dot(
    image: &mut RgbaImage,
    cx: f32,
    cy: f32,
    box_w: u32,
    box_h: u32,
    border: u32,
    color: [u8; 3],
) {
    let left = (cx - box_w as f32 / 2.0).round() as i32;
    let top = (cy - box_h as f32 / 2.0).round() as i32;
    let right = left + box_w as i32;
    let bottom = top + box_h as i32;
    let b = border as i32;

    fill_rect_black_unless_own_color(image, left - b, top - b, right + b, bottom + b, color);
    fill_rect(image, left, top, right, bottom, color);
}

/// Fills `[left, right) x [top, bottom)` with opaque black, skipping any pixel that's already
/// exactly `skip_color` (opaque) — see [`draw_dot`]'s docs. Bounds-clipped directly (no temp
/// buffer): dot rects are tiny (native-resolution, `<=` a handful of px per axis), so a
/// `blend_over`-style scratch allocation would be pure overhead here.
fn fill_rect_black_unless_own_color(
    image: &mut RgbaImage,
    left: i32,
    top: i32,
    right: i32,
    bottom: i32,
    skip_color: [u8; 3],
) {
    let y0 = top.max(0);
    let y1 = bottom.min(image.height as i32);
    let x0 = left.max(0);
    let x1 = right.min(image.width as i32);
    for y in y0..y1 {
        for x in x0..x1 {
            let offset = (y as usize * image.width as usize + x as usize) * 4;
            let Some(texel) = image.data.get_mut(offset..offset + 4) else {
                continue;
            };
            if texel[0] == skip_color[0]
                && texel[1] == skip_color[1]
                && texel[2] == skip_color[2]
                && texel[3] == 255
            {
                continue;
            }
            texel[0] = 0;
            texel[1] = 0;
            texel[2] = 0;
            texel[3] = 255;
        }
    }
}

/// Fills `[left, right) x [top, bottom)` with a flat, opaque color, bounds-clipped. See
/// [`fill_rect_black_unless_own_color`] on why this writes directly rather than going through
/// `crate::overlay::blend_over`.
fn fill_rect(image: &mut RgbaImage, left: i32, top: i32, right: i32, bottom: i32, color: [u8; 3]) {
    let y0 = top.max(0);
    let y1 = bottom.min(image.height as i32);
    let x0 = left.max(0);
    let x1 = right.min(image.width as i32);
    for y in y0..y1 {
        for x in x0..x1 {
            let offset = (y as usize * image.width as usize + x as usize) * 4;
            if let Some(texel) = image.data.get_mut(offset..offset + 4) {
                texel[0] = color[0];
                texel[1] = color[1];
                texel[2] = color[2];
                texel[3] = 255;
            }
        }
    }
}

// -------------------------------------------------------------------------------------------
// Start locations (native resolution)
// -------------------------------------------------------------------------------------------

/// Draws every start location as a player token (`crate::token::draw_player_token`, the same
/// "ColorBlock" style `crate::overlay::draw_start_location_blocks` uses for the full preview
/// renderer) into the native-resolution `base` image, always painted last so it sits on top of
/// every terrain/unit/resource pixel. [`StartLocations::Hidden`] draws nothing;
/// [`StartLocations::Sprite`] is treated exactly like [`StartLocations::ColorBlock`] (see
/// [`MinimapOptions::start_locations`]'s docs -- there's no art to draw here).
fn draw_start_locations(
    base: &mut RgbaImage,
    units: &[PlacedUnit],
    player_colors: &PlayerColors,
    data: Option<&GameData>,
    options: &MinimapOptions,
    native_ppt: f32,
) {
    if options.start_locations == StartLocations::Hidden {
        return;
    }

    let (box_w, box_h) = start_location_box(data);
    let box_w = box_w.min(MAX_START_LOCATION_BOX_LOGICAL_PX);
    let box_h = box_h.min(MAX_START_LOCATION_BOX_LOGICAL_PX);
    let zoom = native_ppt / 32.0;

    for unit in units.iter().filter(|u| u.unit_id == UNIT_ID_START_LOCATION) {
        let color = owner_color(unit.owner.unwrap_or(u8::MAX), player_colors);
        let left = ((unit.x as f32 - box_w as f32 / 2.0) * zoom).round() as i32;
        let top = ((unit.y as f32 - box_h as f32 / 2.0) * zoom).round() as i32;
        let right = ((unit.x as f32 + box_w as f32 / 2.0) * zoom).round() as i32;
        let bottom = ((unit.y as f32 + box_h as f32 / 2.0) * zoom).round() as i32;
        draw_player_token(
            base,
            left,
            top,
            right.max(left + 1),
            bottom.max(top + 1),
            color,
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gamedata::tests::{synthetic_parts, synthetic_source};
    use crate::options::UnitFilter;
    use broodmap::chk::placed_units::UnitInstanceId;
    use broodmap::chk::player_colors::PlayerColor;
    use broodmap::chk::terrain::TileId;
    use tables::tests::{synthetic_table_bytes, synthetic_table_bytes_with_quadrants};

    fn pixel(image: &RgbaImage, x: u32, y: u32) -> [u8; 4] {
        let o = (y as usize * image.width as usize + x as usize) * 4;
        image.data[o..o + 4].try_into().unwrap()
    }

    /// The pixel's RGB only (dropping alpha), so tests can compare against a plain `[u8; 3]`
    /// without fighting slice-vs-array `PartialEq` mismatches.
    fn rgb(image: &RgbaImage, x: u32, y: u32) -> [u8; 3] {
        let p = pixel(image, x, y);
        [p[0], p[1], p[2]]
    }

    fn unit(unit_id: u16, owner: Option<u8>, x: u16, y: u16) -> PlacedUnit {
        PlacedUnit {
            instance_id: UnitInstanceId(0),
            x,
            y,
            unit_id,
            owner,
            hp_percent: None,
            shield_percent: None,
            energy_percent: None,
            resource_amount: None,
            hangar_count: None,
            state: UnitState::empty(),
            linked_id: None,
        }
    }

    fn player_colors() -> PlayerColors {
        let mut colors = [PlayerColor::default(); 8];
        colors[0] = PlayerColor::Indexed(0); // red [244, 4, 4]
        PlayerColors { colors }
    }

    fn flat_terrain(width: usize, height: usize) -> TerrainTileIds {
        TerrainTileIds {
            width,
            height,
            tiles: vec![TileId(0); width * height],
        }
    }

    // ---------------------------------------------------------------------------------------
    // Native layout
    // ---------------------------------------------------------------------------------------

    #[test]
    fn native_layout_picks_the_mode_by_map_size() {
        assert_eq!(native_layout(64, 10).0, NativeMode::Quad);
        assert_eq!(native_layout(10, 64).0, NativeMode::Quad);
        assert_eq!(native_layout(65, 1).0, NativeMode::Single);
        assert_eq!(native_layout(128, 1).0, NativeMode::Single);
        assert_eq!(native_layout(129, 1).0, NativeMode::Subsample);
    }

    #[test]
    fn native_layout_base_dims_match_the_mode() {
        assert_eq!(native_layout(4, 4), (NativeMode::Quad, 2.0, 8, 8));
        assert_eq!(native_layout(65, 1), (NativeMode::Single, 1.0, 65, 1));
        // Odd map dims round the subsampled base dim up, not down.
        assert_eq!(native_layout(129, 5), (NativeMode::Subsample, 0.5, 65, 3));
    }

    // ---------------------------------------------------------------------------------------
    // Terrain sampling
    // ---------------------------------------------------------------------------------------

    #[test]
    fn empty_terrain_returns_empty_image() {
        let terrain = TerrainTileIds {
            width: 0,
            height: 0,
            tiles: Vec::new(),
        };
        let image = render_minimap_with_table(
            &terrain,
            None,
            &[],
            &[],
            &PlayerColors::default(),
            None,
            &MinimapOptions::default(),
        );
        assert_eq!((image.width, image.height), (0, 0));
        assert!(image.data.is_empty());
    }

    #[test]
    fn no_table_renders_solid_black_terrain() {
        let terrain = flat_terrain(2, 2); // M = 2 <= 64 -> Quad mode, base 4x4
        let image = render_minimap_with_table(
            &terrain,
            None,
            &[],
            &[],
            &PlayerColors::default(),
            None,
            &MinimapOptions::default(),
        );
        assert_eq!((image.width, image.height), (4, 4));
        for texel in image.data.chunks_exact(4) {
            assert_eq!(texel, [0, 0, 0, 255]);
        }
    }

    #[test]
    fn quad_mode_places_four_distinct_quadrant_colors() {
        // A single-tile map (M = 1 <= 64 -> Quad mode, base 2x2), one tile id whose four
        // quadrants (TL, TR, BL, BR) resolve to four distinct palette indices.
        let table_bytes = synthetic_table_bytes_with_quadrants(&[[1, 2, 3, 4]]);
        let table = tables::parse_table(&table_bytes).unwrap();
        let terrain = flat_terrain(1, 1);

        let image = render_minimap_with_table(
            &terrain,
            Some(table),
            &[],
            &[],
            &PlayerColors::default(),
            None,
            &MinimapOptions::default(),
        );
        assert_eq!((image.width, image.height), (2, 2));
        assert_eq!(rgb(&image, 0, 0), [1, 1, 1], "TL");
        assert_eq!(rgb(&image, 1, 0), [2, 2, 2], "TR");
        assert_eq!(rgb(&image, 0, 1), [3, 3, 3], "BL");
        assert_eq!(rgb(&image, 1, 1), [4, 4, 4], "BR");
    }

    #[test]
    fn single_mode_averages_the_four_quadrants() {
        // M = 65 -> Single mode: one native pixel per tile, colored by the mean of its four
        // quadrants. (9 + 2 + 3 + 4) / 4 = 4.
        let table_bytes = synthetic_table_bytes_with_quadrants(&[[9, 2, 3, 4]]);
        let table = tables::parse_table(&table_bytes).unwrap();
        let terrain = flat_terrain(65, 1);

        let image = render_minimap_with_table(
            &terrain,
            Some(table),
            &[],
            &[],
            &PlayerColors::default(),
            None,
            &MinimapOptions::default(),
        );
        assert_eq!((image.width, image.height), (65, 1));
        assert_eq!(rgb(&image, 0, 0), [4, 4, 4]);
    }

    #[test]
    fn subsample_mode_averages_every_tile_in_the_2x2_block() {
        // M = 130 -> Subsample mode: base_w = ceil(130 / 2) = 65, one native pixel per 2x2 tile
        // block. With height 1 each block covers two tiles (2*bx and 2*bx + 1). Give tile t
        // all-equal quadrants = t, so each tile's own average is t and the block's is
        // (2*bx + (2*bx + 1)) / 2 = 2*bx.
        let entries: Vec<[u8; 4]> = (0..130u8).map(|t| [t; 4]).collect();
        let table_bytes = synthetic_table_bytes_with_quadrants(&entries);
        let table = tables::parse_table(&table_bytes).unwrap();
        let terrain = TerrainTileIds {
            width: 130,
            height: 1,
            tiles: (0..130u16).map(TileId).collect(),
        };

        let image = render_minimap_with_table(
            &terrain,
            Some(table),
            &[],
            &[],
            &PlayerColors::default(),
            None,
            &MinimapOptions::default(),
        );
        assert_eq!((image.width, image.height), (65, 1));
        for bx in 0..65u32 {
            let expected = (2 * bx) as u8;
            assert_eq!(
                rgb(&image, bx, 0),
                [expected, expected, expected],
                "block {bx} should average tiles {} and {}",
                2 * bx,
                2 * bx + 1
            );
        }
    }

    #[test]
    fn terrain_tile_resolves_through_the_table() {
        // A 1-tile-id table (unified id 0, every quadrant -> palette index 5), with palette
        // index 5 recolored to [50, 60, 70] so a hit is unambiguous.
        let mut bytes = synthetic_table_bytes(1, 5);
        let palette_start = 2 + 4; // header (2 bytes) + 1 tile id's 4 quadrant bytes
        bytes[palette_start + 5 * 3..palette_start + 5 * 3 + 3].copy_from_slice(&[50, 60, 70]);
        let table = tables::parse_table(&bytes).unwrap();

        let terrain = flat_terrain(1, 1);
        let image = render_minimap_with_table(
            &terrain,
            Some(table),
            &[],
            &[],
            &PlayerColors::default(),
            None,
            &MinimapOptions::default(),
        );
        for texel in image.data.chunks_exact(4) {
            assert_eq!(texel, [50, 60, 70, 255]);
        }
    }

    #[test]
    fn out_of_range_tile_id_degrades_to_index_0_color() {
        let table_bytes = synthetic_table_bytes(1, 3);
        let table = tables::parse_table(&table_bytes).unwrap();
        let terrain = TerrainTileIds {
            width: 1,
            height: 1,
            tiles: vec![TileId(200)], // unified id 200, way past the 1-entry table
        };
        let image = render_minimap_with_table(
            &terrain,
            Some(table),
            &[],
            &[],
            &PlayerColors::default(),
            None,
            &MinimapOptions::default(),
        );
        // Index 0's color, per `synthetic_table_bytes`'s `[i, i, i]` identity palette.
        for texel in image.data.chunks_exact(4) {
            assert_eq!(texel, [0, 0, 0, 255]);
        }
    }

    #[test]
    fn creep_flagged_tile_id_masks_to_the_same_color_as_unflagged() {
        // The creep flag is masked off (`TileId::id`) and the resulting id is sampled exactly
        // like any other, so a creep-flagged tile and its unflagged counterpart must resolve
        // identically.
        let table_bytes = synthetic_table_bytes(1, 6);
        let table = tables::parse_table(&table_bytes).unwrap();
        let terrain = TerrainTileIds {
            width: 2,
            height: 1,
            tiles: vec![TileId(0), TileId(0x8000)], // unified id 0, with and without the flag
        };
        let image = render_minimap_with_table(
            &terrain,
            Some(table),
            &[],
            &[],
            &PlayerColors::default(),
            None,
            &MinimapOptions::default(),
        );
        assert_eq!(rgb(&image, 0, 0), rgb(&image, 1, 0));
    }

    // ---------------------------------------------------------------------------------------
    // Integer upscale
    // ---------------------------------------------------------------------------------------

    #[test]
    fn scale_replicates_each_native_pixel_into_a_scale_by_scale_block() {
        // A single-tile map (Quad mode, base 2x2, four distinct quadrant colors) at scale 2 ->
        // output 4x4, each native pixel expanding into its own 2x2 block.
        let table_bytes = synthetic_table_bytes_with_quadrants(&[[1, 2, 3, 4]]);
        let table = tables::parse_table(&table_bytes).unwrap();
        let terrain = flat_terrain(1, 1);
        let options = MinimapOptions {
            scale: 2,
            ..Default::default()
        };
        let image = render_minimap_with_table(
            &terrain,
            Some(table),
            &[],
            &[],
            &PlayerColors::default(),
            None,
            &options,
        );
        assert_eq!((image.width, image.height), (4, 4));
        let blocks: [(u32, u32, u8); 4] = [(0, 0, 1), (1, 0, 2), (0, 1, 3), (1, 1, 4)];
        for (bx, by, expected) in blocks {
            for dy in 0..2u32 {
                for dx in 0..2u32 {
                    assert_eq!(
                        rgb(&image, bx * 2 + dx, by * 2 + dy),
                        [expected, expected, expected]
                    );
                }
            }
        }
    }

    #[test]
    fn scale_is_clamped_to_sixteen() {
        let terrain = flat_terrain(1, 1);
        let options = MinimapOptions {
            scale: 1000,
            ..Default::default()
        };
        let image = render_minimap_with_table(
            &terrain,
            None,
            &[],
            &[],
            &PlayerColors::default(),
            None,
            &options,
        );
        // Base is 2x2 (Quad mode); clamped scale 16 -> 32x32.
        assert_eq!((image.width, image.height), (32, 32));
    }

    #[test]
    fn scale_one_is_the_native_image_unmodified() {
        let table_bytes = synthetic_table_bytes(1, 5);
        let table = tables::parse_table(&table_bytes).unwrap();
        let terrain = flat_terrain(1, 1);
        let image = render_minimap_with_table(
            &terrain,
            Some(table),
            &[],
            &[],
            &PlayerColors::default(),
            None,
            &MinimapOptions::default(), // scale: 1
        );
        assert_eq!((image.width, image.height), (2, 2));
    }

    // ---------------------------------------------------------------------------------------
    // Dots
    // ---------------------------------------------------------------------------------------

    #[test]
    fn unit_dot_draws_in_owner_color() {
        let terrain = flat_terrain(4, 4); // Quad mode, native_ppt 2
        // Placed at the centre of tile (2, 2) -- logical px (80, 80) -> native (5.0, 5.0).
        let units = [unit(0, Some(0), 80, 80)];
        let options = MinimapOptions {
            unit_filter: UnitFilter::AsPlaced,
            ..Default::default()
        };
        let image = render_minimap_with_table(
            &terrain,
            None,
            &units,
            &[],
            &player_colors(),
            None,
            &options,
        );
        // Data-less: fixed 2-native-px dot centred at native (5, 5) -- inner rect [4, 6) both
        // axes -- so (5, 5) is squarely inside the fill.
        assert_eq!(rgb(&image, 5, 5), [244, 4, 4]);
    }

    #[test]
    fn dot_has_a_black_border_around_its_fill() {
        // Contrasting terrain (bright gray) so the black border is unambiguous.
        let table_bytes = synthetic_table_bytes(1, 200);
        let table = tables::parse_table(&table_bytes).unwrap();
        let terrain = flat_terrain(8, 4); // Quad mode, native_ppt 2, base 16x8
        // x = 48, y = 80 -> native (3.0, 5.0) exactly (48 * 2 / 32 == 3, 80 * 2 / 32 == 5).
        let units = [unit(0, Some(0), 48, 80)];
        let options = MinimapOptions {
            unit_filter: UnitFilter::AsPlaced,
            ..Default::default()
        };
        let image = render_minimap_with_table(
            &terrain,
            Some(table),
            &units,
            &[],
            &player_colors(),
            None,
            &options,
        );
        // Fixed 2-native-px, data-less dot: inner rect columns [2, 4), border 1 -> black at
        // column 1 and 4, terrain at column 0 and 5.
        assert_eq!(rgb(&image, 0, 5), [200, 200, 200], "outside the border");
        assert_eq!(rgb(&image, 1, 5), [0, 0, 0], "border");
        assert_eq!(rgb(&image, 2, 5), [244, 4, 4], "fill");
        assert_eq!(rgb(&image, 3, 5), [244, 4, 4], "fill");
        assert_eq!(rgb(&image, 4, 5), [0, 0, 0], "border");
        assert_eq!(rgb(&image, 5, 5), [200, 200, 200], "outside the border");
    }

    #[test]
    fn building_dots_are_bigger_than_fixed_two_px_non_building_dots() {
        // Contrasting terrain so both dots' full border+fill footprint is visible.
        let table_bytes = synthetic_table_bytes(1, 200);
        let table = tables::parse_table(&table_bytes).unwrap();
        let terrain = flat_terrain(8, 4); // Quad mode, native_ppt 2, base 16x8

        // Unit 5: flagged as a building with a huge placebox -- clamps to the 4-native-px max.
        let mut parts = synthetic_parts(0, 0, 0, 0, None);
        set_unit_building(&mut parts.0, 5);
        let off = UNITS_PLACEBOX_COLUMN + 5 * 4;
        parts.0[off..off + 2].copy_from_slice(&320i16.to_le_bytes());
        parts.0[off + 2..off + 4].copy_from_slice(&320i16.to_le_bytes());
        let data = GameData::load(&synthetic_source(parts)).unwrap();

        // x = 48 -> native cx = 3.0 (resource-sized dot); x = 176 -> native cx = 11.0 (building).
        // Both at y = 80 -> native cy = 5.0. Spaced far enough apart that the 2px vs. 4px
        // footprints (plus 1px borders each side) never overlap.
        let units = [
            unit(176, Some(11), 48, 80), // mineral field: non-building, fixed 2px
            unit(5, Some(0), 176, 80),   // building: clamps to 4px
        ];
        let options = MinimapOptions {
            unit_filter: UnitFilter::AsPlaced,
            ..Default::default()
        };
        let image = render_minimap_with_table(
            &terrain,
            Some(table),
            &units,
            &[],
            &player_colors(),
            Some(&data),
            &options,
        );

        // Mineral (non-building, fixed 2px): inner [2, 4), border at 1 and 4.
        assert_eq!(rgb(&image, 0, 5), [200, 200, 200]);
        assert_eq!(rgb(&image, 1, 5), [0, 0, 0]);
        assert_eq!(rgb(&image, 4, 5), [0, 0, 0]);
        assert_eq!(rgb(&image, 5, 5), [200, 200, 200]);

        // Building (clamped to 4px): inner [9, 13), border at 8 and 13.
        assert_eq!(rgb(&image, 7, 5), [200, 200, 200]);
        assert_eq!(rgb(&image, 8, 5), [0, 0, 0]);
        assert_eq!(rgb(&image, 9, 5), [244, 4, 4]);
        assert_eq!(rgb(&image, 12, 5), [244, 4, 4]);
        assert_eq!(rgb(&image, 13, 5), [0, 0, 0]);
        assert_eq!(rgb(&image, 14, 5), [200, 200, 200]);
    }

    #[test]
    fn minerals_and_geysers_are_both_fixed_at_two_native_px() {
        // Real placeboxes differ (a geyser's is bigger than a mineral field's), but neither is a
        // building, so both must size to the same fixed 2 native px.
        let mut parts = synthetic_parts(0, 0, 0, 0, None);
        let mineral_off = UNITS_PLACEBOX_COLUMN + 176 * 4;
        parts.0[mineral_off..mineral_off + 2].copy_from_slice(&32i16.to_le_bytes());
        parts.0[mineral_off + 2..mineral_off + 4].copy_from_slice(&32i16.to_le_bytes());
        let geyser_off = UNITS_PLACEBOX_COLUMN + 188 * 4;
        parts.0[geyser_off..geyser_off + 2].copy_from_slice(&128i16.to_le_bytes());
        parts.0[geyser_off + 2..geyser_off + 4].copy_from_slice(&96i16.to_le_bytes());
        let data = GameData::load(&synthetic_source(parts)).unwrap();

        assert_eq!(dot_native_size(176, 2.0, Some(&data)), (2, 2));
        assert_eq!(dot_native_size(188, 2.0, Some(&data)), (2, 2));
    }

    #[test]
    fn dot_native_size_clamps_a_building_between_two_and_four() {
        let mut parts = synthetic_parts(0, 0, 0, 0, None);
        set_unit_building(&mut parts.0, 5);
        let off = UNITS_PLACEBOX_COLUMN + 5 * 4;
        // A tiny placebox clamps up to the 2px minimum.
        parts.0[off..off + 2].copy_from_slice(&0i16.to_le_bytes());
        parts.0[off + 2..off + 4].copy_from_slice(&0i16.to_le_bytes());
        let data = GameData::load(&synthetic_source(parts)).unwrap();
        assert_eq!(dot_native_size(5, 2.0, Some(&data)), (2, 2));

        // A huge placebox clamps down to the 4px maximum.
        let mut parts2 = synthetic_parts(0, 0, 0, 0, None);
        set_unit_building(&mut parts2.0, 5);
        parts2.0[off..off + 2].copy_from_slice(&1000i16.to_le_bytes());
        parts2.0[off + 2..off + 4].copy_from_slice(&1000i16.to_le_bytes());
        let data2 = GameData::load(&synthetic_source(parts2)).unwrap();
        assert_eq!(dot_native_size(5, 2.0, Some(&data2)), (4, 4));
    }

    #[test]
    fn dot_native_size_without_data_is_always_two_px() {
        assert_eq!(dot_native_size(5, 2.0, None), (2, 2));
        assert_eq!(dot_native_size(214, 0.5, None), (2, 2));
    }

    #[test]
    fn resource_dot_uses_the_highlight_color_by_default() {
        let terrain = flat_terrain(2, 2);
        let units = [unit(176, Some(11), 16, 16)]; // mineral field, tile (0, 0)
        let options = MinimapOptions {
            unit_filter: UnitFilter::AsPlaced,
            ..Default::default()
        };
        let image = render_minimap_with_table(
            &terrain,
            None,
            &units,
            &[],
            &player_colors(),
            None,
            &options,
        );
        // Native (16*2/32, 16*2/32) == (1, 1) -- squarely inside the fixed 2px inner rect.
        assert_eq!(rgb(&image, 1, 1), RESOURCE_MINIMAP_COLOR);

        let no_highlight = MinimapOptions {
            unit_filter: UnitFilter::AsPlaced,
            highlight_resources: false,
            ..Default::default()
        };
        let image2 = render_minimap_with_table(
            &terrain,
            None,
            &units,
            &[],
            &player_colors(),
            None,
            &no_highlight,
        );
        // No owner slot recorded for this unit besides 11 (neutral) -- neutral color, not red.
        assert_ne!(rgb(&image2, 1, 1), [244, 4, 4]);
    }

    #[test]
    fn start_location_block_draws_on_top_of_everything() {
        let terrain = flat_terrain(8, 8);
        // A player-owned unit exactly under the start location -- as-placed keeps both, and the
        // start location must still win since it's drawn last.
        let units = [
            unit(0, Some(1), 128, 128),   // blue, tile (4, 4)
            unit(214, Some(0), 128, 128), // start location, red, same spot
        ];
        let options = MinimapOptions {
            unit_filter: UnitFilter::AsPlaced,
            scale: 4,
            ..Default::default()
        };
        let image = render_minimap_with_table(
            &terrain,
            None,
            &units,
            &[],
            &player_colors(),
            None,
            &options,
        );
        assert!(
            image
                .data
                .chunks_exact(4)
                .any(|t| close_to_player_color([t[0], t[1], t[2]], [244, 4, 4])),
            "the start-location token must draw over the unit dot beneath it"
        );
    }

    #[test]
    fn hidden_start_locations_draw_nothing() {
        let terrain = flat_terrain(4, 4);
        let units = [unit(214, Some(0), 64, 64)];
        let options = MinimapOptions {
            start_locations: StartLocations::Hidden,
            ..Default::default()
        };
        let image = render_minimap_with_table(
            &terrain,
            None,
            &units,
            &[],
            &player_colors(),
            None,
            &options,
        );
        for texel in image.data.chunks_exact(4) {
            assert_ne!([texel[0], texel[1], texel[2]], [244, 4, 4]);
        }
    }

    #[test]
    fn sprite_start_locations_behave_like_color_block() {
        let terrain = flat_terrain(8, 8);
        let units = [unit(214, Some(0), 128, 128)];
        for mode in [StartLocations::ColorBlock, StartLocations::Sprite] {
            let options = MinimapOptions {
                start_locations: mode,
                scale: 4,
                ..Default::default()
            };
            let image = render_minimap_with_table(
                &terrain,
                None,
                &units,
                &[],
                &player_colors(),
                None,
                &options,
            );
            assert!(
                image
                    .data
                    .chunks_exact(4)
                    .any(|t| close_to_player_color([t[0], t[1], t[2]], [244, 4, 4])),
                "{mode:?}: no pixel near the expected player color"
            );
        }
    }

    /// Whether `a` is close enough to player color `b` to count as "the token's fill color",
    /// tolerating `crate::token::draw_player_token`'s vertical gradient (+/-12%) and rounding --
    /// mirrors `broodmap-render/tests/real_assets.rs`'s identical helper.
    fn close_to_player_color(a: [u8; 3], b: [u8; 3]) -> bool {
        a.iter().zip(b).all(|(&x, y)| {
            let tol = 2 + (y as u32 * 12).div_ceil(100) as i32;
            (x as i32 - y as i32).abs() <= tol
        })
    }

    #[test]
    fn doodad_sprites_are_never_drawn_but_unit_sprites_are() {
        let terrain = flat_terrain(4, 4);
        let sprites = [
            Sprite {
                id: 0,
                x: 32,
                y: 32,
                owner: 0,
                flags: SpriteFlags::DRAW_AS_SPRITE, // doodad: never drawn
            },
            Sprite {
                id: 0,
                x: 96,
                y: 96,
                owner: 0,
                flags: SpriteFlags::empty(), // unit sprite: counts as a unit
            },
        ];
        let options = MinimapOptions {
            unit_filter: UnitFilter::AsPlaced,
            ..Default::default()
        };
        let image = render_minimap_with_table(
            &terrain,
            None,
            &[],
            &sprites,
            &player_colors(),
            None,
            &options,
        );
        let has_red = |img: &RgbaImage| {
            img.data
                .chunks_exact(4)
                .any(|t| [t[0], t[1], t[2]] == [244, 4, 4])
        };
        assert!(has_red(&image), "the unit sprite must be drawn somewhere");

        // Rendering only the doodad (dropping the unit sprite) must draw no red at all.
        let doodad_only = [sprites[0]];
        let image2 = render_minimap_with_table(
            &terrain,
            None,
            &[],
            &doodad_only,
            &player_colors(),
            None,
            &options,
        );
        assert!(!has_red(&image2), "doodad sprite must not be drawn");
    }

    #[test]
    fn dataless_melee_still_drops_player_owned_units() {
        let terrain = flat_terrain(4, 4);
        let units = [
            unit(0, Some(0), 32, 32),  // player-owned: dropped under melee
            unit(0, Some(11), 96, 96), // neutral: kept
        ];
        let options = MinimapOptions::default(); // Melee is the default
        let image = render_minimap_with_table(
            &terrain,
            None,
            &units,
            &[],
            &player_colors(),
            None, // no GameData -- the zero-asset path
            &options,
        );
        let has_red = image
            .data
            .chunks_exact(4)
            .any(|t| [t[0], t[1], t[2]] == [244, 4, 4]);
        assert!(!has_red, "player-owned unit must be dropped under melee");
        let has_neutral = image
            .data
            .chunks_exact(4)
            .any(|t| [t[0], t[1], t[2]] == RESOURCE_MINIMAP_COLOR);
        assert!(has_neutral, "neutral unit must be kept and drawn");
    }

    #[test]
    fn as_placed_keeps_player_owned_units_even_without_data() {
        let terrain = flat_terrain(4, 4);
        let units = [unit(0, Some(0), 32, 32)];
        let options = MinimapOptions {
            unit_filter: UnitFilter::AsPlaced,
            ..Default::default()
        };
        let image = render_minimap_with_table(
            &terrain,
            None,
            &units,
            &[],
            &player_colors(),
            None,
            &options,
        );
        assert!(
            image
                .data
                .chunks_exact(4)
                .any(|t| [t[0], t[1], t[2]] == [244, 4, 4])
        );
    }

    #[test]
    fn hallucinated_units_are_never_drawn() {
        let terrain = flat_terrain(4, 4);
        let mut hallucinated = unit(0, Some(0), 32, 32);
        hallucinated.state = UnitState::HALLUCINATED;
        let options = MinimapOptions {
            unit_filter: UnitFilter::AsPlaced,
            ..Default::default()
        };
        let image = render_minimap_with_table(
            &terrain,
            None,
            &[hallucinated],
            &[],
            &player_colors(),
            None,
            &options,
        );
        assert!(
            !image
                .data
                .chunks_exact(4)
                .any(|t| [t[0], t[1], t[2]] == [244, 4, 4])
        );
    }

    #[test]
    fn render_chk_minimap_reports_missing_terrain() {
        // A `Chk` with the required VER/STR/DIM/ERA chunks but deliberately no MTXM chunk, so
        // `.terrain()` fails: `render_chk_minimap` must degrade to an empty image plus a warning,
        // exactly like `render_chk_preview` (mirrors `crate::overlay`'s private
        // `chk_without_terrain` test helper -- reimplemented here since it isn't reachable from
        // this module).
        fn chunk(tag: &[u8; 4], data: &[u8]) -> Vec<u8> {
            let mut out = Vec::new();
            out.extend_from_slice(tag);
            out.extend_from_slice(&(data.len() as i32).to_le_bytes());
            out.extend_from_slice(data);
            out
        }
        let mut chk_bytes = Vec::new();
        chk_bytes.extend(chunk(b"VER ", &205u16.to_le_bytes())); // Brood War 1.04
        chk_bytes.extend(chunk(b"STR ", &0u16.to_le_bytes())); // empty string table
        let mut dim = Vec::new();
        dim.extend_from_slice(&4u16.to_le_bytes());
        dim.extend_from_slice(&4u16.to_le_bytes());
        chk_bytes.extend(chunk(b"DIM ", &dim));
        chk_bytes.extend(chunk(b"ERA ", &0u16.to_le_bytes())); // Badlands
        // No MTXM chunk at all.
        let chk = broodmap::Chk::from_bytes(chk_bytes, None).expect("minimal CHK should parse");
        assert!(chk.terrain().is_err());

        let preview = render_chk_minimap(&chk, None, &MinimapOptions::default());
        assert_eq!((preview.image.width, preview.image.height), (0, 0));
        assert_eq!(preview.warnings.len(), 1);
    }

    /// A malformed/degenerate `.wpe` is the only input `build_minimap_table` can fail on; every
    /// other lookup failure (missing CV5 group, out-of-range megatile/minitile id) degrades to
    /// palette index 0 instead.
    #[test]
    fn build_minimap_table_fails_only_on_a_short_wpe() {
        let cv5 = Vec::new();
        let vx4ex = Vec::new();
        let vr4 = Vec::new();
        assert!(build_minimap_table(&cv5, &vx4ex, &vr4, &[0u8; 1023]).is_err());

        let wpe = vec![0u8; 1024];
        let out = build_minimap_table(&cv5, &vx4ex, &vr4, &wpe).unwrap();
        // Empty CV5 -> 0 tile ids: header (2) + 0 quadrant bytes + 768 palette.
        assert_eq!(out.len(), 2 + 768);
        assert_eq!(&out[0..2], &0u16.to_le_bytes());
    }

    /// Exercises the full sampling chain end to end with real (synthetic-but-well-formed) CV5/
    /// VX4EX/VR4/WPE bytes: one CV5 group whose megatile 0 has four distinct minitiles at
    /// indices 0/1/4/5 (TL/TR/BL/BR), each resolving to a distinct palette index/color.
    #[test]
    fn build_minimap_table_samples_all_four_quadrants() {
        let mut mega_tiles = [0u16; 16];
        mega_tiles[0] = 0; // -> megatile id 0
        let cv5 = cv5_entry(mega_tiles);

        // Megatile 0's 16 minitile refs (u32 each): index 0 -> vr4 10 (flip set, must be
        // ignored), index 1 -> vr4 11, index 4 -> vr4 12, index 5 -> vr4 13, rest -> 0.
        let mut vx4ex = vec![0u8; 16 * 4];
        let entries: [(usize, u32, bool); 4] = [
            (0, 10, true),
            (1, 11, false),
            (4, 12, false),
            (5, 13, false),
        ];
        for (index, vr4_index, flip) in entries {
            let raw = (vr4_index << 1) | flip as u32;
            let off = index * 4;
            vx4ex[off..off + 4].copy_from_slice(&raw.to_le_bytes());
        }

        let mut vr4 = vec![0u8; 14 * 64];
        for (vr4_index, palette_index) in [(10, 7u8), (11, 8), (12, 9), (13, 10)] {
            vr4[vr4_index * 64 + 55] = palette_index;
        }

        let mut wpe = vec![0u8; 1024];
        for (palette_index, color) in [
            (7u8, [10u8, 20, 30]),
            (8, [40, 50, 60]),
            (9, [70, 80, 90]),
            (10, [11, 12, 13]),
        ] {
            let off = palette_index as usize * 4;
            wpe[off..off + 3].copy_from_slice(&color);
        }

        let out = build_minimap_table(&cv5, &vx4ex, &vr4, &wpe).unwrap();
        let table = tables::parse_table(&out).unwrap();
        assert_eq!(table.quadrant_color(0, 0), [10, 20, 30], "TL");
        assert_eq!(table.quadrant_color(0, 1), [40, 50, 60], "TR");
        assert_eq!(table.quadrant_color(0, 2), [70, 80, 90], "BL");
        assert_eq!(table.quadrant_color(0, 3), [11, 12, 13], "BR");
    }

    fn cv5_entry(mega_tiles: [u16; 16]) -> Vec<u8> {
        let mut entry = Vec::with_capacity(52);
        entry.extend_from_slice(&0u16.to_le_bytes()); // group_type
        entry.extend_from_slice(&0u16.to_le_bytes()); // flags
        entry.extend_from_slice(&[0u8; 16]); // unused rects
        for tile in mega_tiles {
            entry.extend_from_slice(&tile.to_le_bytes());
        }
        entry
    }

    /// Byte offset of `units.dat`'s `placebox` column (the 37th of 54) -- the sum of every
    /// column ahead of it. Mirrors `crate::overlay`'s own independently-derived
    /// `UNITS_PLACEBOX_COLUMN` test constant.
    const UNITS_COUNT: usize = 228;
    const UNITS_PLACEBOX_COLUMN: usize = UNITS_COUNT // flingy: u8
        + UNITS_COUNT * 2 // sub_unit_1: u16
        + UNITS_COUNT * 2 // sub_unit_2: u16
        + 96 * 2 // infestation: u16 (buildings-only)
        + UNITS_COUNT * 4 // construction_image: u32
        + UNITS_COUNT // unit_direction: u8
        + UNITS_COUNT // shield_enabled: u8
        + UNITS_COUNT * 2 // shield_amount: i16
        + UNITS_COUNT * 4 // hit_points: i32
        + UNITS_COUNT * 3 // elevation_level, unknown, sub_label: u8 each
        + UNITS_COUNT * 5 // five AI/order columns: u8 each
        + UNITS_COUNT * 5 // ground_weapon..ai_internal: u8 each
        + UNITS_COUNT * 4 // special_ability_flags: u32
        + UNITS_COUNT * 6 // target_acquisition_range..right_click_action: u8 each
        + 106 * 2 // ready_sound: u16 (units-only)
        + UNITS_COUNT * 4 // what_sound_start/end: u16 each
        + 106 * 8; // piss/yes sound start/end: u16 each, units-only (4 columns)

    /// Byte offset of `units.dat`'s `special_ability_flags` column -- see `crate::gamedata`'s
    /// identically-derived `SPECIAL_ABILITY_FLAGS_COLUMN` test constant. Every column ahead of it
    /// is the same prefix [`UNITS_PLACEBOX_COLUMN`] sums, minus that constant's own trailing
    /// `special_ability_flags`/`target_acquisition_range..right_click_action`/sound-column terms
    /// (this column sits right after `ground_weapon..ai_internal`, several columns before
    /// `placebox`).
    const UNITS_SPECIAL_ABILITY_FLAGS_COLUMN: usize = UNITS_COUNT // flingy: u8
        + UNITS_COUNT * 2 // sub_unit_1: u16
        + UNITS_COUNT * 2 // sub_unit_2: u16
        + 96 * 2 // infestation: u16 (buildings-only)
        + UNITS_COUNT * 4 // construction_image: u32
        + UNITS_COUNT // unit_direction: u8
        + UNITS_COUNT // shield_enabled: u8
        + UNITS_COUNT * 2 // shield_amount: i16
        + UNITS_COUNT * 4 // hit_points: i32
        + UNITS_COUNT * 3 // elevation_level, unknown, sub_label: u8 each
        + UNITS_COUNT * 5 // five AI/order columns: u8 each
        + UNITS_COUNT * 5; // ground_weapon..ai_internal: u8 each

    /// Sets unit `unit_id`'s `special_ability_flags` Building bit (`0x1`) directly on raw
    /// `units.dat` bytes (index 0 of a `DatBytes`, matching `crate::gamedata::tests`'
    /// `set_render_style`-style direct pokes).
    fn set_unit_building(units: &mut [u8], unit_id: u16) {
        let off = UNITS_SPECIAL_ABILITY_FLAGS_COLUMN + unit_id as usize * 4;
        units[off..off + 4].copy_from_slice(&1u32.to_le_bytes());
    }

    #[test]
    fn dot_border_native_px_is_always_at_least_one() {
        assert_eq!(dot_border_native_px(2.0), 1);
        assert_eq!(dot_border_native_px(1.0), 1);
        assert_eq!(dot_border_native_px(0.5), 1);
    }
}
