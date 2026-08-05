//! The zero-asset minimap renderer: a preset color per terrain tile (baked from real SC:R
//! assets at dev time, embedded at compile time — see `tables`), plus unit/resource/start-location
//! dots. See `docs/render-design.md`'s "Minimap" section for the full design (pinned and
//! authoritative); this module implements it exactly.
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
//! Confirmed from OpenBW (`ui/ui.h`, `draw_minimap`): exactly 1 px per map tile, no averaging.
//! Per tile: look up its megatile, then minitile `[0]` (top-left of the 4x4 grid, VX4's flip bit
//! *deliberately ignored* — the minimap path doesn't apply it), then sample byte 55 (row 6,
//! column 7) of that minitile's 8x8 VR4 bitmap; the resulting palette index, resolved through
//! WPE, is the tile's color. Creep-flagged tiles substitute a pseudo-random megatile from the
//! creep tile group (`cv5[1]`), weighted ~4% toward variant tiles 6-12 and otherwise uniform over
//! 0-5, chosen once per map cell via a deterministic hash (see [`creep_slot`]) so renders stay
//! byte-reproducible.
//!
//! [`build_minimap_table`] bakes this into small per-tileset tables (dev-time only, run via
//! `broodmap-cli`'s hidden `gen-minimap-tables` subcommand against a real install); `tables`
//! embeds the 8 committed outputs via `include_bytes!` so render time never touches VX4EX/VR4/WPE
//! (or even a [`crate::TilesetDataSource`]) at all.

mod tables;

use broodmap::chk::Chk;
use broodmap::chk::placed_units::{PlacedUnit, UnitState};
use broodmap::chk::player_colors::PlayerColors;
use broodmap::chk::sprites::{Sprite, SpriteFlags};
use broodmap::chk::terrain::{TerrainTileIds, TileId};
use broodmap::chk::tileset::Tileset;
use broodmap_formats::{Cv5, Vr4, Vx4, parse_cv5, parse_vr4, parse_vx4ex, parse_wpe};

use crate::error::RenderError;
use crate::gamedata::{GameData, UNIT_ID_START_LOCATION, is_resource};
use crate::image::RgbaImage;
use crate::options::{StartLocations, UnitFilter};
use crate::overlay::{
    MAX_START_LOCATION_BOX_LOGICAL_PX, Preview, blend_over, hq_clear_bounds,
    overlaps_any_start_area, owner_color, start_location_box,
};
use crate::token::draw_player_token;
use tables::MinimapTable;

/// Number of creep palette-index slots baked into a table (`cv5[1]`'s first 13 megatile slots) —
/// mirrors `tables::CREEP_SLOTS`, duplicated here (rather than made `pub(crate)` there) since it's
/// meaningful at a different level: there it's a blob-layout constant, here it's "how many creep
/// variants BW's tile group table defines".
const CREEP_SLOTS: usize = 13;

/// The CHK parser's own documented invariant on map dimensions — see `crate::terrain`'s identical
/// constant for why this is re-clamped here rather than trusted from `TerrainTileIds`.
const MAX_TERRAIN_DIM: usize = 256;

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
/// [0..2)          u16   num_tile_ids            (= cv5 group count * 16)
/// [2..2+n)        u8    palette index per CHK *unified* tile id   (n = num_tile_ids)
/// [2+n..2+n+13)   u8    13 creep palette indices (creep group cv5[1], megatile slots 0..13)
/// [2+n+13..+768)  u8    256 x [r,g,b] palette (from the tileset's .wpe)
/// ```
///
/// Per tile id `t`: group = `t >> 4`, index = `t & 15`; megatile = `cv5.group(group)?
/// .mega_tiles[index]`; minitile `[0]` (top-left of the 4x4 grid) from `.vx4ex`, ignoring its
/// flip bit (deliberate — BW's minimap sampling doesn't apply it either); the palette index is
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
    let mut indices = Vec::with_capacity(num_tile_ids as usize);
    for unified_tile_id in 0..num_tile_ids {
        indices.push(sample_tile_palette_index(
            &cv5,
            &vx4ex,
            &vr4,
            unified_tile_id,
        ));
    }

    let mut creep = [0u8; CREEP_SLOTS];
    if let Some(group) = cv5.group(1) {
        for (slot, &megatile_id) in creep.iter_mut().zip(group.mega_tiles.iter()) {
            *slot = sample_megatile_palette_index(&vx4ex, &vr4, megatile_id);
        }
    }

    let mut out = Vec::with_capacity(2 + indices.len() + CREEP_SLOTS + 768);
    out.extend_from_slice(&num_tile_ids.to_le_bytes());
    out.extend_from_slice(&indices);
    out.extend_from_slice(&creep);
    for i in 0..=u8::MAX {
        out.extend_from_slice(&wpe.color(i));
    }

    Ok(out)
}

/// Resolves a CHK unified tile id to its minimap palette index: `t >> 4` picks the CV5 group,
/// `t & 15` the megatile within it, then [`sample_megatile_palette_index`]. A missing group (an
/// id the tileset's CV5 doesn't define) degrades to index 0.
fn sample_tile_palette_index(
    cv5: &Cv5,
    vx4ex: &Vx4<'_>,
    vr4: &Vr4<'_>,
    unified_tile_id: u16,
) -> u8 {
    let group_id = unified_tile_id >> 4;
    let index = (unified_tile_id & 0xF) as usize;
    let Some(group) = cv5.group(group_id) else {
        return 0;
    };
    sample_megatile_palette_index(vx4ex, vr4, group.mega_tiles[index])
}

/// Samples a megatile's minimap palette index: minitile `[0]` of its 4x4 grid (flip bit ignored),
/// byte 55 of that minitile's VR4 bitmap. Any missing lookup (an out-of-range megatile or minitile
/// id) degrades to index 0.
fn sample_megatile_palette_index(vx4ex: &Vx4<'_>, vr4: &Vr4<'_>, megatile_id: u16) -> u8 {
    let Some(minitile) = vx4ex.minitile(megatile_id as usize, 0) else {
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
    /// Output pixels per map tile. Clamped to `1..=8` at render time (not at construction, so a
    /// caller can freely set/read any value). `1` (BW's own native minimap resolution) is the
    /// default.
    pub px_per_tile: u32,
    /// Substitute a pseudo-random creep-group color for creep-flagged tiles (see the module
    /// docs's "BW's minimap algorithm" section). Default `true`.
    pub show_creep: bool,
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
            px_per_tile: 1,
            show_creep: true,
            show_units: true,
            show_resources: true,
            highlight_resources: true,
            start_locations: StartLocations::default(),
            unit_filter: UnitFilter::default(),
        }
    }
}

// -------------------------------------------------------------------------------------------
// Rendering
// -------------------------------------------------------------------------------------------

/// Renders a zero-asset minimap: a preset color per terrain tile plus unit/resource/start-location
/// dots. Needs no [`crate::TilesetDataSource`] at all — see the module docs.
///
/// `data` is an already-loaded [`GameData`] (from [`GameData::load`]), used only to size unit/
/// resource dots from `units.dat`'s placebox and to apply melee's start-area clearing; `None`
/// still renders a complete, correct minimap (the zero-asset promise), just with 1x1-tile dots and
/// no start-area clearing (documented on [`MinimapOptions`]'s fields and below).
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

    let ppt = options.px_per_tile.clamp(1, 8);
    let out_w = map_w * ppt;
    let out_h = map_h * ppt;
    let mut image = RgbaImage {
        width: out_w,
        height: out_h,
        data: vec![0u8; out_w as usize * out_h as usize * 4],
    };

    draw_terrain(
        &mut image,
        terrain,
        map_w,
        map_h,
        ppt,
        &table,
        options.show_creep,
    );

    let dots = collect_dots(units, sprites, data, options);
    // Paint order: resources, then ordinary units, then start locations (always on top) — see
    // the module docs.
    draw_dots(&mut image, &dots, true, player_colors, data, options, ppt);
    draw_dots(&mut image, &dots, false, player_colors, data, options, ppt);
    draw_start_locations(&mut image, units, player_colors, data, options, ppt);

    image
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
// Terrain pass
// -------------------------------------------------------------------------------------------

/// Draws every terrain tile as a `ppt`x`ppt` block of its resolved color: no resampling/
/// averaging, matching BW's own "1 px per tile" minimap sampling (scaled up by nearest-neighbor
/// replication when `ppt > 1`).
fn draw_terrain(
    image: &mut RgbaImage,
    terrain: &TerrainTileIds,
    map_w: u32,
    map_h: u32,
    ppt: u32,
    table: &Option<MinimapTable<'_>>,
    show_creep: bool,
) {
    for y in 0..map_h {
        for x in 0..map_w {
            let tile = terrain
                .tiles
                .get(y as usize * terrain.width + x as usize)
                .copied()
                .unwrap_or_default();
            let color = tile_pixel_color(table, tile, x, y, show_creep);
            set_tile_block(image, x, y, ppt, color);
        }
    }
}

/// The color one terrain cell resolves to: a creep-group color (if creep-flagged and
/// `show_creep`) or the tile's own baked color. A tileset with no (or a malformed) committed
/// table renders solid black — documented degradation, never a panic or an error.
fn tile_pixel_color(
    table: &Option<MinimapTable<'_>>,
    tile: TileId,
    x: u32,
    y: u32,
    show_creep: bool,
) -> [u8; 3] {
    let Some(table) = table else {
        return [0, 0, 0];
    };
    if tile.has_creep() && show_creep {
        table.creep_color(creep_slot(x, y))
    } else {
        table.tile_color(tile.id())
    }
}

/// Deterministic stand-in for BW's per-cell creep-variant RNG (see the module docs): a hash of the
/// tile's map coordinates picks a creep-group megatile slot, weighted ~4% toward the "variant"
/// slots 6-12 and otherwise uniform over the "plain" slots 0-5. Determinism is load-bearing here —
/// the same map must render byte-identically every time — so this is a fixed hash, not an RNG.
fn creep_slot(x: u32, y: u32) -> usize {
    let h = x.wrapping_mul(0x9E37_79B1) ^ y.wrapping_mul(0x85EB_CA77);
    let h = h ^ (h >> 16);
    if h % 100 < 4 {
        (6 + (h / 100) % 7) as usize
    } else {
        ((h / 100) % 6) as usize
    }
}

/// Fills the `ppt`x`ppt` output block for map tile `(x, y)` with a flat, opaque color. Direct
/// writes (no temp buffer/blend) since terrain tiles are opaque and non-overlapping by
/// construction, unlike the dot passes below.
fn set_tile_block(image: &mut RgbaImage, x: u32, y: u32, ppt: u32, color: [u8; 3]) {
    let x0 = x * ppt;
    let y0 = y * ppt;
    for dy in 0..ppt {
        let row = y0 + dy;
        if row >= image.height {
            break;
        }
        let row_start = (row as usize * image.width as usize + x0 as usize) * 4;
        for dx in 0..ppt {
            let col = x0 + dx;
            if col >= image.width {
                break;
            }
            let offset = row_start + dx as usize * 4;
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
// Unit/resource dots
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
/// then units" order from [`render_minimap_with_table`]).
fn draw_dots(
    image: &mut RgbaImage,
    dots: &[Dot],
    draw_resources: bool,
    player_colors: &PlayerColors,
    data: Option<&GameData>,
    options: &MinimapOptions,
    ppt: u32,
) {
    let zoom = ppt as f32 / 32.0;
    for dot in dots.iter().filter(|d| d.is_resource == draw_resources) {
        let color = if dot.is_resource && options.highlight_resources {
            RESOURCE_MINIMAP_COLOR
        } else {
            owner_color(dot.owner, player_colors)
        };
        let (box_w, box_h) = dot_box_logical_px(dot.unit_id, data);
        let left = ((dot.x as f32 - box_w as f32 / 2.0) * zoom).round() as i32;
        let top = ((dot.y as f32 - box_h as f32 / 2.0) * zoom).round() as i32;
        let right = ((dot.x as f32 + box_w as f32 / 2.0) * zoom).round() as i32;
        let bottom = ((dot.y as f32 + box_h as f32 / 2.0) * zoom).round() as i32;
        fill_rect(
            image,
            left,
            top,
            right.max(left + 1),
            bottom.max(top + 1),
            color,
        );
    }
}

/// A dot's box size in logical pixels (32/tile): with `data`, `ceil(units.dat placebox / 32)`
/// tiles per axis, clamped `1..=4`; without `data`, a flat 1x1 tile — see [`MinimapOptions`]'s
/// docs ("Dot geometry" in `docs/render-design.md`).
fn dot_box_logical_px(unit_id: u16, data: Option<&GameData>) -> (u32, u32) {
    const LOGICAL_PX_PER_TILE: u32 = 32;
    match data {
        Some(data) => {
            let (w, h) = data
                .units_dat()
                .entry(unit_id)
                .map(|entry| entry.placebox)
                .unwrap_or((0, 0));
            let tiles_w = (w.max(0) as u32).div_ceil(LOGICAL_PX_PER_TILE).clamp(1, 4);
            let tiles_h = (h.max(0) as u32).div_ceil(LOGICAL_PX_PER_TILE).clamp(1, 4);
            (tiles_w * LOGICAL_PX_PER_TILE, tiles_h * LOGICAL_PX_PER_TILE)
        }
        None => (LOGICAL_PX_PER_TILE, LOGICAL_PX_PER_TILE),
    }
}

// -------------------------------------------------------------------------------------------
// Start locations
// -------------------------------------------------------------------------------------------

/// Draws every start location as a player token (`crate::token::draw_player_token`, the same
/// "ColorBlock" style `crate::overlay::draw_start_location_blocks` uses for the full preview
/// renderer), always painted last so it sits on top of every terrain/unit/resource pixel.
/// [`StartLocations::Hidden`] draws nothing; [`StartLocations::Sprite`] is treated exactly like
/// [`StartLocations::ColorBlock`] (see [`MinimapOptions::start_locations`]'s docs -- there's no
/// art to draw here).
fn draw_start_locations(
    image: &mut RgbaImage,
    units: &[PlacedUnit],
    player_colors: &PlayerColors,
    data: Option<&GameData>,
    options: &MinimapOptions,
    ppt: u32,
) {
    if options.start_locations == StartLocations::Hidden {
        return;
    }

    let (box_w, box_h) = start_location_box(data);
    let box_w = box_w.min(MAX_START_LOCATION_BOX_LOGICAL_PX);
    let box_h = box_h.min(MAX_START_LOCATION_BOX_LOGICAL_PX);
    let zoom = ppt as f32 / 32.0;

    for unit in units.iter().filter(|u| u.unit_id == UNIT_ID_START_LOCATION) {
        let color = owner_color(unit.owner.unwrap_or(u8::MAX), player_colors);
        let left = ((unit.x as f32 - box_w as f32 / 2.0) * zoom).round() as i32;
        let top = ((unit.y as f32 - box_h as f32 / 2.0) * zoom).round() as i32;
        let right = ((unit.x as f32 + box_w as f32 / 2.0) * zoom).round() as i32;
        let bottom = ((unit.y as f32 + box_h as f32 / 2.0) * zoom).round() as i32;
        draw_player_token(
            image,
            left,
            top,
            right.max(left + 1),
            bottom.max(top + 1),
            color,
        );
    }
}

/// Fills an output-space rect `[left, right) x [top, bottom)` with a flat, opaque color,
/// bounds-clipped via [`blend_over`]. `left..bottom` come from unit positions/placeboxes already
/// bounded by [`dot_box_logical_px`]'s `1..=4`-tile clamp (at most `4 * 8 == 32` output px per
/// axis, since [`render_minimap_with_table`] clamps `ppt` to `1..=8`), so unlike
/// `crate::token::draw_player_token`'s huge-placebox guard, no extra pre-clip is needed here.
fn fill_rect(image: &mut RgbaImage, left: i32, top: i32, right: i32, bottom: i32, color: [u8; 3]) {
    let w = (right - left).max(0) as u32;
    let h = (bottom - top).max(0) as u32;
    if w == 0 || h == 0 {
        return;
    }
    let mut buf = Vec::with_capacity(w as usize * h as usize * 4);
    for _ in 0..(w * h) {
        buf.extend_from_slice(&[color[0], color[1], color[2], 255]);
    }
    blend_over(image, &buf, w, h, left, top);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gamedata::tests::{synthetic_parts, synthetic_source};
    use crate::options::UnitFilter;
    use broodmap::chk::placed_units::UnitInstanceId;
    use broodmap::chk::player_colors::PlayerColor;
    use tables::tests::synthetic_table_bytes;

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

    fn checkerboard_terrain() -> TerrainTileIds {
        // group_id is bits 4..15 of the tile id, tile_index the bottom 4 bits: tile 0 -> unified
        // id 0 (table fill color), tile 16 -> unified id 16 (out of a 1-entry table's range,
        // degrading to index 0's color -- same color, chosen so both halves of the checkerboard
        // are deliberately identical unless a test overrides one half with creep).
        TerrainTileIds {
            width: 2,
            height: 2,
            tiles: vec![TileId(0), TileId(16), TileId(16), TileId(0)],
        }
    }

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
        let terrain = checkerboard_terrain();
        let image = render_minimap_with_table(
            &terrain,
            None,
            &[],
            &[],
            &PlayerColors::default(),
            None,
            &MinimapOptions::default(),
        );
        assert_eq!((image.width, image.height), (2, 2));
        for texel in image.data.chunks_exact(4) {
            assert_eq!(texel, [0, 0, 0, 255]);
        }
    }

    #[test]
    fn terrain_tile_resolves_through_the_table() {
        // A 1-tile-id table (only unified id 0 defined) where palette index 5 -> [50, 60, 70].
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&1u16.to_le_bytes());
        bytes.push(5); // unified tile id 0 -> palette index 5
        bytes.extend([0u8; CREEP_SLOTS]); // creep slots unused by this test
        for i in 0..=u8::MAX {
            if i == 5 {
                bytes.extend([50, 60, 70]);
            } else {
                bytes.extend([i, i, i]);
            }
        }
        let table = tables::parse_table(&bytes).unwrap();

        let terrain = TerrainTileIds {
            width: 1,
            height: 1,
            tiles: vec![TileId(0)],
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
        assert_eq!(pixel(&image, 0, 0), [50, 60, 70, 255]);
    }

    #[test]
    fn out_of_range_tile_id_degrades_to_index_0_color() {
        let table_bytes = synthetic_table_bytes(1, 3, 9);
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
        // Index 0's color, per `synthetic_table_bytes`'s `[i, i, i]` palette.
        assert_eq!(pixel(&image, 0, 0), [0, 0, 0, 255]);
    }

    #[test]
    fn creep_flag_substitutes_the_creep_color_and_the_hash_is_pinned() {
        let table_bytes = synthetic_table_bytes(1, 3, 9);
        let table = tables::parse_table(&table_bytes).unwrap();
        // Pin the hash for (0, 0): documents the deterministic mapping so a future accidental
        // change to the constants/formula shows up as a test failure, not a silent drift. h(0,0)
        // == 0, and 0 % 100 < 4, so this lands in the "variant" branch: 6 + (0 / 100) % 7 == 6.
        assert_eq!(creep_slot(0, 0), 6);

        let terrain = TerrainTileIds {
            width: 1,
            height: 1,
            tiles: vec![TileId(0x8000)], // creep flag set, unified id 0
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
        // synthetic_table_bytes fills every creep slot with palette index 9 -> [9, 9, 9], vs.
        // the non-creep fill index 3 -> [3, 3, 3]: distinguishable proof creep substitution ran.
        assert_eq!(pixel(&image, 0, 0), [9, 9, 9, 255]);

        let no_creep_options = MinimapOptions {
            show_creep: false,
            ..Default::default()
        };
        let table2_bytes = synthetic_table_bytes(1, 3, 9);
        let table2 = tables::parse_table(&table2_bytes).unwrap();
        let image2 = render_minimap_with_table(
            &terrain,
            Some(table2),
            &[],
            &[],
            &PlayerColors::default(),
            None,
            &no_creep_options,
        );
        assert_eq!(
            pixel(&image2, 0, 0),
            [3, 3, 3, 255],
            "show_creep: false must draw the tile's own color even when creep-flagged"
        );
    }

    #[test]
    fn px_per_tile_scales_terrain_by_nearest_replication() {
        let table_bytes = synthetic_table_bytes(1, 5, 9);
        let table = tables::parse_table(&table_bytes).unwrap();
        let terrain = TerrainTileIds {
            width: 1,
            height: 1,
            tiles: vec![TileId(0)],
        };
        let options = MinimapOptions {
            px_per_tile: 3,
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
        assert_eq!((image.width, image.height), (3, 3));
        for texel in image.data.chunks_exact(4) {
            assert_eq!(texel, [5, 5, 5, 255]);
        }
    }

    #[test]
    fn px_per_tile_is_clamped_to_eight() {
        let terrain = TerrainTileIds {
            width: 1,
            height: 1,
            tiles: vec![TileId(0)],
        };
        let options = MinimapOptions {
            px_per_tile: 100,
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
        assert_eq!((image.width, image.height), (8, 8));
    }

    #[test]
    fn unit_dot_draws_in_owner_color() {
        let table_bytes = synthetic_table_bytes(1, 0, 0);
        let table = tables::parse_table(&table_bytes).unwrap();
        let terrain = TerrainTileIds {
            width: 4,
            height: 4,
            tiles: vec![TileId(0); 16],
        };
        // Placed at the centre of tile (2, 2) -- logical px (80, 80).
        let units = [unit(0, Some(0), 80, 80)];
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
        // Data-less: a flat 1x1-tile dot centred on tile (2, 2).
        assert_eq!(pixel(&image, 2, 2), [244, 4, 4, 255]);
    }

    #[test]
    fn resource_dot_uses_the_highlight_color_by_default() {
        let table_bytes = synthetic_table_bytes(1, 0, 0);
        let table = tables::parse_table(&table_bytes).unwrap();
        let terrain = TerrainTileIds {
            width: 2,
            height: 2,
            tiles: vec![TileId(0); 4],
        };
        let units = [unit(176, Some(11), 16, 16)]; // mineral field, tile (0, 0)
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
        assert_eq!(rgb(&image, 0, 0), RESOURCE_MINIMAP_COLOR);

        let no_highlight = MinimapOptions {
            unit_filter: UnitFilter::AsPlaced,
            highlight_resources: false,
            ..Default::default()
        };
        let table2_bytes = synthetic_table_bytes(1, 0, 0);
        let table2 = tables::parse_table(&table2_bytes).unwrap();
        let image2 = render_minimap_with_table(
            &terrain,
            Some(table2),
            &units,
            &[],
            &player_colors(),
            None,
            &no_highlight,
        );
        // No owner slot recorded for this unit besides 11 (neutral) -- neutral color, not red.
        assert_ne!(rgb(&image2, 0, 0), [244, 4, 4]);
    }

    #[test]
    fn start_location_block_draws_on_top_of_everything() {
        let table_bytes = synthetic_table_bytes(1, 0, 0);
        let table = tables::parse_table(&table_bytes).unwrap();
        let terrain = TerrainTileIds {
            width: 8,
            height: 8,
            tiles: vec![TileId(0); 64],
        };
        // A player-owned unit exactly under the start location -- as-placed keeps both, and the
        // start location must still win the pixel since it's drawn last.
        let units = [
            unit(0, Some(1), 128, 128),   // blue, tile (4, 4)
            unit(214, Some(0), 128, 128), // start location, red, same spot
        ];
        let options = MinimapOptions {
            unit_filter: UnitFilter::AsPlaced,
            px_per_tile: 4,
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
        // Centre of the start-location token: the fallback 128x96-logical-px box centred on tile
        // (4, 4) at 4 px/tile lands its output-space box at x in [8, 24), y in [10, 22) -- centre
        // (16, 16). The token's vertical gradient means only the exact mid-row is the unmixed
        // player color, so this allows the same tolerance `tests/real_assets.rs` uses.
        let (x, y) = (16, 16);
        assert!(
            close_to_player_color(rgb(&image, x, y), [244, 4, 4]),
            "the start-location token must draw over the unit dot beneath it, got {:?}",
            rgb(&image, x, y)
        );
    }

    #[test]
    fn hidden_start_locations_draw_nothing() {
        let table_bytes = synthetic_table_bytes(1, 0, 0);
        let table = tables::parse_table(&table_bytes).unwrap();
        let terrain = TerrainTileIds {
            width: 4,
            height: 4,
            tiles: vec![TileId(0); 16],
        };
        let units = [unit(214, Some(0), 64, 64)];
        let options = MinimapOptions {
            start_locations: StartLocations::Hidden,
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
        // Terrain color everywhere (table fill index 0 -> [0, 0, 0]) -- no red token anywhere.
        for y in 0..image.height {
            for x in 0..image.width {
                assert_ne!(rgb(&image, x, y), [244, 4, 4]);
            }
        }
    }

    #[test]
    fn sprite_start_locations_behave_like_color_block() {
        let terrain = TerrainTileIds {
            width: 8,
            height: 8,
            tiles: vec![TileId(0); 64],
        };
        let units = [unit(214, Some(0), 128, 128)];
        for mode in [StartLocations::ColorBlock, StartLocations::Sprite] {
            let table_bytes = synthetic_table_bytes(1, 0, 0);
            let table = tables::parse_table(&table_bytes).unwrap();
            let options = MinimapOptions {
                start_locations: mode,
                px_per_tile: 4,
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
            assert!(
                close_to_player_color(rgb(&image, 16, 16), [244, 4, 4]),
                "{mode:?}: got {:?}",
                rgb(&image, 16, 16)
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
        let terrain = TerrainTileIds {
            width: 4,
            height: 4,
            tiles: vec![TileId(0); 16],
        };
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
        let table_bytes = synthetic_table_bytes(1, 0, 0);
        let table = tables::parse_table(&table_bytes).unwrap();
        let options = MinimapOptions {
            unit_filter: UnitFilter::AsPlaced,
            ..Default::default()
        };
        let image = render_minimap_with_table(
            &terrain,
            Some(table),
            &[],
            &sprites,
            &player_colors(),
            None,
            &options,
        );
        assert_ne!(
            rgb(&image, 1, 1),
            [244, 4, 4],
            "doodad sprite must not be drawn"
        );
        assert_eq!(rgb(&image, 3, 3), [244, 4, 4], "unit sprite must be drawn");
    }

    #[test]
    fn dataless_melee_still_drops_player_owned_units() {
        let table_bytes = synthetic_table_bytes(1, 0, 0);
        let table = tables::parse_table(&table_bytes).unwrap();
        let terrain = TerrainTileIds {
            width: 4,
            height: 4,
            tiles: vec![TileId(0); 16],
        };
        let units = [
            unit(0, Some(0), 32, 32),  // player-owned: dropped under melee
            unit(0, Some(11), 96, 96), // neutral: kept
        ];
        let options = MinimapOptions::default(); // Melee is the default
        let image = render_minimap_with_table(
            &terrain,
            Some(table),
            &units,
            &[],
            &player_colors(),
            None, // no GameData -- the zero-asset path
            &options,
        );
        // Player-owned dot's tile (1, 1) must stay the terrain color, not red.
        assert_ne!(rgb(&image, 1, 1), [244, 4, 4]);
        // Neutral dot's tile (3, 3) must show its (neutral) color -- the same value as
        // `RESOURCE_MINIMAP_COLOR`, reached here via `owner_color`'s neutral fallback rather than
        // resource highlighting (this unit isn't a resource).
        assert_eq!(rgb(&image, 3, 3), RESOURCE_MINIMAP_COLOR);
    }

    #[test]
    fn as_placed_keeps_player_owned_units_even_without_data() {
        let table_bytes = synthetic_table_bytes(1, 0, 0);
        let table = tables::parse_table(&table_bytes).unwrap();
        let terrain = TerrainTileIds {
            width: 4,
            height: 4,
            tiles: vec![TileId(0); 16],
        };
        let units = [unit(0, Some(0), 32, 32)];
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
        assert_eq!(rgb(&image, 1, 1), [244, 4, 4]);
    }

    #[test]
    fn hallucinated_units_are_never_drawn() {
        let table_bytes = synthetic_table_bytes(1, 0, 0);
        let table = tables::parse_table(&table_bytes).unwrap();
        let terrain = TerrainTileIds {
            width: 4,
            height: 4,
            tiles: vec![TileId(0); 16],
        };
        let mut hallucinated = unit(0, Some(0), 32, 32);
        hallucinated.state = UnitState::HALLUCINATED;
        let options = MinimapOptions {
            unit_filter: UnitFilter::AsPlaced,
            ..Default::default()
        };
        let image = render_minimap_with_table(
            &terrain,
            Some(table),
            &[hallucinated],
            &[],
            &player_colors(),
            None,
            &options,
        );
        assert_ne!(rgb(&image, 1, 1), [244, 4, 4]);
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
        // Empty CV5 -> 0 tile ids: header (2) + 0 indices + 13 creep + 768 palette.
        assert_eq!(out.len(), 2 + CREEP_SLOTS + 768);
        assert_eq!(&out[0..2], &0u16.to_le_bytes());
    }

    /// Exercises the full sampling chain end to end with real (synthetic-but-well-formed) CV5/
    /// VX4EX/VR4/WPE bytes, and pins the creep-group sampling too.
    #[test]
    fn build_minimap_table_samples_the_real_chain() {
        // A single CV5 group (group 0) whose megatile 0 -> minitile[0] -> vr4 index 0 -> palette
        // index 7 (byte 55). Also serves as the creep group (cv5.group(1) is absent, so the
        // creep slots all degrade to index 0).
        let mut cv5 = Vec::new();
        let mut mega_tiles = [0u16; 16];
        mega_tiles[0] = 0; // -> megatile id 0
        cv5.extend(cv5_entry(mega_tiles));

        let mut vx4ex = Vec::new();
        // 16 minitile refs (u32 each); index 0 -> vr4 index 3, flip bit set (must be ignored).
        let raw = (3u32 << 1) | 1;
        vx4ex.extend_from_slice(&raw.to_le_bytes());
        for _ in 1..16 {
            vx4ex.extend_from_slice(&0u32.to_le_bytes());
        }

        let mut vr4 = Vec::new();
        for i in 0..4u8 {
            let mut bitmap = [0u8; 64];
            bitmap[55] = if i == 3 { 7 } else { 0 };
            vr4.extend_from_slice(&bitmap);
        }

        let mut wpe = vec![0u8; 1024];
        wpe[7 * 4..7 * 4 + 3].copy_from_slice(&[10, 20, 30]);

        let out = build_minimap_table(&cv5, &vx4ex, &vr4, &wpe).unwrap();
        let table = tables::parse_table(&out).unwrap();
        assert_eq!(table.tile_color(0), [10, 20, 30]);
        // No group 1 -> every creep slot degrades to palette index 0 ([0, 0, 0] here).
        assert_eq!(table.creep_color(0), [0, 0, 0]);
    }

    #[test]
    fn build_minimap_table_samples_the_creep_group() {
        // Group 1 (the creep group) has megatile 0 -> minitile[0] -> vr4 index 5 -> palette
        // index 42.
        let mut cv5 = Vec::new();
        cv5.extend(cv5_entry([0u16; 16])); // group 0: unused by this test
        let mut creep_mega_tiles = [0u16; 16];
        creep_mega_tiles[0] = 1; // creep slot 0 -> megatile id 1
        cv5.extend(cv5_entry(creep_mega_tiles));

        let mut vx4ex = Vec::new();
        vx4ex.extend(vec![0u8; 16 * 4]); // megatile 0: all zero (unused)
        let mut megatile1 = Vec::new();
        let raw = 5u32 << 1; // vr4 index 5, no flip
        megatile1.extend_from_slice(&raw.to_le_bytes());
        for _ in 1..16 {
            megatile1.extend_from_slice(&0u32.to_le_bytes());
        }
        vx4ex.extend(megatile1);

        let mut vr4 = Vec::new();
        for i in 0..6u8 {
            let mut bitmap = [0u8; 64];
            bitmap[55] = if i == 5 { 42 } else { 0 };
            vr4.extend_from_slice(&bitmap);
        }

        let mut wpe = vec![0u8; 1024];
        wpe[42 * 4..42 * 4 + 3].copy_from_slice(&[1, 2, 3]);

        let out = build_minimap_table(&cv5, &vx4ex, &vr4, &wpe).unwrap();
        let table = tables::parse_table(&out).unwrap();
        assert_eq!(table.creep_color(0), [1, 2, 3]);
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

    #[test]
    fn dot_box_logical_px_uses_the_placebox_with_data_and_a_flat_tile_without() {
        // Build `units.dat` bytes directly so unit 5's placebox is 96x64 (3x2 tiles -> exactly
        // 96x64 logical px, no clamping needed).
        let mut parts = synthetic_parts(5, 0, 0, 0, None);
        let off = UNITS_PLACEBOX_COLUMN + 5 * 4;
        parts.0[off..off + 2].copy_from_slice(&96i16.to_le_bytes());
        parts.0[off + 2..off + 4].copy_from_slice(&64i16.to_le_bytes());

        let source = synthetic_source(parts);
        let data = GameData::load(&source).unwrap();

        assert_eq!(dot_box_logical_px(5, Some(&data)), (96, 64));
        assert_eq!(dot_box_logical_px(5, None), (32, 32));
    }

    #[test]
    fn dot_box_logical_px_clamps_to_four_tiles() {
        // A placebox declaring 10 tiles wide (320 logical px) must clamp to 4 tiles (128).
        let mut parts = synthetic_parts(5, 0, 0, 0, None);
        let off = UNITS_PLACEBOX_COLUMN + 5 * 4;
        parts.0[off..off + 2].copy_from_slice(&320i16.to_le_bytes());
        parts.0[off + 2..off + 4].copy_from_slice(&320i16.to_le_bytes());

        let source = synthetic_source(parts);
        let data = GameData::load(&source).unwrap();

        assert_eq!(dot_box_logical_px(5, Some(&data)), (128, 128));
    }
}
