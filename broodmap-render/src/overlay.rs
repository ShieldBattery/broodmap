//! The unit/sprite overlay: what turns a terrain render into an actual map *preview*.
//!
//! Placed units (the CHK `UNIT` chunk) and sprites (`THG2`) are resolved through the `.dat`
//! chain to image IDs (see [`crate::GameData`]), each image's `.anim` art is fetched, the needed
//! frame is cropped out of its layer atlas, team-colored, downscaled to the render's zoom, and
//! composited onto the terrain in painter's order.
//!
//! Everything here is permissive in the same way the parsers are: an image that can't be
//! resolved, an `.anim` that's missing or in an unsupported container, a layer in a format we
//! can't decode — each of those drops exactly one drawable and the render continues. Only the
//! whole-file `.dat`/`.rel` tables (and the terrain assets) can fail a render outright.
//!
//! # Coordinate spaces
//!
//! Three of them, and mixing them up is the classic way to get sprites that are the right art in
//! the wrong place:
//!
//! - **Logical pixels** — BW's own space, 32 per tile. CHK unit/sprite positions are in it.
//! - **4K units** — the `.anim` frame table's space: 4 units per logical pixel, the same at
//!   every tier (so a frame's placement is tier-independent).
//! - **Texels** — the pixels of the `.anim`'s embedded layer textures, `anim.scale()` of them
//!   per logical pixel (SD 1, HD2 2, HD 4).
//!
//! The render's zoom is `z = ppt / 32` output pixels per logical pixel, so a frame's bitmap
//! scales by `z / anim.scale()` and its placement offsets (4K units) by `z / 4`.

use std::collections::HashMap;

use broodmap::chk::Chk;
use broodmap::chk::placed_units::{PlacedUnit, UnitState};
use broodmap::chk::player_colors::{PlayerColors, resolve_color};
use broodmap::chk::sprites::{Sprite, SpriteFlags};
use broodmap::chk::terrain::TerrainTileIds;
use broodmap::chk::tileset::Tileset;
use broodmap_formats::{Anim, AnimFrame, AnimLayer, DdsFormat, parse_dds};

use crate::bc::{decode_bc1, decode_bc3};
use crate::error::RenderError;
use crate::gamedata::{
    GameData, IMAGE_ID_START_LOCATION, UNIT_ID_START_LOCATION, UNIT_ID_VESPENE_GEYSER,
    UNIT_IDS_MINERAL_FIELD, anim_request, is_critter, is_resource,
};
use crate::image::{RgbaImage, scale_rgba_premultiplied};
use crate::options::{RenderOptions, StartLocations, UnitFilter, resolve_tier, resolve_unit_tier};
use crate::source::{AssetRequest, TilesetDataSource};
use crate::terrain::render_terrain;
use crate::tier::{ArtPack, AssetTier};

/// Upper bound on the width/height we'll decode an `.anim` layer atlas at. Real HD atlases top
/// out well under this; the cap just keeps a corrupt or hostile header from driving an unbounded
/// allocation (4096x4096 RGBA is 64 MiB, and only one atlas is ever live at a time).
const MAX_ANIM_TEXTURE_DIM: u32 = 4096;

/// Upper bound on a single output tile's width/height, in pixels, applied to the already-
/// downscaled crop (never an upscale target). Redundant with [`MAX_ANIM_TEXTURE_DIM`] today
/// (the crop this scales *from* is itself capped there), but kept as an explicit, independent
/// belt-and-suspenders bound at the point the final allocation actually happens — larger than
/// any legitimate frame at max zoom, and cheap insurance if the crop-side bound ever changes.
const MAX_TILE_OUTPUT_DIM: u32 = 4096;

/// Logical pixels per tile in BW's coordinate space.
const LOGICAL_PX_PER_TILE: f32 = 32.0;

/// 4K coordinate-space units per logical pixel (see the module docs).
const UNITS_PER_LOGICAL_PX: f32 = 4.0;

/// The color used for units owned by a non-player slot (neutral, rescuable, ...): SC:R's cyan.
const NEUTRAL_COLOR: [u8; 3] = [0, 228, 252];

/// Start-location placement box (logical pixels) used only when `units.dat` isn't available —
/// i.e. an [`crate::ArtStyle::Original`] unit layer, where the `.dat` tables are deliberately
/// not part of the prefetch set. This is `units.dat`'s own value for unit 214 (4x3 tiles); when
/// the table *is* loaded, the value is read from it rather than from here.
const FALLBACK_START_LOCATION_BOX: (u32, u32) = (128, 96);

/// Upper bound (logical px, per axis) on the start-location box used to size the color-block
/// token. `units.dat`'s placebox is untrusted (a custom asset source controls its bytes), and it
/// feeds a token allocation that's clipped to the image *inside* `crate::token` — this is an
/// earlier, logical-space belt-and-suspenders clamp so a hostile/corrupted placebox (up to the
/// raw `i16` range) can't drive that sizing math with an absurd value in the first place. Real
/// start locations are 128x96; this is generous enough to never affect a legitimate render.
const MAX_START_LOCATION_BOX_LOGICAL_PX: u32 = 1024;

/// Knuth's multiplicative-hash constant, used to turn a unit's instance ID into a stable
/// "random" facing so that renders of the same map are byte-identical.
const FACING_HASH_MULTIPLIER: u32 = 2654435761;

/// The `unit_direction` value meaning "pick a random facing".
const RANDOM_DIRECTION: u8 = 32;

/// A rendered preview: the image, plus any non-fatal notes about things the render skipped.
#[derive(Debug, Clone)]
pub struct Preview {
    pub image: RgbaImage,
    /// Non-fatal notes (e.g. "unit art was skipped because the Original style isn't supported
    /// yet"). Empty on a fully successful render.
    pub warnings: Vec<String>,
}

/// Renders a full map preview: terrain with the unit/sprite overlay composited on top.
///
/// See [`render_preview_with_warnings`] for the same render with the non-fatal skip notes
/// attached, and [`render_chk_preview`] for the convenience wrapper that pulls all of these
/// inputs out of a [`Chk`].
pub fn render_preview(
    terrain: &TerrainTileIds,
    tileset: Tileset,
    units: &[PlacedUnit],
    sprites: &[Sprite],
    player_colors: &PlayerColors,
    source: &dyn TilesetDataSource,
    options: &RenderOptions,
) -> Result<RgbaImage, RenderError> {
    render_preview_with_warnings(
        terrain,
        tileset,
        units,
        sprites,
        player_colors,
        source,
        options,
    )
    .map(|preview| preview.image)
}

/// [`render_preview`], additionally reporting anything the render skipped for non-fatal reasons.
pub fn render_preview_with_warnings(
    terrain: &TerrainTileIds,
    tileset: Tileset,
    units: &[PlacedUnit],
    sprites: &[Sprite],
    player_colors: &PlayerColors,
    source: &dyn TilesetDataSource,
    options: &RenderOptions,
) -> Result<Preview, RenderError> {
    let mut image = render_terrain(terrain, tileset, source, options)?;
    let mut warnings = Vec::new();

    if image.width == 0 || image.height == 0 {
        return Ok(Preview { image, warnings });
    }

    let (_, _, ppt) = resolve_tier(options, terrain.width as u32, terrain.height as u32);
    let zoom = ppt as f32 / LOGICAL_PX_PER_TILE;

    // The `.dat`/`.rel` tables are a hard dependency of the unit layer, but only a nice-to-have
    // for start-location blocks (they supply the box size), so an Original-style render loads
    // them best-effort and falls back rather than failing. A map with no units and no sprites at
    // all can't resolve anything through them either way, so a terrain-only source (no `.dat`
    // tables present) must still render successfully — nothing here would ever read them.
    let needs_game_data = !units.is_empty() || !sprites.is_empty();
    let data = if !needs_game_data {
        None
    } else if options.unit_art_available() {
        Some(GameData::load(source)?)
    } else {
        warnings.push(
            "unit and sprite art skipped: the Original (SD) art style needs mainSD.anim, which \
             is not implemented yet"
                .to_string(),
        );
        GameData::load(source).ok()
    };

    if let Some(data) = data.as_ref().filter(|_| options.unit_art_available()) {
        let (tier, pack) = resolve_unit_tier(options, ppt);
        let drawables = collect_drawables(units, sprites, data, options, tileset);
        draw_overlay(
            &mut image,
            &drawables,
            player_colors,
            source,
            tier,
            pack,
            zoom,
        );
    }

    draw_start_location_blocks(
        &mut image,
        units,
        player_colors,
        data.as_ref(),
        options,
        zoom,
    );

    Ok(Preview { image, warnings })
}

/// Renders a map preview straight from a parsed [`Chk`].
///
/// A map with no `UNIT`/`THG2` chunk simply has no units/sprites (that's normal for minimal and
/// protected maps, not an error). Terrain, however, is required.
pub fn render_chk_preview(
    chk: &Chk,
    source: &dyn TilesetDataSource,
    options: &RenderOptions,
) -> Result<Preview, RenderError> {
    let terrain = match chk.terrain() {
        Ok(terrain) => terrain,
        // A map without readable terrain still renders — as an empty image of the right shape,
        // which is what `render_terrain` produces for an empty tile grid.
        Err(_) => {
            return Ok(Preview {
                image: RgbaImage {
                    width: 0,
                    height: 0,
                    data: Vec::new(),
                },
                warnings: vec!["map has no readable terrain".to_string()],
            });
        }
    };

    static NO_UNITS: &[PlacedUnit] = &[];
    static NO_SPRITES: &[Sprite] = &[];
    let units = chk.placed_units().map(Vec::as_slice).unwrap_or(NO_UNITS);
    let sprites = chk.sprites().map(Vec::as_slice).unwrap_or(NO_SPRITES);

    render_preview_with_warnings(
        terrain,
        chk.tileset(),
        units,
        sprites,
        chk.player_colors(),
        source,
        options,
    )
}

/// [`crate::required_preview_assets`], pulling the tileset and map dimensions out of a [`Chk`]
/// directly, so a prefetching caller can't drift from what [`render_chk_preview`] will actually
/// read. A map with no readable terrain requests nothing, matching `render_chk_preview`'s own
/// empty-image fallback for the same case.
pub fn required_preview_assets_for_chk(chk: &Chk, options: &RenderOptions) -> Vec<AssetRequest> {
    let Ok(terrain) = chk.terrain() else {
        return Vec::new();
    };
    crate::options::required_preview_assets(
        chk.tileset(),
        terrain.width as u32,
        terrain.height as u32,
        options,
    )
}

/// [`required_preview_graphics`], pulling the map's units, sprites and dimensions out of a
/// [`Chk`] directly (a missing `UNIT`/`THG2` chunk resolves to an empty list, matching
/// `render_chk_preview`). `data` is round 1's result — see [`required_preview_assets_for_chk`].
pub fn required_preview_graphics_for_chk(
    chk: &Chk,
    data: &GameData,
    options: &RenderOptions,
) -> Vec<AssetRequest> {
    let Ok(terrain) = chk.terrain() else {
        return Vec::new();
    };

    static NO_UNITS: &[PlacedUnit] = &[];
    static NO_SPRITES: &[Sprite] = &[];
    let units = chk.placed_units().map(Vec::as_slice).unwrap_or(NO_UNITS);
    let sprites = chk.sprites().map(Vec::as_slice).unwrap_or(NO_SPRITES);

    required_preview_graphics(
        units,
        sprites,
        data,
        terrain.width as u32,
        terrain.height as u32,
        options,
    )
}

/// The `.anim` assets a preview render will request for its unit/sprite layer, deduplicated and
/// in a stable order.
///
/// This is round 2 of the two-round prefetch API: it needs the [`GameData`] tables fetched in
/// round 1 ([`crate::required_preview_assets`]) to resolve units and sprites to image IDs. The
/// same filtering the renderer applies is applied here, so assets for units that `options`
/// filters out are never requested.
///
/// `map_w`/`map_h` (in tiles) are needed to derive the same effective resolution — and therefore
/// the same asset tier — the render itself will pick.
pub fn required_preview_graphics(
    units: &[PlacedUnit],
    sprites: &[Sprite],
    data: &GameData,
    map_w: u32,
    map_h: u32,
    options: &RenderOptions,
) -> Vec<AssetRequest> {
    if !options.unit_art_available() {
        return Vec::new();
    }

    let (_, _, ppt) = resolve_tier(options, map_w, map_h);
    let (tier, pack) = resolve_unit_tier(options, ppt);

    // The tileset only selects the geyser's *frame*, never which image is loaded, so any value
    // yields the same asset list.
    let drawables = collect_drawables(units, sprites, data, options, Tileset::Badlands);

    let mut seen = Vec::new();
    for drawable in &drawables {
        let req = anim_request(drawable.art_image_id, tier, pack);
        if !seen.contains(&req) {
            seen.push(req);
        }
    }
    seen
}

// ---------------------------------------------------------------------------------------------
// Drawable collection and filtering
// ---------------------------------------------------------------------------------------------

/// One thing to draw: a resolved image, the frame of it to use, and where it goes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Drawable {
    /// The image ID whose `.anim` file holds the art (post-`images.rel`-redirect).
    art_image_id: u16,
    /// The frame index selected from game rules (facing, resource amount, tileset). Clamped to
    /// the `.anim`'s real frame count later, once it's parsed.
    frame: usize,
    /// Whether the frame is mirrored horizontally (the upper half of BW's 32 facings).
    flip: bool,
    /// Position in logical pixels.
    x: i32,
    y: i32,
    /// Owning player slot, used for team color.
    owner: u8,
    /// Painter's-order band: 0 for the map's own contents, 1 for start-location graphics, which
    /// always sit on top.
    layer: u8,
    /// Whether this is a shadow underlay rather than the drawable it belongs to (see
    /// [`push_with_shadow`]). Shadows are never team-colored and are drawn as a translucent black
    /// silhouette (see [`apply_shadow_tint`]).
    is_shadow: bool,
}

/// Resolves and filters the map's units and sprites into an ordered list of drawables.
///
/// Ordering is BW-style painter's order: ascending y, then ascending x, then original order, so
/// things lower on the map overlap things above them. Start-location graphics are pushed to a
/// separate band that draws over everything.
fn collect_drawables(
    units: &[PlacedUnit],
    sprites: &[Sprite],
    data: &GameData,
    options: &RenderOptions,
    tileset: Tileset,
) -> Vec<Drawable> {
    let mut drawables: Vec<Drawable> = Vec::new();

    // Melee start-area clearing: at melee start BW destroys any construct within the bounds of
    // the HQ building it spawns at each start location (neobrood's `init_melee_game` documents
    // the same rule). The box is the HQ's *collision bounds* from units.dat — NOT its placebox,
    // which is the (larger) placement-grid footprint. The HQ depends on the spawning player's
    // race, which a preview can't know; see `hq_clear_bounds` for why the smallest HQ's box is
    // the race-neutral choice.
    let start_clear_rects: Vec<(i32, i32, i32, i32)> = if options.unit_filter == UnitFilter::Melee {
        let (l, t, r, b) = hq_clear_bounds(data);
        units
            .iter()
            .filter(|u| u.unit_id == UNIT_ID_START_LOCATION)
            .map(|u| {
                let (x, y) = (u.x as i32, u.y as i32);
                (x - l, y - t, x + r, y + b)
            })
            .collect()
    } else {
        Vec::new()
    };

    for unit in units {
        if unit.unit_id == UNIT_ID_START_LOCATION {
            // Handled by the start-location path, not as an ordinary unit.
            if options.start_locations == StartLocations::Sprite {
                drawables.push(Drawable {
                    art_image_id: data.resolve_art(IMAGE_ID_START_LOCATION),
                    frame: 0,
                    flip: false,
                    x: unit.x as i32,
                    y: unit.y as i32,
                    owner: unit.owner.unwrap_or(u8::MAX),
                    layer: 1,
                    is_shadow: false,
                });
            }
            continue;
        }
        if !unit_passes_filters(unit, data, options) {
            continue;
        }
        if overlaps_any_start_area(unit, data, &start_clear_rects) {
            continue;
        }
        let Some(image_id) = data.unit_image_pre_redirect(unit.unit_id) else {
            continue;
        };
        let (frame, flip) = select_unit_frame(unit, image_id, data, tileset);
        push_with_shadow(
            &mut drawables,
            data,
            options,
            image_id,
            Some(unit.unit_id),
            Drawable {
                art_image_id: data.resolve_art(image_id),
                frame,
                flip,
                x: unit.x as i32,
                y: unit.y as i32,
                owner: unit.owner.unwrap_or(u8::MAX),
                layer: 0,
                is_shadow: false,
            },
        );
    }

    for sprite in sprites {
        if sprite.is_disabled() {
            continue;
        }
        let is_doodad = sprite.flags.contains(SpriteFlags::DRAW_AS_SPRITE);
        if is_doodad && !options.show_doodad_sprites {
            continue;
        }
        // THG2's ID field means different things depending on the flag: a "pure sprite" entry is
        // a sprites.dat ID (map doodads), while a "unit sprite" entry is a unit ID, taking the
        // full units.dat chain. This matches BW (and bw-chk's `_parseUnits`, which sorts THG2
        // entries into its sprite and unit lists on exactly this bit).
        let image_id = if is_doodad {
            data.sprite_image_pre_redirect(sprite.id)
        } else {
            // THG2 sprites carry a plain `u8` owner (never "unrecorded"), so neutral-ness is
            // just "not one of the 8 real player slots" — the same rule as a placed unit's
            // `owner == None || owner >= 8`.
            let is_neutral = sprite.owner as usize >= 8;
            if !unit_id_passes_class_filters(sprite.id, is_neutral, data, options) {
                continue;
            }
            data.unit_image_pre_redirect(sprite.id)
        };
        let Some(image_id) = image_id else {
            continue;
        };
        // Doodads (pure sprites) have no unit ID, so they only ever qualify for the same-GRP
        // skip rule in `shadow_image_pre_redirect`; "unit sprite" THG2 entries carry a real unit
        // ID (their own `id` field, per the chain above) and so also get the subunit rule.
        let unit_id_for_shadow = (!is_doodad).then_some(sprite.id);
        push_with_shadow(
            &mut drawables,
            data,
            options,
            image_id,
            unit_id_for_shadow,
            Drawable {
                art_image_id: data.resolve_art(image_id),
                // THG2 entries carry no facing or resource data, so they always use frame 0 (the
                // same thing bw-chk draws for them).
                frame: 0,
                flip: false,
                x: sprite.x as i32,
                y: sprite.y as i32,
                owner: sprite.owner,
                layer: 0,
                is_shadow: false,
            },
        );
    }

    drawables.sort_by_key(|d| (d.layer, d.y, d.x));
    drawables
}

/// Pushes `owner_drawable`, first pushing its shadow underlay (if [`RenderOptions::show_shadows`]
/// is on and [`GameData::shadow_image_pre_redirect`]'s bounded scan from `pre_redirect_image_id`
/// finds one) immediately before it. `unit_id` is the drawable's own unit ID where one exists
/// (placed units and THG2 unit-sprites; `None` for THG2 doodad sprites), enabling the scan's
/// subunit-skip rule — see that function's docs.
///
/// Ordering note: this is genuine per-drawable ordering, not a global "shadows first" pass. The
/// shadow is given the exact same `(layer, y, x)` as its owner, and [`Vec::sort_by_key`] (used by
/// [`collect_drawables`]) is a stable sort, so pushing the shadow immediately before its owner
/// keeps it immediately before the owner after sorting too — i.e. painted directly beneath it —
/// without needing a separate shadows-first band. Start-location graphics never call this (they
/// build their `Drawable` directly), so they never get a shadow.
fn push_with_shadow(
    drawables: &mut Vec<Drawable>,
    data: &GameData,
    options: &RenderOptions,
    pre_redirect_image_id: u16,
    unit_id: Option<u16>,
    owner_drawable: Drawable,
) {
    if options.show_shadows
        && let Some(shadow_pre_redirect) =
            data.shadow_image_pre_redirect(pre_redirect_image_id, unit_id)
    {
        drawables.push(Drawable {
            art_image_id: data.resolve_art(shadow_pre_redirect),
            is_shadow: true,
            ..owner_drawable
        });
    }
    drawables.push(owner_drawable);
}

/// Whether a placed unit survives `options`' filters (start locations excluded — they're handled
/// separately and are never dropped by the melee filter).
fn unit_passes_filters(unit: &PlacedUnit, data: &GameData, options: &RenderOptions) -> bool {
    // Units preplaced in the hallucinated state only exist for a moment in a real game (they
    // appear and then quickly expire) — mappers use hallucinated units/minerals to push
    // starting workers into position. Dropped regardless of the melee/as-placed filter.
    if unit.state.contains(UnitState::HALLUCINATED) {
        return false;
    }
    // Neutral-ness for the class filters below: no recorded owner, or a slot past the 8 real
    // players (rescuable/neutral/critter slots).
    let is_neutral = !matches!(unit.owner, Some(owner) if (owner as usize) < 8);
    // Melee: the game deletes preplaced units belonging to a real player slot and gives that
    // player starting workers instead. Neutral-owned things (resources, critters, neutral
    // buildings, eggs) survive, and so does everything with no owner recorded.
    if options.unit_filter == UnitFilter::Melee && !is_neutral {
        return false;
    }
    unit_id_passes_class_filters(unit.unit_id, is_neutral, data, options)
}

/// The unit-class toggles (critters, resources, neutral buildings), shared by `UNIT` entries and
/// THG2 unit sprites. `is_neutral` is the caller's own owner check (a placed unit's `Option<u8>`
/// owner and a THG2 sprite's plain `u8` owner mean "neutral" slightly differently, so it's
/// resolved by the caller rather than here).
fn unit_id_passes_class_filters(
    unit_id: u16,
    is_neutral: bool,
    data: &GameData,
    options: &RenderOptions,
) -> bool {
    if !options.show_critters && is_critter(unit_id) {
        return false;
    }
    if !options.show_resources && is_resource(unit_id) {
        return false;
    }
    // Resources are exempt even if a corrupted units.dat somehow flagged one as a Building: they
    // have their own dedicated toggle above, and this one must never interact with it.
    if !options.show_neutral_buildings
        && is_neutral
        && !is_resource(unit_id)
        && data.is_building(unit_id)
    {
        return false;
    }
    true
}

/// Picks the frame (and horizontal flip) for a placed unit.
///
/// Three rules, in the order BW's iscripts effectively apply them:
///
/// - Mineral fields show how much is left: four frames, thresholded on the placed amount.
/// - A vespene geyser's frame is the map's tileset (its art holds one geyser per tileset).
/// - Otherwise, images flagged as having directional frames map the unit type's facing (0-31,
///   clockwise from north) onto frames 0-16, mirroring the west-facing half.
///
/// The mineral/geyser rules follow `bw-chk`'s conventions (BW itself sets these frames from
/// iscript animations rather than hardcoding them).
fn select_unit_frame(
    unit: &PlacedUnit,
    image_id: u16,
    data: &GameData,
    tileset: Tileset,
) -> (usize, bool) {
    if UNIT_IDS_MINERAL_FIELD.contains(&unit.unit_id) {
        return (mineral_frame(unit.resource_amount), false);
    }
    if unit.unit_id == UNIT_ID_VESPENE_GEYSER {
        return (tileset as usize, false);
    }

    let directional = data
        .images_dat()
        .entry(image_id)
        .is_some_and(|entry| entry.has_directional_frames);
    if !directional {
        return (0, false);
    }

    let direction = data
        .units_dat()
        .entry(unit.unit_id)
        .map(|entry| entry.unit_direction)
        .unwrap_or(0);
    let direction = if direction >= RANDOM_DIRECTION {
        // Deterministic stand-in for BW's random facing, so repeat renders are identical.
        (unit.instance_id.0.wrapping_mul(FACING_HASH_MULTIPLIER) % 32) as u8
    } else {
        direction
    };

    direction_frame(direction)
}

/// Maps one of BW's 32 facings to a frame index and whether it's mirrored: frames 0-16 cover
/// north through south going clockwise, and the western half (17-31) reuses them flipped.
fn direction_frame(direction: u8) -> (usize, bool) {
    if direction <= 16 {
        (direction as usize, false)
    } else {
        (32 - direction as usize, true)
    }
}

/// Mineral-field frame from the amount of minerals left (`bw-chk`'s thresholds). A field with no
/// recorded amount uses the fullest frame.
fn mineral_frame(resource_amount: Option<u32>) -> usize {
    match resource_amount.unwrap_or(u32::MAX) {
        750.. => 0,
        500..750 => 1,
        250..500 => 2,
        _ => 3,
    }
}

/// The RGB a drawable's team color resolves to: the player's own color for the 8 real slots,
/// SC:R's neutral cyan for anything else (neutral, rescuable, unowned).
fn owner_color(owner: u8, player_colors: &PlayerColors) -> [u8; 3] {
    match player_colors.colors.get(owner as usize) {
        Some(color) => resolve_color(color, owner),
        None => NEUTRAL_COLOR,
    }
}

// ---------------------------------------------------------------------------------------------
// Drawing
// ---------------------------------------------------------------------------------------------

/// A decoded `.anim` layer atlas.
struct DecodedLayer {
    rgba: Vec<u8>,
    width: u32,
    height: u32,
}

/// A frame cropped out of an atlas, team-colored, and scaled to output resolution.
struct Tile {
    rgba: Vec<u8>,
    width: u32,
    height: u32,
}

impl Tile {
    /// Approximate retained size, for the tile-cache byte budget.
    fn byte_size(&self) -> usize {
        self.rgba.len()
    }
}

/// Identifies a distinct output tile: the same image/frame drawn for two players needs two
/// tiles, but the same image/frame/color drawn a hundred times needs only one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct TileKey {
    art_image_id: u16,
    frame: usize,
    flip: bool,
    color: [u8; 3],
    /// Whether this tile is a shadow underlay. Shadows are colorless (see
    /// [`apply_shadow_tint`]), so their `color` is always [`NO_TEAMCOLOR_SENTINEL`] regardless of
    /// the owning drawable's real color — one cached tile per shadow image/frame/flip, no matter
    /// how many differently-colored owners share it. Carried as its own field (rather than
    /// relying solely on the sentinel color) so a shadow tile can never alias a same-keyed
    /// non-shadow tile of the same `art_image_id`/frame/flip, which is compositied completely
    /// differently (see [`build_tile`]).
    is_shadow: bool,
}

/// The color placeholder used in a [`TileKey`] when the art has no `teamcolor` layer at all.
///
/// Without a mask layer, [`apply_team_color`] never runs, so the decoded tile is byte-identical
/// regardless of the drawable's owner color — keying on this fixed sentinel instead of the real
/// color lets every owner drawing the same image/frame/flip share one cached tile instead of one
/// copy per distinct owner color (e.g. 8 players' worth of an unowned-color-agnostic doodad).
const NO_TEAMCOLOR_SENTINEL: [u8; 3] = [0, 0, 0];

/// The color a [`TileKey`] should carry: the drawable's real owner color when the art has a
/// `teamcolor` layer (it genuinely affects the pixels), or [`NO_TEAMCOLOR_SENTINEL`] when it
/// doesn't (so unrelated owner colors alias to the same cached tile).
fn tile_key_color(color: [u8; 3], has_teamcolor: bool) -> [u8; 3] {
    if has_teamcolor {
        color
    } else {
        NO_TEAMCOLOR_SENTINEL
    }
}

/// Either a tile retained in the shared cache (looked up by key at blit time) or one built but
/// not retained (the cache was already at its byte budget) — carried by value so painter order
/// can still be respected without re-fetching a not-cached tile.
enum TileRef {
    Cached(TileKey),
    Owned(Tile),
}

/// A drawable that survived asset loading: which tile to blit and where.
struct Placement {
    tile: TileRef,
    x: i32,
    y: i32,
}

/// Upper bound on how many bytes of decoded tiles [`draw_overlay`] keeps alive at once, matching
/// `crate::terrain`'s megatile-cache precedent. A busy map with many distinct image/frame/color
/// combinations could otherwise retain one tile per combination for the whole render; past the
/// budget, a tile is still built and used for the drawable that needed it, just not kept for
/// later reuse (so a repeat of the same key rebuilds instead of caching — bounded memory at the
/// cost of some redundant work, which is only ever reached by pathological inputs).
const MAX_OVERLAY_TILE_CACHE_BYTES: usize = 64 * 1024 * 1024;

/// Generous upper bound (logical px) on how far a real `.anim` frame's art could extend past its
/// placed position, used only to cull drawables that are unambiguously off the rendered image
/// *before* their `.anim` is ever fetched. Real SC:R sprites (even the largest buildings) are
/// well under this; it's several times larger so a legitimate frame is never wrongly culled,
/// while a unit/sprite placed far outside a (typically much smaller) map — a shape corrupted or
/// hostile CHK position data can take — skips its art fetch entirely instead of paying for one
/// that can only ever land off-screen.
const MAX_PLAUSIBLE_SPRITE_HALF_EXTENT_LOGICAL_PX: f32 = 512.0;

/// Conservative, position-only visibility test applied before a drawable's `.anim` is fetched:
/// `false` only when the drawable's placed position is so far outside the image that no frame,
/// however large a real one could plausibly be, could reach back onto it.
fn could_possibly_be_visible(x: i32, y: i32, image_w: u32, image_h: u32, zoom: f32) -> bool {
    let margin = (MAX_PLAUSIBLE_SPRITE_HALF_EXTENT_LOGICAL_PX * zoom).max(1.0);
    let (x, y) = (x as f32 * zoom, y as f32 * zoom);
    x + margin >= 0.0
        && x - margin <= image_w as f32
        && y + margin >= 0.0
        && y - margin <= image_h as f32
}

/// Whether an output-space rect `[x, x + w) x [y, y + h)` intersects the image bounds at all.
fn rect_intersects_image(x: i32, y: i32, w: u32, h: u32, image_w: u32, image_h: u32) -> bool {
    if w == 0 || h == 0 {
        return false;
    }
    let x1 = x.saturating_add(w as i32);
    let y1 = y.saturating_add(h as i32);
    x < image_w as i32 && y < image_h as i32 && x1 > 0 && y1 > 0
}

/// Fetches art for every distinct image among `drawables`, builds the output tiles, and blits
/// them onto `image` in `drawables`' (already painter-sorted) order.
///
/// Art is loaded one image at a time and its atlases dropped before moving on, so peak memory is
/// one decoded atlas plus the (small, output-resolution) tiles — never every atlas at once. A
/// drawable unambiguously off the output image never triggers its `.anim`'s fetch at all (see
/// [`could_possibly_be_visible`]), and one whose exact placement rect clips entirely off the
/// image is dropped before its tile is built (see [`rect_intersects_image`]).
#[allow(clippy::too_many_arguments)]
fn draw_overlay(
    image: &mut RgbaImage,
    drawables: &[Drawable],
    player_colors: &PlayerColors,
    source: &dyn TilesetDataSource,
    tier: AssetTier,
    pack: ArtPack,
    zoom: f32,
) {
    let (image_w, image_h) = (image.width, image.height);

    // Group drawable indices by image so each `.anim` is fetched, parsed and decoded once. A
    // drawable that's definitely off-image (by position alone, before any art is known) never
    // even enters this map, so its image is never fetched if nothing else needs it.
    let mut by_image: HashMap<u16, Vec<usize>> = HashMap::new();
    for (i, drawable) in drawables.iter().enumerate() {
        if !could_possibly_be_visible(drawable.x, drawable.y, image_w, image_h, zoom) {
            continue;
        }
        by_image.entry(drawable.art_image_id).or_default().push(i);
    }

    let mut tiles: HashMap<TileKey, Tile> = HashMap::new();
    let mut cache_bytes: usize = 0;
    let mut placements: Vec<Option<Placement>> = (0..drawables.len()).map(|_| None).collect();

    for (art_image_id, indices) in by_image {
        let Ok(bytes) = source.read(&anim_request(art_image_id, tier, pack)) else {
            continue;
        };
        let Ok(anim) = Anim::parse(bytes.as_ref()) else {
            continue;
        };
        let Some(diffuse) = anim.layer("diffuse").and_then(decode_layer) else {
            continue;
        };
        let teamcolor = anim.layer("teamcolor").and_then(decode_layer);
        let has_teamcolor = teamcolor.is_some();

        let (canvas_w, canvas_h) = anim.canvas_size();
        let frame_count = anim.frame_count();
        if frame_count == 0 {
            continue;
        }
        let texel_scale = (anim.scale() as f32).max(1.0);

        for &i in &indices {
            let drawable = &drawables[i];
            let frame_index = drawable.frame.min(frame_count - 1);
            let Some(frame) = anim.frame(frame_index).copied() else {
                continue;
            };
            let Some(rect) = anim.frame_texel_rect(frame_index) else {
                continue;
            };
            let (_, _, rect_w, rect_h) = rect;

            let canvas_w = effective_canvas(canvas_w, frame.offset_x, frame.width);
            let canvas_h = effective_canvas(canvas_h, frame.offset_y, frame.height);
            let x = frame_origin(
                drawable.x,
                frame_offset_x(&frame, canvas_w, drawable.flip),
                canvas_w,
                zoom,
            );
            let y = frame_origin(drawable.y, frame.offset_y as i32, canvas_h, zoom);

            // The tile's eventual size, estimated exactly the way `build_tile` computes it (no
            // allocation involved) — worth checking before cropping/team-coloring/scaling a
            // frame that can only ever land off the output image.
            let (est_w, est_h) = tile_output_dims(rect_w, rect_h, zoom, texel_scale);
            if !rect_intersects_image(x, y, est_w, est_h, image_w, image_h) {
                continue;
            }

            let color = owner_color(drawable.owner, player_colors);
            let key = TileKey {
                art_image_id,
                frame: frame_index,
                flip: drawable.flip,
                // Shadows are colorless (see `apply_shadow_tint`): always the no-teamcolor
                // sentinel, regardless of the owning drawable's real color or whether this art
                // even has a `teamcolor` layer, so every owner sharing a shadow image/frame/flip
                // shares one cached tile.
                color: if drawable.is_shadow {
                    NO_TEAMCOLOR_SENTINEL
                } else {
                    tile_key_color(color, has_teamcolor)
                },
                is_shadow: drawable.is_shadow,
            };

            let tile_ref = match tiles.entry(key) {
                std::collections::hash_map::Entry::Occupied(_) => TileRef::Cached(key),
                std::collections::hash_map::Entry::Vacant(entry) => {
                    let Some(tile) = build_tile(
                        &diffuse,
                        teamcolor.as_ref(),
                        rect,
                        color,
                        drawable.flip,
                        zoom,
                        texel_scale,
                        drawable.is_shadow,
                    ) else {
                        continue;
                    };
                    if cache_bytes + tile.byte_size() <= MAX_OVERLAY_TILE_CACHE_BYTES {
                        cache_bytes += tile.byte_size();
                        entry.insert(tile);
                        TileRef::Cached(key)
                    } else {
                        TileRef::Owned(tile)
                    }
                }
            };

            placements[i] = Some(Placement {
                tile: tile_ref,
                x,
                y,
            });
        }
    }

    for placement in placements.into_iter().flatten() {
        let tile = match &placement.tile {
            TileRef::Cached(key) => tiles.get(key),
            TileRef::Owned(tile) => Some(tile),
        };
        if let Some(tile) = tile {
            blend_over(
                image,
                &tile.rgba,
                tile.width,
                tile.height,
                placement.x,
                placement.y,
            );
        }
    }
}

/// The canvas extent to centre a frame in, working around `.anim` files that declare a zero
/// canvas.
///
/// 10 of the 868 HD2 anims in a real install (all editor-only graphics — including 588, the
/// start-location marker) have a `0x0` canvas in their frame-table header. Taking that
/// literally would place the frame's *top-left* at the unit's position instead of its centre,
/// which visibly slides the start-location ring about two tiles down and right. Falling back to
/// the frame's own extent (`2 * offset + size`) centres the frame on the position, which is what
/// every editor draws and what the non-degenerate files' canvases do anyway.
fn effective_canvas(canvas: u16, offset: i16, size: u16) -> u16 {
    if canvas != 0 {
        return canvas;
    }
    (2 * offset as i32 + size as i32).clamp(0, u16::MAX as i32) as u16
}

/// A frame's horizontal offset within its canvas (4K units), mirrored about the canvas centre
/// when the frame is drawn flipped — otherwise a mirrored sprite would slide sideways by the
/// difference between its own offset and its mirror's.
fn frame_offset_x(frame: &AnimFrame, canvas_w: u16, flip: bool) -> i32 {
    if flip {
        canvas_w as i32 - frame.offset_x as i32 - frame.width as i32
    } else {
        frame.offset_x as i32
    }
}

/// Converts a drawable's logical position plus a frame offset (4K units) into an output pixel
/// coordinate. The frame's canvas is centred on the drawable's position, and the frame sits at
/// `offset` within that canvas.
fn frame_origin(pos_logical: i32, offset_units: i32, canvas_units: u16, zoom: f32) -> i32 {
    let canvas_units = canvas_units as f32;
    let from_canvas_centre = offset_units as f32 - canvas_units / 2.0;
    (pos_logical as f32 * zoom + from_canvas_centre * zoom / UNITS_PER_LOGICAL_PX).round() as i32
}

/// Decodes an `.anim` layer's embedded texture to RGBA8. `None` for anything we can't decode
/// (an unrecognized DDS format, a PNG payload, a zero-sized or unparseable texture) — the
/// drawable is skipped rather than the render failing.
///
/// PNG support is deliberately deferred: measured against a real SC:R install, the entire HD2
/// `.anim` corpus is BC1/BC3 DDS (868/868 diffuse layers BC3, 155/155 teamcolor layers BC1), so
/// there's no real art this crate needs today that a PNG payload would unlock. `parse_dds`
/// simply fails its magic check on a PNG blob, which this treats the same as any other
/// undecodable layer — add real decoding here if/when a real asset actually needs it.
fn decode_layer(layer: &AnimLayer<'_>) -> Option<DecodedLayer> {
    let dds = parse_dds(layer.data).ok()?;
    let width = dds.width.min(MAX_ANIM_TEXTURE_DIM);
    let height = dds.height.min(MAX_ANIM_TEXTURE_DIM);
    if width == 0 || height == 0 {
        return None;
    }

    let rgba = match dds.format {
        DdsFormat::Bc1 => decode_bc1(dds.payload, width, height),
        DdsFormat::Bc3 => decode_bc3(dds.payload, width, height),
        DdsFormat::Rgba8 => {
            let needed = width as usize * height as usize * 4;
            let mut buf = vec![0u8; needed];
            let copy_len = needed.min(dds.payload.len());
            buf[..copy_len].copy_from_slice(&dds.payload[..copy_len]);
            buf
        }
        _ => return None,
    };

    Some(DecodedLayer {
        rgba,
        width,
        height,
    })
}

/// The length of a 1-D span `[start, start + len)` after clipping it to `[0, bound)`. Used to
/// intersect an untrusted rect with a decoded atlas's real dimensions using only checked/
/// saturating arithmetic — no subtraction that could underflow, no product computed before the
/// clip.
fn clipped_span_len(start: u32, len: u32, bound: u32) -> u32 {
    if start >= bound {
        return 0;
    }
    len.min(bound - start)
}

/// The alpha multiplier applied to a shadow's diffuse frame after its RGB is zeroed (see
/// [`apply_shadow_tint`]) — BW's classic shadow draw-function semantics: a flat, translucent
/// black silhouette, never team-colored. Calibrated visually against real renders (Lost Temple's
/// mineral lines/trees, and `Impossible_Scen._Future.scx`'s dense units) within the brief's
/// suggested 0.4-0.6 range — see the "Shadows" note in `docs/render-design.md`.
const SHADOW_ALPHA_SCALE: f32 = 0.5;

/// Recolors a cropped diffuse frame into a translucent black shadow silhouette: RGB is zeroed
/// (BW always draws shadows as flat black, never team-colored — there is no shadow-specific
/// `teamcolor` layer to apply) and alpha is scaled by [`SHADOW_ALPHA_SCALE`], so the diffuse
/// frame's own alpha channel still provides the silhouette's shape.
fn apply_shadow_tint(frame: &mut [u8]) {
    for texel in frame.chunks_exact_mut(4) {
        texel[0] = 0;
        texel[1] = 0;
        texel[2] = 0;
        texel[3] = (texel[3] as f32 * SHADOW_ALPHA_SCALE).round() as u8;
    }
}

/// Crops `rect` out of the diffuse atlas, applies the team color mask (or, for a shadow tile, the
/// black/alpha-scaled shadow tint — see [`apply_shadow_tint`] — instead), downscales to output
/// resolution and mirrors it if needed.
///
/// `rect` is untrusted: it's derived from a frame's `u16` width/height (see
/// [`broodmap_formats::Anim::frame_texel_rect`]), so a hostile/corrupted `.anim` can claim a rect
/// far larger than the atlas actually decoded (up to ~65535 texels per axis, ~17 GiB of RGBA if
/// taken at face value). `rect` is intersected with `diffuse`'s real dimensions *before* any
/// allocation happens — the atlas itself is already capped at [`MAX_ANIM_TEXTURE_DIM`], so the
/// intersection is always bounded by that regardless of what `rect` claims.
#[allow(clippy::too_many_arguments)]
fn build_tile(
    diffuse: &DecodedLayer,
    teamcolor: Option<&DecodedLayer>,
    rect: (u32, u32, u32, u32),
    color: [u8; 3],
    flip: bool,
    zoom: f32,
    texel_scale: f32,
    is_shadow: bool,
) -> Option<Tile> {
    let (rect_x, rect_y, rect_w, rect_h) = rect;
    let rect_w = clipped_span_len(rect_x, rect_w, diffuse.width);
    let rect_h = clipped_span_len(rect_y, rect_h, diffuse.height);
    if rect_w == 0 || rect_h == 0 {
        return None;
    }

    let mut cropped = crop(diffuse, rect_x, rect_y, rect_w, rect_h);
    if is_shadow {
        apply_shadow_tint(&mut cropped);
    } else if let Some(teamcolor) = teamcolor {
        let mask = crop(teamcolor, rect_x, rect_y, rect_w, rect_h);
        apply_team_color(&mut cropped, &mask, color);
    }

    let (out_w, out_h) = tile_output_dims(rect_w, rect_h, zoom, texel_scale);
    let mut rgba = scale_rgba_premultiplied(&cropped, rect_w, rect_h, out_w, out_h);
    if rgba.len() < out_w as usize * out_h as usize * 4 {
        return None;
    }

    if flip {
        flip_horizontal(&mut rgba, out_w, out_h);
    }

    Some(Tile {
        rgba,
        width: out_w,
        height: out_h,
    })
}

/// The output pixel size a `rect_w`x`rect_h` texel crop scales to: `zoom` output px per logical
/// px, `texel_scale` texels per logical px. Never upscales past the source (nothing is gained and
/// it costs memory), and never past [`MAX_TILE_OUTPUT_DIM`] regardless of what `zoom`/`texel_scale`
/// would otherwise compute (belt and suspenders — see that constant's docs).
///
/// Used both to size the tile [`build_tile`] actually allocates and, before that, to conservatively
/// estimate a not-yet-built tile's footprint for the off-image cull in [`draw_overlay`] — the two
/// must agree, or the cull could skip a tile that would've landed on the image.
fn tile_output_dims(rect_w: u32, rect_h: u32, zoom: f32, texel_scale: f32) -> (u32, u32) {
    let out_w = ((rect_w as f32 * zoom / texel_scale).round() as u32)
        .clamp(1, rect_w.max(1))
        .min(MAX_TILE_OUTPUT_DIM);
    let out_h = ((rect_h as f32 * zoom / texel_scale).round() as u32)
        .clamp(1, rect_h.max(1))
        .min(MAX_TILE_OUTPUT_DIM);
    (out_w, out_h)
}

/// Copies a `w`x`h` rect out of a decoded atlas. Texels outside the atlas read as transparent
/// black — `.anim` frame rects are ceiled from 4K units, so they can legitimately overhang an
/// atlas edge by a texel.
fn crop(layer: &DecodedLayer, x: u32, y: u32, w: u32, h: u32) -> Vec<u8> {
    let mut out = vec![0u8; w as usize * h as usize * 4];
    for row in 0..h {
        let sy = y + row;
        if sy >= layer.height {
            break;
        }
        let copy_w = w.min(layer.width.saturating_sub(x));
        if copy_w == 0 {
            break;
        }
        let src_start = (sy as usize * layer.width as usize + x as usize) * 4;
        let dst_start = row as usize * w as usize * 4;
        let len = copy_w as usize * 4;
        if let (Some(src), Some(dst)) = (
            layer.rgba.get(src_start..src_start + len),
            out.get_mut(dst_start..dst_start + len),
        ) {
            dst.copy_from_slice(src);
        }
    }
    out
}

/// Tints the team-colored regions of a decoded frame.
///
/// SC:R replaces the classic palette-remap trick with a `teamcolor` mask layer: the diffuse art
/// paints those regions as bright, desaturated shading detail, and the mask says how much of the
/// player's color to multiply in. So the blend is a per-channel lerp between "leave the diffuse
/// alone" and "multiply the diffuse by the player color":
///
/// ```text
/// out = diffuse * ((255 - m) + m * player / 255) / 255
/// ```
///
/// # Why multiply
///
/// There's no reference implementation to copy (neobrood parses the layer but never uses it), so
/// this was pinned by rendering a dense player-owned map and comparing candidates against how
/// SC:R itself draws:
///
/// - **Multiply (this)**: team panels keep the art's highlights and shadows — Terran hulls read
///   as painted metal, and a start-location marker (whose diffuse is near-white under a solid
///   mask) comes out as the saturated player color, exactly as in game.
/// - **Replace / `lerp(diffuse, player, m)`**: masked regions collapse into flat, shadeless
///   blobs of pure player color. Visibly wrong at a glance.
/// - **Additive (`diffuse + m * player`)**: blows the masked regions out toward white.
fn apply_team_color(frame: &mut [u8], mask: &[u8], color: [u8; 3]) {
    for (texel, mask_texel) in frame.chunks_exact_mut(4).zip(mask.chunks_exact(4)) {
        let m = team_color_mask(mask_texel) as u32;
        if m == 0 {
            continue;
        }
        for (channel, &player) in texel[..3].iter_mut().zip(color.iter()) {
            let tint = (255 - m) * 255 + m * player as u32;
            *channel = ((*channel as u32 * tint) / (255 * 255)).min(255) as u8;
        }
    }
}

/// How much of the player color a `teamcolor` texel calls for.
///
/// Measured against a real install: `teamcolor` layers are **BC1**, not BC3, so they carry no
/// meaningful alpha at all — every texel decodes opaque. The coverage lives in the color
/// channels, written as grayscale (across every image sampled, `r == g == b` for 100% of texels).
/// Reading alpha here would tint entire sprites instead of just their team-colored panels.
fn team_color_mask(texel: &[u8]) -> u8 {
    texel[0]
}

/// Mirrors an RGBA8 buffer horizontally, in place.
fn flip_horizontal(rgba: &mut [u8], width: u32, height: u32) {
    let row_len = width as usize * 4;
    for row in 0..height as usize {
        let start = row * row_len;
        let Some(row_bytes) = rgba.get_mut(start..start + row_len) else {
            break;
        };
        for x in 0..(width as usize / 2) {
            let (left, right) = (x * 4, (width as usize - 1 - x) * 4);
            for c in 0..4 {
                row_bytes.swap(left + c, right + c);
            }
        }
    }
}

/// Composites a straight-alpha RGBA8 tile over `image` at `(dst_x, dst_y)`, clipped to the
/// image's bounds (sprites routinely hang off the edges of a map).
pub(crate) fn blend_over(
    image: &mut RgbaImage,
    src: &[u8],
    src_w: u32,
    src_h: u32,
    dst_x: i32,
    dst_y: i32,
) {
    if src_w == 0 || src_h == 0 {
        return;
    }
    let image_w = image.width as i32;
    let image_h = image.height as i32;

    for row in 0..src_h as i32 {
        let y = dst_y + row;
        if y < 0 || y >= image_h {
            continue;
        }
        for col in 0..src_w as i32 {
            let x = dst_x + col;
            if x < 0 || x >= image_w {
                continue;
            }

            let src_offset = (row as usize * src_w as usize + col as usize) * 4;
            let Some(s) = src.get(src_offset..src_offset + 4) else {
                continue;
            };
            let sa = s[3] as u32;
            if sa == 0 {
                continue;
            }

            let dst_offset = (y as usize * image.width as usize + x as usize) * 4;
            let Some(d) = image.data.get_mut(dst_offset..dst_offset + 4) else {
                continue;
            };

            let da = d[3] as u32;
            let inv = 255 - sa;
            // Both terms are scaled by 255 so the division is exact for the common opaque-
            // destination case (out_a == 255*255).
            let out_a = sa * 255 + da * inv;
            if out_a == 0 {
                continue;
            }
            for c in 0..3 {
                let value = s[c] as u32 * sa * 255 + d[c] as u32 * da * inv;
                d[c] = (value / out_a).min(255) as u8;
            }
            d[3] = (out_a / 255).min(255) as u8;
        }
    }
}

/// Unit IDs of the three melee HQ buildings (Terran Command Center, Zerg Hatchery, Protoss
/// Nexus), used to derive the melee start-area clearing box.
const HQ_UNIT_IDS: [u16; 3] = [106, 131, 154];

/// Fallback for [`hq_clear_bounds`] when units.dat is absent/degenerate: approximately the
/// smallest HQ's collision bounds (left, top, right, bottom extents from the unit's position).
/// Only reachable when the .dat tables couldn't be loaded at all, so precision isn't critical.
const FALLBACK_HQ_BOUNDS: (i32, i32, i32, i32) = (49, 32, 49, 32);

/// The intersection of the three HQ buildings' `units.dat` collision bounds (per-side minima),
/// as positive extents from a start location's position. Collision bounds — not the placebox,
/// which is the larger placement-grid footprint and would over-clear.
///
/// The *smallest* box is the right preview semantics: a unit overlapping it is destroyed at
/// game start no matter which race spawns there, and mapmakers keep race-dependent placement
/// out of the in-between zone precisely so the map behaves the same for everyone.
fn hq_clear_bounds(data: &GameData) -> (i32, i32, i32, i32) {
    let mut smallest: Option<(i32, i32, i32, i32)> = None;
    for id in HQ_UNIT_IDS {
        let Some(entry) = data.units_dat().entry(id) else {
            continue;
        };
        let (l, t, r, b) = entry.bounds;
        let (l, t, r, b) = (
            l.max(0) as i32,
            t.max(0) as i32,
            r.max(0) as i32,
            b.max(0) as i32,
        );
        if l + t + r + b == 0 {
            continue;
        }
        smallest = Some(match smallest {
            None => (l, t, r, b),
            Some((ul, ut, ur, ub)) => (ul.min(l), ut.min(t), ur.min(r), ub.min(b)),
        });
    }
    smallest.unwrap_or(FALLBACK_HQ_BOUNDS)
}

/// Whether a placed unit's own collision box (its `units.dat` bounds around its position — a
/// bare point when the table has no entry for it) overlaps any melee start-area clearing rect.
fn overlaps_any_start_area(
    unit: &PlacedUnit,
    data: &GameData,
    clear_rects: &[(i32, i32, i32, i32)],
) -> bool {
    if clear_rects.is_empty() {
        return false;
    }
    let (l, t, r, b) = data
        .units_dat()
        .entry(unit.unit_id)
        .map(|e| {
            let (l, t, r, b) = e.bounds;
            (
                l.max(0) as i32,
                t.max(0) as i32,
                r.max(0) as i32,
                b.max(0) as i32,
            )
        })
        .unwrap_or((0, 0, 0, 0));
    let (ux0, uy0, ux1, uy1) = (
        unit.x as i32 - l,
        unit.y as i32 - t,
        unit.x as i32 + r,
        unit.y as i32 + b,
    );
    clear_rects
        .iter()
        .any(|&(cx0, cy0, cx1, cy1)| ux0 <= cx1 && ux1 >= cx0 && uy0 <= cy1 && uy1 >= cy0)
}

/// The start location's footprint in logical pixels: its `units.dat` placement box, falling
/// back to the well-known 128x96 when the table is absent or degenerate.
fn start_location_box(data: Option<&GameData>) -> (u32, u32) {
    data.and_then(|data| data.units_dat().entry(UNIT_ID_START_LOCATION))
        .map(|entry| {
            (
                entry.placebox.0.max(0) as u32,
                entry.placebox.1.max(0) as u32,
            )
        })
        .filter(|&(w, h)| w > 0 && h > 0)
        .unwrap_or(FALLBACK_START_LOCATION_BOX)
}

/// Draws the start-location color blocks: a polished "player token" (rounded, gradient-filled,
/// bezel-stroked — see [`crate::token`]) in each owner's color, sized to the start location's
/// `units.dat` placement box, centred on the placed position, over everything else.
fn draw_start_location_blocks(
    image: &mut RgbaImage,
    units: &[PlacedUnit],
    player_colors: &PlayerColors,
    data: Option<&GameData>,
    options: &RenderOptions,
    zoom: f32,
) {
    if options.start_locations != StartLocations::ColorBlock {
        return;
    }

    let (box_w, box_h) = start_location_box(data);
    let box_w = box_w.min(MAX_START_LOCATION_BOX_LOGICAL_PX);
    let box_h = box_h.min(MAX_START_LOCATION_BOX_LOGICAL_PX);

    for unit in units.iter().filter(|u| u.unit_id == UNIT_ID_START_LOCATION) {
        let color = owner_color(unit.owner.unwrap_or(u8::MAX), player_colors);
        let left = ((unit.x as f32 - box_w as f32 / 2.0) * zoom).round() as i32;
        let top = ((unit.y as f32 - box_h as f32 / 2.0) * zoom).round() as i32;
        let right = ((unit.x as f32 + box_w as f32 / 2.0) * zoom).round() as i32;
        let bottom = ((unit.y as f32 + box_h as f32 / 2.0) * zoom).round() as i32;
        crate::token::draw_player_token(
            image,
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
    use crate::source::{DatKind, MemorySource};
    use crate::tier::ArtStyle;
    use broodmap::chk::placed_units::{UnitInstanceId, UnitState};
    use broodmap::chk::player_colors::PlayerColor;
    use broodmap::chk::terrain::TileId;

    /// Byte offset of `units.dat`'s `unit_direction` column: the sum of the five columns before
    /// it (flingy u8, sub_unit_1 u16, sub_unit_2 u16, infestation u16 over the 96 buildings,
    /// construction_image u32), for 228 units.
    const UNITS_DIRECTION_COLUMN: usize = 228 + 228 * 2 + 228 * 2 + 96 * 2 + 228 * 4;
    /// Byte offset of `units.dat`'s `special_ability_flags` column (the 23rd of 54), reached by
    /// summing every column ahead of it — see `broodmap_formats::dat`'s `UNITS_COLUMN_SIZES` for
    /// the layout.
    const UNITS_SPECIAL_ABILITY_FLAGS_COLUMN: usize = UNITS_DIRECTION_COLUMN
        + 228 * 2   // unit_direction, shield_enabled
        + 228 * 2   // shield_amount (i16)
        + 228 * 4   // hit_points (i32)
        + 228 * 3   // elevation_level, unknown, sub_label
        + 228 * 5   // five AI/order columns
        + 228 * 5; // ground_weapon..ai_internal
    /// Byte offset of `units.dat`'s `placebox` column (the 37th of 54), continuing on from
    /// `special_ability_flags`.
    const UNITS_PLACEBOX_COLUMN: usize = UNITS_SPECIAL_ABILITY_FLAGS_COLUMN
        + 228 * 4   // special_ability_flags (u32)
        + 228 * 6   // target_acquisition_range..right_click_action
        + 106 * 2   // ready_sound (units-only)
        + 228 * 4   // what_sound_start/end (u16 each)
        + 106 * 8; // piss/yes sound start/end (units-only u16s)
    /// Byte offset of `images.dat`'s `has_directional_frames` column: right after the `grp` u32
    /// column, for 999 images.
    const IMAGES_DIRECTIONAL_COLUMN: usize = 999 * 4;
    /// Byte offset of `images.dat`'s `render_style` column (the 6th of 14): the `grp` u32 column
    /// plus the four single-byte columns ahead of it (`has_directional_frames`, `clickable`,
    /// `use_full_iscript`, `always_visible`) — see `broodmap_formats::dat`'s
    /// `IMAGES_COLUMN_SIZES`, whose own test independently pins this same offset at 7992.
    const IMAGES_RENDER_STYLE_COLUMN: usize = 999 * 4 + 999 * 4;
    /// `images.dat`'s raw `render_style` code for BW's "shadow" draw function.
    const RENDER_STYLE_SHADOW: u8 = 10;

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

    fn doodad(id: u16) -> Sprite {
        Sprite {
            id,
            x: 0,
            y: 0,
            owner: 11,
            flags: SpriteFlags::DRAW_AS_SPRITE,
        }
    }

    fn unit_sprite(id: u16, flags: SpriteFlags) -> Sprite {
        Sprite {
            id,
            x: 0,
            y: 0,
            owner: 0,
            flags,
        }
    }

    /// Game data where every unit and sprite resolves to image 0 (all tables zeroed), which is
    /// all the filtering tests need.
    fn game_data() -> GameData {
        GameData::load(&synthetic_source(synthetic_parts(0, 0, 0, 0, None))).unwrap()
    }

    /// Game data where unit/sprite ID 0 resolves to `main_image_id`, with `images.dat`'s
    /// `render_style` column at `main_image_id + 1` set to `shadow_render_style` -- the shadow
    /// tests' one knob.
    fn game_data_with_shadow_render_style(main_image_id: u16, shadow_render_style: u8) -> GameData {
        let mut parts = synthetic_parts(0, 0, 0, main_image_id, None);
        let shadow_id = main_image_id as usize + 1;
        parts.3[IMAGES_RENDER_STYLE_COLUMN + shadow_id] = shadow_render_style;
        GameData::load(&synthetic_source(parts)).unwrap()
    }

    /// Baseline options for tests: as-placed (the library default is Melee, but most of these
    /// tests are about drawing mechanics and don't want melee filtering in the way).
    fn options() -> RenderOptions {
        RenderOptions {
            art_style: ArtStyle::Remastered,
            unit_filter: UnitFilter::AsPlaced,
            ..Default::default()
        }
    }

    // -----------------------------------------------------------------------------------------
    // Filtering
    // -----------------------------------------------------------------------------------------

    #[test]
    fn melee_filter_drops_player_owned_units_but_keeps_neutrals_and_start_locations() {
        // The start location sits far away from every other unit so the melee start-area
        // clearing (tested separately) doesn't interact with the ownership filtering here.
        let units = [
            unit(0, Some(0), 10, 10),                        // player-owned: dropped
            unit(0, Some(7), 20, 20),                        // player slot 7: dropped
            unit(176, Some(11), 30, 30),                     // neutral mineral field: kept
            unit(0, Some(11), 40, 40),                       // neutral unit: kept
            unit(0, None, 50, 50),                           // no recorded owner: kept
            unit(UNIT_ID_START_LOCATION, Some(0), 900, 900), // start location: never dropped
        ];
        let sprites = [doodad(1)];
        let data = game_data();

        // 5 ordinary units + 1 doodad (start locations aren't drawables in ColorBlock mode).
        let as_placed = collect_drawables(&units, &sprites, &data, &options(), Tileset::Jungle);
        assert_eq!(as_placed.len(), 6);

        let melee = RenderOptions {
            unit_filter: UnitFilter::Melee,
            ..options()
        };
        let drawn = collect_drawables(&units, &sprites, &data, &melee, Tileset::Jungle);
        assert_eq!(drawn.len(), 4);
        assert!(
            drawn.iter().all(|d| d.y != 10 && d.y != 20),
            "player-owned units must be dropped"
        );
    }

    #[test]
    fn hallucinated_units_are_never_drawn() {
        let mut hallucinated = unit(0, Some(11), 10, 10);
        hallucinated.state = UnitState::HALLUCINATED;
        let data = game_data();

        for opts in [
            options(), // as-placed
            RenderOptions {
                unit_filter: UnitFilter::Melee,
                ..options()
            },
        ] {
            let drawn = collect_drawables(&[hallucinated], &[], &data, &opts, Tileset::Jungle);
            assert!(
                drawn.is_empty(),
                "hallucinated units expire moments into a real game and must not be drawn"
            );
        }
    }

    #[test]
    fn melee_clears_units_overlapping_start_location_spawn_areas() {
        let units = [
            unit(0, Some(11), 210, 205), // neutral, inside the spawn area: cleared
            unit(0, Some(11), 210, 500), // neutral, far away: kept
            unit(UNIT_ID_START_LOCATION, Some(0), 200, 200),
        ];
        let data = game_data();

        let melee = RenderOptions {
            unit_filter: UnitFilter::Melee,
            ..options()
        };
        let drawn = collect_drawables(&units, &[], &data, &melee, Tileset::Jungle);
        assert_eq!(drawn.len(), 1, "the unit in the spawn area must be cleared");
        assert_eq!(drawn[0].y, 500);

        // As-placed shows the map as authored, spawn areas included.
        let drawn = collect_drawables(&units, &[], &data, &options(), Tileset::Jungle);
        assert_eq!(drawn.len(), 2);
    }

    #[test]
    fn melee_filter_still_draws_the_start_location_sprite() {
        let units = [unit(UNIT_ID_START_LOCATION, Some(3), 64, 64)];
        let opts = RenderOptions {
            unit_filter: UnitFilter::Melee,
            start_locations: StartLocations::Sprite,
            ..options()
        };
        let drawn = collect_drawables(&units, &[], &game_data(), &opts, Tileset::Jungle);
        assert_eq!(drawn.len(), 1);
        assert_eq!(drawn[0].owner, 3);
        assert_eq!(drawn[0].layer, 1, "start locations draw on top");
    }

    #[test]
    fn start_locations_hidden_and_color_block_produce_no_drawable() {
        let units = [unit(UNIT_ID_START_LOCATION, Some(0), 64, 64)];
        for mode in [StartLocations::Hidden, StartLocations::ColorBlock] {
            let opts = RenderOptions {
                start_locations: mode,
                ..options()
            };
            assert!(
                collect_drawables(&units, &[], &game_data(), &opts, Tileset::Jungle).is_empty()
            );
        }
    }

    #[test]
    fn critter_and_resource_toggles_drop_only_their_own_classes() {
        let units = [
            unit(89, Some(11), 1, 1),  // Rhynadon
            unit(96, Some(11), 2, 2),  // Ursadon
            unit(176, Some(11), 3, 3), // mineral field
            unit(188, Some(11), 4, 4), // vespene geyser
            unit(0, Some(11), 5, 5),   // ordinary unit
        ];
        let data = game_data();

        assert_eq!(
            collect_drawables(&units, &[], &data, &options(), Tileset::Jungle).len(),
            5
        );
        let no_critters = RenderOptions {
            show_critters: false,
            ..options()
        };
        assert_eq!(
            collect_drawables(&units, &[], &data, &no_critters, Tileset::Jungle).len(),
            3
        );
        let no_resources = RenderOptions {
            show_resources: false,
            ..options()
        };
        assert_eq!(
            collect_drawables(&units, &[], &data, &no_resources, Tileset::Jungle).len(),
            3
        );
    }

    #[test]
    fn neutral_buildings_toggle_drops_only_neutral_owned_buildings() {
        let mut parts = synthetic_parts(0, 0, 0, 0, None);
        // Unit 0's `units.dat` entry has the Building special-ability flag (bit 0x1) set.
        let off = UNITS_SPECIAL_ABILITY_FLAGS_COLUMN;
        parts.0[off..off + 4].copy_from_slice(&1u32.to_le_bytes());
        let data = GameData::load(&synthetic_source(parts)).unwrap();

        let units = [
            unit(0, Some(11), 1, 1), // neutral-owned building: dropped when disabled
            unit(0, None, 2, 2),     // no recorded owner: also neutral, dropped when disabled
            unit(0, Some(3), 3, 3),  // player-owned: never affected by this toggle
        ];

        assert_eq!(
            collect_drawables(&units, &[], &data, &options(), Tileset::Jungle).len(),
            3,
            "kept by default"
        );

        let no_neutral_buildings = RenderOptions {
            show_neutral_buildings: false,
            ..options()
        };
        let drawn = collect_drawables(&units, &[], &data, &no_neutral_buildings, Tileset::Jungle);
        assert_eq!(drawn.len(), 1, "only the player-owned building survives");
        assert_eq!(drawn[0].owner, 3);
    }

    #[test]
    fn doodad_toggle_and_disabled_sprites() {
        let sprites = [
            doodad(1),
            doodad(2),
            unit_sprite(0, SpriteFlags::empty()),
            // Disabled *unit* sprites are never drawn...
            unit_sprite(0, SpriteFlags::DISABLED),
            // ...but the DISABLED bit on a pure sprite doesn't mean disabled (see
            // `Sprite::is_disabled`).
            doodad(3),
        ];
        let data = game_data();

        assert_eq!(
            collect_drawables(&[], &sprites, &data, &options(), Tileset::Jungle).len(),
            4,
            "one disabled unit sprite dropped"
        );

        let no_doodads = RenderOptions {
            show_doodad_sprites: false,
            ..options()
        };
        assert_eq!(
            collect_drawables(&[], &sprites, &data, &no_doodads, Tileset::Jungle).len(),
            1,
            "only the enabled unit sprite survives"
        );
    }

    #[test]
    fn drawables_are_sorted_by_y_then_x_with_start_locations_on_top() {
        let units = [
            unit(0, Some(11), 50, 30),
            unit(0, Some(11), 10, 30),
            unit(0, Some(11), 99, 10),
            unit(UNIT_ID_START_LOCATION, Some(0), 0, 0),
        ];
        let opts = RenderOptions {
            start_locations: StartLocations::Sprite,
            ..options()
        };
        let drawn = collect_drawables(&units, &[], &game_data(), &opts, Tileset::Jungle);
        let order: Vec<(u8, i32, i32)> = drawn.iter().map(|d| (d.layer, d.x, d.y)).collect();
        assert_eq!(
            order,
            vec![(0, 99, 10), (0, 10, 30), (0, 50, 30), (1, 0, 0)]
        );
    }

    // -----------------------------------------------------------------------------------------
    // Shadows
    // -----------------------------------------------------------------------------------------

    #[test]
    fn shadow_emitted_when_plus_one_is_render_style_shadow() {
        let data = game_data_with_shadow_render_style(5, RENDER_STYLE_SHADOW);
        let units = [unit(0, Some(11), 10, 20)];
        let drawn = collect_drawables(&units, &[], &data, &options(), Tileset::Jungle);

        assert_eq!(
            drawn.len(),
            2,
            "expected a shadow plus its owner: {drawn:?}"
        );
        assert!(
            drawn[0].is_shadow,
            "the shadow must come first (painted beneath its owner)"
        );
        assert!(!drawn[1].is_shadow);
        assert_eq!(
            drawn[0].art_image_id, 6,
            "shadow art is the +1 (post-redirect) image"
        );
        assert_eq!(drawn[1].art_image_id, 5);
        // Same position, frame and flip as the owner.
        assert_eq!((drawn[0].x, drawn[0].y), (drawn[1].x, drawn[1].y));
        assert_eq!(drawn[0].frame, drawn[1].frame);
        assert_eq!(drawn[0].flip, drawn[1].flip);
        assert_eq!(drawn[0].layer, drawn[1].layer);
    }

    #[test]
    fn no_shadow_when_plus_one_is_not_render_style_shadow() {
        // render_style 9 ("use remapping"/teamcolor) is a real draw style, just not Shadow.
        let data = game_data_with_shadow_render_style(5, 9);
        let units = [unit(0, Some(11), 10, 20)];
        let drawn = collect_drawables(&units, &[], &data, &options(), Tileset::Jungle);
        assert_eq!(drawn.len(), 1, "no shadow should be emitted: {drawn:?}");
        assert!(!drawn[0].is_shadow);
    }

    #[test]
    fn no_shadow_when_plus_one_entry_is_missing() {
        // Image 998 is the last valid images.dat index (999 entries, 0-indexed); +1 (999) is out
        // of range, so there's no entry to gate on at all.
        let data = game_data_with_shadow_render_style(998, RENDER_STYLE_SHADOW);
        let units = [unit(0, Some(11), 10, 20)];
        let drawn = collect_drawables(&units, &[], &data, &options(), Tileset::Jungle);
        assert_eq!(
            drawn.len(),
            1,
            "out-of-range +1 must not panic or synthesize a shadow"
        );
        assert!(!drawn[0].is_shadow);
    }

    #[test]
    fn geyser_shadow_uses_plus_two_not_plus_one() {
        // Mirrors the real data (image 344, +1=345 a same-GRP render_style-0 variant, +2=346
        // "neutral\geyShad.grp" render_style 10): a vespene geyser's shadow lives at +2, not +1.
        let mut parts = synthetic_parts(UNIT_ID_VESPENE_GEYSER, 0, 0, 10, None);
        parts.3[IMAGES_RENDER_STYLE_COLUMN + 11] = 0; // +1: a variant, not a shadow
        parts.3[IMAGES_RENDER_STYLE_COLUMN + 12] = RENDER_STYLE_SHADOW; // +2: the real shadow
        let data = GameData::load(&synthetic_source(parts)).unwrap();

        let units = [unit(UNIT_ID_VESPENE_GEYSER, Some(11), 10, 20)];
        let drawn = collect_drawables(&units, &[], &data, &options(), Tileset::Jungle);
        assert_eq!(
            drawn.len(),
            2,
            "the geyser should get a shadow at +2: {drawn:?}"
        );
        assert!(drawn[0].is_shadow);
        assert_eq!(
            drawn[0].art_image_id, 12,
            "shadow art must be the +2 image, not +1"
        );
    }

    #[test]
    fn non_geyser_units_never_fall_back_to_plus_two() {
        // Same images.dat layout as the geyser case above (+1 not a shadow, +2 is), but for an
        // ordinary unit ID: only +1 is ever consulted for anything other than the geyser special
        // case, so no shadow should be emitted even though +2 is a real one.
        let mut parts = synthetic_parts(0, 0, 0, 10, None);
        parts.3[IMAGES_RENDER_STYLE_COLUMN + 11] = 0;
        parts.3[IMAGES_RENDER_STYLE_COLUMN + 12] = RENDER_STYLE_SHADOW;
        let data = GameData::load(&synthetic_source(parts)).unwrap();

        let units = [unit(0, Some(11), 10, 20)];
        let drawn = collect_drawables(&units, &[], &data, &options(), Tileset::Jungle);
        assert_eq!(
            drawn.len(),
            1,
            "ordinary units must not fall back to +2: {drawn:?}"
        );
        assert!(!drawn[0].is_shadow);
    }

    #[test]
    fn doodad_sprites_never_get_the_geyser_plus_two_special_case() {
        // Sprite ID 188 coincides with the vespene geyser's *unit* ID, but doodads have no unit
        // ID at all (THG2 pure-sprite entries), so the special case must never apply to them.
        let mut parts = synthetic_parts(0, 0, UNIT_ID_VESPENE_GEYSER, 10, None);
        parts.3[IMAGES_RENDER_STYLE_COLUMN + 11] = 0;
        parts.3[IMAGES_RENDER_STYLE_COLUMN + 12] = RENDER_STYLE_SHADOW;
        let data = GameData::load(&synthetic_source(parts)).unwrap();

        let drawn = collect_drawables(
            &[],
            &[doodad(UNIT_ID_VESPENE_GEYSER)],
            &data,
            &options(),
            Tileset::Jungle,
        );
        assert_eq!(
            drawn.len(),
            1,
            "doodads must never use the geyser +2 rule: {drawn:?}"
        );
    }

    #[test]
    fn show_shadows_false_suppresses_shadows_even_when_the_gate_would_pass() {
        let data = game_data_with_shadow_render_style(5, RENDER_STYLE_SHADOW);
        let units = [unit(0, Some(11), 10, 20)];
        let opts = RenderOptions {
            show_shadows: false,
            ..options()
        };
        let drawn = collect_drawables(&units, &[], &data, &opts, Tileset::Jungle);
        assert_eq!(drawn.len(), 1);
        assert!(!drawn[0].is_shadow);
    }

    #[test]
    fn shadows_apply_to_thg2_doodad_and_unit_sprites_too() {
        let data = game_data_with_shadow_render_style(5, RENDER_STYLE_SHADOW);

        // A pure-sprite (doodad) THG2 entry resolves through `sprites.dat` directly, but the
        // synthetic table chain routes both unit ID 0 and sprite ID 0 to image 5 (see
        // `synthetic_parts`), so `doodad(0)` exercises the same shadow gate through the
        // sprite-image path.
        let drawn = collect_drawables(&[], &[doodad(0)], &data, &options(), Tileset::Jungle);
        assert_eq!(drawn.len(), 2, "doodad sprites get shadows too: {drawn:?}");
        assert!(drawn[0].is_shadow);

        // A "unit sprite" THG2 entry (DRAW_AS_SPRITE clear) takes the full units.dat chain.
        let drawn = collect_drawables(
            &[],
            &[unit_sprite(0, SpriteFlags::empty())],
            &data,
            &options(),
            Tileset::Jungle,
        );
        assert_eq!(
            drawn.len(),
            2,
            "THG2 unit-sprites get shadows too: {drawn:?}"
        );
        assert!(drawn[0].is_shadow);
    }

    #[test]
    fn start_locations_never_get_a_shadow() {
        // Give image 588 (the start-location graphic) a real shadow-styled +1 neighbor; the
        // start-location path must still never emit one.
        let mut parts = synthetic_parts(0, 0, 0, 0, None);
        let shadow_id = IMAGE_ID_START_LOCATION as usize + 1;
        parts.3[IMAGES_RENDER_STYLE_COLUMN + shadow_id] = RENDER_STYLE_SHADOW;
        let data = GameData::load(&synthetic_source(parts)).unwrap();

        let units = [unit(UNIT_ID_START_LOCATION, Some(0), 64, 64)];
        let opts = RenderOptions {
            start_locations: StartLocations::Sprite,
            ..options()
        };
        let drawn = collect_drawables(&units, &[], &data, &opts, Tileset::Jungle);
        assert_eq!(
            drawn.len(),
            1,
            "start locations must never get a shadow: {drawn:?}"
        );
        assert!(!drawn[0].is_shadow);
    }

    #[test]
    fn required_preview_graphics_includes_shadow_anims_only_when_enabled() {
        let data = game_data_with_shadow_render_style(5, RENDER_STYLE_SHADOW);
        let units = [unit(0, Some(11), 10, 20)];
        let opts = RenderOptions {
            art_style: ArtStyle::Remastered,
            max_dimension: Some(1024),
            unit_filter: UnitFilter::AsPlaced,
            ..Default::default()
        };

        let with_shadows = required_preview_graphics(&units, &[], &data, 64, 64, &opts);
        assert!(
            with_shadows.contains(&AssetRequest::Anim {
                image_id: 5,
                tier: AssetTier::Hd2,
                pack: ArtPack::Standard,
            }),
            "{with_shadows:?}"
        );
        assert!(
            with_shadows.contains(&AssetRequest::Anim {
                image_id: 6,
                tier: AssetTier::Hd2,
                pack: ArtPack::Standard,
            }),
            "the shadow anim must be prefetched too: {with_shadows:?}"
        );

        let no_shadows_opts = RenderOptions {
            show_shadows: false,
            ..opts
        };
        let without_shadows =
            required_preview_graphics(&units, &[], &data, 64, 64, &no_shadows_opts);
        assert!(
            without_shadows.contains(&AssetRequest::Anim {
                image_id: 5,
                tier: AssetTier::Hd2,
                pack: ArtPack::Standard,
            }),
            "{without_shadows:?}"
        );
        assert!(
            !without_shadows.contains(&AssetRequest::Anim {
                image_id: 6,
                tier: AssetTier::Hd2,
                pack: ArtPack::Standard,
            }),
            "show_shadows: false must not prefetch the shadow anim: {without_shadows:?}"
        );
    }

    // -----------------------------------------------------------------------------------------
    // Frame selection
    // -----------------------------------------------------------------------------------------

    #[test]
    fn mineral_frames_follow_the_amount_thresholds() {
        assert_eq!(mineral_frame(Some(1500)), 0);
        assert_eq!(mineral_frame(Some(750)), 0);
        assert_eq!(mineral_frame(Some(749)), 1);
        assert_eq!(mineral_frame(Some(500)), 1);
        assert_eq!(mineral_frame(Some(499)), 2);
        assert_eq!(mineral_frame(Some(250)), 2);
        assert_eq!(mineral_frame(Some(249)), 3);
        assert_eq!(mineral_frame(Some(0)), 3);
        // No recorded amount: treat the field as full.
        assert_eq!(mineral_frame(None), 0);
    }

    #[test]
    fn direction_maps_to_frame_and_flip_over_the_full_circle() {
        assert_eq!(direction_frame(0), (0, false)); // north
        assert_eq!(direction_frame(8), (8, false)); // east
        assert_eq!(direction_frame(16), (16, false)); // south
        // The western half mirrors the eastern one.
        assert_eq!(direction_frame(17), (15, true));
        assert_eq!(direction_frame(24), (8, true));
        assert_eq!(direction_frame(31), (1, true));
    }

    #[test]
    fn frame_selection_covers_resources_facings_and_random() {
        let mut parts = synthetic_parts(0, 0, 0, 0, None);
        // Make image 0 directional; give unit 0 a west facing and unit 1 the "random" value.
        parts.3[IMAGES_DIRECTIONAL_COLUMN] = 1;
        parts.0[UNITS_DIRECTION_COLUMN] = 24;
        parts.0[UNITS_DIRECTION_COLUMN + 1] = RANDOM_DIRECTION;
        let data = GameData::load(&synthetic_source(parts)).unwrap();

        assert_eq!(
            select_unit_frame(&unit(0, None, 0, 0), 0, &data, Tileset::Jungle),
            (8, true),
            "west == east mirrored"
        );

        // A random facing is deterministic per instance ID and stays in range.
        let mut random_unit = unit(1, None, 0, 0);
        for id in 0..64u32 {
            random_unit.instance_id = UnitInstanceId(id);
            let (frame, _) = select_unit_frame(&random_unit, 0, &data, Tileset::Jungle);
            assert!(frame <= 16, "instance {id} gave frame {frame}");
        }
        random_unit.instance_id = UnitInstanceId(7);
        let first = select_unit_frame(&random_unit, 0, &data, Tileset::Jungle);
        assert_eq!(
            first,
            select_unit_frame(&random_unit, 0, &data, Tileset::Jungle),
            "the same instance must always render the same way"
        );

        // Resources ignore facing entirely, even on a directional image.
        let mut mineral = unit(176, None, 0, 0);
        mineral.resource_amount = Some(400);
        assert_eq!(
            select_unit_frame(&mineral, 0, &data, Tileset::Jungle),
            (2, false)
        );
        assert_eq!(
            select_unit_frame(&unit(188, None, 0, 0), 0, &data, Tileset::Jungle),
            (Tileset::Jungle as usize, false)
        );
        assert_eq!(
            select_unit_frame(&unit(188, None, 0, 0), 0, &data, Tileset::Badlands),
            (0, false)
        );
    }

    #[test]
    fn non_directional_images_always_use_frame_zero() {
        let mut parts = synthetic_parts(0, 0, 0, 0, None);
        parts.0[UNITS_DIRECTION_COLUMN] = 24;
        let data = GameData::load(&synthetic_source(parts)).unwrap();
        assert_eq!(
            select_unit_frame(&unit(0, None, 0, 0), 0, &data, Tileset::Jungle),
            (0, false)
        );
    }

    // -----------------------------------------------------------------------------------------
    // Geometry helpers
    // -----------------------------------------------------------------------------------------

    #[test]
    fn zero_canvas_falls_back_to_the_frames_own_extent() {
        assert_eq!(effective_canvas(400, 10, 100), 400);
        assert_eq!(effective_canvas(0, 4, 484), 492);
        // A negative offset can't drive the fallback below zero.
        assert_eq!(effective_canvas(0, -300, 100), 0);
    }

    #[test]
    fn flipping_mirrors_placement_about_the_canvas_centre() {
        let frame = AnimFrame {
            texture_x: 0,
            texture_y: 0,
            offset_x: 10,
            offset_y: 0,
            width: 40,
            height: 40,
        };
        // In a 100-unit canvas the frame spans 10..50, so its mirror spans 50..90.
        assert_eq!(frame_offset_x(&frame, 100, false), 10);
        assert_eq!(frame_offset_x(&frame, 100, true), 50);
    }

    #[test]
    fn frame_origin_centres_the_canvas_on_the_position() {
        // A 32-unit (8 logical px) canvas at zoom 1: its centre lands on the position, so a
        // frame at the canvas origin starts 4 px to the left.
        assert_eq!(frame_origin(64, 0, 32, 1.0), 60);
        // Zoom doubles every distance.
        assert_eq!(frame_origin(64, 0, 32, 2.0), 120);
        // An offset halfway into the canvas puts the frame back on the position.
        assert_eq!(frame_origin(64, 16, 32, 1.0), 64);
    }

    #[test]
    fn horizontal_flip_mirrors_rows() {
        let mut rgba = vec![
            1, 0, 0, 255, 2, 0, 0, 255, 3, 0, 0, 255, // row 0
            4, 0, 0, 255, 5, 0, 0, 255, 6, 0, 0, 255, // row 1
        ];
        flip_horizontal(&mut rgba, 3, 2);
        let reds: Vec<u8> = rgba.chunks_exact(4).map(|t| t[0]).collect();
        assert_eq!(reds, vec![3, 2, 1, 6, 5, 4]);
    }

    #[test]
    fn team_color_multiplies_only_where_the_mask_says_so() {
        // Two white diffuse texels: the first fully masked, the second not masked at all.
        let mut frame = vec![255, 255, 255, 255, 255, 255, 255, 255];
        let mask = vec![255, 255, 255, 255, 0, 0, 0, 255];
        apply_team_color(&mut frame, &mask, [244, 4, 4]);
        assert_eq!(&frame[0..4], &[244, 4, 4, 255], "white * red == red");
        assert_eq!(&frame[4..8], &[255, 255, 255, 255], "unmasked is untouched");

        // Multiplying preserves the art's shading: a mid-grey texel under a full mask comes out
        // as a *darkened* player color rather than flat player color (which is what the
        // replace/lerp alternative would give).
        let mut frame = vec![128, 128, 128, 255];
        apply_team_color(&mut frame, &[255, 255, 255, 255], [244, 4, 4]);
        assert_eq!(&frame[0..3], &[122, 2, 2]);
    }

    // -----------------------------------------------------------------------------------------
    // End-to-end, through a MemorySource
    // -----------------------------------------------------------------------------------------

    /// A minimal BC1 DDS whose every texel decodes to `color565` at full alpha.
    fn solid_bc1_dds(width: u32, height: u32, color565: u16) -> Vec<u8> {
        let mut data = Vec::new();
        data.extend_from_slice(b"DDS ");
        data.extend_from_slice(&124u32.to_le_bytes());
        data.extend_from_slice(&0u32.to_le_bytes());
        data.extend_from_slice(&height.to_le_bytes());
        data.extend_from_slice(&width.to_le_bytes());
        data.extend_from_slice(&0u32.to_le_bytes());
        data.extend_from_slice(&0u32.to_le_bytes());
        data.extend_from_slice(&1u32.to_le_bytes());
        data.extend_from_slice(&[0u8; 44]);
        data.extend_from_slice(&32u32.to_le_bytes());
        data.extend_from_slice(&0x4u32.to_le_bytes()); // DDPF_FOURCC
        data.extend_from_slice(b"DXT1");
        data.extend_from_slice(&0u32.to_le_bytes());
        data.extend_from_slice(&[0u8; 16]);
        data.extend_from_slice(&[0u8; 20]);
        // color0 == color1 with all-zero indices: every texel reads palette[0] == color0.
        for _ in 0..(width.div_ceil(4) * height.div_ceil(4)) {
            data.extend_from_slice(&color565.to_le_bytes());
            data.extend_from_slice(&color565.to_le_bytes());
            data.extend_from_slice(&0u32.to_le_bytes());
        }
        data
    }

    /// A single-layer, single-frame HD `.anim`.
    fn anim_bytes(scale: u8, canvas: (u16, u16), frame: AnimFrame, diffuse: &[u8]) -> Vec<u8> {
        const FRAME_TABLE_HEADER_OFFSET: usize = 0x14C;
        const LAYER_RECORDS_OFFSET: usize = 0x158;

        let mut data = vec![0u8; LAYER_RECORDS_OFFSET + 12];
        data[0..4].copy_from_slice(b"ANIM");
        data[4] = scale;
        data[5] = 2; // HD container
        data[8..10].copy_from_slice(&1u16.to_le_bytes()); // num_layers
        data[10..12].copy_from_slice(&1u16.to_le_bytes()); // num_entries
        data[0x0C..0x0C + 7].copy_from_slice(b"diffuse");

        let frame_arr_offset = data.len();
        let h = FRAME_TABLE_HEADER_OFFSET;
        data[h..h + 2].copy_from_slice(&1u16.to_le_bytes()); // frame_count
        data[h + 2..h + 4].copy_from_slice(&0xFFFFu16.to_le_bytes()); // ref_id: none
        data[h + 4..h + 6].copy_from_slice(&canvas.0.to_le_bytes());
        data[h + 6..h + 8].copy_from_slice(&canvas.1.to_le_bytes());
        data[h + 8..h + 12].copy_from_slice(&(frame_arr_offset as u32).to_le_bytes());

        let payload_offset = frame_arr_offset + 16;
        let l = LAYER_RECORDS_OFFSET;
        data[l..l + 4].copy_from_slice(&(payload_offset as u32).to_le_bytes());
        data[l + 4..l + 8].copy_from_slice(&(diffuse.len() as u32).to_le_bytes());

        data.extend_from_slice(&frame.texture_x.to_le_bytes());
        data.extend_from_slice(&frame.texture_y.to_le_bytes());
        data.extend_from_slice(&frame.offset_x.to_le_bytes());
        data.extend_from_slice(&frame.offset_y.to_le_bytes());
        data.extend_from_slice(&frame.width.to_le_bytes());
        data.extend_from_slice(&frame.height.to_le_bytes());
        data.extend_from_slice(&0u32.to_le_bytes()); // unknown
        data.extend_from_slice(diffuse);
        data
    }

    /// A one-group CV5 plus a one-frame `.dds.vr4` of a solid color.
    fn terrain_assets(color565: u16) -> (Vec<u8>, Vec<u8>) {
        let mut cv5 = Vec::with_capacity(52);
        cv5.extend_from_slice(&0u16.to_le_bytes()); // group_type
        cv5.extend_from_slice(&0u16.to_le_bytes()); // flags
        cv5.extend_from_slice(&[0u8; 16]); // unused rects
        cv5.extend_from_slice(&0u16.to_le_bytes()); // mega_tiles[0] = 0
        cv5.extend_from_slice(&[0u8; 30]);

        let frame = solid_bc1_dds(4, 4, color565);
        let mut dds_vr4 = Vec::new();
        dds_vr4.extend_from_slice(&0u32.to_le_bytes()); // file size (unvalidated)
        dds_vr4.extend_from_slice(&1u16.to_le_bytes()); // frame count
        dds_vr4.extend_from_slice(&0x1004u16.to_le_bytes()); // DDS-record layout
        dds_vr4.extend_from_slice(&0u32.to_le_bytes());
        dds_vr4.extend_from_slice(&0u16.to_le_bytes());
        dds_vr4.extend_from_slice(&0u16.to_le_bytes());
        dds_vr4.extend_from_slice(&(frame.len() as u32).to_le_bytes());
        dds_vr4.extend_from_slice(&frame);
        (cv5, dds_vr4)
    }

    /// Builds a source with terrain assets at `tier` plus the five `.dat`/`.rel` tables.
    fn preview_source(
        tier: AssetTier,
        terrain_color: u16,
        dats: crate::gamedata::tests::DatBytes,
    ) -> MemorySource {
        let (cv5, dds_vr4) = terrain_assets(terrain_color);
        let (units, flingy, sprites, images, rel) = dats;
        [
            (AssetRequest::Cv5(Tileset::Jungle), cv5),
            (
                AssetRequest::TilesetDds(Tileset::Jungle, tier, ArtPack::Standard),
                dds_vr4,
            ),
            (AssetRequest::Dat(DatKind::Units), units),
            (AssetRequest::Dat(DatKind::Flingy), flingy),
            (AssetRequest::Dat(DatKind::Sprites), sprites),
            (AssetRequest::Dat(DatKind::Images), images),
            (AssetRequest::ImagesRel, rel),
        ]
        .into_iter()
        .collect()
    }

    fn square_terrain(tiles: usize) -> TerrainTileIds {
        TerrainTileIds {
            width: tiles,
            height: tiles,
            tiles: vec![TileId(0); tiles * tiles],
        }
    }

    /// A synthetic 1-frame anim placed at a known position must land on exactly the output
    /// pixels the placement/scaling maths predicts — not one pixel over.
    #[test]
    fn end_to_end_placement_lands_on_the_expected_output_pixels() {
        let terrain = square_terrain(4);
        let opts = RenderOptions {
            art_style: ArtStyle::Remastered,
            max_dimension: Some(128), // 4 tiles => 32 px/tile => zoom 1
            ..Default::default()
        };
        let (_, _, ppt) = resolve_tier(&opts, 4, 4);
        assert_eq!(ppt, 32, "this test relies on a 1:1 logical-to-output zoom");

        // HD2 art (2 texels per logical px). A 32x32-unit frame is 8x8 logical px, backed by a
        // 16x16-texel crop; the 32x32-unit canvas centres it on the unit's position.
        let frame = AnimFrame {
            texture_x: 0,
            texture_y: 0,
            offset_x: 0,
            offset_y: 0,
            width: 32,
            height: 32,
        };
        let mut source = preview_source(
            AssetTier::Hd2,
            0x001F, // blue terrain
            synthetic_parts(5, 0, 0, 0, None),
        );
        source.insert(
            AssetRequest::Anim {
                image_id: 0,
                tier: AssetTier::Hd2,
                pack: ArtPack::Standard,
            },
            anim_bytes(2, (32, 32), frame, &solid_bc1_dds(16, 16, 0xF800)),
        );

        let units = [unit(5, Some(11), 64, 64)];
        let image = render_preview(
            &terrain,
            Tileset::Jungle,
            &units,
            &[],
            &PlayerColors::default(),
            &source,
            &opts,
        )
        .unwrap();

        assert_eq!((image.width, image.height), (128, 128));
        let pixel = |x: u32, y: u32| -> [u8; 4] {
            let o = (y as usize * 128 + x as usize) * 4;
            image.data[o..o + 4].try_into().unwrap()
        };
        let red = [255, 0, 0, 255];
        let blue = [0, 0, 255, 255];

        // The 8x8 sprite is centred on (64, 64), covering x and y 60..=67.
        assert_eq!(pixel(60, 60), red, "top-left corner of the sprite");
        assert_eq!(pixel(64, 64), red);
        assert_eq!(pixel(67, 67), red, "bottom-right corner of the sprite");
        assert_eq!(pixel(59, 60), blue, "one pixel left of the sprite");
        assert_eq!(pixel(60, 59), blue, "one pixel above the sprite");
        assert_eq!(pixel(68, 67), blue, "one pixel right of the sprite");
        assert_eq!(pixel(67, 68), blue, "one pixel below the sprite");
    }

    #[test]
    fn missing_anim_skips_the_drawable_instead_of_failing() {
        let terrain = square_terrain(4);
        let opts = RenderOptions {
            art_style: ArtStyle::Remastered,
            max_dimension: Some(128),
            ..Default::default()
        };
        // Everything except the `.anim` itself.
        let source = preview_source(AssetTier::Hd2, 0x001F, synthetic_parts(5, 0, 0, 0, None));

        let units = [unit(5, Some(11), 64, 64)];
        let image = render_preview(
            &terrain,
            Tileset::Jungle,
            &units,
            &[],
            &PlayerColors::default(),
            &source,
            &opts,
        )
        .unwrap();
        for texel in image.data.chunks_exact(4) {
            assert_eq!(texel, [0, 0, 255, 255], "pure terrain, and no error");
        }
    }

    #[test]
    fn missing_dats_error_only_when_the_unit_layer_needs_them() {
        let terrain = square_terrain(4);
        let (cv5, dds_vr4) = terrain_assets(0x001F);
        let (_, sd_vr4) = terrain_assets(0x001F);
        let source: MemorySource = [
            (AssetRequest::Cv5(Tileset::Jungle), cv5),
            (
                AssetRequest::TilesetDds(Tileset::Jungle, AssetTier::Hd2, ArtPack::Standard),
                dds_vr4,
            ),
            (
                AssetRequest::TilesetDds(Tileset::Jungle, AssetTier::Sd, ArtPack::Standard),
                sd_vr4,
            ),
        ]
        .into_iter()
        .collect();

        let remastered = RenderOptions {
            art_style: ArtStyle::Remastered,
            max_dimension: Some(128),
            ..Default::default()
        };
        // A non-empty unit list: something actually needs resolving through the `.dat` chain, so
        // this is a genuine hard dependency (an empty map wouldn't need the tables at all — see
        // `empty_units_and_sprites_skip_gamedata_even_when_unit_art_is_enabled`).
        let units = [unit(0, Some(11), 64, 64)];
        assert!(
            render_preview(
                &terrain,
                Tileset::Jungle,
                &units,
                &[],
                &PlayerColors::default(),
                &source,
                &remastered,
            )
            .is_err(),
            "the unit layer can't proceed without the .dat tables"
        );

        // The Original style skips unit art entirely, so the missing tables are only a warning
        // and the start-location box falls back to its documented default.
        let original = RenderOptions {
            art_style: ArtStyle::Original,
            max_dimension: Some(128),
            ..Default::default()
        };
        let preview = render_preview_with_warnings(
            &terrain,
            Tileset::Jungle,
            &[unit(UNIT_ID_START_LOCATION, Some(0), 64, 64)],
            &[],
            &PlayerColors::default(),
            &source,
            &original,
        )
        .unwrap();
        assert_eq!(preview.warnings.len(), 1);
        assert!(preview.warnings[0].contains("mainSD.anim"));
        // The start-location block still drew, in player 0's red.
        let o = (64usize * 128 + 64) * 4;
        assert_eq!(&preview.image.data[o..o + 4], &[244, 4, 4, 255]);
    }

    #[test]
    fn start_location_block_uses_the_units_dat_placebox_and_player_color() {
        let terrain = square_terrain(8);
        let opts = RenderOptions {
            art_style: ArtStyle::Remastered,
            max_dimension: Some(256), // 8 tiles => 32 px/tile => zoom 1
            ..Default::default()
        };

        let mut dats = synthetic_parts(0, 0, 0, 0, None);
        // Give unit 214 a 20x10 logical-pixel placement box.
        let placebox = UNITS_PLACEBOX_COLUMN + UNIT_ID_START_LOCATION as usize * 4;
        dats.0[placebox..placebox + 2].copy_from_slice(&20i16.to_le_bytes());
        dats.0[placebox + 2..placebox + 4].copy_from_slice(&10i16.to_le_bytes());
        let source = preview_source(AssetTier::Hd2, 0x001F, dats);

        let mut colors = PlayerColors::default();
        colors.colors[0] = PlayerColor::Rgb([1, 2, 3]);

        let units = [unit(UNIT_ID_START_LOCATION, Some(0), 100, 100)];
        let image = render_preview(
            &terrain,
            Tileset::Jungle,
            &units,
            &[],
            &colors,
            &source,
            &opts,
        )
        .unwrap();

        let pixel = |x: u32, y: u32| -> [u8; 4] {
            let o = (y as usize * image.width as usize + x as usize) * 4;
            image.data[o..o + 4].try_into().unwrap()
        };
        // The 20x10 box centred on (100, 100) covers x 90..110 and y 95..105. The token is a
        // rounded, stroked, gradient-filled shape rather than a flat rect (see `crate::token`),
        // so only its untouched centre (the vertical gradient is exactly neutral there) and a
        // point comfortably clear of the whole token (padding included) render as exact colors;
        // right at the box's corners the rounding/AA blends the color with the terrain.
        assert_eq!(
            pixel(100, 100),
            [1, 2, 3, 255],
            "token centre is the flat player color"
        );
        assert_eq!(
            pixel(60, 100),
            [0, 0, 255, 255],
            "well clear of the token: pure terrain"
        );
        assert_eq!(
            pixel(100, 60),
            [0, 0, 255, 255],
            "well clear of the token: pure terrain"
        );
    }

    /// A hostile/corrupted `units.dat` claiming the largest box an `i16` placebox column can
    /// hold must not inflate the render (both the logical-space clamp in this module and
    /// `crate::token`'s own image-bounds clip guard against it), and the token must still cover
    /// the placed position correctly.
    #[test]
    fn huge_units_dat_placebox_is_clamped_and_still_renders() {
        let terrain = square_terrain(8);
        let opts = RenderOptions {
            art_style: ArtStyle::Remastered,
            max_dimension: Some(256), // 8 tiles => 32 px/tile => zoom 1
            ..Default::default()
        };

        let mut dats = synthetic_parts(0, 0, 0, 0, None);
        let placebox = UNITS_PLACEBOX_COLUMN + UNIT_ID_START_LOCATION as usize * 4;
        dats.0[placebox..placebox + 2].copy_from_slice(&i16::MAX.to_le_bytes());
        dats.0[placebox + 2..placebox + 4].copy_from_slice(&i16::MAX.to_le_bytes());
        let source = preview_source(AssetTier::Hd2, 0x001F, dats);

        let units = [unit(UNIT_ID_START_LOCATION, Some(0), 100, 100)];
        let image = render_preview(
            &terrain,
            Tileset::Jungle,
            &units,
            &[],
            &PlayerColors::default(),
            &source,
            &opts,
        )
        .unwrap();

        // The huge placebox must not inflate the output past the map's own size.
        assert_eq!((image.width, image.height), (256, 256));

        let o = (100usize * image.width as usize + 100) * 4;
        assert_eq!(
            &image.data[o..o + 4],
            &[244, 4, 4, 255],
            "the (clamped) token still covers the placed position in player 0's color"
        );
    }

    /// The empty-map contract `render_chk_preview`'s docs promise: a map with no units and no
    /// sprites must render successfully even when unit art is enabled and the source has no
    /// `.dat`/`.rel` tables at all, because nothing could ever read them.
    #[test]
    fn empty_units_and_sprites_skip_gamedata_even_when_unit_art_is_enabled() {
        let terrain = square_terrain(4);
        let opts = RenderOptions {
            art_style: ArtStyle::Remastered,
            max_dimension: Some(128),
            ..Default::default()
        };
        let (cv5, dds_vr4) = terrain_assets(0x001F);
        let source: MemorySource = [
            (AssetRequest::Cv5(Tileset::Jungle), cv5),
            (
                AssetRequest::TilesetDds(Tileset::Jungle, AssetTier::Hd2, ArtPack::Standard),
                dds_vr4,
            ),
        ]
        .into_iter()
        .collect();

        let preview = render_preview_with_warnings(
            &terrain,
            Tileset::Jungle,
            &[],
            &[],
            &PlayerColors::default(),
            &source,
            &opts,
        )
        .unwrap();
        assert!(preview.warnings.is_empty(), "{:?}", preview.warnings);
        for texel in preview.image.data.chunks_exact(4) {
            assert_eq!(texel, [0, 0, 255, 255], "pure terrain, and no error");
        }
    }

    #[test]
    fn tile_key_color_collapses_to_a_sentinel_without_a_teamcolor_layer() {
        // Without a teamcolor layer, `apply_team_color` never runs, so two different owners'
        // drawables of the same image/frame/flip decode to byte-identical tiles -- the cache key
        // must collapse to one shared value instead of a distinct entry per owner color.
        assert_eq!(
            tile_key_color([244, 4, 4], false),
            tile_key_color([0, 66, 255], false),
        );
        // With a teamcolor layer, the color really does affect the output and must stay distinct.
        assert_ne!(
            tile_key_color([244, 4, 4], true),
            tile_key_color([0, 66, 255], true),
        );
    }

    /// A drawable placed far enough outside the output image that no legitimate frame could ever
    /// reach back onto it must never trigger its `.anim`'s fetch at all -- not even the byte
    /// read, let alone parsing or decoding it.
    #[test]
    fn offscreen_drawable_never_reads_its_anim() {
        use std::cell::RefCell;
        use std::sync::Arc;

        struct CountingSource<'a> {
            inner: &'a MemorySource,
            reads: RefCell<Vec<AssetRequest>>,
        }
        impl TilesetDataSource for CountingSource<'_> {
            fn read(&self, req: &AssetRequest) -> Result<Arc<[u8]>, crate::source::SourceError> {
                self.reads.borrow_mut().push(req.clone());
                self.inner.read(req)
            }
        }

        let frame = AnimFrame {
            texture_x: 0,
            texture_y: 0,
            offset_x: 0,
            offset_y: 0,
            width: 32,
            height: 32,
        };
        let mut source = MemorySource::new();
        for image_id in [0u16, 1] {
            source.insert(
                AssetRequest::Anim {
                    image_id,
                    tier: AssetTier::Hd2,
                    pack: ArtPack::Standard,
                },
                anim_bytes(2, (32, 32), frame, &solid_bc1_dds(16, 16, 0xF800)),
            );
        }
        let counting = CountingSource {
            inner: &source,
            reads: RefCell::new(Vec::new()),
        };

        let mut image = RgbaImage {
            width: 128,
            height: 128,
            data: vec![0u8; 128 * 128 * 4],
        };
        let drawables = [
            Drawable {
                art_image_id: 0,
                frame: 0,
                flip: false,
                x: 64,
                y: 64,
                owner: 0,
                layer: 0,
                is_shadow: false,
            },
            // Placed absurdly far away: at zoom 1 this is tens of thousands of px off a 128x128
            // image, so no legitimate frame could ever reach back onto it.
            Drawable {
                art_image_id: 1,
                frame: 0,
                flip: false,
                x: 60_000,
                y: 60_000,
                owner: 0,
                layer: 0,
                is_shadow: false,
            },
        ];

        draw_overlay(
            &mut image,
            &drawables,
            &PlayerColors::default(),
            &counting,
            AssetTier::Hd2,
            ArtPack::Standard,
            1.0,
        );

        let requested_ids: Vec<u16> = counting
            .reads
            .borrow()
            .iter()
            .filter_map(|r| match r {
                AssetRequest::Anim { image_id, .. } => Some(*image_id),
                _ => None,
            })
            .collect();
        assert_eq!(
            requested_ids,
            vec![0],
            "the offscreen drawable's image must never be fetched"
        );
    }

    // -----------------------------------------------------------------------------------------
    // Prefetch
    // -----------------------------------------------------------------------------------------

    #[test]
    fn required_preview_graphics_dedupes_filters_and_applies_the_carbot_quirk() {
        let data = game_data();
        // The start location sits far from the other units: the default unit filter is Melee,
        // whose start-area clearing would otherwise prune everything near it.
        let units = [
            unit(0, Some(11), 1, 1),
            unit(0, Some(11), 2, 2), // same image: deduplicated
            unit(176, Some(11), 3, 3),
            unit(UNIT_ID_START_LOCATION, Some(0), 900, 900),
        ];

        let opts = RenderOptions {
            art_style: ArtStyle::Cartooned,
            max_dimension: Some(1024),
            start_locations: StartLocations::Sprite,
            ..Default::default()
        };
        let requests = required_preview_graphics(&units, &[], &data, 64, 64, &opts);
        // Everything resolves to image 0 except the start location's image 588.
        assert_eq!(requests.len(), 2, "{requests:?}");
        assert!(requests.contains(&AssetRequest::Anim {
            image_id: 0,
            tier: AssetTier::Hd2,
            pack: ArtPack::Carbot,
        }));
        assert!(
            requests.contains(&AssetRequest::Anim {
                image_id: IMAGE_ID_START_LOCATION,
                tier: AssetTier::Hd2,
                pack: ArtPack::Standard,
            }),
            "Carbot ships no start-location art, so 588 falls back to the standard pack: \
             {requests:?}"
        );

        // Filtering prunes the request list too.
        let no_resources = RenderOptions {
            show_resources: false,
            start_locations: StartLocations::Hidden,
            ..opts.clone()
        };
        assert_eq!(
            required_preview_graphics(&units, &[], &data, 64, 64, &no_resources).len(),
            1
        );

        // An Original unit layer requests no art at all (mainSD.anim isn't supported yet).
        let original = RenderOptions {
            unit_style: Some(ArtStyle::Original),
            ..opts
        };
        assert!(required_preview_graphics(&units, &[], &data, 64, 64, &original).is_empty());
    }

    #[test]
    fn required_preview_assets_includes_the_tables_unless_nothing_could_use_them() {
        let opts = RenderOptions {
            art_style: ArtStyle::Remastered,
            max_dimension: Some(1024),
            ..Default::default()
        };
        let assets = crate::required_preview_assets(Tileset::Jungle, 64, 64, &opts);
        assert_eq!(assets.len(), 7, "2 terrain + 4 dats + images.rel");
        assert!(assets.contains(&AssetRequest::ImagesRel));

        // Original units with start locations hidden/sprite-drawn: nothing in the render could
        // possibly touch the tables (no unit art, no ColorBlock token to size), so round 1 is
        // terrain-only.
        for no_color_block in [StartLocations::Hidden, StartLocations::Sprite] {
            let original_no_tables = RenderOptions {
                unit_style: Some(ArtStyle::Original),
                start_locations: no_color_block,
                ..opts.clone()
            };
            assert_eq!(
                crate::required_preview_assets(Tileset::Jungle, 64, 64, &original_no_tables),
                crate::required_terrain_assets(Tileset::Jungle, 64, 64, &original_no_tables),
                "{no_color_block:?}"
            );
        }

        // Original units but the *default* ColorBlock start locations: the tables are still
        // needed, just to size the token from units.dat's placebox, even with no unit art at
        // all.
        let original_colorblock = RenderOptions {
            unit_style: Some(ArtStyle::Original),
            ..opts
        };
        assert_eq!(
            crate::required_preview_assets(Tileset::Jungle, 64, 64, &original_colorblock).len(),
            7,
            "ColorBlock alone must still pull in the tables"
        );
    }

    // -----------------------------------------------------------------------------------------
    // Chk-based prefetch wrappers
    // -----------------------------------------------------------------------------------------

    fn lost_temple_chk() -> broodmap::Chk {
        let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../broodmap/assets/lt.scm");
        let map_bytes = std::fs::read(path).expect("lt.scm should exist in broodmap/assets");
        let (chk, _mpq) =
            broodmap::extract_chk_from_map(&map_bytes, None, None).expect("lt.scm should parse");
        chk
    }

    /// A `Chk` with the required VER/DIM/ERA chunks but deliberately no MTXM chunk, so
    /// `.terrain()` fails — used to check the wrappers' empty-list fallback.
    fn chk_without_terrain() -> broodmap::Chk {
        fn chunk(tag: &[u8; 4], data: &[u8]) -> Vec<u8> {
            let mut out = Vec::new();
            out.extend_from_slice(tag);
            out.extend_from_slice(&(data.len() as i32).to_le_bytes());
            out.extend_from_slice(data);
            out
        }

        let mut data = Vec::new();
        data.extend(chunk(b"VER ", &205u16.to_le_bytes())); // Brood War 1.04
        data.extend(chunk(b"STR ", &0u16.to_le_bytes())); // empty string table
        let mut dim = Vec::new();
        dim.extend_from_slice(&4u16.to_le_bytes());
        dim.extend_from_slice(&4u16.to_le_bytes());
        data.extend(chunk(b"DIM ", &dim));
        data.extend(chunk(b"ERA ", &0u16.to_le_bytes())); // Badlands
        // No MTXM chunk at all.

        broodmap::Chk::from_bytes(data, None).expect("minimal CHK should parse")
    }

    #[test]
    fn required_preview_assets_for_chk_matches_the_manual_call() {
        let chk = lost_temple_chk();
        let terrain = chk.terrain().unwrap();
        let opts = RenderOptions {
            art_style: ArtStyle::Remastered,
            max_dimension: Some(512),
            ..Default::default()
        };
        assert_eq!(
            required_preview_assets_for_chk(&chk, &opts),
            crate::required_preview_assets(
                chk.tileset(),
                terrain.width as u32,
                terrain.height as u32,
                &opts,
            )
        );
    }

    #[test]
    fn required_preview_graphics_for_chk_matches_the_manual_call() {
        let chk = lost_temple_chk();
        let terrain = chk.terrain().unwrap();
        let data = game_data(); // synthetic/zeroed tables -- content doesn't matter here

        let opts = RenderOptions {
            art_style: ArtStyle::Remastered,
            max_dimension: Some(512),
            ..Default::default()
        };
        let units = chk.placed_units().unwrap();
        let sprites = chk.sprites().unwrap();
        assert_eq!(
            required_preview_graphics_for_chk(&chk, &data, &opts),
            required_preview_graphics(
                units,
                sprites,
                &data,
                terrain.width as u32,
                terrain.height as u32,
                &opts,
            )
        );
    }

    #[test]
    fn chk_wrapper_prefetch_is_empty_for_unreadable_terrain() {
        let chk = chk_without_terrain();
        assert!(chk.terrain().is_err());

        let opts = RenderOptions::default();
        assert!(required_preview_assets_for_chk(&chk, &opts).is_empty());

        let data = game_data();
        assert!(required_preview_graphics_for_chk(&chk, &data, &opts).is_empty());
    }
}
