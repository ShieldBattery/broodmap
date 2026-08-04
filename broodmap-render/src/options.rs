//! Rendering options and the (options, map size) -> (tier, px-per-tile) resolution logic.

use crate::source::{AssetRequest, DatKind};
use crate::tier::{ArtPack, ArtStyle, AssetTier};
use broodmap::chk::tileset::Tileset;

/// Default for [`RenderOptions::max_output_pixels`]: 64Mi pixels (a 256 MiB RGBA buffer;
/// 8192x8192 for a square output). Large enough for a 128x128-tile map at HD2's native 64
/// px/tile, while keeping a full 256x256 map's default render around 32 px/tile instead of a
/// silent multi-GiB allocation.
pub const DEFAULT_MAX_OUTPUT_PIXELS: u32 = 64 * 1024 * 1024;

/// How a map's start locations are drawn.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
pub enum StartLocations {
    /// A rounded, gradient-filled "player token" in the owning player's resolved color (see
    /// `crate::token`), sized to the start location's `units.dat` placebox and centered on the
    /// placed position. The pro-map-preview convention, and the default: the in-game
    /// start-location graphic reads poorly at preview scale.
    #[default]
    ColorBlock,
    /// The actual in-game start-location graphic (image ID 588).
    Sprite,
    /// Not drawn at all.
    Hidden,
}

/// Which placed units survive into the render.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
pub enum UnitFilter {
    /// Everything the map placed, as placed. The right choice for UMS maps (and for seeing
    /// what a map actually contains).
    AsPlaced,
    /// Melee rules: drop preplaced units owned by a real player slot, because the game removes
    /// them and hands the player starting workers instead. Neutral-owned units (resources,
    /// critters, neutral buildings) and start locations are kept, as are THG2 sprites.
    ///
    /// This is the default: previews overwhelmingly serve melee play, and the game applies
    /// exactly this filtering when a map is hosted as melee — regardless of what the map
    /// placed.
    #[default]
    Melee,
}

/// Options controlling how a map is rendered.
///
/// See `docs/render-design.md` for the full design. Defaults are preview-oriented: everything a
/// player navigating the map would want to see is on.
#[derive(Debug, Clone)]
pub struct RenderOptions {
    /// Which art to render. The concrete asset tier (HD vs. HD2 for the Remastered-family
    /// styles) is derived from this plus the output size — see [`ArtStyle`].
    pub art_style: ArtStyle,
    /// Render the unit/sprite layer in a different style than the terrain (e.g. Cartooned
    /// terrain with Remastered units). `None` (the default) uses `art_style` for both.
    ///
    /// [`ArtStyle::Original`] units need `mainSD.anim`, which isn't implemented yet: unit and
    /// sprite art is skipped (not an error) when this resolves to `Original`.
    pub unit_style: Option<ArtStyle>,
    /// Target maximum output dimension in pixels. `None` renders at the native resolution of
    /// the chosen tier (`map dimension in tiles * tier.tile_px()`), subject to
    /// `max_output_pixels`.
    pub max_dimension: Option<u32>,
    /// Hard budget on total output pixels (width * height). The resolution is clamped down —
    /// never errored — until the output fits, so rendering always succeeds. Resolution floors
    /// at 1 px/tile, though (see `resolve_tier`), so an absurdly small budget can't shrink the
    /// output to nothing: the output can exceed the budget by at most one pixel per tile — worst
    /// case 64Ki pixels over, on a full 256x256 map — rather than ever collapsing to an empty
    /// image. That overrun is intentional and bounded, not a bug. Raise this if you genuinely
    /// want huge renders (e.g. a full 256x256 map at HD2/HD native resolution).
    pub max_output_pixels: u32,
    /// How start locations are drawn. Defaults to [`StartLocations::ColorBlock`].
    pub start_locations: StartLocations,
    /// Which placed units survive into the render. Defaults to [`UnitFilter::Melee`].
    pub unit_filter: UnitFilter,
    /// Draw critters (Rhynadon, Bengalaas, Scantid, Kakaru, Ragnasaur, Ursadon). Default `true`.
    pub show_critters: bool,
    /// Draw mineral fields and vespene geysers. Default `true`.
    pub show_resources: bool,
    /// Draw THG2 doodad sprites (the "pure sprite" entries: trees, rocks, and similar map
    /// decoration). Default `true`.
    pub show_doodad_sprites: bool,
    /// Draw neutral-owned buildings: `UNIT`-chunk entries with no recorded owner or an owner
    /// slot outside the 8 real players, whose `units.dat` entry has the Building
    /// special-ability flag set (e.g. an unclaimed Zerg Extractor, or scenario-placed neutral
    /// structures). Never hides resources (mineral fields/vespene geysers have their own
    /// toggle, [`Self::show_resources`]) or start locations, and never affects player-owned
    /// buildings. Default `true`.
    pub show_neutral_buildings: bool,
}

impl Default for RenderOptions {
    fn default() -> Self {
        Self {
            art_style: ArtStyle::default(),
            unit_style: None,
            max_dimension: None,
            max_output_pixels: DEFAULT_MAX_OUTPUT_PIXELS,
            start_locations: StartLocations::default(),
            unit_filter: UnitFilter::default(),
            show_critters: true,
            show_resources: true,
            show_doodad_sprites: true,
            show_neutral_buildings: true,
        }
    }
}

impl RenderOptions {
    /// The art style the unit/sprite layer is drawn in: [`Self::unit_style`] if set, otherwise
    /// [`Self::art_style`].
    pub fn effective_unit_style(&self) -> ArtStyle {
        self.unit_style.unwrap_or(self.art_style)
    }

    /// Whether any unit/sprite `.anim` art will be drawn at all. `false` when the unit layer
    /// resolves to [`ArtStyle::Original`] (SD art needs `mainSD.anim`, a later phase), in which
    /// case the preview renders terrain plus start-location color blocks only.
    pub(crate) fn unit_art_available(&self) -> bool {
        self.effective_unit_style() != ArtStyle::Original
    }
}

/// The assets a [`crate::render_terrain`] call with these inputs will request from its
/// [`crate::TilesetDataSource`], so prefetching callers (e.g. WASM, where fetches are async and
/// the render is sync over a [`crate::MemorySource`]) know exactly what to load without
/// duplicating the renderer's tier-selection logic.
///
/// `map_w`/`map_h` are the map's dimensions in tiles (`TerrainTileIds::width`/`height`).
pub fn required_terrain_assets(
    tileset: Tileset,
    map_w: u32,
    map_h: u32,
    options: &RenderOptions,
) -> Vec<AssetRequest> {
    let (tier, pack, _) = resolve_tier(options, map_w, map_h);
    vec![
        AssetRequest::Cv5(tileset),
        AssetRequest::TilesetDds(tileset, tier, pack),
    ]
}

/// Resolves `(options, map width, map height)` (in tiles) to the [`AssetTier`] and [`ArtPack`]
/// to fetch assets from and the pixels-per-tile to render at.
///
/// - `desired_ppt` is derived from `max_dimension` by dividing it across the map's longer
///   dimension (so the output's largest side lands at or under `max_dimension`), floored to a
///   minimum of 1 tile pixel. `None` (no cap requested) defers to the chosen tier's native size.
/// - The tier follows the art style: `Original` is always `Sd` (the only resolution that art
///   exists at). The Remastered-family styles (`Remastered`/`Cartooned`) select `Hd` only when
///   the desired resolution exceeds 64px/tile (HD2's native size); everything else — including
///   no cap at all — gets the lighter `Hd2`, since HD2 is the same art and full HD is only
///   worth fetching/decoding when the output is actually large enough to show it. (There is
///   deliberately no way to force a tier: full-HD callers just request a big enough output.)
/// - The final pixels-per-tile is never more than the chosen tier's native size — this function
///   only ever downscales, never upscales, matching `docs/render-design.md`'s memory-bounding
///   requirement ("must never materialize" a full-resolution intermediate).
/// - `max_output_pixels` clamps the resolution further (before tier selection, so a
///   budget-shrunk render also fetches the cheaper tier): the largest `ppt` is chosen such that
///   `(map_w * ppt) * (map_h * ppt)` fits the budget, floored at 1 px/tile (a 256x256 map at 1
///   px/tile is 64Ki pixels, so the worst-case overrun is tiny and bounded by the map-size cap).
pub(crate) fn resolve_tier(
    options: &RenderOptions,
    map_w: u32,
    map_h: u32,
) -> (AssetTier, ArtPack, u32) {
    let longest_side = map_w.max(map_h).max(1);
    let tile_count = map_w.max(1) as u64 * map_h.max(1) as u64;
    let budget_ppt = ((options.max_output_pixels as u64 / tile_count).isqrt() as u32).max(1);

    let desired_ppt = options
        .max_dimension
        .map(|dim| (dim / longest_side).clamp(1, budget_ppt));

    let tier = match options.art_style {
        ArtStyle::Original => AssetTier::Sd,
        ArtStyle::Remastered | ArtStyle::Cartooned => match desired_ppt {
            Some(ppt) if ppt > 64 => AssetTier::Hd,
            _ => AssetTier::Hd2,
        },
    };

    let native_px = tier.tile_px();
    let px_per_tile = desired_ppt
        .unwrap_or(native_px)
        .min(native_px)
        .min(budget_ppt);

    (tier, options.art_style.pack(), px_per_tile)
}

/// Resolves the [`AssetTier`]/[`ArtPack`] the unit/sprite layer's `.anim` art comes from, given
/// the pixels-per-tile the terrain layer settled on (from [`resolve_tier`]).
///
/// The unit layer deliberately reuses the terrain layer's effective resolution rather than
/// re-deriving one: mixing tiers is fine (all drawing normalizes through draw-time scaling), so
/// the only question is which tier is worth *fetching*, and that's decided by how many output
/// pixels a tile actually gets. The HD/HD2 threshold matches [`resolve_tier`]'s.
pub(crate) fn resolve_unit_tier(options: &RenderOptions, px_per_tile: u32) -> (AssetTier, ArtPack) {
    let style = options.effective_unit_style();
    let tier = match style {
        ArtStyle::Original => AssetTier::Sd,
        ArtStyle::Remastered | ArtStyle::Cartooned => {
            if px_per_tile > 64 {
                AssetTier::Hd
            } else {
                AssetTier::Hd2
            }
        }
    };
    (tier, style.pack())
}

/// The round-1 assets a preview render needs: the terrain assets, plus the `.dat`/`.rel` tables
/// required to resolve placed units and THG2 sprites to image IDs.
///
/// This is the first half of the two-round prefetch API (see `docs/render-design.md`, "Prefetch
/// support"): which `.anim` files a map needs can only be known *after* these tables are loaded
/// and the CHK's units are resolved through them, which is what
/// [`crate::required_preview_graphics`] does.
///
/// The tables are included whenever *anything* the render does might read them: either the unit
/// layer needs them to resolve unit/sprite art (`RenderOptions::unit_art_available`), or
/// `start_locations` is [`StartLocations::ColorBlock`] — even with unit art unavailable
/// ([`ArtStyle::Original`]), the render still best-effort-loads `units.dat` to size the
/// color-block token from its placebox (falling back to a documented constant if that load
/// fails, so this is never a hard requirement, just an undeclared read if omitted from round 1).
/// They're omitted only when neither is true — i.e. [`ArtStyle::Original`] units with
/// `start_locations` set to [`StartLocations::Sprite`] or [`StartLocations::Hidden`], where
/// nothing in the render could possibly touch them.
pub fn required_preview_assets(
    tileset: Tileset,
    map_w: u32,
    map_h: u32,
    options: &RenderOptions,
) -> Vec<AssetRequest> {
    let mut assets = required_terrain_assets(tileset, map_w, map_h, options);
    if options.unit_art_available() || options.start_locations == StartLocations::ColorBlock {
        assets.extend([
            AssetRequest::Dat(DatKind::Units),
            AssetRequest::Dat(DatKind::Flingy),
            AssetRequest::Dat(DatKind::Sprites),
            AssetRequest::Dat(DatKind::Images),
            AssetRequest::ImagesRel,
        ]);
    }
    assets
}

#[cfg(test)]
mod tests {
    use super::*;

    fn options(art_style: ArtStyle, max_dimension: Option<u32>) -> RenderOptions {
        RenderOptions {
            art_style,
            max_dimension,
            ..Default::default()
        }
    }

    #[test]
    fn original_style_always_picks_sd() {
        let (tier, pack, ppt) = resolve_tier(&options(ArtStyle::Original, None), 64, 64);
        assert_eq!(tier, AssetTier::Sd);
        assert_eq!(pack, ArtPack::Standard);
        assert_eq!(ppt, 32); // native SD size, never upscaled

        let (tier, _, ppt) = resolve_tier(&options(ArtStyle::Original, Some(4096)), 64, 64);
        assert_eq!(tier, AssetTier::Sd);
        assert_eq!(ppt, 32); // desired_ppt (64) capped at SD's native 32
    }

    #[test]
    fn remastered_with_no_cap_picks_hd2_native() {
        // No cap does NOT mean "best available": HD2 is the same art, and full HD is only
        // fetched when the requested output actually exceeds HD2's resolution.
        let (tier, pack, ppt) = resolve_tier(&options(ArtStyle::Remastered, None), 128, 128);
        assert_eq!(tier, AssetTier::Hd2);
        assert_eq!(pack, ArtPack::Standard);
        assert_eq!(ppt, 64);
    }

    #[test]
    fn remastered_small_output_picks_hd2() {
        // 64x64 map, 1024px cap => desired_ppt = 1024/64 = 16, well under the 64 threshold.
        let (tier, _, ppt) = resolve_tier(&options(ArtStyle::Remastered, Some(1024)), 64, 64);
        assert_eq!(tier, AssetTier::Hd2);
        assert_eq!(ppt, 16);
    }

    #[test]
    fn remastered_large_output_picks_hd() {
        // 32x32 map, 4096px cap => desired_ppt = 128, over the 64 threshold.
        let (tier, _, ppt) = resolve_tier(&options(ArtStyle::Remastered, Some(4096)), 32, 32);
        assert_eq!(tier, AssetTier::Hd);
        assert_eq!(ppt, 128); // capped at HD's native size, not upscaled past it
    }

    #[test]
    fn cartooned_selects_carbot_pack_with_same_tier_logic() {
        let (tier, pack, ppt) = resolve_tier(&options(ArtStyle::Cartooned, Some(1024)), 64, 64);
        assert_eq!(tier, AssetTier::Hd2);
        assert_eq!(pack, ArtPack::Carbot);
        assert_eq!(ppt, 16);

        let (tier, pack, _) = resolve_tier(&options(ArtStyle::Cartooned, Some(4096)), 32, 32);
        assert_eq!(tier, AssetTier::Hd);
        assert_eq!(pack, ArtPack::Carbot);
    }

    #[test]
    fn never_upscales_past_native() {
        // Tiny map, huge cap => desired_ppt would be enormous; still capped at tier native.
        let (tier, _, ppt) = resolve_tier(&options(ArtStyle::Remastered, Some(1_000_000)), 1, 1);
        assert_eq!(tier, AssetTier::Hd);
        assert_eq!(ppt, 128);
    }

    #[test]
    fn zero_map_dimensions_do_not_panic() {
        let (_, _, ppt) = resolve_tier(&options(ArtStyle::Remastered, Some(64)), 0, 0);
        assert!(ppt >= 1);
    }

    #[test]
    fn desired_ppt_never_below_one() {
        // 512-tile map with a 1px cap would compute 0 without the floor.
        let (_, _, ppt) = resolve_tier(&options(ArtStyle::Remastered, Some(1)), 256, 256);
        assert_eq!(ppt, 1);
    }

    #[test]
    fn output_pixel_budget_clamps_native_renders() {
        // A 256x256 map with no max_dimension would be 16384^2 (~1 GiB RGBA) at HD2 native;
        // the default 64Mi-pixel budget clamps it to 32 px/tile (8192^2).
        let (tier, _, ppt) = resolve_tier(&options(ArtStyle::Remastered, None), 256, 256);
        assert_eq!(tier, AssetTier::Hd2);
        assert_eq!(ppt, 32);
    }

    #[test]
    fn output_pixel_budget_downgrades_tier_selection() {
        // Requesting a huge output of a big map would pick HD by size alone, but the budget
        // clamps the effective resolution to 32 px/tile first, so the cheaper HD2 is fetched.
        let (tier, _, ppt) = resolve_tier(&options(ArtStyle::Remastered, Some(32768)), 256, 256);
        assert_eq!(tier, AssetTier::Hd2);
        assert_eq!(ppt, 32);
    }

    #[test]
    fn output_pixel_budget_is_configurable() {
        let opts = RenderOptions {
            art_style: ArtStyle::Remastered,
            max_dimension: None,
            max_output_pixels: u32::MAX,
            ..Default::default()
        };
        let (tier, _, ppt) = resolve_tier(&opts, 256, 256);
        assert_eq!(tier, AssetTier::Hd2);
        assert_eq!(ppt, 64); // full HD2 native once the budget allows it

        // Tiny budget: floors at 1 px/tile rather than erroring or dividing to zero.
        let opts = RenderOptions {
            art_style: ArtStyle::Remastered,
            max_dimension: None,
            max_output_pixels: 16,
            ..Default::default()
        };
        let (_, _, ppt) = resolve_tier(&opts, 256, 256);
        assert_eq!(ppt, 1);
    }

    #[test]
    fn required_terrain_assets_match_render_requests() {
        use broodmap::chk::tileset::Tileset;

        let opts = options(ArtStyle::Cartooned, Some(1024));
        let assets = required_terrain_assets(Tileset::Jungle, 64, 64, &opts);
        assert_eq!(
            assets,
            vec![
                AssetRequest::Cv5(Tileset::Jungle),
                AssetRequest::TilesetDds(Tileset::Jungle, AssetTier::Hd2, ArtPack::Carbot),
            ]
        );
    }
}
