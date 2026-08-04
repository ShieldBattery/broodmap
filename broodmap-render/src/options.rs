//! Rendering options and the (options, map size) -> (tier, px-per-tile) resolution logic.

use crate::source::AssetRequest;
use crate::tier::{ArtPack, ArtStyle, AssetTier};
use broodmap::chk::tileset::Tileset;

/// Default for [`RenderOptions::max_output_pixels`]: 64Mi pixels (a 256 MiB RGBA buffer;
/// 8192x8192 for a square output). Large enough for a 128x128-tile map at HD2's native 64
/// px/tile, while keeping a full 256x256 map's default render around 32 px/tile instead of a
/// silent multi-GiB allocation.
pub const DEFAULT_MAX_OUTPUT_PIXELS: u32 = 64 * 1024 * 1024;

/// Options controlling how a map is rendered.
///
/// See `docs/render-design.md` for the full design; this milestone (terrain-only rendering)
/// implements the art style and output sizing. Later milestones add unit/sprite filtering,
/// creep toggles, and start-location display.
#[derive(Debug, Clone)]
pub struct RenderOptions {
    /// Which art to render. The concrete asset tier (HD vs. HD2 for the Remastered-family
    /// styles) is derived from this plus the output size — see [`ArtStyle`].
    pub art_style: ArtStyle,
    /// Target maximum output dimension in pixels. `None` renders at the native resolution of
    /// the chosen tier (`map dimension in tiles * tier.tile_px()`), subject to
    /// `max_output_pixels`.
    pub max_dimension: Option<u32>,
    /// Hard budget on total output pixels (width * height). The resolution is clamped down —
    /// never errored — until the output fits, so rendering always succeeds and the output
    /// buffer never exceeds `max_output_pixels * 4` bytes. Raise this if you genuinely want
    /// huge renders (e.g. a full 256x256 map at HD2/HD native resolution).
    pub max_output_pixels: u32,
}

impl Default for RenderOptions {
    fn default() -> Self {
        Self {
            art_style: ArtStyle::default(),
            max_dimension: None,
            max_output_pixels: DEFAULT_MAX_OUTPUT_PIXELS,
        }
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
        };
        let (tier, _, ppt) = resolve_tier(&opts, 256, 256);
        assert_eq!(tier, AssetTier::Hd2);
        assert_eq!(ppt, 64); // full HD2 native once the budget allows it

        // Tiny budget: floors at 1 px/tile rather than erroring or dividing to zero.
        let opts = RenderOptions {
            art_style: ArtStyle::Remastered,
            max_dimension: None,
            max_output_pixels: 16,
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
