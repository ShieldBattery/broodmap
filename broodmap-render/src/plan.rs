//! The public render plan: the compositor's decide/draw split, as plain data.
//!
//! [`crate::plan_preview`] resolves everything that requires *game knowledge* — CHK tile IDs
//! through CV5 to megatiles, units/sprites through the `.dat` chain to image IDs, frames,
//! filters, painter order, player colors — into a [`RenderPlan`]. Executing a plan
//! ([`crate::execute_plan`], the built-in CPU rasterizer) then only touches *art*: it fetches
//! the textures the plan's manifest names and draws exactly what the plan says. The built-in
//! preview render is literally plan-then-execute, so the plan cannot drift from what
//! [`crate::render_preview`] draws.
//!
//! The split exists for consumers that aren't this crate's rasterizer (see
//! `docs/render-design.md`, "Render plan"): a browser GPU renderer takes the plan plus the raw
//! BC-compressed payloads and draws quads itself, a server can plan once and execute many
//! times, and the plan is plain data — `serde` impls behind the `serde` feature — so it
//! crosses the WASM/JS boundary as JSON.
//!
//! # What an executor must do
//!
//! - **Terrain**: draw `terrain.megatiles` (row-major) using frame `megatile_id` of the
//!   tileset's `.dds.vr4`, at `px_per_tile` output pixels per tile (compositing at the art's
//!   native size and downscaling — see the "Terrain downscaling" design-doc section for what
//!   the CPU rasterizer does).
//! - **Sprites**: draw in list order (it *is* the painter order). For each sprite, the art is
//!   image `image_id`'s at `unit_tier`/`unit_pack`; the frame's placement follows from the
//!   `.anim` frame table plus the canvas rules (`(x, y)` is the canvas *centre* in logical
//!   pixels; 1 tile = 32 logical px; for SD art the true canvas is the width/height of the GRP
//!   header named by `sd_canvases`). `tint` is composited through the art's `teamcolor` mask
//!   layer if it has one; a sprite with `is_shadow` draws as a translucent black silhouette
//!   instead.
//! - **Blocks**: solid player-token rectangles (start locations), drawn over everything, each
//!   centred on `(x, y)` with the given logical-pixel size.

use broodmap::chk::tileset::Tileset;

use crate::source::AssetRequest;
use crate::tier::{ArtPack, AssetTier};

/// A fully-decided preview render: what to draw, in what order, from which assets — everything
/// except the pixels. Built by [`crate::plan_preview`]; drawable by [`crate::execute_plan`] or
/// any external executor (see the module docs for the contract).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct RenderPlan {
    /// Output image width in pixels (`terrain.width * px_per_tile`).
    pub width: u32,
    /// Output image height in pixels (`terrain.height * px_per_tile`).
    pub height: u32,
    /// Output pixels per map tile. The sprite-space zoom is `px_per_tile / 32` output pixels
    /// per logical pixel.
    pub px_per_tile: u32,
    /// The terrain layer.
    pub terrain: TerrainPlan,
    /// The tier every sprite's art comes from.
    pub unit_tier: AssetTier,
    /// The art pack every sprite's art comes from. (The one exception is baked in upstream:
    /// requests for the start-location image under the Carbot pack resolve to the standard
    /// pack, which `AssetRequest`-construction via the manifest already reflects.)
    pub unit_pack: ArtPack,
    /// The unit/sprite draw list, in painter order (draw front to back exactly as listed).
    pub sprites: Vec<PlannedSprite>,
    /// For an SD (`AssetTier::Sd`) unit layer: which classic GRP header supplies each image's
    /// true placement canvas (`mainSD.anim` declares 0x0 for every entry). One entry per
    /// distinct image that resolves one; images without an entry fall back to content
    /// centering. Always empty for HD/HD2 unit layers.
    pub sd_canvases: Vec<SdCanvasSource>,
    /// Start-location color-block tokens, drawn over everything else, in list order.
    pub blocks: Vec<PlannedBlock>,
    /// Every asset executing this plan reads, deduplicated: the terrain `.dds.vr4`, each
    /// distinct sprite image's art (per-image `.anim`s, or the one `MainSdAnim` for SD), and
    /// each `sd_canvases` GRP. A prefetching executor (the browser flow) fetches exactly this
    /// list; nothing outside it is touched at execute time.
    pub manifest: Vec<AssetRequest>,
}

/// The terrain layer of a [`RenderPlan`]: a resolved megatile grid plus which tileset texture
/// file its IDs index.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TerrainPlan {
    /// The map's tileset.
    #[cfg_attr(feature = "serde", serde(with = "tileset_serde"))]
    pub tileset: Tileset,
    /// The tier of the tileset texture file to sample.
    pub tier: AssetTier,
    /// The art pack of the tileset texture file to sample.
    pub pack: ArtPack,
    /// Map width in tiles (clamped to the CHK format's 256 maximum).
    pub width: u32,
    /// Map height in tiles (clamped to the CHK format's 256 maximum).
    pub height: u32,
    /// Row-major megatile IDs, `width * height` of them: each is the frame index to draw from
    /// the tileset's `.dds.vr4`. CV5 tile-group resolution has already been folded in.
    pub megatiles: Vec<u16>,
}

/// One sprite to draw: a resolved image/frame and where it goes. See the module docs for the
/// placement contract.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PlannedSprite {
    /// The image whose art to draw (post-`images.rel`-redirect — the ID that names real art).
    pub image_id: u16,
    /// The frame to draw, selected from game rules (facing, resource amount, tileset). Clamp
    /// to the art's real frame count when drawing.
    pub frame: u32,
    /// Whether the frame is mirrored horizontally (the western half of BW's 32 facings).
    pub flip: bool,
    /// Canvas-centre position, logical pixels (32 per tile).
    pub x: i32,
    /// Canvas-centre position, logical pixels.
    pub y: i32,
    /// The resolved team color, composited through the art's `teamcolor` mask layer if it has
    /// one (art without a mask draws identically for every tint). `None` only for shadows,
    /// which are never team-colored.
    pub tint: Option<[u8; 3]>,
    /// Whether this is a shadow underlay: drawn as a translucent black silhouette of its frame
    /// rather than the art's own colors.
    pub is_shadow: bool,
}

/// The classic GRP whose 6-byte header supplies `image_id`'s placement canvas in an SD unit
/// layer (see [`crate::AssetRequest::Grp`] and `broodmap_formats::grp`).
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct SdCanvasSource {
    /// The (post-redirect) image the canvas applies to.
    pub image_id: u16,
    /// The GRP's `images.tbl` path verbatim, backslash separators and all — exactly what
    /// [`crate::AssetRequest::Grp`] takes.
    pub grp_path: String,
}

/// A start-location color-block token: a solid rounded rectangle in the owning player's color
/// (see `crate::token` for how the CPU rasterizer styles it).
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PlannedBlock {
    /// Centre position, logical pixels.
    pub x: i32,
    /// Centre position, logical pixels.
    pub y: i32,
    /// Box width, logical pixels (the start location's `units.dat` placebox).
    pub width: u32,
    /// Box height, logical pixels.
    pub height: u32,
    /// The owning player's resolved color.
    pub color: [u8; 3],
}

/// Serializes `broodmap::chk::tileset::Tileset` as its stable CHK discriminant (0..=7).
/// `Tileset` lives in the `broodmap` crate, which has no serde support; its discriminants are
/// explicit and format-defined, so the raw value is the honest wire form.
#[cfg(feature = "serde")]
pub(crate) mod tileset_serde {
    use broodmap::chk::tileset::Tileset;
    use serde::{Deserialize, Deserializer, Serializer, de::Error};

    pub fn serialize<S: Serializer>(tileset: &Tileset, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_u8(*tileset as u8)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Tileset, D::Error> {
        match u8::deserialize(deserializer)? {
            0 => Ok(Tileset::Badlands),
            1 => Ok(Tileset::SpacePlatform),
            2 => Ok(Tileset::Installation),
            3 => Ok(Tileset::Ashworld),
            4 => Ok(Tileset::Jungle),
            5 => Ok(Tileset::Desert),
            6 => Ok(Tileset::Arctic),
            7 => Ok(Tileset::Twilight),
            other => Err(D::Error::custom(format!("invalid tileset {other}"))),
        }
    }
}
