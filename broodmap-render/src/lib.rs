//! Renders StarCraft: Brood War map previews from a parsed [`broodmap::chk::Chk`] plus SC:R
//! (Remastered) game assets.
//!
//! This is phases 1-2 of the design in `docs/render-design.md` (see that file for the full plan,
//! including minimap rendering and the plan/execute split for GPU consumers): terrain plus the
//! unit/sprite overlay, with draw-time downscaling so memory stays proportional to the requested
//! output size rather than the source assets' native resolution.
//!
//! ```text
//! &Chk -> render_chk_preview(chk, &source, &options) -> Preview { image, warnings }
//! ```
//!
//! [`render_terrain`] renders terrain alone; [`render_preview`] takes the pieces (terrain,
//! units, sprites, player colors) individually for callers that already have them.
//!
//! Callers that must prefetch (WASM, where fetches are async and the render is sync) use the
//! two-round API: [`required_preview_assets`] names the terrain assets and `.dat`/`.rel` tables,
//! then [`required_preview_graphics`] names the `.anim` files those tables resolve the map's
//! units and sprites to.
//!
//! Assets are fetched through the [`TilesetDataSource`] trait, keeping all game-data knowledge
//! (paths, tiers, filenames) in this crate while sources stay dumb byte fetchers — see
//! `docs/render-design.md`, "Data sources". [`MemorySource`] is always available (and is the
//! primary WASM pattern: prefetch async, then render sync); [`DirSource`] (feature `fs`) and
//! [`CascSource`] (feature `casc`) read from a plain directory or a SC:R install/CDN,
//! respectively.
//!
//! Following the rest of the workspace's conventions (`AGENTS.md`): permissive parsing (garbage
//! input never panics), `thiserror` error types, and a pure core that compiles for
//! `wasm32-unknown-unknown` (`std::fs` and `broodcasc` stay strictly behind the `fs`/`casc`
//! features).

mod bc;
mod error;
mod gamedata;
mod image;
mod minimap;
mod options;
mod overlay;
mod resample;
mod source;
mod terrain;
mod tier;
mod token;

pub use error::RenderError;
pub use gamedata::GameData;
#[cfg(feature = "png")]
pub use image::PngEncodeError;
pub use image::RgbaImage;
pub use minimap::{
    MinimapOptions, RESOURCE_MINIMAP_COLOR, build_minimap_table, compress_minimap_table,
    render_chk_minimap, render_minimap,
};
pub use options::{
    DEFAULT_MAX_OUTPUT_PIXELS, RenderOptions, StartLocations, UnitFilter, required_preview_assets,
    required_terrain_assets,
};
pub use overlay::{
    Preview, render_chk_preview, render_preview, render_preview_with_warnings,
    required_preview_assets_for_chk, required_preview_graphics, required_preview_graphics_for_chk,
};
pub use source::{AssetRequest, DatKind, MemorySource, SourceError, TilesetDataSource};
pub use terrain::render_terrain;
pub use tier::{ArtPack, ArtStyle, AssetTier};

#[cfg(feature = "casc")]
pub use source::CascSource;
#[cfg(feature = "fs")]
pub use source::DirSource;

// The BC decoders (`bc`), box-downscaler (`image::scale_rgba`) and terrain resampler
// (`resample`) are deliberately NOT public:
// they accept unrestricted dimensions and rely on their bounded in-crate call sites (decode
// dims capped, ppt <= 128) for memory safety margins. If a future consumer (minimap table
// generator, `.anim` renderer) needs them externally, wrap them in checked, limit-aware APIs
// first.
