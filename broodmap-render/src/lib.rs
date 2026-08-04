//! Renders StarCraft: Brood War map previews from a parsed [`broodmap::chk::Chk`] plus SC:R
//! (Remastered) game assets.
//!
//! This is phase 1 of the design in `docs/render-design.md` (see that file for the full plan,
//! including unit/sprite overlays, minimap rendering, and the plan/execute split for GPU
//! consumers): terrain-only rendering, with draw-time downscaling so memory stays proportional
//! to the requested output size rather than the source assets' native resolution.
//!
//! ```text
//! Chk::terrain() + Chk::tileset() -> render_terrain(..., &source, &options) -> RgbaImage
//! ```
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
mod image;
mod options;
mod source;
mod terrain;
mod tier;

#[cfg(feature = "png")]
pub use image::PngEncodeError;
pub use image::RgbaImage;
pub use options::{DEFAULT_MAX_OUTPUT_PIXELS, RenderOptions, required_terrain_assets};
pub use source::{AssetRequest, MemorySource, SourceError, TilesetDataSource};
pub use terrain::{RenderError, render_terrain};
pub use tier::{ArtPack, ArtStyle, AssetTier};

#[cfg(feature = "casc")]
pub use source::CascSource;
#[cfg(feature = "fs")]
pub use source::DirSource;

// The BC decoders (`bc`) and box-downscaler (`image::scale_rgba`) are deliberately NOT public:
// they accept unrestricted dimensions and rely on their bounded in-crate call sites (decode
// dims capped, ppt <= 128) for memory safety margins. If a future consumer (minimap table
// generator, `.anim` renderer) needs them externally, wrap them in checked, limit-aware APIs
// first.
