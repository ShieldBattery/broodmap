//! The renderer's error type.
//!
//! Deliberately small, because rendering is permissive by design (see `AGENTS.md`): anything
//! that can be skipped is skipped rather than erroring. Only a *whole file* that the render
//! cannot proceed without — the tileset's CV5 or megatile container, or the `.dat`/`.rel` tables
//! when the unit layer is actually being drawn — produces an error. Individual missing megatile
//! frames, missing `.anim` files, unsupported `.anim` containers and undecodable layers all just
//! drop the thing that would have been drawn.

use thiserror::Error;

use broodmap_formats::DdsVr4Error;

use crate::source::SourceError;

/// Errors rendering a map.
#[derive(Error, Debug)]
pub enum RenderError {
    #[error("failed to read tileset asset: {0}")]
    Source(#[from] SourceError),
    #[error("failed to parse tileset megatile texture container: {0}")]
    DdsVr4(#[from] DdsVr4Error),
}
