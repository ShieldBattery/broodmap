//! Parsers for StarCraft: Remastered asset file formats (CV5, VF4, `.dds.vr4`, DDS, ...).
//!
//! This crate is pure: no I/O, no async, and it compiles for `wasm32-unknown-unknown`. Parsing is
//! permissive, matching the rest of the workspace's philosophy (see the root `AGENTS.md`): garbage
//! input should produce garbage output or a clean error, never a panic, since these formats come
//! from untrusted map/asset files and are fuzzed. Where a format's payload is large (e.g. DDS
//! pixel data), parsers borrow from the input slice rather than copying.
//!
//! See `docs/render-design.md` in the workspace root for how these formats fit into the broader
//! rendering pipeline.

pub mod anim;
pub mod cv5;
pub mod dat;
pub mod dds;
pub mod dds_vr4;
pub mod rel;
pub mod tbl;
pub mod vf4;

pub use anim::{Anim, AnimError, AnimFrame, AnimLayer};
pub use cv5::{Cv5, TileGroup, TileGroupFlags, parse_cv5};
pub use dat::{
    FlingyDat, ImageEntry, ImagesDat, SpritesDat, UnitEntry, UnitsDat, parse_flingy_dat,
    parse_images_dat, parse_sprites_dat, parse_units_dat,
};
pub use dds::{DdsError, DdsFile, DdsFormat, parse_dds};
pub use dds_vr4::{DdsVr4, DdsVr4Error, Frame, Palette};
pub use rel::{ImagesRel, parse_images_rel};
pub use tbl::{Tbl, parse_tbl};
pub use vf4::{MiniTileFlags, Vf4, parse_vf4};
