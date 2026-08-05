//! Parser for classic (pre-Remastered) `.grp` sprite files -- **header only**.
//!
//! `.grp` is BW's original (SD-era) unit/sprite art format: a `u16` frame count, `u16` width,
//! `u16` height (all little-endian), followed by `frame_count` 8-byte frame records (`u8`
//! x_offset, `u8` y_offset, `u8` width, `u8` height, `u32` line_data_offset) and then RLE-encoded,
//! 8-bit paletted pixel data.
//!
//! This module deliberately parses **only the 6-byte header** -- frame records and RLE pixel
//! data are out of scope. The classic GRP *rendering* pipeline (RLE decode, palette application)
//! is a firm non-goal of this crate (see `docs/render-design.md`): SC:R ships fully rendered SD
//! art in `mainSD.anim` (see [`crate::mainsd`]), which is what this workspace actually draws. The
//! one thing a classic GRP is still needed for is its header's declared canvas size -- see below.
//!
//! # Why: `mainSD.anim`'s missing canvas
//!
//! Every one of `mainSD.anim`'s 999 entries declares a `0x0` canvas (`canvas_width`/
//! `canvas_height` in its entry header -- see [`crate::mainsd`]'s module docs), so a renderer
//! falling back to "centre the frame on its own bounding box" for every image erases the true
//! displacement between, say, a unit and its shadow (both end up centred on the same point --
//! hiding every SD shadow directly behind its owner). This was diagnosed and verified empirically
//! against a real SC:R install:
//!
//! - **`mainSD.anim`'s frame tables are the classic GRP frame tables, verbatim.** Every probed
//!   file's frame count and per-frame offsets/sizes are byte-identical between the two formats
//!   (7 files, 547 frames total): `marine.grp` 229/229, `tmashad.grp` 221/221, `avenger.grp`
//!   85/85, `geyser.grp` 8/8, `geyShad.grp` 4/4, `min01.grp` 4/4, `min01sha.grp` 4/4.
//! - **The missing canvas is the classic GRP header's declared width/height.** E.g. `marine.grp`
//!   is 64x64 with its shadow (`tmashad.grp`) 44x44; `min01.grp` is 64x96 with its shadow
//!   (`min01sha.grp`) 128x96; `geyser.grp` is 128x64 with its shadow (`geyShad.grp`) 160x64. BW
//!   itself anchors a classic GRP frame at `unit_pos - grp_width/2 + frame_offset_x` (same for
//!   y), so using the GRP header's dimensions as the canvas reproduces the HD/HD2 ground truth
//!   (which *does* carry a real per-frame canvas) to sub-pixel accuracy.
//!
//! So a renderer wanting correct SD placement needs the classic GRP's header purely for its
//! width/height -- never its frame table or pixel data, which `mainSD.anim` already supplies
//! pre-decoded (and, per the finding above, table-for-table identical to the GRP's own). See
//! `broodmap-render`'s overlay module for how the SD render path resolves an image id to its GRP
//! path (via `images.dat`'s `grp` column and `images.tbl`) and applies this override.
//!
//! Classic GRPs ship in SC:R's CASC catalog at `unit/<images.tbl string, with backslashes
//! flipped to forward slashes>` (e.g. the `images.tbl` entry `"terran\marine.grp"` lives at CASC
//! path `unit/terran/marine.grp`).

use thiserror::Error;

/// Byte size of the fixed GRP header (`frame_count`, `width`, `height`).
const HEADER_SIZE: usize = 6;

#[derive(Error, Debug, Clone, Eq, PartialEq)]
pub enum GrpError {
    #[error("data too short to contain a valid GRP header")]
    TooShort,
}

/// A classic GRP file's 6-byte header: frame count and canvas dimensions. See the module docs
/// for why this crate parses only this much of the format.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct GrpHeader {
    pub frame_count: u16,
    pub width: u16,
    pub height: u16,
}

/// Parses a classic GRP file's header only: `frame_count`, `width`, `height`, all little-endian.
/// Errors with [`GrpError::TooShort`] if `data` is under [`HEADER_SIZE`] (6) bytes; anything past
/// the header (frame records, RLE pixel data) is never read or validated -- see the module docs.
pub fn parse_grp_header(data: &[u8]) -> Result<GrpHeader, GrpError> {
    if data.len() < HEADER_SIZE {
        return Err(GrpError::TooShort);
    }
    Ok(GrpHeader {
        frame_count: u16::from_le_bytes(data[0..2].try_into().unwrap()),
        width: u16::from_le_bytes(data[2..4].try_into().unwrap()),
        height: u16::from_le_bytes(data[4..6].try_into().unwrap()),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_a_valid_header_ignoring_trailing_bytes() {
        let mut data = Vec::new();
        data.extend_from_slice(&229u16.to_le_bytes());
        data.extend_from_slice(&64u16.to_le_bytes());
        data.extend_from_slice(&64u16.to_le_bytes());
        // Trailing bytes (frame records / RLE data in a real file) are never read.
        data.extend_from_slice(&[0xABu8; 16]);

        let header = parse_grp_header(&data).unwrap();
        assert_eq!(
            header,
            GrpHeader {
                frame_count: 229,
                width: 64,
                height: 64,
            }
        );
    }

    #[test]
    fn empty_input_is_too_short() {
        assert_eq!(parse_grp_header(&[]), Err(GrpError::TooShort));
    }

    #[test]
    fn five_bytes_is_too_short() {
        assert_eq!(parse_grp_header(&[0u8; 5]), Err(GrpError::TooShort));
    }

    #[test]
    fn exactly_six_bytes_is_the_minimum_valid_header() {
        let data = [1, 0, 44, 0, 160, 0];
        let header = parse_grp_header(&data).unwrap();
        assert_eq!(
            header,
            GrpHeader {
                frame_count: 1,
                width: 44,
                height: 160,
            }
        );
    }
}
