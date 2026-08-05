//! Parser for `.wpe` files: a tileset's 256-color palette.
//!
//! Headerless, fixed-size: exactly 256 entries of `{r, g, b, pad}`, 4 bytes each, in RGB order.
//! The `pad` byte carries no data (matches the same convention as `.dds.vr4`'s embedded palette,
//! see [`crate::dds_vr4::Palette`]'s docs). A real `.wpe` file is always exactly 1024 bytes (256
//! x 4); this parser requires at least that much and ignores anything past it.
//!
//! Unlike the other formats in this module, [`Wpe`] copies its 256 colors (768 bytes: `r`, `g`,
//! `b` per entry, dropping the unused `pad` byte) into an owned array rather than borrowing from
//! the input. The table is small, always read in full by the minimap generator, and dropping
//! `pad` at parse time keeps [`Wpe::color`] a plain infallible array index instead of a
//! stride-4-skip-1 read -- worth the copy here, unlike the large payloads (DDS pixel data, VR4
//! bitmaps) the rest of this crate borrows.
//!
//! See `docs/render-design.md`'s "Minimap" section: this is the final step of the dev-time
//! minimap color-table generator (phase 3) -- a VX4/VR4-derived palette index is resolved through
//! this table to the RGB baked into the per-tileset minimap color table. Not used at render time.

use thiserror::Error;

/// Number of palette entries in a `.wpe` file.
const ENTRY_COUNT: usize = 256;
/// Byte size of one raw palette entry (`r`, `g`, `b`, `pad`).
const RAW_ENTRY_SIZE: usize = 4;
/// Minimum byte size of a valid `.wpe` file (256 raw entries).
const MIN_SIZE: usize = ENTRY_COUNT * RAW_ENTRY_SIZE;
/// Byte size of one stored (pad-dropped) entry.
const STORED_ENTRY_SIZE: usize = 3;

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum WpeError {
    #[error("data too short to contain a valid WPE palette (need {MIN_SIZE} bytes)")]
    TooShort,
}

/// A parsed `.wpe` file: a tileset's 256-color palette, RGB only (the source file's `pad` byte
/// is dropped at parse time -- see the module docs).
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Wpe([u8; ENTRY_COUNT * STORED_ENTRY_SIZE]);

impl Wpe {
    /// The `[r, g, b]` bytes of palette entry `index`. Every `u8` index is in range (256
    /// entries), so this never fails.
    pub fn color(&self, index: u8) -> [u8; 3] {
        let offset = index as usize * STORED_ENTRY_SIZE;
        [self.0[offset], self.0[offset + 1], self.0[offset + 2]]
    }
}

/// Parses a `.wpe` file. Errors with [`WpeError::TooShort`] if `data` is under 1024 bytes (256
/// entries x 4 bytes); any bytes past the 256th entry are ignored.
pub fn parse_wpe(data: &[u8]) -> Result<Wpe, WpeError> {
    if data.len() < MIN_SIZE {
        return Err(WpeError::TooShort);
    }

    let mut colors = [0u8; ENTRY_COUNT * STORED_ENTRY_SIZE];
    for i in 0..ENTRY_COUNT {
        let raw_offset = i * RAW_ENTRY_SIZE;
        let stored_offset = i * STORED_ENTRY_SIZE;
        colors[stored_offset..stored_offset + 3].copy_from_slice(&data[raw_offset..raw_offset + 3]);
    }

    Ok(Wpe(colors))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn raw_palette() -> Vec<u8> {
        let mut data = vec![0u8; MIN_SIZE];
        // Entry 0: opaque black, pad nonzero to prove it's dropped.
        data[0..4].copy_from_slice(&[0, 0, 0, 0xFF]);
        // Entry 1: distinctive color.
        data[4..8].copy_from_slice(&[0x12, 0x34, 0x56, 0x00]);
        // Entry 255: another distinctive color.
        data[255 * 4..255 * 4 + 4].copy_from_slice(&[0xAA, 0xBB, 0xCC, 0x99]);
        data
    }

    #[test]
    fn parses_exact_size_palette() {
        let wpe = parse_wpe(&raw_palette()).unwrap();
        assert_eq!(wpe.color(0), [0, 0, 0]);
        assert_eq!(wpe.color(1), [0x12, 0x34, 0x56]);
        assert_eq!(wpe.color(255), [0xAA, 0xBB, 0xCC]);
    }

    #[test]
    fn trailing_bytes_are_ignored() {
        let mut data = raw_palette();
        data.extend_from_slice(&[0xEE; 100]);

        let wpe = parse_wpe(&data).unwrap();
        assert_eq!(wpe.color(255), [0xAA, 0xBB, 0xCC]);
    }

    #[test]
    fn too_short_is_an_error() {
        assert_eq!(parse_wpe(&[]), Err(WpeError::TooShort));
        assert_eq!(parse_wpe(&[0u8; MIN_SIZE - 1]), Err(WpeError::TooShort));
    }

    #[test]
    fn exactly_minimum_size_parses() {
        let data = raw_palette();
        assert_eq!(data.len(), MIN_SIZE);
        assert!(parse_wpe(&data).is_ok());
    }
}
