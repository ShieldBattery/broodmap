//! Parser for `.vr4` files: a tileset's minitile bitmap table.
//!
//! Headerless, like VX4/CV5/VF4 (see `vx4.rs`/`cv5.rs`/`vf4.rs`): a flat array of fixed-size
//! records, one per minitile. Each record is a `64`-byte, 8x8, row-major grid of 8-bit palette
//! indices (index into the tileset's WPE palette -- see [`crate::wpe`]). Minitile ID == entry
//! index (0-based), matching [`crate::vx4::MinitileRef::vr4_index`]'s index space.
//!
//! Real `jungle.vr4` is 3,564,160 bytes = 55,690 minitiles (64 bytes each).
//!
//! See `docs/render-design.md`'s "Minimap" section: this parser is a **dev-time** input to the
//! minimap color-table generator (phase 3), not something the render-time crate loads. That
//! generator's whole reason for touching VR4 is a single byte per minitile: BW's minimap samples
//! byte 55 (row 6, column 7 of the 8x8 grid) of minitile `[0]` of each megatile's 4x4 grid, and
//! resolves that palette index through WPE to get the tile's minimap color. No averaging, no
//! other bytes read. `bitmap(index)?[55]` is that sample; there's no dedicated accessor for it
//! since indexing the returned array is already the whole operation.

/// Byte size of one minitile bitmap (an 8x8 grid of palette-index bytes).
const BITMAP_SIZE: usize = 64;

/// A parsed `.vr4` file: the minitile bitmap table for a tileset. Borrows from the input.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Vr4<'a> {
    data: &'a [u8],
    minitile_count: usize,
}

impl<'a> Vr4<'a> {
    /// The number of complete minitile bitmaps in the file (any trailing partial record is
    /// ignored).
    pub fn minitile_count(&self) -> usize {
        self.minitile_count
    }

    /// The 8x8, row-major palette-index bitmap for minitile `index`. `None` if out of range.
    pub fn bitmap(&self, index: usize) -> Option<&'a [u8; BITMAP_SIZE]> {
        if index >= self.minitile_count {
            return None;
        }
        let offset = index * BITMAP_SIZE;
        self.data[offset..offset + BITMAP_SIZE].try_into().ok()
    }
}

/// Parses a `.vr4` file: 64 bytes (an 8x8 grid of palette indices) per minitile.
pub fn parse_vr4(data: &[u8]) -> Vr4<'_> {
    Vr4 {
        data,
        minitile_count: data.len() / BITMAP_SIZE,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn bitmap_with(fill: u8, byte_55: u8) -> [u8; BITMAP_SIZE] {
        let mut bitmap = [fill; BITMAP_SIZE];
        bitmap[55] = byte_55;
        bitmap
    }

    #[test]
    fn parses_bitmaps_and_samples_byte_55() {
        let bitmap_a = bitmap_with(1, 0xAB);
        let bitmap_b = bitmap_with(2, 0xCD);

        let mut data = Vec::new();
        data.extend_from_slice(&bitmap_a);
        data.extend_from_slice(&bitmap_b);

        let vr4 = parse_vr4(&data);
        assert_eq!(vr4.minitile_count(), 2);
        assert_eq!(vr4.bitmap(0), Some(&bitmap_a));
        assert_eq!(vr4.bitmap(1), Some(&bitmap_b));
        assert_eq!(vr4.bitmap(0).unwrap()[55], 0xAB);
        assert_eq!(vr4.bitmap(1).unwrap()[55], 0xCD);
        assert!(vr4.bitmap(2).is_none());
    }

    #[test]
    fn truncated_tail_is_ignored() {
        let bitmap_a = bitmap_with(1, 0);
        let mut data = Vec::new();
        data.extend_from_slice(&bitmap_a);
        data.extend_from_slice(&[0xAB; 10]); // partial trailing entry

        let vr4 = parse_vr4(&data);
        assert_eq!(vr4.minitile_count(), 1);
        assert!(vr4.bitmap(1).is_none());
    }

    #[test]
    fn empty_input_yields_no_minitiles() {
        let vr4 = parse_vr4(&[]);
        assert_eq!(vr4.minitile_count(), 0);
        assert!(vr4.bitmap(0).is_none());
    }

    #[test]
    fn out_of_range_lookup_is_none_never_a_panic() {
        let data = bitmap_with(0, 0);
        let vr4 = parse_vr4(&data);
        assert!(vr4.bitmap(usize::MAX).is_none());
    }
}
