//! Parser for `.vx4`/`.vx4ex` files: a tileset's megatile-to-minitile graphic reference table.
//!
//! Each entry describes one megatile as a 4x4, row-major grid of 16 minitile references (index
//! into the tileset's `.vr4` bitmap table -- see [`crate::vr4`] -- plus a horizontal-flip bit).
//! There is no file header: the format is a flat, fixed-size record array, headerless like CV5
//! and VF4 (see `cv5.rs`/`vf4.rs`). Two entry widths exist:
//!
//! | Variant           | Bytes/entry | Bytes/value | Value layout                                    |
//! |--------------------|------------:|------------:|--------------------------------------------------|
//! | Classic `.vx4`      |          32 |    2 (`u16`) | bit 0 = h-flip; bits 15..1 = VR4 minitile index |
//! | Extended `.vx4ex`   |          64 |    4 (`u32`) | bit 0 = h-flip; bits 31..1 = VR4 minitile index |
//!
//! Both variants share identical semantics, just different value widths; [`Vx4`] hides the
//! distinction behind one type. Megatile ID == entry index (0-based), matching CV5's
//! `mega_tiles`/VF4's `mega_tile_id` and VR4's minitile index space.
//!
//! SC:R's CASC catalog ships only `.vx4ex` -- there is no classic `.vx4` in a real install
//! (verified: `jungle.vx4ex` is 450,432 bytes = 7,038 megatiles at 64 bytes/entry). Classic
//! `.vx4` support exists for other asset sources (e.g. non-Remastered installs, mod tooling);
//! `bw-chk` handles both for the same reason. See `docs/render-design.md`'s "Minimap" section --
//! this parser is a **dev-time** input to the minimap color-table generator (phase 3: baking a
//! megatile-ID -> RGB table per tileset), not something the render-time crate loads.

/// Number of minitile references per megatile (a 4x4, row-major grid).
const MINITILES_PER_MEGATILE: usize = 16;
/// Byte width of one minitile value in a classic `.vx4` file.
const CLASSIC_VALUE_SIZE: usize = 2;
/// Byte width of one minitile value in an extended `.vx4ex` file.
const EXTENDED_VALUE_SIZE: usize = 4;

/// A single minitile reference decoded from a VX4/VX4EX entry: which VR4 minitile bitmap to draw
/// (see [`crate::vr4::Vr4::bitmap`]) and whether to mirror it horizontally.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct MinitileRef {
    pub vr4_index: u32,
    pub flipped: bool,
}

/// A parsed `.vx4`/`.vx4ex` file: the megatile -> minitile reference table for a tileset. Borrows
/// from the input; entry width (classic `u16` vs extended `u32`) is recorded internally and is
/// transparent to callers -- both variants decode through the same [`MinitileRef`] shape.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Vx4<'a> {
    data: &'a [u8],
    value_size: usize,
    megatile_count: usize,
}

impl<'a> Vx4<'a> {
    fn parse(data: &'a [u8], value_size: usize) -> Vx4<'a> {
        let record_size = MINITILES_PER_MEGATILE * value_size;
        let megatile_count = data.len() / record_size;
        Vx4 {
            data,
            value_size,
            megatile_count,
        }
    }

    /// The number of complete megatile entries in the file (any trailing partial record is
    /// ignored, matching the rest of this crate's headerless-table parsers).
    pub fn megatile_count(&self) -> usize {
        self.megatile_count
    }

    /// The minitile reference at `index` (0..16, row-major within the megatile's 4x4 grid) of
    /// `megatile`. `None` if either is out of range.
    pub fn minitile(&self, megatile: usize, index: usize) -> Option<MinitileRef> {
        if megatile >= self.megatile_count || index >= MINITILES_PER_MEGATILE {
            return None;
        }
        let record_size = MINITILES_PER_MEGATILE * self.value_size;
        let offset = megatile * record_size + index * self.value_size;
        let bytes = &self.data[offset..offset + self.value_size];

        Some(if self.value_size == CLASSIC_VALUE_SIZE {
            let raw = u16::from_le_bytes(bytes.try_into().unwrap());
            MinitileRef {
                flipped: raw & 1 != 0,
                vr4_index: (raw >> 1) as u32,
            }
        } else {
            let raw = u32::from_le_bytes(bytes.try_into().unwrap());
            MinitileRef {
                flipped: raw & 1 != 0,
                vr4_index: raw >> 1,
            }
        })
    }
}

/// Parses a classic `.vx4` file: 16 x `u16` per megatile (32 bytes/entry).
pub fn parse_vx4(data: &[u8]) -> Vx4<'_> {
    Vx4::parse(data, CLASSIC_VALUE_SIZE)
}

/// Parses an extended `.vx4ex` file: 16 x `u32` per megatile (64 bytes/entry). This is the only
/// layout SC:R's CASC catalog ships -- see the module docs.
pub fn parse_vx4ex(data: &[u8]) -> Vx4<'_> {
    Vx4::parse(data, EXTENDED_VALUE_SIZE)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Builds a classic `.vx4` entry (16 `u16`s) from `(vr4_index, flipped)` pairs.
    fn classic_entry(minitiles: &[(u16, bool)]) -> Vec<u8> {
        let mut data = Vec::with_capacity(MINITILES_PER_MEGATILE * CLASSIC_VALUE_SIZE);
        for &(vr4_index, flipped) in minitiles {
            let raw = (vr4_index << 1) | flipped as u16;
            data.extend_from_slice(&raw.to_le_bytes());
        }
        data
    }

    /// Builds an extended `.vx4ex` entry (16 `u32`s) from `(vr4_index, flipped)` pairs.
    fn extended_entry(minitiles: &[(u32, bool)]) -> Vec<u8> {
        let mut data = Vec::with_capacity(MINITILES_PER_MEGATILE * EXTENDED_VALUE_SIZE);
        for &(vr4_index, flipped) in minitiles {
            let raw = (vr4_index << 1) | flipped as u32;
            data.extend_from_slice(&raw.to_le_bytes());
        }
        data
    }

    #[test]
    fn parses_classic_two_megatiles_with_known_flip_bits() {
        let megatile_a: Vec<(u16, bool)> = (0..16).map(|i| (i as u16 * 3, i % 2 == 0)).collect();
        let megatile_b = [(0u16, false); 16];

        let mut data = classic_entry(&megatile_a);
        data.extend(classic_entry(&megatile_b));

        let vx4 = parse_vx4(&data);
        assert_eq!(vx4.megatile_count(), 2);

        for (i, &(vr4_index, flipped)) in megatile_a.iter().enumerate() {
            assert_eq!(
                vx4.minitile(0, i),
                Some(MinitileRef {
                    vr4_index: vr4_index as u32,
                    flipped
                })
            );
        }
        assert_eq!(
            vx4.minitile(1, 0),
            Some(MinitileRef {
                vr4_index: 0,
                flipped: false
            })
        );
        assert!(vx4.minitile(2, 0).is_none());
        assert!(vx4.minitile(0, 16).is_none());
    }

    #[test]
    fn extended_matches_classic_for_the_same_logical_content() {
        let minitiles: Vec<(u32, bool)> = (0..16).map(|i| (1000 + i as u32, i % 3 == 0)).collect();
        let classic_minitiles: Vec<(u16, bool)> =
            minitiles.iter().map(|&(v, f)| (v as u16, f)).collect();

        let classic_data = classic_entry(&classic_minitiles);
        let extended_data = extended_entry(&minitiles);

        let vx4 = parse_vx4(&classic_data);
        let vx4ex = parse_vx4ex(&extended_data);

        assert_eq!(vx4.megatile_count(), 1);
        assert_eq!(vx4ex.megatile_count(), 1);

        for i in 0..MINITILES_PER_MEGATILE {
            assert_eq!(vx4.minitile(0, i), vx4ex.minitile(0, i));
        }
    }

    #[test]
    fn truncated_tail_is_ignored() {
        let mut data = classic_entry(&[(5, true); 16]);
        data.extend_from_slice(&[0xAB; 10]); // partial trailing entry

        let vx4 = parse_vx4(&data);
        assert_eq!(vx4.megatile_count(), 1);
        assert!(vx4.minitile(1, 0).is_none());
    }

    #[test]
    fn empty_input_yields_no_megatiles() {
        let vx4 = parse_vx4(&[]);
        assert_eq!(vx4.megatile_count(), 0);
        assert!(vx4.minitile(0, 0).is_none());

        let vx4ex = parse_vx4ex(&[]);
        assert_eq!(vx4ex.megatile_count(), 0);
        assert!(vx4ex.minitile(0, 0).is_none());
    }

    #[test]
    fn out_of_range_lookups_are_none_never_a_panic() {
        let data = classic_entry(&[(0, false); 16]);
        let vx4 = parse_vx4(&data);
        assert!(vx4.minitile(usize::MAX, 0).is_none());
        assert!(vx4.minitile(0, usize::MAX).is_none());
    }

    #[test]
    fn high_bit_indices_round_trip_through_extended_but_not_classic() {
        // A VR4 index that doesn't fit in a classic u16's 15 index bits (>0x7FFF).
        let big_index = 0x1_0000u32;
        let data = extended_entry(&[(big_index, true); 16]);
        let vx4ex = parse_vx4ex(&data);
        assert_eq!(
            vx4ex.minitile(0, 0),
            Some(MinitileRef {
                vr4_index: big_index,
                flipped: true
            })
        );
    }
}
