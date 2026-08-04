//! Parser for `.tbl` string tables (`arr.tbl`, `images.tbl`, etc.).
//!
//! Format: a `u16` count at offset 0, followed by that many `u16` byte offsets (absolute, from
//! the start of the file), each pointing at a NUL-terminated string. Strings are referenced
//! elsewhere by a 1-based index (0 meaning "none"), but [`Tbl::get`] takes the 0-based table
//! index directly — callers subtract 1 themselves.
//!
//! # Parsing cost
//!
//! Parsing only validates and stores each entry's byte offset — it never scans for the
//! terminating NUL or decodes any string data, so it's `O(entry count)` regardless of how long
//! (or how invalid) the string data itself is. A hostile file with many entries all pointing at
//! the same long, unterminated, invalid-UTF-8 tail is just as cheap to parse as a well-formed one:
//! without this, every entry would eagerly scan to the next NUL and Latin-1-decode into an owned
//! `String`, so duplicate offsets into one long tail would scan and allocate that tail's worth of
//! data once *per entry* instead of once total. The scan and decode happen lazily in [`Tbl::get`],
//! so their cost is `O(that entry's span)` and is only ever paid for entries actually looked up.

use std::borrow::Cow;

/// Size in bytes of the `u16` count field and each `u16` offset entry.
const ENTRY_SIZE: usize = 2;

/// A parsed `.tbl` string table. Stores the source bytes plus each entry's validated byte offset;
/// the string itself is scanned and decoded lazily in [`Tbl::get`] (see the module docs).
#[derive(Debug, Clone, Default)]
pub struct Tbl<'a> {
    data: &'a [u8],
    /// Each entry's validated byte offset into `data` (`None` if the entry's offset table slot
    /// was missing/truncated, or the offset it held pointed at or past EOF).
    offsets: Vec<Option<usize>>,
}

/// Parses a `.tbl` file. Parsing is permissive: a missing/truncated offset entry, an offset
/// pointing past the end of the file, or the count claiming more entries than the offset table
/// can actually hold all just yield `None` for the affected entries. A string with no NUL before
/// EOF reads to EOF. See the module docs for why this only validates offsets and does not scan or
/// decode any string data.
pub fn parse_tbl(data: &[u8]) -> Tbl<'_> {
    if data.len() < ENTRY_SIZE {
        return Tbl {
            data,
            offsets: Vec::new(),
        };
    }

    let count = u16::from_le_bytes(data[0..2].try_into().unwrap()) as usize;

    let offsets = (0..count)
        .map(|i| {
            let entry_offset = ENTRY_SIZE + i * ENTRY_SIZE;
            if entry_offset + ENTRY_SIZE > data.len() {
                return None;
            }
            let str_offset = u16::from_le_bytes(
                data[entry_offset..entry_offset + ENTRY_SIZE]
                    .try_into()
                    .unwrap(),
            ) as usize;
            if str_offset >= data.len() {
                None
            } else {
                Some(str_offset)
            }
        })
        .collect();

    Tbl { data, offsets }
}

/// Decodes the NUL-terminated string starting at `offset`, as UTF-8, falling back to Latin-1
/// (byte -> char) if the bytes aren't valid UTF-8. Caller guarantees `offset < data.len()`.
fn decode_string(data: &[u8], offset: usize) -> Cow<'_, str> {
    let rest = &data[offset..];
    let end = rest.iter().position(|&b| b == 0).unwrap_or(rest.len());
    let bytes = &rest[..end];

    match std::str::from_utf8(bytes) {
        Ok(s) => Cow::Borrowed(s),
        Err(_) => Cow::Owned(bytes.iter().map(|&b| b as char).collect()),
    }
}

impl<'a> Tbl<'a> {
    /// Looks up a string by its 0-based table index. `None` if out of range or the entry's offset
    /// was invalid. The NUL scan and UTF-8/Latin-1 decode happen here, lazily, and cost only
    /// `O(this entry's span)` — see the module docs.
    pub fn get(&self, index: u16) -> Option<Cow<'a, str>> {
        let offset = (*self.offsets.get(index as usize)?)?;
        Some(decode_string(self.data, offset))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Builds a `.tbl` file from a list of `Option<&str>` (`None` entries get an offset that
    /// points past the end of the file, to exercise the bad-offset path).
    fn build_tbl(entries: &[Option<&str>]) -> Vec<u8> {
        let header_size = ENTRY_SIZE + entries.len() * ENTRY_SIZE;
        let mut offsets = Vec::with_capacity(entries.len());
        let mut strings_blob = Vec::new();
        for entry in entries {
            match entry {
                Some(s) => {
                    offsets.push((header_size + strings_blob.len()) as u16);
                    strings_blob.extend_from_slice(s.as_bytes());
                    strings_blob.push(0);
                }
                None => {
                    offsets.push(0xFFFF); // deliberately out of range
                }
            }
        }

        let mut data = Vec::with_capacity(header_size + strings_blob.len());
        data.extend_from_slice(&(entries.len() as u16).to_le_bytes());
        for offset in offsets {
            data.extend_from_slice(&offset.to_le_bytes());
        }
        data.extend_from_slice(&strings_blob);
        data
    }

    #[test]
    fn looks_up_strings_by_zero_based_index() {
        let data = build_tbl(&[Some("Zerg"), Some("Terran"), Some("Protoss")]);
        let tbl = parse_tbl(&data);
        assert_eq!(tbl.get(0).as_deref(), Some("Zerg"));
        assert_eq!(tbl.get(1).as_deref(), Some("Terran"));
        assert_eq!(tbl.get(2).as_deref(), Some("Protoss"));
        assert_eq!(tbl.get(3).as_deref(), None);
    }

    #[test]
    fn latin1_fallback_for_invalid_utf8() {
        let header_size = ENTRY_SIZE + ENTRY_SIZE;
        let mut data = Vec::new();
        data.extend_from_slice(&1u16.to_le_bytes());
        data.extend_from_slice(&(header_size as u16).to_le_bytes());
        // 0xE9 is invalid as a UTF-8 continuation-less lead byte in this position; as Latin-1
        // it's 'é'.
        data.push(0xE9);
        data.push(0);

        let tbl = parse_tbl(&data);
        assert_eq!(tbl.get(0).as_deref(), Some("\u{E9}"));
        // The Latin-1 fallback is the one case that must own its bytes (they're transcoded, not a
        // valid-UTF-8 slice of the original data), so this entry is `Cow::Owned`.
        assert!(matches!(tbl.get(0), Some(Cow::Owned(_))));
    }

    #[test]
    fn valid_utf8_is_borrowed_not_owned() {
        let data = build_tbl(&[Some("Zerg")]);
        let tbl = parse_tbl(&data);
        assert!(matches!(tbl.get(0), Some(Cow::Borrowed(_))));
    }

    #[test]
    fn out_of_range_offset_yields_none_for_that_entry() {
        let data = build_tbl(&[Some("ok"), None, Some("also ok")]);
        let tbl = parse_tbl(&data);
        assert_eq!(tbl.get(0).as_deref(), Some("ok"));
        assert_eq!(tbl.get(1).as_deref(), None);
        assert_eq!(tbl.get(2).as_deref(), Some("also ok"));
    }

    #[test]
    fn missing_nul_reads_to_eof() {
        let header_size = ENTRY_SIZE + ENTRY_SIZE;
        let mut data = Vec::new();
        data.extend_from_slice(&1u16.to_le_bytes());
        data.extend_from_slice(&(header_size as u16).to_le_bytes());
        data.extend_from_slice(b"no nul terminator");
        // Deliberately no trailing 0 byte.

        let tbl = parse_tbl(&data);
        assert_eq!(tbl.get(0).as_deref(), Some("no nul terminator"));
    }

    #[test]
    fn truncated_offset_table_yields_none_for_missing_entries() {
        // Count claims 3 entries but the file only has room for 1 offset.
        let mut data = Vec::new();
        data.extend_from_slice(&3u16.to_le_bytes());
        data.extend_from_slice(&8u16.to_le_bytes());
        // No more data: offsets 2 and 3 are missing entirely.

        let tbl = parse_tbl(&data);
        assert_eq!(tbl.get(0), None); // offset 8 is past EOF
        assert_eq!(tbl.get(1), None);
        assert_eq!(tbl.get(2), None);
    }

    #[test]
    fn empty_input_yields_no_strings() {
        let tbl = parse_tbl(&[]);
        assert_eq!(tbl.get(0), None);
    }

    #[test]
    fn too_short_for_count_yields_no_strings() {
        let tbl = parse_tbl(&[0u8]);
        assert_eq!(tbl.get(0), None);
    }

    /// The amplification attack this module's lazy design defends against: thousands of entries
    /// whose offsets all point at the *same* long, unterminated, invalid-UTF-8 tail. An eager
    /// parser that scanned-and-decoded every entry at parse time would scan and allocate that
    /// tail's worth of data once per entry; this test doesn't assert on timing (per the review
    /// scope), but its point is that parsing several thousand duplicate-offset entries over a
    /// ~64 KiB file is unremarkable work, and a single lookup still round-trips correctly.
    #[test]
    fn parses_pathological_duplicate_offset_file_cheaply() {
        const ENTRY_COUNT: usize = 2000;
        let header_size = ENTRY_SIZE + ENTRY_COUNT * ENTRY_SIZE;
        // Fill the remaining space (up to ~64 KiB) with a single long, NUL-free, invalid-UTF-8
        // tail that every entry's offset points at.
        let tail_len = 65536 - header_size;
        let tail_offset = header_size;

        let mut data = Vec::with_capacity(header_size + tail_len);
        data.extend_from_slice(&(ENTRY_COUNT as u16).to_le_bytes());
        for _ in 0..ENTRY_COUNT {
            data.extend_from_slice(&(tail_offset as u16).to_le_bytes());
        }
        // 0xFF is never a valid UTF-8 lead byte, and never a NUL, so this is the "one long
        // unterminated invalid-UTF-8 tail" the attack depends on.
        data.extend(std::iter::repeat_n(0xFFu8, tail_len));

        let tbl = parse_tbl(&data);
        // A single lookup still works and decodes (via the Latin-1 fallback) correctly. Each
        // 0xFF byte decodes to one char (U+00FF), though it's 2 UTF-8 bytes once re-encoded in
        // the returned `String`, so compare char counts against the original byte-length tail.
        let value = tbl.get(0).expect("entry 0 should resolve");
        assert_eq!(value.chars().count(), tail_len);
        assert!(matches!(tbl.get(0), Some(Cow::Owned(_))));
        // Every duplicate-offset entry resolves the same way.
        assert_eq!(tbl.get((ENTRY_COUNT - 1) as u16), tbl.get(0));
    }
}
