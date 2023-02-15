use std::borrow::Cow;
use std::collections::HashSet;
use thiserror::Error;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct StringId(pub usize);

impl From<u16> for StringId {
    fn from(value: u16) -> Self {
        Self(value as usize)
    }
}

impl From<u32> for StringId {
    fn from(value: u32) -> Self {
        Self(value as usize)
    }
}

/// A CHK structure that contains string IDs that need to be decoded from the strings chunk. This
/// allows you to specify a general way to decode the whole struct.
pub trait ChkDecode<T> {
    /// Decodes this CHK structure into a [T] using the strings from `strings_chunk`.
    fn decode_strings(&self, strings_chunk: &StringsChunk) -> T;
}

/// A type that can return an iterator over all the CHK string IDs it references.
pub trait UsedChkStrings {
    /// Returns an iterator over all the string IDs that this CHK structure references.
    fn used_string_ids(&self) -> Box<dyn Iterator<Item = StringId> + '_>;
}

impl<T, E> UsedChkStrings for &Result<T, E>
where
    T: UsedChkStrings,
{
    fn used_string_ids(&self) -> Box<dyn Iterator<Item = StringId> + '_> {
        match self {
            Ok(v) => v.used_string_ids(),
            Err(_) => Box::new(std::iter::empty()),
        }
    }
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum StringsChunkError {
    #[error("No strings chunk found")]
    NoStringsChunk,
    #[error("Strings data too short")]
    DataTooShort,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum StringsChunkKind {
    Legacy,
    Extended,
}

#[derive(Debug, Clone)]
pub struct StringsChunkData {
    pub kind: StringsChunkKind,
    data: Vec<u8>,
}

impl StringsChunkData {
    pub fn legacy(data: Vec<u8>) -> Self {
        Self {
            kind: StringsChunkKind::Legacy,
            data,
        }
    }

    pub fn extended(data: Vec<u8>) -> Self {
        Self {
            kind: StringsChunkKind::Extended,
            data,
        }
    }

    /// Returns the size of a dimension (an offset or the string count) in this strings chunk.
    pub fn dim_size(&self) -> usize {
        match self.kind {
            StringsChunkKind::Legacy => 2,
            StringsChunkKind::Extended => 4,
        }
    }

    /// Returns the length of the total data section in bytes.
    pub fn bytes_len(&self) -> usize {
        self.data.len()
    }

    /// Returns the bytes for a particular string at `offset`, reading all bytes until a null byte
    /// or the end of the data is encountered. The encoding of this string is not guaranteed, and
    /// it should be decoded before use.
    ///
    /// This does not check the bounds of the data section, and will panic if `offset` exceeds the
    /// bounds.
    pub fn read_str_bytes(&self, offset: usize) -> &[u8] {
        let data = &self.data[offset..];
        data.split(|b| *b == 0).next().unwrap_or(data)
    }

    /// Returns a dimension-sized value at the specified index, or [None] if the index exceeds the
    /// bounds of the data. Note that this will only check the bounds relative to the entire data
    /// section, so indexes that exceed the chunk's stated maximum length but still reside in the
    /// string data will return a value.
    pub fn get_dim(&self, index: usize) -> Option<usize> {
        match self.kind {
            StringsChunkKind::Legacy => {
                let offset = index * 2;
                if offset > self.data.len() - 2 {
                    None
                } else {
                    Some(
                        u16::from_le_bytes(self.data[offset..offset + 2].try_into().unwrap())
                            as usize,
                    )
                }
            }
            StringsChunkKind::Extended => {
                let offset = index * 4;
                if offset > self.data.len() - 4 {
                    None
                } else {
                    Some(
                        u32::from_le_bytes(self.data[offset..offset + 4].try_into().unwrap())
                            as usize,
                    )
                }
            }
        }
    }

    /// Returns a dimension-sized value at the specified index. Will panic if the index exceeds the
    /// bounds of the data section.
    pub fn get_dim_unchecked(&self, index: usize) -> usize {
        match self.kind {
            StringsChunkKind::Legacy => {
                let offset = index * 2;
                u16::from_le_bytes(self.data[offset..offset + 2].try_into().unwrap()) as usize
            }
            StringsChunkKind::Extended => {
                let offset = index * 4;
                u32::from_le_bytes(self.data[offset..offset + 4].try_into().unwrap()) as usize
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct RawStringsChunk {
    /// The string data any reads will be done from.
    pub data: StringsChunkData,
    /// The maximum number of strings this chunk contains. This is the value its header claims it
    /// contains (but due to map protection techniques, the actual number may be less).
    pub max_len: usize,
}

impl RawStringsChunk {
    pub fn from_bytes(
        legacy_bytes: Option<Cow<'_, [u8]>>,
        extended_bytes: Option<Cow<'_, [u8]>>,
    ) -> Result<RawStringsChunk, StringsChunkError> {
        let data = match (legacy_bytes, extended_bytes) {
            (_, Some(extended_bytes)) => StringsChunkData::extended(extended_bytes.into_owned()),
            (Some(legacy_bytes), None) => StringsChunkData::legacy(legacy_bytes.into_owned()),
            (None, None) => return Err(StringsChunkError::NoStringsChunk),
        };

        let dim_size = data.dim_size();
        let max_len = data
            .get_dim(0)
            .ok_or(StringsChunkError::DataTooShort)?
            .min((data.bytes_len() - dim_size) / dim_size);

        Ok(RawStringsChunk { data, max_len })
    }

    /// Returns the bytes that make up the string at index `index`, or [None] if the index is out of
    /// bounds. The encoding of this string is not guaranteed, and it should be decoded before use.
    pub fn get_raw_bytes(&self, index: StringId) -> Option<&[u8]> {
        let index = index.0;
        if index == 0 || index >= self.max_len {
            return None;
        }
        let offset = self.data.get_dim_unchecked(index);
        if offset >= self.data.bytes_len() {
            return None;
        }

        Some(self.data.read_str_bytes(offset))
    }
}

/// Legacy string encoding methods used before SC:R. This can be used in a [StringEncoding] either
/// to always decode via the legacy encoding, or to fall back to the legacy encoding if the string
/// is not valid UTF-8.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum LegacyCodePage {
    /// Strings will be decoded using CP-1252 (lossy).
    Latin,
    // Strings will be decoded using CP-949 (lossy).
    Korean,
}

/// Specifies how strings in the CHK file should be decoded.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum StringEncoding {
    /// All strings will be decoded using UTF-8 (lossy).
    Utf8,
    /// All strings will be decoded using UTF-8, falling back to the specified encoding if the
    /// string is not valid UTF-8 (e.g. if it is actually CP-949). The fallback conversion will be
    /// done in a lossy fashion.
    Utf8WithFallback(LegacyCodePage),
    /// All strings will be decoded using the specified legacy encoding (lossy).
    Legacy(LegacyCodePage),
}

#[derive(Debug, Clone)]
pub struct StringsChunk {
    pub encoding: StringEncoding,
    pub inner: RawStringsChunk,
}

impl StringsChunk {
    pub fn with_known_encoding(
        strings_chunk: RawStringsChunk,
        encoding: StringEncoding,
    ) -> StringsChunk {
        StringsChunk {
            encoding,
            inner: strings_chunk,
        }
    }

    pub fn with_auto_encoding(
        strings_chunk: RawStringsChunk,
        used_string_ids: HashSet<StringId>,
    ) -> StringsChunk {
        // SC 1.16.1 used either CP-949 or CP-1252 encoding for all map strings depending on whether
        // the system locale was set to Korea or not. SC:R considers each string separately,
        // defaulting to UTF-8(*), and if the string cannot be decoded with UTF-8, SC:R guesses
        // between CP-949 and CP-1252 based on the string contents.
        //
        // (*) Unit names(?) seem to have special handling to use 949 unless the 949-vs-1252 guess
        // prefers 1252, in which case UTF-8 used if possible before falling back to 1252. This
        // doesn't make much sense but that's how it appears to go.
        //
        // This library tries to do a bit better than SC:R does, as maps with full 1252/949
        // encoding may have a few short strings that weren't intended to be UTF-8 but could be
        // decoded using it. At the same time, we want to try to be compatible with maps that in do
        // mix UTF-8 and legacy encoding -- they do exist, so when we detect the map to be using
        // UTF-8, we will also try to pick a fallback to one of the legacy codepages like SC:R does.
        //
        // There have been maps in the wild that mix 949/1252 encodings due to being edited over
        // years(?) by different creators. Currently, we don't try to support such cases.
        let mut weight_korean = 0;
        let mut weight_other = 0;
        let mut weight_utf8 = 0;
        for id in used_string_ids {
            let Some(bytes) = strings_chunk.get_raw_bytes(id) else {
                continue;
            };

            let non_ascii_1252 = bytes
                .iter()
                .filter(|&b| *b >= 0x80)
                .collect::<HashSet<_>>()
                .len();

            let is_valid_utf8 = std::str::from_utf8(bytes).is_ok();

            let (korean, _, korean_had_errors) = encoding_rs::EUC_KR.decode(bytes);
            let mut hangul_chars = 0;
            let mut possible_non_ascii = 0;
            if korean_had_errors {
                // This was not valid 949 encoding. Sometimes there may be maps that have been
                // edited in both encodings, so just take this as a heavy hint towards 1252
                if !is_valid_utf8 {
                    weight_other += 5
                }
            } else {
                (hangul_chars, possible_non_ascii) =
                    korean
                        .chars()
                        .fold((0, 0), |(hangul_chars, possible_non_ascii), c| {
                            if matches!(c, '\u{ac00}'..='\u{d7af}' | '\u{3130}'..='\u{318f}') {
                                (hangul_chars + 1, possible_non_ascii)
                            } else if c as u32 >= 0x80 {
                                (hangul_chars, possible_non_ascii + 1)
                            } else {
                                (hangul_chars, possible_non_ascii)
                            }
                        });
            }

            // Since some 1252 characters can appear as hangul, if there is only a single
            // non-ascii character used, assume it is 1252
            let had_hangul = hangul_chars >= 1 && non_ascii_1252 >= 2;
            if is_valid_utf8 && non_ascii_1252 > 10 {
                // This map definitely should be counted as having UTF-8 strings
                weight_utf8 += 50
            } else if had_hangul && (hangul_chars >= 5 || hangul_chars > possible_non_ascii) {
                weight_korean += 1
            } else if non_ascii_1252 > 0 {
                if is_valid_utf8 {
                    weight_utf8 += 1
                } else {
                    weight_other += 1
                }
            }
        }

        let fallback = if weight_korean == 0 && weight_other == 0 {
            None
        } else if weight_korean >= weight_other {
            Some(LegacyCodePage::Korean)
        } else {
            Some(LegacyCodePage::Latin)
        };
        let use_utf8 = match fallback {
            Some(LegacyCodePage::Korean) => {
                weight_utf8 > 5 || (weight_utf8 > 0 && weight_korean < 20)
            }
            Some(LegacyCodePage::Latin) => {
                weight_utf8 > 10 || (weight_utf8 > 0 && weight_other < 20)
            }
            None => true,
        };

        let encoding = if use_utf8 {
            if let Some(fallback) = fallback {
                StringEncoding::Utf8WithFallback(fallback)
            } else {
                StringEncoding::Utf8
            }
        } else {
            assert!(fallback.is_some());
            StringEncoding::Legacy(fallback.unwrap())
        };

        Self::with_known_encoding(strings_chunk, encoding)
    }

    fn decode_bytes(bytes: &[u8], encoding: StringEncoding) -> Option<(Cow<str>, StringEncoding)> {
        match encoding {
            StringEncoding::Utf8 => Some((String::from_utf8_lossy(bytes), encoding)),
            StringEncoding::Utf8WithFallback(fallback) => std::str::from_utf8(bytes)
                .map(|s| Some((s.into(), encoding)))
                .unwrap_or_else(|_| Self::decode_bytes(bytes, StringEncoding::Legacy(fallback))),
            StringEncoding::Legacy(LegacyCodePage::Latin) => {
                let (result, _, _) = encoding_rs::WINDOWS_1252.decode(bytes);
                Some((result, encoding))
            }
            StringEncoding::Legacy(LegacyCodePage::Korean) => {
                let (result, _, _) = encoding_rs::EUC_KR.decode(bytes);
                Some((result, encoding))
            }
        }
    }

    pub fn get(&self, index: StringId) -> Option<Cow<str>> {
        self.inner
            .get_raw_bytes(index)
            .and_then(|bytes| Self::decode_bytes(bytes, self.encoding).map(|(s, _)| s))
    }
}
