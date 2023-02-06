use std::borrow::Cow;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum StringsChunkError {
    #[error("No strings chunk found")]
    NoStringsChunk,
    #[error("Strings data too short")]
    DataTooShort,
}

pub enum StringsChunkData<'a> {
    LegacyStringChunk(Cow<'a, [u8]>),
    ExtendedStringChunk(Cow<'a, [u8]>),
}

impl<'a> StringsChunkData<'a> {
    /// Returns the size of a dimension (an offset or the string count) in this strings chunk.
    pub fn dim_size(&self) -> usize {
        match self {
            StringsChunkData::LegacyStringChunk(_) => 2,
            StringsChunkData::ExtendedStringChunk(_) => 4,
        }
    }

    /// Returns the length of the total data section in bytes.
    pub fn bytes_len(&self) -> usize {
        match self {
            StringsChunkData::LegacyStringChunk(data) => data.len(),
            StringsChunkData::ExtendedStringChunk(data) => data.len(),
        }
    }

    /// Returns the bytes for a particular string at `offset`, reading all bytes until a null byte
    /// or the end of the data is encountered. The encoding of this string is not guaranteed, and
    /// it should be decoded before use.
    ///
    /// This does not check the bounds of the data section, and will panic if `offset` exceeds the
    /// bounds.
    pub fn read_str_bytes(&'a self, offset: usize) -> &'a [u8] {
        let data = match self {
            StringsChunkData::LegacyStringChunk(data) => data,
            StringsChunkData::ExtendedStringChunk(data) => data,
        };
        let data = &data[offset..];
        data.split(|b| *b == 0).next().unwrap_or(data)
    }

    /// Returns a dimension-sized value at the specified index, or [None] if the index exceeds the
    /// bounds of the data. Note that this will only check the bounds relative to the entire data
    /// section, so indexes that exceed the chunk's stated maximum length but still reside in the
    /// string data will return a value.
    pub fn get_dim(&self, index: usize) -> Option<usize> {
        match self {
            StringsChunkData::LegacyStringChunk(data) => {
                let offset = index * 2;
                if offset > data.len() - 2 {
                    None
                } else {
                    Some(u16::from_le_bytes(data[offset..offset + 2].try_into().unwrap()) as usize)
                }
            }
            StringsChunkData::ExtendedStringChunk(data) => {
                let offset = index * 4;
                if offset > data.len() - 4 {
                    None
                } else {
                    Some(u32::from_le_bytes(data[offset..offset + 4].try_into().unwrap()) as usize)
                }
            }
        }
    }

    /// Returns a dimension-sized value at the specified index. Will panic if the index exceeds the
    /// bounds of the data section.
    pub fn get_dim_unchecked(&self, index: usize) -> usize {
        match self {
            StringsChunkData::LegacyStringChunk(data) => {
                let offset = index * 2;
                u16::from_le_bytes(data[offset..offset + 2].try_into().unwrap()) as usize
            }
            StringsChunkData::ExtendedStringChunk(data) => {
                let offset = index * 4;
                u32::from_le_bytes(data[offset..offset + 4].try_into().unwrap()) as usize
            }
        }
    }
}

pub struct StringsChunk<'a> {
    /// The string data any reads will be done from.
    pub data: StringsChunkData<'a>,
    /// The maximum number of strings this chunk contains. This is the value its header claims it
    /// contains (but due to map protection techniques, the actual number may be less).
    pub max_len: usize,
}

impl<'a> StringsChunk<'a> {
    pub fn from_bytes(
        legacy_bytes: Option<Cow<'a, [u8]>>,
        extended_bytes: Option<Cow<'a, [u8]>>,
    ) -> Result<StringsChunk<'a>, StringsChunkError> {
        let data = match (legacy_bytes, extended_bytes) {
            (_, Some(extended_bytes)) => StringsChunkData::ExtendedStringChunk(extended_bytes),
            (Some(legacy_bytes), None) => StringsChunkData::LegacyStringChunk(legacy_bytes),
            (None, None) => return Err(StringsChunkError::NoStringsChunk),
        };

        let dim_size = data.dim_size();
        let max_len = data
            .get_dim(0)
            .ok_or(StringsChunkError::DataTooShort)?
            .min((data.bytes_len() - dim_size) / dim_size);

        Ok(StringsChunk { data, max_len })
    }

    /// Returns the bytes that make up the string at index `index`, or [None] if the index is out of
    /// bounds.
    pub fn get_raw_bytes(&'a self, index: usize) -> Option<&'a [u8]> {
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
