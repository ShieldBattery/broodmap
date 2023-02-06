use std::borrow::Cow;
use thiserror::Error;

#[derive(Error, Debug)]
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

#[derive(Debug)]
pub struct StringsChunkData<'a> {
    pub kind: StringsChunkKind,
    data: Cow<'a, [u8]>,
}

impl<'a> StringsChunkData<'a> {
    pub fn legacy(data: Cow<'a, [u8]>) -> Self {
        Self {
            kind: StringsChunkKind::Legacy,
            data,
        }
    }

    pub fn extended(data: Cow<'a, [u8]>) -> Self {
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
    pub fn read_str_bytes(&'a self, offset: usize) -> &'a [u8] {
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
            (_, Some(extended_bytes)) => StringsChunkData::extended(extended_bytes),
            (Some(legacy_bytes), None) => StringsChunkData::legacy(legacy_bytes),
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
