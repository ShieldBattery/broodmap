use std::borrow::Cow;
use std::collections::HashMap;

use smallvec::SmallVec;
use thiserror::Error;

use chunk_type::{ChunkTag, ChunkType, MultiChunkHandling};
use forces::{read_force_settings, ForceSettingsError, RawForceSettings};
use format_version::{read_format_version, FormatVersion, FormatVersionError};
use scenario_props::{read_scenario_props, RawScenarioProps, ScenarioPropsError};
use strings::{RawStringsChunk, StringsChunkError};
use strings::{StringEncoding, StringsChunk};

pub mod chunk_type;
pub mod forces;
pub mod format_version;
pub mod scenario_props;
pub mod strings;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ChkChunk {
    /// The offset of the data for this chunk (*not* the chunk header).
    pub offset: usize,
    /// The length of the data for this chunk (not including the chunk header).
    pub length: usize,
}

#[derive(Error, Debug)]
pub enum ChkError {
    #[error("Invalid force settings: {0}")]
    InvalidForceSettings(ForceSettingsError),
    #[error("Invalid format version: {0}")]
    InvalidFormatVersion(FormatVersionError),
    /// A chunk was encountered that jumped past the beginning of the file.
    #[error("Invalid jump chunk")]
    InvalidJumpChunk,
    #[error("Invalid scenario props: {0}")]
    InvalidScenarioProps(ScenarioPropsError),
    #[error("Invalid strings chunk: {0}")]
    InvalidStringsChunk(StringsChunkError),
    #[error("Missing required chunk: {0}")]
    MissingRequiredChunk(ChunkType),
}

pub type ChunkMap = HashMap<ChunkTag, SmallVec<[ChkChunk; 1]>>;

pub struct Chk<'a> {
    pub data: &'a [u8],
    pub desired_encoding: Option<StringEncoding>,
    pub chunks: ChunkMap,

    format_version: FormatVersion,
    scenario_props: RawScenarioProps,
    force_settings: RawForceSettings,

    pub strings: StringsChunk<'a>,
    // TODO(tec27): Replace these with string-decoded versions
}

impl<'a> Chk<'a> {
    /// Creates a [Chk] from the specified bytes in memory.
    ///
    /// If `str_encoding` is [None], the string encoding will be automatically detected from the
    /// contents of the file. Note that this detection is not guaranteed to be correct (but neither
    /// is BW's own detection).
    pub fn from_bytes(
        data: &'a [u8],
        str_encoding: Option<StringEncoding>,
    ) -> Result<Self, ChkError> {
        let chunks = gather_chunk_map(data)?;

        let format_version = read_format_version(
            &read_chunk_data(data, &chunks, ChunkType::VER)
                .ok_or(ChkError::MissingRequiredChunk(ChunkType::VER))?,
        )
        .map_err(ChkError::InvalidFormatVersion)?;
        let strings = RawStringsChunk::from_bytes(
            read_chunk_data(data, &chunks, ChunkType::STR),
            read_chunk_data(data, &chunks, ChunkType::STRx),
        )
        .map_err(ChkError::InvalidStringsChunk)?;
        let scenario_props = read_scenario_props(
            &read_chunk_data(data, &chunks, ChunkType::SPRP)
                .ok_or(ChkError::MissingRequiredChunk(ChunkType::SPRP))?,
        )
        .map_err(ChkError::InvalidScenarioProps)?;
        let force_settings = read_force_settings(
            &read_chunk_data(data, &chunks, ChunkType::FORC)
                .ok_or(ChkError::MissingRequiredChunk(ChunkType::FORC))?,
        )
        .map_err(ChkError::InvalidForceSettings)?;

        let strings = if let Some(encoding) = str_encoding {
            StringsChunk::with_known_encoding(strings, encoding)
        } else {
            StringsChunk::with_auto_encoding(strings, &scenario_props, &force_settings)
        };

        Ok(Chk {
            data,
            desired_encoding: str_encoding,
            chunks,

            format_version,
            strings,
            scenario_props,
            force_settings,
        })
    }
}

fn gather_chunk_map(data: &[u8]) -> Result<ChunkMap, ChkError> {
    let mut sections = ChunkMap::new();
    // NOTE(tec27): Some maps use "jump" chunks to skip back and reuse parts of previous chunks
    // as new chunks. They could potentially jump back to a chunk we had already seen before, in
    // which case they'd introduce an infinite loop in the parsing. To stop this, we track the
    // starting offsets we've already seen and skip them if we see one again.
    let mut by_offset: HashMap<usize, usize> = HashMap::new();

    let mut offset = 0;
    while data.len() - offset >= 8 {
        if let Some(&length) = by_offset.get(&offset) {
            // We've already processed this chunk (i.e. a jump chunk caused us to go back, now
            // we're hitting chunks we've already seen before). We can just skip this
            offset += length + 8;
            continue;
        }

        let tag: ChunkTag = data[offset..offset + 4].try_into().unwrap();
        let length = i32::from_le_bytes(data[offset + 4..offset + 8].try_into().unwrap());
        offset += 8;

        if length >= 0 {
            let chunk_type: ChunkType = tag.into();
            let length = (length as usize).min(data.len() - offset);
            let min_size = chunk_type.min_size().unwrap_or(length);
            let max_size = chunk_type.max_size().unwrap_or(length);

            if length >= min_size {
                let length = length.min(max_size);
                sections
                    .entry(tag)
                    .or_default()
                    .push(ChkChunk { offset, length });
            }
            by_offset.insert(offset - 8, length);
            offset += length;
        } else {
            // This is a negative value, i.e. a "jump chunk" that reuses some previous data for
            // a new chunk
            if length.unsigned_abs() as usize > offset {
                return Err(ChkError::InvalidJumpChunk);
            }

            // Jump sections have no data, they purely modify the current read position
            by_offset.insert(offset - 8, 0);
            offset -= length.unsigned_abs() as usize;
        }
    }

    Ok(sections)
}

fn read_chunk_data<'a>(
    data: &'a [u8],
    chunk_map: &ChunkMap,
    chunk_type: ChunkType,
) -> Option<Cow<'a, [u8]>> {
    let chunks = chunk_map
        .get::<ChunkTag>(&chunk_type.into())
        .map(|v| v.as_slice());
    let Some(chunks) = chunks else {
        return None;
    };

    match chunks.len() {
        0 => None,
        1 => {
            let chunk = &chunks[0];
            Some(Cow::Borrowed(
                &data[chunk.offset..chunk.offset + chunk.length],
            ))
        }
        _ => {
            match chunk_type.multi_chunk_handling() {
                MultiChunkHandling::FullOverwrite => chunks
                    .last()
                    .map(|c| Cow::Borrowed(&data[c.offset..c.offset + c.length])),
                MultiChunkHandling::PartialOverwrite => {
                    let max_length = chunks.iter().fold(0, |acc, c| acc.max(c.length));
                    let mut result = vec![0; max_length];
                    for chunk in chunks {
                        // We have to chop the slice down to the length of this chunk first, so that
                        // copy_from_slice doesn't panic
                        let result = &mut result[..chunk.length];
                        result.copy_from_slice(&data[chunk.offset..chunk.offset + chunk.length]);
                    }
                    Some(Cow::Owned(result))
                }
                MultiChunkHandling::Append => {
                    let mut result = Vec::new();
                    for chunk in chunks {
                        result.extend_from_slice(&data[chunk.offset..chunk.offset + chunk.length]);
                    }
                    Some(Cow::Owned(result))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use assert_ok::assert_ok;
    use smallvec::smallvec;

    use super::strings::*;
    use super::*;

    const LT_CHK: &[u8] = include_bytes!("../../assets/lt.chk");

    #[test]
    fn sections_normal() {
        use crate::chk::chunk_type::ChunkType::*;

        let result = assert_ok!(Chk::from_bytes(LT_CHK, None));

        assert_eq!(result.format_version, FormatVersion::OriginalRetail);

        let mut chunks = result
            .chunks
            .iter()
            .map(|(k, v)| (ChunkType::from(k), v.clone()))
            .collect::<Vec<_>>();
        chunks.sort_by_key(|(_, v)| v[0].offset);

        assert_eq!(
            chunks,
            vec![
                (
                    VER,
                    smallvec![ChkChunk {
                        offset: 8,
                        length: 2
                    }]
                ),
                (
                    IVER,
                    smallvec![ChkChunk {
                        offset: 18,
                        length: 2
                    }]
                ),
                (
                    IVE2,
                    smallvec![ChkChunk {
                        offset: 28,
                        length: 2
                    }]
                ),
                (
                    VCOD,
                    smallvec![ChkChunk {
                        offset: 38,
                        length: 1040
                    }]
                ),
                (
                    IOWN,
                    smallvec![ChkChunk {
                        offset: 1086,
                        length: 12
                    }]
                ),
                (
                    OWNR,
                    smallvec![ChkChunk {
                        offset: 1106,
                        length: 12
                    }]
                ),
                (
                    ERA,
                    smallvec![ChkChunk {
                        offset: 1126,
                        length: 2
                    }]
                ),
                (
                    DIM,
                    smallvec![ChkChunk {
                        offset: 1136,
                        length: 4
                    }]
                ),
                (
                    SIDE,
                    smallvec![ChkChunk {
                        offset: 1148,
                        length: 12
                    }]
                ),
                (
                    MTXM,
                    smallvec![ChkChunk {
                        offset: 1168,
                        length: 32768
                    }]
                ),
                (
                    PUNI,
                    smallvec![ChkChunk {
                        offset: 33944,
                        length: 5700
                    }]
                ),
                (
                    UPGR,
                    smallvec![ChkChunk {
                        offset: 39652,
                        length: 1748
                    }]
                ),
                (
                    PTEC,
                    smallvec![ChkChunk {
                        offset: 41408,
                        length: 912
                    }]
                ),
                (
                    UNIT,
                    smallvec![ChkChunk {
                        offset: 42328,
                        length: 4248
                    }]
                ),
                (
                    ISOM,
                    smallvec![ChkChunk {
                        offset: 46584,
                        length: 67080
                    }]
                ),
                (
                    TILE,
                    smallvec![ChkChunk {
                        offset: 113672,
                        length: 32768
                    }]
                ),
                (
                    DD2,
                    smallvec![ChkChunk {
                        offset: 146448,
                        length: 896
                    }]
                ),
                (
                    THG2,
                    smallvec![ChkChunk {
                        offset: 147352,
                        length: 540
                    }]
                ),
                (
                    MASK,
                    smallvec![ChkChunk {
                        offset: 147900,
                        length: 16384
                    }]
                ),
                (
                    STR,
                    smallvec![ChkChunk {
                        offset: 164292,
                        length: 2273
                    }]
                ),
                (
                    UPRP,
                    smallvec![ChkChunk {
                        offset: 166573,
                        length: 1280
                    }]
                ),
                (
                    UPUS,
                    smallvec![ChkChunk {
                        offset: 167861,
                        length: 64
                    }]
                ),
                (
                    MRGN,
                    smallvec![ChkChunk {
                        offset: 167933,
                        length: 1280
                    }]
                ),
                (
                    TRIG,
                    smallvec![ChkChunk {
                        offset: 169221,
                        length: 7200
                    }]
                ),
                (
                    MBRF,
                    smallvec![ChkChunk {
                        offset: 176429,
                        length: 0
                    }]
                ),
                (
                    SPRP,
                    smallvec![ChkChunk {
                        offset: 176437,
                        length: 4
                    }]
                ),
                (
                    FORC,
                    smallvec![ChkChunk {
                        offset: 176449,
                        length: 20
                    }]
                ),
                (
                    WAV,
                    smallvec![ChkChunk {
                        offset: 176477,
                        length: 2048
                    }]
                ),
                (
                    UNIS,
                    smallvec![ChkChunk {
                        offset: 178533,
                        length: 4048
                    }]
                ),
                (
                    UPGS,
                    smallvec![ChkChunk {
                        offset: 182589,
                        length: 598
                    }]
                ),
                (
                    TECS,
                    smallvec![ChkChunk {
                        offset: 183195,
                        length: 216
                    }]
                ),
                (
                    SWNM,
                    smallvec![ChkChunk {
                        offset: 183419,
                        length: 1024
                    }]
                ),
                (
                    PUPx,
                    smallvec![ChkChunk {
                        offset: 184451,
                        length: 2318
                    }]
                ),
                (
                    PTEx,
                    smallvec![ChkChunk {
                        offset: 186777,
                        length: 1672
                    }]
                ),
                (
                    UNIx,
                    smallvec![ChkChunk {
                        offset: 188457,
                        length: 4168
                    }]
                ),
                (
                    UPGx,
                    smallvec![ChkChunk {
                        offset: 192633,
                        length: 794
                    }]
                ),
                (
                    TECx,
                    smallvec![ChkChunk {
                        offset: 193435,
                        length: 396
                    }]
                )
            ]
        )
    }

    #[test]
    fn lt_strings() {
        let result = assert_ok!(Chk::from_bytes(LT_CHK, None));

        assert_eq!(result.strings.inner.data.kind, StringsChunkKind::Legacy);
        assert_eq!(result.strings.inner.max_len, 1024);
        assert_eq!(
            result.strings.inner.get_raw_bytes(1.into()).unwrap(),
            b"Untitled Scenario"
        );

        assert_eq!(result.scenario_props.name_id, 4.into());
        assert_eq!(result.scenario_props.description_id, 5.into());

        assert_eq!(
            result.strings.get(result.scenario_props.name_id),
            Some("The Lost Temple".into())
        );
    }
}
