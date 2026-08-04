use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, OnceLock};

use crate::chk::briefing::{
    BriefingError, RawBriefingTrigger, read_briefing,
    scan_used_string_ids as scan_used_briefing_string_ids,
};
use smallvec::SmallVec;
use thiserror::Error;

use crate::chk::chunk_type::{ChunkTag, ChunkType, MultiChunkHandling};
use crate::chk::dimensions::{DimensionsError, MapDimensions, read_dimensions};
use crate::chk::forces::{
    ForceSettings, ForceSettingsError, RawForceSettings, read_force_settings,
};
use crate::chk::format_version::{FormatVersion, FormatVersionError, read_format_version};
use crate::chk::placed_units::{PlacedUnit, PlacedUnitsError, read_placed_units};
use crate::chk::player_colors::{PlayerColors, read_colr, read_crgb, resolve_player_colors};
use crate::chk::scenario_props::{
    RawScenarioProps, ScenarioProps, ScenarioPropsError, read_scenario_props,
};
use crate::chk::sprites::{Sprite, SpriteError, read_sprites};
use crate::chk::strings::{
    ChkDecode, RawStringsChunk, StringEncoding, StringsChunk, StringsChunkError, UsedChkStrings,
};
use crate::chk::terrain::{TerrainError, TerrainTileIds, read_terrain};
use crate::chk::tileset::{Tileset, TilesetError, read_tileset};
use crate::chk::triggers::{
    RawTrigger, TriggersError, read_triggers, scan_used_string_ids as scan_used_trigger_string_ids,
};
use crate::chk::unit_settings::{RawUnitSettings, UnitSettingsError};
use crate::limits::{
    Resource, ResourceLimitError, ResourceLimits, allocation_error, ensure_within,
};

pub mod briefing;
pub mod chunk_type;
pub mod dimensions;
pub mod forces;
pub mod format_version;
pub mod placed_units;
pub mod player_colors;
pub mod scenario_props;
pub mod sprites;
pub mod strings;
pub mod terrain;
pub mod tileset;
pub mod triggers;
pub mod unit_settings;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ChkChunk {
    /// The offset of the data for this chunk (*not* the chunk header).
    pub offset: usize,
    /// The length of the data for this chunk (not including the chunk header).
    pub length: usize,
}

#[derive(Error, Debug)]
pub enum ChkError {
    #[error("Resource limit: {0}")]
    ResourceLimit(#[from] ResourceLimitError),
    #[error("Invalid dimensions: {0}")]
    InvalidDimensions(DimensionsError),
    #[error("Invalid format version: {0}")]
    InvalidFormatVersion(FormatVersionError),
    #[error("Invalid strings chunk: {0}")]
    InvalidStringsChunk(StringsChunkError),
    #[error("Invalid tileset: {0}")]
    InvalidTileset(TilesetError),
    #[error("Missing required chunk: {0}")]
    MissingRequiredChunk(ChunkType),
}

pub type ChunkMap = HashMap<ChunkTag, SmallVec<[ChkChunk; 1]>>;

#[derive(Debug)]
pub struct Chk {
    data: Vec<u8>,
    desired_encoding: Option<StringEncoding>,
    chunks: ChunkMap,

    format_version: FormatVersion,
    raw_strings: Arc<RawStringsChunk>,
    dimensions: MapDimensions,
    tileset: Tileset,
    raw_scenario_props: OnceLock<Result<RawScenarioProps, ScenarioPropsError>>,
    raw_force_settings: OnceLock<Result<RawForceSettings, ForceSettingsError>>,
    raw_triggers: OnceLock<Result<Vec<RawTrigger>, TriggersError>>,
    raw_briefing: OnceLock<Result<Vec<RawBriefingTrigger>, BriefingError>>,
    raw_unit_settings: OnceLock<Result<RawUnitSettings, UnitSettingsError>>,

    strings: OnceLock<StringsChunk>,
    scenario_props: OnceLock<Result<ScenarioProps, ScenarioPropsError>>,
    force_settings: OnceLock<Result<ForceSettings, ForceSettingsError>>,
    terrain: OnceLock<Result<TerrainTileIds, TerrainError>>,
    sprites: OnceLock<Result<Vec<Sprite>, SpriteError>>,
    placed_units: OnceLock<Result<Vec<PlacedUnit>, PlacedUnitsError>>,
    player_colors: OnceLock<PlayerColors>,
}

impl Chk {
    /// Creates a [Chk] from the specified bytes in memory.
    ///
    /// If `str_encoding` is [None], the string encoding will be automatically detected from the
    /// contents of the file. Note that this detection is not guaranteed to be correct (but neither
    /// is BW's own detection).
    pub fn from_bytes(
        data: Vec<u8>,
        str_encoding: Option<StringEncoding>,
    ) -> Result<Self, ChkError> {
        Self::from_bytes_with_limits(data, str_encoding, &ResourceLimits::default())
    }

    /// Creates a [Chk] from the specified bytes in memory, applying resource limits while
    /// gathering its chunks.
    ///
    /// If `str_encoding` is [None], the string encoding will be automatically detected from the
    /// contents of the file. Note that this detection is not guaranteed to be correct (but neither
    /// is BW's own detection).
    pub fn from_bytes_with_limits(
        data: Vec<u8>,
        str_encoding: Option<StringEncoding>,
        limits: &ResourceLimits,
    ) -> Result<Self, ChkError> {
        ensure_within(Resource::ChkBytes, data.len(), limits.max_chk_bytes)?;
        let chunks = gather_chunk_map(&data, limits)?;

        let format_version = read_format_version(
            &read_chunk_data(&data, &chunks, ChunkType::VER)
                .ok_or(ChkError::MissingRequiredChunk(ChunkType::VER))?,
        )
        .map_err(ChkError::InvalidFormatVersion)?;
        let raw_strings = RawStringsChunk::from_bytes(
            read_chunk_data(&data, &chunks, ChunkType::STR),
            read_chunk_data(&data, &chunks, ChunkType::STRx),
        )
        .map_err(ChkError::InvalidStringsChunk)?;
        let dimensions = read_dimensions(
            &read_chunk_data(&data, &chunks, ChunkType::DIM)
                .ok_or(ChkError::MissingRequiredChunk(ChunkType::DIM))?,
        )
        .map_err(ChkError::InvalidDimensions)?;
        let tileset = read_tileset(
            &read_chunk_data(&data, &chunks, ChunkType::ERA)
                .ok_or(ChkError::MissingRequiredChunk(ChunkType::ERA))?,
        )
        .map_err(ChkError::InvalidTileset)?;

        Ok(Chk {
            data,
            desired_encoding: str_encoding,
            chunks,

            format_version,
            raw_strings: Arc::new(raw_strings),
            dimensions,
            tileset,
            raw_scenario_props: OnceLock::new(),
            raw_force_settings: OnceLock::new(),
            raw_triggers: OnceLock::new(),
            raw_briefing: OnceLock::new(),
            raw_unit_settings: OnceLock::new(),

            strings: OnceLock::new(),
            scenario_props: OnceLock::new(),
            force_settings: OnceLock::new(),
            terrain: OnceLock::new(),
            sprites: OnceLock::new(),
            placed_units: OnceLock::new(),
            player_colors: OnceLock::new(),
        })
    }

    /// Returns the original CHK bytes.
    pub fn data(&self) -> &[u8] {
        &self.data
    }

    /// Returns the encoding requested by the caller, or [`None`] when encoding is detected
    /// automatically.
    pub fn desired_encoding(&self) -> Option<StringEncoding> {
        self.desired_encoding
    }

    /// Returns the validated chunk index gathered while constructing this CHK.
    pub fn chunks(&self) -> &ChunkMap {
        &self.chunks
    }

    pub fn strings(&self) -> &StringsChunk {
        self.strings.get_or_init(|| {
            if let Some(encoding) = self.desired_encoding {
                StringsChunk::with_known_encoding(self.raw_strings.clone(), encoding)
            } else {
                let mut used_strings = HashSet::new();
                used_strings.extend(self.raw_scenario_props().used_string_ids());
                used_strings.extend(self.raw_force_settings().used_string_ids());
                if let Some(data) = read_chunk_data(&self.data, &self.chunks, ChunkType::TRIG) {
                    used_strings.extend(scan_used_trigger_string_ids(&data));
                }
                if let Some(data) = read_chunk_data(&self.data, &self.chunks, ChunkType::MBRF) {
                    used_strings.extend(scan_used_briefing_string_ids(&data));
                }
                used_strings.extend(self.raw_unit_settings().used_string_ids());
                StringsChunk::with_auto_encoding(self.raw_strings.clone(), used_strings)
            }
        })
    }

    pub fn format_version(&self) -> FormatVersion {
        self.format_version
    }

    /// Returns the width of the map in 32x32 tiles.
    pub fn width(&self) -> usize {
        self.dimensions.width.into()
    }

    /// Returns the height of the map in 32x32 tiles.
    pub fn height(&self) -> usize {
        self.dimensions.height.into()
    }

    /// Returns the tileset used by the map.
    pub fn tileset(&self) -> Tileset {
        self.tileset
    }

    fn raw_scenario_props(&self) -> &Result<RawScenarioProps, ScenarioPropsError> {
        self.raw_scenario_props.get_or_init(|| {
            read_scenario_props(
                &read_chunk_data(&self.data, &self.chunks, ChunkType::SPRP)
                    .ok_or(ScenarioPropsError::ChunkMissing)?,
            )
        })
    }

    pub fn scenario_props(&self) -> Result<&ScenarioProps, &ScenarioPropsError> {
        self.scenario_props
            .get_or_init(|| {
                self.raw_scenario_props()
                    .map(|raw| raw.decode_strings(self.strings()))
            })
            .as_ref()
    }

    fn raw_force_settings(&self) -> &Result<RawForceSettings, ForceSettingsError> {
        self.raw_force_settings.get_or_init(|| {
            read_force_settings(
                &read_chunk_data(&self.data, &self.chunks, ChunkType::FORC)
                    .ok_or(ForceSettingsError::ChunkMissing)?,
            )
        })
    }

    pub fn force_settings(&self) -> Result<&ForceSettings, &ForceSettingsError> {
        self.force_settings
            .get_or_init(|| {
                self.raw_force_settings()
                    .map(|raw| raw.decode_strings(self.strings()))
            })
            .as_ref()
    }

    // NOTE(tec27): This is private because we don't generally want to return references to the
    // Result, just their contents. Triggers are sort of weird because we don't provide a non-raw
    // vresion of them, and thus do expose the raw version. So we need a different name here
    fn raw_triggers_private(&self) -> &Result<Vec<RawTrigger>, TriggersError> {
        self.raw_triggers.get_or_init(|| {
            let chunk_data = read_chunk_data(&self.data, &self.chunks, ChunkType::TRIG);
            match chunk_data {
                Some(ref data) => read_triggers(data),
                None => Ok(Vec::new()),
            }
        })
    }

    pub fn raw_triggers(&self) -> Result<&Vec<RawTrigger>, &TriggersError> {
        self.raw_triggers_private().as_ref()
    }

    // NOTE(tec27): This is private because we don't generally want to return references to the
    // Result, just their contents. Triggers are sort of weird because we don't provide a non-raw
    // vresion of them, and thus do expose the raw version. So we need a different name here
    fn raw_briefing_private(&self) -> &Result<Vec<RawBriefingTrigger>, BriefingError> {
        self.raw_briefing.get_or_init(|| {
            let chunk_data = read_chunk_data(&self.data, &self.chunks, ChunkType::MBRF);
            match chunk_data {
                Some(ref data) => read_briefing(data),
                None => Ok(Vec::new()),
            }
        })
    }

    pub fn raw_briefing(&self) -> Result<&Vec<RawBriefingTrigger>, &BriefingError> {
        self.raw_briefing_private().as_ref()
    }

    // TODO(tec27): Provide a way to retrieve non-raw unit settings
    fn raw_unit_settings(&self) -> &Result<RawUnitSettings, UnitSettingsError> {
        self.raw_unit_settings.get_or_init(|| {
            RawUnitSettings::from_bytes(
                read_chunk_data(&self.data, &self.chunks, ChunkType::UNIS),
                read_chunk_data(&self.data, &self.chunks, ChunkType::UNIx),
            )
        })
    }

    pub fn terrain(&self) -> Result<&TerrainTileIds, &TerrainError> {
        self.terrain
            .get_or_init(|| {
                read_terrain(
                    &read_chunk_data(&self.data, &self.chunks, ChunkType::MTXM)
                        .ok_or(TerrainError::ChunkMissing)?,
                    self.width(),
                    self.height(),
                )
            })
            .as_ref()
    }

    pub fn sprites(&self) -> Result<&Vec<Sprite>, &SpriteError> {
        self.sprites
            .get_or_init(|| {
                read_sprites(
                    &read_chunk_data(&self.data, &self.chunks, ChunkType::THG2)
                        .ok_or(SpriteError::ChunkMissing)?,
                )
            })
            .as_ref()
    }

    pub fn placed_units(&self) -> Result<&Vec<PlacedUnit>, &PlacedUnitsError> {
        self.placed_units
            .get_or_init(|| {
                read_placed_units(
                    &read_chunk_data(&self.data, &self.chunks, ChunkType::UNIT)
                        .ok_or(PlacedUnitsError::ChunkMissing)?,
                )
            })
            .as_ref()
    }

    /// Returns the resolved player colors for this scenario, combining the `COLR` and `CRGB`
    /// chunks. Works even when neither chunk is present (all players resolve to
    /// [`player_colors::PlayerColor::Default`] in that case), and malformed chunk data is treated
    /// the same as an absent chunk (matching this crate's permissive parsing style).
    pub fn player_colors(&self) -> &PlayerColors {
        self.player_colors.get_or_init(|| {
            let colr = read_chunk_data(&self.data, &self.chunks, ChunkType::COLR)
                .and_then(|data| read_colr(&data).ok());
            let crgb = read_chunk_data(&self.data, &self.chunks, ChunkType::CRGB)
                .and_then(|data| read_crgb(&data).ok());
            resolve_player_colors(colr.as_ref(), crgb.as_ref())
        })
    }
}

fn gather_chunk_map(data: &[u8], limits: &ResourceLimits) -> Result<ChunkMap, ChkError> {
    let mut sections = ChunkMap::new();
    // NOTE(tec27): Some maps use "jump" chunks to skip back and reuse parts of previous chunks
    // as new chunks. They could potentially jump back to a chunk we had already seen before, in
    // which case they'd introduce an infinite loop in the parsing. To stop this, we track the
    // starting offsets we've already seen and skip them if we see one again.
    let mut by_offset: HashMap<usize, usize> = HashMap::new();

    let mut offset = 0;
    let mut visited_chunks = 0usize;
    while data.len() - offset >= 8 {
        visited_chunks = visited_chunks
            .checked_add(1)
            .ok_or(ResourceLimitError::Exceeded {
                resource: Resource::ChkChunks,
                observed: usize::MAX,
                limit: limits.max_chk_chunks,
            })?;
        ensure_within(Resource::ChkChunks, visited_chunks, limits.max_chk_chunks)?;

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
                if !sections.contains_key(&tag) {
                    sections
                        .try_reserve(1)
                        .map_err(|_| allocation_error(Resource::ChkChunks, 1))?;
                }
                let chunks = sections.entry(tag).or_default();
                let chunk_count =
                    chunks
                        .len()
                        .checked_add(1)
                        .ok_or(ResourceLimitError::Exceeded {
                            resource: Resource::ChkChunksPerTag,
                            observed: usize::MAX,
                            limit: limits.max_chk_chunks_per_tag,
                        })?;
                ensure_within(
                    Resource::ChkChunksPerTag,
                    chunk_count,
                    limits.max_chk_chunks_per_tag,
                )?;
                chunks
                    .try_reserve(1)
                    .map_err(|_| allocation_error(Resource::ChkChunksPerTag, 1))?;
                chunks.push(ChkChunk { offset, length });
            }
            by_offset
                .try_reserve(1)
                .map_err(|_| allocation_error(Resource::ChkChunks, 1))?;
            by_offset.insert(offset - 8, length);
            offset += length;
        } else {
            // This is a negative value, i.e. a "jump chunk" that reuses some previous data for
            // a new chunk

            // Jump sections have no data, they purely modify the current read position
            by_offset
                .try_reserve(1)
                .map_err(|_| allocation_error(Resource::ChkChunks, 1))?;
            by_offset.insert(offset - 8, 0);

            // Ensure that the jumped-to offset is within bounds (after the beginning of the file).
            // SC:R seems to ignore jump chunks that exceed the bounds, so we do as well.
            if length.unsigned_abs() as usize <= offset {
                offset -= length.unsigned_abs() as usize;
            }
        }
    }

    preflight_merged_chunk_sizes(&sections, limits)?;

    Ok(sections)
}

fn preflight_merged_chunk_sizes(
    chunk_map: &ChunkMap,
    limits: &ResourceLimits,
) -> Result<(), ChkError> {
    for (tag, chunks) in chunk_map {
        let chunk_type: ChunkType = (*tag).into();
        let merged_len = match chunk_type.multi_chunk_handling() {
            MultiChunkHandling::FullOverwrite => chunks.last().map_or(0, |chunk| chunk.length),
            MultiChunkHandling::PartialOverwrite => chunks
                .iter()
                .fold(0, |max_length, chunk| max_length.max(chunk.length)),
            MultiChunkHandling::Append => {
                let mut total = 0usize;
                for chunk in chunks {
                    total =
                        total
                            .checked_add(chunk.length)
                            .ok_or(ResourceLimitError::Exceeded {
                                resource: Resource::MergedChunkBytes,
                                observed: usize::MAX,
                                limit: limits.max_merged_chunk_bytes,
                            })?;
                }
                total
            }
        };
        ensure_within(
            Resource::MergedChunkBytes,
            merged_len,
            limits.max_merged_chunk_bytes,
        )?;
    }

    Ok(())
}

fn read_chunk_data<'a>(
    data: &'a [u8],
    chunk_map: &ChunkMap,
    chunk_type: ChunkType,
) -> Option<Cow<'a, [u8]>> {
    let chunks = chunk_map
        .get::<ChunkTag>(&chunk_type.into())
        .map(|v| v.as_slice())?;

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
                    let total_length = chunks
                        .iter()
                        .try_fold(0usize, |total, chunk| total.checked_add(chunk.length))?;
                    let mut result = Vec::new();
                    result.try_reserve_exact(total_length).ok()?;
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
    use crate::chk::placed_units::{UnitInstanceId, UnitState};
    use crate::chk::sprites::SpriteFlags;
    use crate::chk::triggers::{
        NumericComparison, PlayerGroup, RawTriggerAction, TriggerCondition,
    };
    use assert_ok::assert_ok;
    use rstest::rstest;
    use smallvec::smallvec;

    use super::strings::*;
    use super::*;

    #[test]
    fn chk_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Chk>();
    }

    const LT_CHK: &[u8] = include_bytes!("../../assets/lt.chk");
    const SECRET_BOUND_CHK: &[u8] = include_bytes!("../../assets/Secret_Bound.chk");

    #[test]
    fn raw_string_scanners_match_typed_trigger_parsers() {
        for chk_bytes in [LT_CHK, SECRET_BOUND_CHK] {
            let chk = assert_ok!(Chk::from_bytes(chk_bytes.into(), None));

            let trigger_data = read_chunk_data(&chk.data, &chk.chunks, ChunkType::TRIG)
                .expect("fixture has a trigger chunk");
            let scanned_trigger_ids =
                scan_used_trigger_string_ids(&trigger_data).collect::<Vec<_>>();
            let parsed_trigger_ids = assert_ok!(read_triggers(&trigger_data))
                .used_string_ids()
                .collect::<Vec<_>>();
            assert_eq!(scanned_trigger_ids, parsed_trigger_ids);

            let briefing_data = read_chunk_data(&chk.data, &chk.chunks, ChunkType::MBRF)
                .expect("fixture has a briefing chunk");
            let scanned_briefing_ids =
                scan_used_briefing_string_ids(&briefing_data).collect::<Vec<_>>();
            let parsed_briefing_ids = assert_ok!(read_briefing(&briefing_data))
                .used_string_ids()
                .collect::<Vec<_>>();
            assert_eq!(scanned_briefing_ids, parsed_briefing_ids);
        }
    }

    #[test]
    fn auto_encoding_detection_does_not_initialize_trigger_caches() {
        let chk = assert_ok!(Chk::from_bytes(SECRET_BOUND_CHK.into(), None));

        assert!(!chk.raw_triggers.get().is_some());
        assert!(!chk.raw_briefing.get().is_some());
        assert_ok!(chk.scenario_props());
        assert!(chk.strings.get().is_some());
        assert!(!chk.raw_triggers.get().is_some());
        assert!(!chk.raw_briefing.get().is_some());
    }

    #[test]
    fn sections_normal() {
        use crate::chk::chunk_type::ChunkType::*;

        let result = assert_ok!(Chk::from_bytes(LT_CHK.into(), None));

        assert_eq!(result.format_version(), FormatVersion::OriginalRetail);

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
        let result = assert_ok!(Chk::from_bytes(LT_CHK.into(), None));

        let raw_strings = result.raw_strings.clone();
        assert_eq!(raw_strings.data.kind, StringsChunkKind::Legacy);
        assert_eq!(raw_strings.max_len, 1024);
        assert_eq!(
            raw_strings.get_raw_bytes(1u16.into()).unwrap(),
            b"Untitled Scenario"
        );

        let scenario_props = assert_ok!(result.scenario_props());
        assert_eq!(scenario_props.name, Some("The Lost Temple".into()));
    }

    #[test]
    fn lt_triggers() {
        let result = assert_ok!(Chk::from_bytes(LT_CHK.into(), None));

        let triggers = assert_ok!(result.raw_triggers());
        assert_eq!(triggers.len(), 3);

        let first = &triggers[0];
        assert_eq!(first.conditions.len(), 16);
        assert_eq!(first.actions.len(), 64);
        let mut all_players = [false; 27];
        all_players[PlayerGroup::AllPlayers as usize] = true;
        assert_eq!(first.enabled_for, all_players);

        assert_eq!(
            first.conditions[0].condition,
            TriggerCondition::Command {
                player_group: PlayerGroup::CurrentPlayer,
                comparison: NumericComparison::AtMost,
                unit_id: 229, /* any unit */
                amount: 0,
            }
        );

        assert_eq!(first.actions[0].action, RawTriggerAction::Defeat);
    }

    #[test]
    fn lt_dimensions() {
        let result = assert_ok!(Chk::from_bytes(LT_CHK.into(), None));
        assert_eq!(result.width(), 128);
        assert_eq!(result.height(), 128);
    }

    #[test]
    fn lt_tileset() {
        let result = assert_ok!(Chk::from_bytes(LT_CHK.into(), None));
        assert_eq!(result.tileset(), Tileset::Jungle);
    }

    #[test]
    fn lt_terrain() {
        let result = assert_ok!(Chk::from_bytes(LT_CHK.into(), None));
        let terrain = assert_ok!(result.terrain());
        assert_eq!(terrain.tiles.len(), 128 * 128);
        assert_eq!(terrain[0][0], 0x16A0.into());
        assert_eq!(terrain[127][127], 0x1710.into());

        assert_eq!(terrain.get(0, 0), Some(0x16A0.into()));
        assert_eq!(terrain.get(128, 0), None);
    }

    const PROTECTED_2: &[u8] = include_bytes!("../../assets/protected-2.chk");

    #[test]
    fn unit_settings_expanded() {
        let result = assert_ok!(Chk::from_bytes(PROTECTED_2.into(), None));
        let unit_settings = assert_ok!(result.raw_unit_settings());
        assert_ne!(unit_settings.use_defaults, [true; 228]);
        assert_eq!(unit_settings.hp[0], 40 * 256);
        assert_eq!(unit_settings.name_id[0], 0x0162u16.into())
    }

    const SNIPER_SEED: &[u8] = include_bytes!("../../assets/Sniper_Seed_vA.chk");

    #[test]
    fn sprites() {
        let result = assert_ok!(Chk::from_bytes(SNIPER_SEED.into(), None));
        let sprites = assert_ok!(result.sprites());
        assert_eq!(sprites.len(), 216);
        assert_eq!(
            sprites[0],
            Sprite {
                id: 318,
                x: 5312,
                y: 7760,
                owner: 11,
                flags: SpriteFlags::DRAW_AS_SPRITE
            }
        );

        assert_eq!(
            sprites[27],
            Sprite {
                id: 417,
                x: 608,
                y: 7536,
                owner: 8,
                flags: SpriteFlags::DRAW_AS_SPRITE
            }
        );
    }

    #[test]
    fn placed_units() {
        let result = assert_ok!(Chk::from_bytes(SNIPER_SEED.into(), None));
        let placed_units = assert_ok!(result.placed_units());
        assert_eq!(placed_units.len(), 409);

        assert_eq!(
            placed_units[0],
            PlacedUnit {
                instance_id: UnitInstanceId(63693512),
                x: 7616,
                y: 7920,
                unit_id: 214,
                owner: Some(0),
                hp_percent: Some(100),
                shield_percent: None,
                energy_percent: None,
                resource_amount: None,
                hangar_count: None,
                state: UnitState::empty(),
                linked_id: Some(UnitInstanceId(1)),
            }
        );

        assert_eq!(
            placed_units[27],
            PlacedUnit {
                instance_id: UnitInstanceId(69203616),
                x: 3360,
                y: 3488,
                unit_id: 156,
                owner: Some(11),
                hp_percent: Some(15),
                shield_percent: Some(100),
                energy_percent: None,
                resource_amount: None,
                hangar_count: None,
                state: UnitState::INVINCIBLE,
                linked_id: None,
            }
        );
    }

    #[test]
    fn start_locations_have_owners() {
        let result = assert_ok!(Chk::from_bytes(LT_CHK.into(), None));
        let placed_units = assert_ok!(result.placed_units());

        let start_locations = placed_units
            .iter()
            .filter(|u| u.unit_id == 214)
            .collect::<Vec<_>>();
        assert_eq!(start_locations.len(), 4);
        assert_eq!(start_locations[0].owner, Some(2));
        assert_eq!(start_locations[1].owner, Some(3));
        assert_eq!(start_locations[2].owner, Some(0));
        assert_eq!(start_locations[3].owner, Some(1));
    }

    const KOR_1: &[u8] = include_bytes!("../../assets/kor_encoding/1.chk");
    const KOR_2: &[u8] = include_bytes!("../../assets/kor_encoding/2.chk");
    const KOR_3: &[u8] = include_bytes!("../../assets/kor_encoding/3.chk");
    // Note: 4.chk is a Korean map which has been edited later to have Western text.
    // As such, the heuristic is more fragile than usual.
    const KOR_4: &[u8] = include_bytes!("../../assets/kor_encoding/4.chk");
    const KOR_5: &[u8] = include_bytes!("../../assets/kor_encoding/5.chk");
    const KOR_6: &[u8] = include_bytes!("../../assets/kor_encoding/6.chk");
    const KOR_7: &[u8] = include_bytes!("../../assets/kor_encoding/7.chk");
    const KOR_8: &[u8] = include_bytes!("../../assets/kor_encoding/8.chk");
    const KOR_9: &[u8] = include_bytes!("../../assets/kor_encoding/9.chk");

    const LATIN_1: &[u8] = include_bytes!("../../assets/lat_encoding/w1.chk");
    const LATIN_2: &[u8] = include_bytes!("../../assets/lat_encoding/w2.chk");
    const LATIN_3: &[u8] = include_bytes!("../../assets/lat_encoding/w3.chk");
    const LATIN_4: &[u8] = include_bytes!("../../assets/lat_encoding/w4.chk");

    #[rstest]
    #[case::kor_1(KOR_1, StringEncoding::Legacy(LegacyCodePage::Korean))]
    #[case::kor_2(KOR_2, StringEncoding::Legacy(LegacyCodePage::Korean))]
    #[case::kor_3(KOR_3, StringEncoding::Legacy(LegacyCodePage::Korean))]
    #[case::kor_4(KOR_4, StringEncoding::Legacy(LegacyCodePage::Korean))]
    #[case::kor_5(KOR_5, StringEncoding::Legacy(LegacyCodePage::Korean))]
    #[case::kor_6(KOR_6, StringEncoding::Legacy(LegacyCodePage::Korean))]
    #[case::kor_7(KOR_7, StringEncoding::Legacy(LegacyCodePage::Korean))]
    #[case::kor_8(KOR_8, StringEncoding::Legacy(LegacyCodePage::Korean))]
    #[case::kor_9(KOR_9, StringEncoding::Legacy(LegacyCodePage::Korean))]
    #[case::latin_1(LATIN_1, StringEncoding::Legacy(LegacyCodePage::Latin))]
    #[case::latin_2(LATIN_2, StringEncoding::Legacy(LegacyCodePage::Latin))]
    #[case::latin_3(LATIN_3, StringEncoding::Legacy(LegacyCodePage::Latin))]
    #[case::latin_4(LATIN_4, StringEncoding::Legacy(LegacyCodePage::Latin))]
    fn encoding_heuristic(#[case] chk: &[u8], #[case] expected: StringEncoding) {
        let result = assert_ok!(Chk::from_bytes(chk.into(), None));
        assert_eq!(result.strings().encoding, expected);
    }

    const MIXED_1: &[u8] = include_bytes!("../../assets/mixed_encoding_1.chk");

    #[test]
    fn mixed_encoding_1() {
        let result = assert_ok!(Chk::from_bytes(MIXED_1.into(), None));
        assert_eq!(
            result.strings().encoding,
            StringEncoding::Utf8WithFallback(LegacyCodePage::Korean)
        );

        let scenario_props = assert_ok!(result.scenario_props());
        assert_eq!(
            scenario_props.name,
            Some("\x06피아노\x03 마스터\x04v5.3A".into())
        );
        assert_eq!(
            scenario_props.description,
            Some(
                concat!(
                    "제작 : 믹넛 TTNSM / korea\r\n",
                    "아이디어 : DeratoY (EDAC)\r\n\r\n",
                    "Thanks for Artanis / 맛있는빙수 / Terran_Wraith\r\n",
                    "Thanks for You"
                )
                .into()
            )
        )
    }

    const MIXED_2: &[u8] = include_bytes!("../../assets/mixed_encoding_2.chk");

    #[test]
    fn mixed_encoding_2() {
        let result = assert_ok!(Chk::from_bytes(MIXED_2.into(), None));
        assert_eq!(
            result.strings().encoding,
            StringEncoding::Utf8WithFallback(LegacyCodePage::Latin)
        );

        let scenario_props = assert_ok!(result.scenario_props());
        assert_eq!(
            scenario_props.description,
            Some("Défendre Map by Sadrio Fuck you No join me ist not me, not sex".into())
        );
    }

    const UTF8_MAP: &[u8] = include_bytes!("../../assets/utf8_encoding_1.chk");

    #[test]
    fn utf8_encoding() {
        let result = assert_ok!(Chk::from_bytes(UTF8_MAP.into(), None));
        assert_eq!(result.strings().encoding, StringEncoding::Utf8);

        let scenario_props = assert_ok!(result.scenario_props());
        assert_eq!(
            scenario_props.description,
            Some(
                concat!(
                    "Fall asleep in the mirror,\r\n",
                    "Inside of this endless forever repeating nightmare\r\n",
                    "Please be stuck here like this forever\r\n\r\n",
                    "Created by - 효진(CrystalDrag)\r\n",
                    "Version 1.31"
                )
                .into()
            )
        );
    }

    const SECRET_BOUND: &[u8] = include_bytes!("../../assets/Secret_Bound.chk");

    #[test]
    fn invalid_jump_chunk_regression() {
        assert_ok!(Chk::from_bytes(SECRET_BOUND.into(), None));
    }

    fn chunk(tag: ChunkTag, contents: &[u8]) -> Vec<u8> {
        let mut result = Vec::with_capacity(8 + contents.len());
        result.extend_from_slice(&tag);
        result.extend_from_slice(&(contents.len() as i32).to_le_bytes());
        result.extend_from_slice(contents);
        result
    }

    #[test]
    fn resource_limit_rejects_oversized_chk_before_scanning() {
        let limits = ResourceLimits {
            max_chk_bytes: 7,
            ..ResourceLimits::trusted()
        };

        let error = Chk::from_bytes_with_limits(vec![0; 8], None, &limits).unwrap_err();
        assert!(matches!(
            error,
            ChkError::ResourceLimit(ResourceLimitError::Exceeded {
                resource: Resource::ChkBytes,
                observed: 8,
                limit: 7,
            })
        ));
    }

    #[test]
    fn resource_limit_counts_jump_chunk_visits() {
        let mut data = Vec::new();
        data.extend_from_slice(b"JUMP");
        data.extend_from_slice(&(-8i32).to_le_bytes());
        let limits = ResourceLimits {
            max_chk_chunks: 1,
            ..ResourceLimits::trusted()
        };

        let error = gather_chunk_map(&data, &limits).unwrap_err();
        assert!(matches!(
            error,
            ChkError::ResourceLimit(ResourceLimitError::Exceeded {
                resource: Resource::ChkChunks,
                observed: 2,
                limit: 1,
            })
        ));
    }

    #[test]
    fn resource_limit_rejects_too_many_chunks_for_one_tag() {
        let data = [
            chunk(*b"UNIT", &[]),
            chunk(*b"UNIT", &[]),
            chunk(*b"UNIT", &[]),
        ]
        .concat();
        let limits = ResourceLimits {
            max_chk_chunks_per_tag: 2,
            ..ResourceLimits::trusted()
        };

        let error = gather_chunk_map(&data, &limits).unwrap_err();
        assert!(matches!(
            error,
            ChkError::ResourceLimit(ResourceLimitError::Exceeded {
                resource: Resource::ChkChunksPerTag,
                observed: 3,
                limit: 2,
            })
        ));
    }

    #[test]
    fn resource_limit_rejects_oversized_appended_chunk_data() {
        let data = [chunk(*b"UNIT", b"abc"), chunk(*b"UNIT", b"def")].concat();
        let limits = ResourceLimits {
            max_merged_chunk_bytes: 5,
            ..ResourceLimits::trusted()
        };

        let error = gather_chunk_map(&data, &limits).unwrap_err();
        assert!(matches!(
            error,
            ChkError::ResourceLimit(ResourceLimitError::Exceeded {
                resource: Resource::MergedChunkBytes,
                observed: 6,
                limit: 5,
            })
        ));
    }

    #[test]
    fn resource_limit_accepts_appended_chunk_data_at_limit() {
        let data = [chunk(*b"UNIT", b"abc"), chunk(*b"UNIT", b"def")].concat();
        let limits = ResourceLimits {
            max_merged_chunk_bytes: 6,
            ..ResourceLimits::trusted()
        };

        let chunks = assert_ok!(gather_chunk_map(&data, &limits));
        assert_eq!(
            read_chunk_data(&data, &chunks, ChunkType::UNIT).as_deref(),
            Some(&b"abcdef"[..])
        );
    }

    /// Builds the minimal set of chunks required for [`Chk::from_bytes`] to succeed (`VER `,
    /// `DIM `, `ERA `, `STR `), followed by any additional raw chunk bytes.
    fn minimal_chk_with_extra(extra: &[u8]) -> Vec<u8> {
        let mut data = Vec::new();
        data.extend(chunk(*b"VER ", &206u16.to_le_bytes()));
        data.extend(chunk(*b"DIM ", &[64, 0, 64, 0]));
        data.extend(chunk(*b"ERA ", &0u16.to_le_bytes()));
        data.extend(chunk(*b"STR ", &0u16.to_le_bytes()));
        data.extend_from_slice(extra);
        data
    }

    #[test]
    fn player_colors_defaults_when_chunks_absent() {
        let data = minimal_chk_with_extra(&[]);
        let chk = assert_ok!(Chk::from_bytes(data, None));

        assert_eq!(
            chk.player_colors().colors,
            [player_colors::PlayerColor::Default; 8]
        );
    }

    #[test]
    fn player_colors_from_colr_only() {
        let colr = chunk(*b"COLR", &[0, 1, 2, 3, 4, 5, 6, 7]);
        let data = minimal_chk_with_extra(&colr);
        let chk = assert_ok!(Chk::from_bytes(data, None));

        for (i, color) in chk.player_colors().colors.iter().enumerate() {
            assert_eq!(*color, player_colors::PlayerColor::Indexed(i as u8));
        }
    }

    #[test]
    fn player_colors_crgb_takes_precedence_over_colr() {
        let colr = chunk(*b"COLR", &[7u8; 8]);

        let mut crgb_bytes = Vec::with_capacity(32);
        crgb_bytes.extend_from_slice(&[1, 2, 3]); // player 0's RGB triple
        crgb_bytes.extend_from_slice(&[0u8; 3 * 7]); // players 1-7's RGB triples (unused here)
        // Selection bytes: player 0 = Custom RGB (2), player 1 = Random (0), rest = Default (1).
        crgb_bytes.extend_from_slice(&[2, 0, 1, 1, 1, 1, 1, 1]);
        let crgb = chunk(*b"CRGB", &crgb_bytes);

        let mut data = minimal_chk_with_extra(&colr);
        data.extend(crgb);
        let chk = assert_ok!(Chk::from_bytes(data, None));

        let colors = chk.player_colors().colors;
        assert_eq!(colors[0], player_colors::PlayerColor::Rgb([1, 2, 3]));
        assert_eq!(colors[1], player_colors::PlayerColor::Random);
        assert_eq!(colors[2], player_colors::PlayerColor::Default);
    }

    #[test]
    fn player_colors_ignores_malformed_chunks() {
        // A COLR chunk that's the wrong size gets skipped by the chunk gatherer (min/max size of
        // 8 bytes), so this should behave the same as no COLR chunk being present at all.
        let colr = chunk(*b"COLR", &[0, 1, 2]);
        let data = minimal_chk_with_extra(&colr);
        let chk = assert_ok!(Chk::from_bytes(data, None));

        assert_eq!(
            chk.player_colors().colors,
            [player_colors::PlayerColor::Default; 8]
        );
    }
}
