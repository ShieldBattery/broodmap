use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::chk::briefing::{read_briefing, BriefingError, RawBriefingTrigger};
use once_cell::sync::OnceCell;
use smallvec::SmallVec;
use thiserror::Error;

use crate::chk::chunk_type::{ChunkTag, ChunkType, MultiChunkHandling};
use crate::chk::dimensions::{read_dimensions, DimensionsError, MapDimensions};
use crate::chk::forces::{
    read_force_settings, ForceSettings, ForceSettingsError, RawForceSettings,
};
use crate::chk::format_version::{read_format_version, FormatVersion, FormatVersionError};
use crate::chk::scenario_props::{
    read_scenario_props, RawScenarioProps, ScenarioProps, ScenarioPropsError,
};
use crate::chk::strings::{
    ChkDecode, RawStringsChunk, StringEncoding, StringsChunk, StringsChunkError, UsedChkStrings,
};
use crate::chk::terrain::{read_terrain_mega_tiles, TerrainMegaTiles, TerrainMegaTilesError};
use crate::chk::tileset::{read_tileset, Tileset, TilesetError};
use crate::chk::triggers::{read_triggers, RawTrigger, TriggersError};
use crate::chk::unit_settings::{RawUnitSettings, UnitSettingsError};

pub mod briefing;
pub mod chunk_type;
pub mod dimensions;
pub mod forces;
pub mod format_version;
pub mod scenario_props;
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
    #[error("Invalid dimensions: {0}")]
    InvalidDimensions(DimensionsError),
    #[error("Invalid format version: {0}")]
    InvalidFormatVersion(FormatVersionError),
    /// A chunk was encountered that jumped past the beginning of the file.
    #[error("Invalid jump chunk")]
    InvalidJumpChunk,
    #[error("Invalid strings chunk: {0}")]
    InvalidStringsChunk(StringsChunkError),
    #[error("Invalid tileset: {0}")]
    InvalidTileset(TilesetError),
    #[error("Missing required chunk: {0}")]
    MissingRequiredChunk(ChunkType),
}

pub type ChunkMap = HashMap<ChunkTag, SmallVec<[ChkChunk; 1]>>;

#[derive(Debug, Clone)]
pub struct Chk {
    pub data: Vec<u8>,
    pub desired_encoding: Option<StringEncoding>,
    pub chunks: ChunkMap,

    format_version: FormatVersion,
    raw_strings: Arc<RawStringsChunk>,
    dimensions: MapDimensions,
    tileset: Tileset,
    raw_scenario_props: OnceCell<Result<RawScenarioProps, ScenarioPropsError>>,
    raw_force_settings: OnceCell<Result<RawForceSettings, ForceSettingsError>>,
    raw_triggers: OnceCell<Result<Vec<RawTrigger>, TriggersError>>,
    raw_briefing: OnceCell<Result<Vec<RawBriefingTrigger>, BriefingError>>,
    raw_unit_settings: OnceCell<Result<RawUnitSettings, UnitSettingsError>>,

    strings: OnceCell<StringsChunk>,
    scenario_props: OnceCell<Result<ScenarioProps, ScenarioPropsError>>,
    force_settings: OnceCell<Result<ForceSettings, ForceSettingsError>>,
    terrain_mega_tiles: OnceCell<Result<TerrainMegaTiles, TerrainMegaTilesError>>,
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
        let chunks = gather_chunk_map(&data)?;

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
            raw_scenario_props: OnceCell::new(),
            raw_force_settings: OnceCell::new(),
            raw_triggers: OnceCell::new(),
            raw_briefing: OnceCell::new(),
            raw_unit_settings: OnceCell::new(),

            strings: OnceCell::new(),
            scenario_props: OnceCell::new(),
            force_settings: OnceCell::new(),
            terrain_mega_tiles: OnceCell::new(),
        })
    }

    pub fn strings(&self) -> &StringsChunk {
        self.strings.get_or_init(|| {
            if let Some(encoding) = self.desired_encoding {
                StringsChunk::with_known_encoding(self.raw_strings.clone(), encoding)
            } else {
                let used_strings = [
                    self.raw_scenario_props().used_string_ids(),
                    self.raw_force_settings().used_string_ids(),
                    self.raw_triggers_private().used_string_ids(),
                    self.raw_briefing_private().used_string_ids(),
                    self.raw_unit_settings().used_string_ids(),
                ]
                .into_iter()
                .flatten()
                .collect::<HashSet<_>>();
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

    pub fn terrain_mega_tiles(&self) -> Result<&TerrainMegaTiles, &TerrainMegaTilesError> {
        self.terrain_mega_tiles
            .get_or_init(|| {
                read_terrain_mega_tiles(
                    &read_chunk_data(&self.data, &self.chunks, ChunkType::MTXM)
                        .ok_or(TerrainMegaTilesError::ChunkMissing)?,
                    self.width(),
                    self.height(),
                )
            })
            .as_ref()
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
        let terrain = assert_ok!(result.terrain_mega_tiles());
        assert_eq!(terrain.tiles.len(), 128 * 128);
        assert_eq!(terrain[0][0], 0x16A0);
        assert_eq!(terrain[127][127], 0x1710);

        assert_eq!(terrain.get(0, 0), Some(0x16A0));
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
}
