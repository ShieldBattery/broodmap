use crate::chk::strings::{ChkDecode, StringId, StringsChunk, UsedChkStrings};
use thiserror::Error;

/// The SPRP chunk of a CHK file (Scenario Properties) with strings that are not decoded.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct RawScenarioProps {
    /// The string ID of the scenario name. Must be looked up/decoded from the strings chunk. `0`
    /// indicates this should be the map's filename.
    pub name_id: StringId,
    /// The string ID of the scenario description. Must be looked up/decoded from the strings chunk.
    /// `0` indicates this should be a default string.
    // TODO(tec27): look up what the default is
    pub description_id: StringId,
}

/// The SPRP chunk of a CHK file (Scenario Properties).
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ScenarioProps {
    /// The scenario name. [None] indicates this should be the map's filename.
    pub name: Option<String>,
    /// The scenario description. [None] indicates this should be a default string.
    // TODO(tec27): look up what the default is
    pub description: Option<String>,
}

impl ChkDecode<ScenarioProps> for RawScenarioProps {
    fn decode_strings(&self, strings_chunk: &StringsChunk) -> ScenarioProps {
        ScenarioProps {
            name: strings_chunk.get(self.name_id).map(|s| s.into()),
            description: strings_chunk.get(self.description_id).map(|s| s.into()),
        }
    }
}

impl UsedChkStrings for RawScenarioProps {
    fn used_string_ids(&self) -> Box<dyn Iterator<Item = StringId>> {
        Box::new([self.name_id, self.description_id].into_iter())
    }
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum ScenarioPropsError {
    #[error("Chunk missing")]
    ChunkMissing,
    #[error("Invalid data length")]
    InvalidDataLength,
}

pub fn read_scenario_props(data: &[u8]) -> Result<RawScenarioProps, ScenarioPropsError> {
    if data.len() != 4 {
        return Err(ScenarioPropsError::InvalidDataLength);
    }

    let name_id = u16::from_le_bytes(data[0..2].try_into().unwrap()).into();
    let description_id = u16::from_le_bytes(data[2..4].try_into().unwrap()).into();

    Ok(RawScenarioProps {
        name_id,
        description_id,
    })
}
