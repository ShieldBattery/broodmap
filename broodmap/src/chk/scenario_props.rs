use thiserror::Error;

/// The SPRP chunk of a CHK file (Scenario Properties).
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct ScenarioProps {
    /// The string ID of the scenario name. Must be looked up/decoded from the strings chunk. `0`
    /// indicates this should be the map's filename.
    pub name_id: u16,
    /// The string ID of the scenario description. Must be looked up/decoded from the strings chunk.
    /// `0` indicates this should be a default string.
    // TODO(tec27): look up what the default is
    pub description_id: u16,
}

#[derive(Error, Debug)]
pub enum ScenarioPropsError {
    #[error("Invalid data length")]
    InvalidDataLength,
}

pub fn read_scenario_props(data: &[u8]) -> Result<ScenarioProps, ScenarioPropsError> {
    if data.len() != 4 {
        return Err(ScenarioPropsError::InvalidDataLength);
    }

    let name_id = u16::from_le_bytes(data[0..2].try_into().unwrap());
    let description_id = u16::from_le_bytes(data[2..4].try_into().unwrap());

    Ok(ScenarioProps {
        name_id,
        description_id,
    })
}
