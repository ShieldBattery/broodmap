use crate::chk::strings::{ChkDecode, StringId, StringsChunk, UsedChkStrings};
use bitflags::bitflags;
use std::borrow::Cow;
use thiserror::Error;

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct ForceFlags: u8 {
        const RANDOM_START_LOCATION = 0b0001;
        const ALLIED = 0b0010;
        const ALLIED_VICTORY = 0b0100;
        const SHARED_VISION = 0b1000;
    }
}

/// A force in a scenario file, with its strings not decoded. Scenarios have 4 forces under which
/// players are grouped, each force having its own settings.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct RawForce {
    /// The string ID of the force name. Must be looked up/decoded from the strings chunk. `0`
    /// indicates this should default to a "Force #" string.
    pub name_id: StringId,
    pub flags: ForceFlags,
}

/// The contents of the FORC chunk of a CHK file (Force Settings) with its strings not decoded.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct RawForceSettings {
    /// The configuration for each force in the scenario.
    pub forces: [RawForce; 4],
    /// The assignment of players to forces (indexed by player ID, value is the index in [forces]).
    /// Players can be outside of 1 of the 4 forces, however they will not appear in the game lobby.
    pub assigned_forces: [u8; 8],
}

/// A force in a scenario file. Scenarios have 4 forces under which players are grouped, each force
/// having its own settings.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Force {
    /// The force name. [None] indicates this should default to a "Force #" string.
    pub name: Option<String>,
    pub flags: ForceFlags,
}

/// The contents of the FORC chunk of a CHK file (Force Settings).
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ForceSettings {
    /// The configuration for each force in the scenario.
    pub forces: [Force; 4],
    /// The assignment of players to forces (indexed by player ID, value is the index in [forces]).
    /// Players can be outside of 1 of the 4 forces, however they will not appear in the game lobby.
    pub assigned_forces: [u8; 8],
}

impl ChkDecode<ForceSettings> for RawForceSettings {
    fn decode_strings(&self, strings_chunk: &StringsChunk) -> ForceSettings {
        ForceSettings {
            forces: self.forces.map(|f| Force {
                name: strings_chunk.get(f.name_id).map(|s| s.into()),
                flags: f.flags,
            }),
            assigned_forces: self.assigned_forces,
        }
    }
}

impl UsedChkStrings for RawForceSettings {
    fn used_string_ids(&self) -> Box<dyn Iterator<Item = StringId> + '_> {
        Box::new(self.forces.iter().map(|f| f.name_id))
    }
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum ForceSettingsError {
    #[error("Chunk missing")]
    ChunkMissing,
    #[error("Invalid data length")]
    InvalidDataLength,
}

pub fn read_force_settings(data: &[u8]) -> Result<RawForceSettings, ForceSettingsError> {
    if data.len() > 20 {
        return Err(ForceSettingsError::InvalidDataLength);
    }

    let mut data = Cow::from(data);
    if data.len() < 20 {
        data.to_mut().resize(20, 0);
    }

    let assigned_forces: [u8; 8] = data[0..8].try_into().unwrap();
    let force_names_iter = data[8..16]
        .chunks_exact(2)
        .map(|c| u16::from_le_bytes(c.try_into().unwrap()).into());
    let force_flags_iter = data[16..20]
        .iter()
        .map(|f| ForceFlags::from_bits_truncate(*f));

    let forces = force_names_iter
        .zip(force_flags_iter)
        .map(|(name_id, flags)| RawForce { name_id, flags })
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();

    Ok(RawForceSettings {
        forces,
        assigned_forces,
    })
}
