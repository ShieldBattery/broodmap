use crate::chk::strings::StringId;
use bitflags::bitflags;
use std::borrow::Cow;
use thiserror::Error;

bitflags! {
    pub struct ForceFlags: u8 {
        const RANDOM_START_LOCATION = 0b0001;
        const ALLIED = 0b0010;
        const ALLIED_VICTORY = 0b0100;
        const SHARED_VISION = 0b1000;
    }
}

/// A force in a scenario file. Scenarios have 4 forces under which players are grouped, each force
/// having its own settings.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Force {
    /// The string ID of the force name. Must be looked up/decoded from the strings chunk. `0`
    /// indicates this should default to a "Force #" string.
    pub name_id: StringId,
    pub flags: ForceFlags,
}

/// The contents of the FORC chunk of a CHK file (Force Settings).
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct ForceSettings {
    /// The configuration for each force in the scenario.
    pub forces: [Force; 4],
    /// The assignment of players to forces (indexed by player ID, value is the index in [forces]).
    /// Players can be outside of 1 of the 4 forces, however they will not appear in the game lobby.
    pub assigned_forces: [u8; 8],
}

#[derive(Error, Debug)]
pub enum ForceSettingsError {
    #[error("Invalid data length")]
    InvalidDataLength,
}

pub fn read_force_settings(data: &[u8]) -> Result<ForceSettings, ForceSettingsError> {
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
        .map(|(name_id, flags)| Force { name_id, flags })
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();

    Ok(ForceSettings {
        forces,
        assigned_forces,
    })
}
