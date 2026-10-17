//! Parsing for the `OWNR` ("StarCraft player types") and `SIDE` ("Player races") chunks, which
//! hold one byte per player slot (12 slots: players 1-8, plus the 3 unused slots 9-11 and the
//! neutral player 12).
//!
//! Values are returned exactly as stored, including ones without a documented meaning, so
//! consumers can apply their own rules for which slots are playable. The documented values come
//! from the StarEdit Network CHK format wiki
//! (<http://www.staredit.net/wiki/index.php?title=Scenario.chk>), which notes that several of them
//! are not valid map options in BW's own editor.
//!
//! Chunks of either type shorter than 12 bytes are ignored entirely (an earlier chunk of the same
//! type remains in effect), longer ones are cut off at 12 bytes, and when multiple chunks of the
//! same type remain the last one wins (see [`ChunkType::min_size`], [`ChunkType::max_size`] and
//! [`ChunkType::multi_chunk_handling`]). Protected maps commonly rely on this by placing decoy
//! chunks ahead of the real one.
//!
//! [`ChunkType::min_size`]: crate::chk::chunk_type::ChunkType::min_size
//! [`ChunkType::max_size`]: crate::chk::chunk_type::ChunkType::max_size
//! [`ChunkType::multi_chunk_handling`]: crate::chk::chunk_type::ChunkType::multi_chunk_handling
use thiserror::Error;

/// The number of player slots described by the `OWNR` and `SIDE` chunks.
pub const PLAYER_SLOT_COUNT: usize = 12;

/// The controller of a player slot, as stored in the `OWNR` chunk.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum PlayerType {
    /// `0`: The slot is inactive.
    Inactive,
    /// `1`: Computer (game). Not a valid map option in the editor.
    ComputerGame,
    /// `2`: Occupied by a human player. Not a valid map option in the editor.
    OccupiedHuman,
    /// `3`: Rescuable (rescue passive).
    RescuePassive,
    /// `4`: Unused.
    Unused,
    /// `5`: Computer.
    Computer,
    /// `6`: Human (an open slot in the lobby).
    Human,
    /// `7`: Neutral.
    Neutral,
    /// `8`: Closed slot. Not a valid map option in the editor.
    Closed,
    /// Any value without a documented meaning.
    Other(u8),
}

impl From<u8> for PlayerType {
    fn from(value: u8) -> Self {
        match value {
            0 => PlayerType::Inactive,
            1 => PlayerType::ComputerGame,
            2 => PlayerType::OccupiedHuman,
            3 => PlayerType::RescuePassive,
            4 => PlayerType::Unused,
            5 => PlayerType::Computer,
            6 => PlayerType::Human,
            7 => PlayerType::Neutral,
            8 => PlayerType::Closed,
            other => PlayerType::Other(other),
        }
    }
}

impl From<PlayerType> for u8 {
    fn from(value: PlayerType) -> Self {
        match value {
            PlayerType::Inactive => 0,
            PlayerType::ComputerGame => 1,
            PlayerType::OccupiedHuman => 2,
            PlayerType::RescuePassive => 3,
            PlayerType::Unused => 4,
            PlayerType::Computer => 5,
            PlayerType::Human => 6,
            PlayerType::Neutral => 7,
            PlayerType::Closed => 8,
            PlayerType::Other(other) => other,
        }
    }
}

/// The race of a player slot, as stored in the `SIDE` chunk.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Race {
    /// `0`: Zerg.
    Zerg,
    /// `1`: Terran.
    Terran,
    /// `2`: Protoss.
    Protoss,
    /// `3`: Independent. Not a valid map option; shown as "Unknown" in the SC:R lobby.
    Independent,
    /// `4`: Neutral. Not a valid map option; shown as "Unknown" in the SC:R lobby.
    Neutral,
    /// `5`: The player selects their own race.
    UserSelectable,
    /// `6`: Forced random (acts as a selected race).
    Random,
    /// `7`: Inactive. Not a valid map option; shown as a locked race selection in the SC:R lobby.
    Inactive,
    /// Any value without a documented meaning.
    Other(u8),
}

impl From<u8> for Race {
    fn from(value: u8) -> Self {
        match value {
            0 => Race::Zerg,
            1 => Race::Terran,
            2 => Race::Protoss,
            3 => Race::Independent,
            4 => Race::Neutral,
            5 => Race::UserSelectable,
            6 => Race::Random,
            7 => Race::Inactive,
            other => Race::Other(other),
        }
    }
}

impl From<Race> for u8 {
    fn from(value: Race) -> Self {
        match value {
            Race::Zerg => 0,
            Race::Terran => 1,
            Race::Protoss => 2,
            Race::Independent => 3,
            Race::Neutral => 4,
            Race::UserSelectable => 5,
            Race::Random => 6,
            Race::Inactive => 7,
            Race::Other(other) => other,
        }
    }
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum PlayerTypesError {
    #[error("Chunk missing")]
    ChunkMissing,
    #[error("Invalid data length")]
    InvalidDataLength,
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum PlayerRacesError {
    #[error("Chunk missing")]
    ChunkMissing,
    #[error("Invalid data length")]
    InvalidDataLength,
}

/// Reads the contents of an `OWNR` chunk: the controller of each of the 12 player slots.
pub fn read_player_types(data: &[u8]) -> Result<[PlayerType; PLAYER_SLOT_COUNT], PlayerTypesError> {
    let data: &[u8; PLAYER_SLOT_COUNT] = data
        .try_into()
        .map_err(|_| PlayerTypesError::InvalidDataLength)?;
    Ok(data.map(PlayerType::from))
}

/// Reads the contents of a `SIDE` chunk: the race of each of the 12 player slots.
pub fn read_player_races(data: &[u8]) -> Result<[Race; PLAYER_SLOT_COUNT], PlayerRacesError> {
    let data: &[u8; PLAYER_SLOT_COUNT] = data
        .try_into()
        .map_err(|_| PlayerRacesError::InvalidDataLength)?;
    Ok(data.map(Race::from))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn player_type_round_trips_every_byte() {
        for value in 0..=u8::MAX {
            assert_eq!(u8::from(PlayerType::from(value)), value);
        }
    }

    #[test]
    fn race_round_trips_every_byte() {
        for value in 0..=u8::MAX {
            assert_eq!(u8::from(Race::from(value)), value);
        }
    }

    #[test]
    fn undocumented_values_are_kept_raw() {
        assert_eq!(PlayerType::from(9), PlayerType::Other(9));
        assert_eq!(PlayerType::from(255), PlayerType::Other(255));
        assert_eq!(Race::from(8), Race::Other(8));
        assert_eq!(Race::from(255), Race::Other(255));
    }

    #[test]
    fn read_player_types_valid() {
        let data = [0u8, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0x80, 7];
        assert_eq!(
            read_player_types(&data),
            Ok([
                PlayerType::Inactive,
                PlayerType::ComputerGame,
                PlayerType::OccupiedHuman,
                PlayerType::RescuePassive,
                PlayerType::Unused,
                PlayerType::Computer,
                PlayerType::Human,
                PlayerType::Neutral,
                PlayerType::Closed,
                PlayerType::Other(9),
                PlayerType::Other(0x80),
                PlayerType::Neutral,
            ])
        );
    }

    #[test]
    fn read_player_races_valid() {
        let data = [0u8, 1, 2, 3, 4, 5, 6, 7, 8, 0xFF, 7, 4];
        assert_eq!(
            read_player_races(&data),
            Ok([
                Race::Zerg,
                Race::Terran,
                Race::Protoss,
                Race::Independent,
                Race::Neutral,
                Race::UserSelectable,
                Race::Random,
                Race::Inactive,
                Race::Other(8),
                Race::Other(0xFF),
                Race::Inactive,
                Race::Neutral,
            ])
        );
    }

    #[test]
    fn read_wrong_size() {
        for len in [0, 8, 11, 13] {
            let data = vec![0u8; len];
            assert_eq!(
                read_player_types(&data),
                Err(PlayerTypesError::InvalidDataLength)
            );
            assert_eq!(
                read_player_races(&data),
                Err(PlayerRacesError::InvalidDataLength)
            );
        }
    }
}
