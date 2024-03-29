use bitflags::bitflags;
use thiserror::Error;

const START_LOCATION_ID: u16 = 214;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct UnitInstanceId(pub u32);

impl From<u32> for UnitInstanceId {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct ValidUnitData: u16 {
        const OWNER = 0x01;
        const HP = 0x02;
        const SHIELD = 0x04;
        const ENERGY = 0x08;
        const RESOURCES = 0x10;
        const HANGAR_COUNT = 0x20;
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct UnitState: u16 {
        const CLOAKED = 0x01;
        const BURROWED = 0x02;
        const IN_TRANSIT = 0x04;
        const HALLUCINATED = 0x08;
        const INVINCIBLE = 0x10;
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct PlacedUnit {
    pub instance_id: UnitInstanceId,
    pub x: u16,
    pub y: u16,
    pub unit_id: u16,
    pub owner: Option<u8>,
    pub hp_percent: Option<u8>,
    pub shield_percent: Option<u8>,
    pub energy_percent: Option<u8>,
    pub resource_amount: Option<u32>,
    pub hangar_count: Option<u16>,
    pub state: UnitState,
    pub linked_id: Option<UnitInstanceId>,
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum PlacedUnitsError {
    #[error("Chunk missing")]
    ChunkMissing,
}

/// Reads the placed units chunk of a CHK file (aka the UNIT chunk).
pub fn read_placed_units(data: &[u8]) -> Result<Vec<PlacedUnit>, PlacedUnitsError> {
    let result = data
        .chunks_exact(36)
        .map(|chunk| {
            let instance_id = u32::from_le_bytes(chunk[0..4].try_into().unwrap());
            let x = u16::from_le_bytes(chunk[4..6].try_into().unwrap());
            let y = u16::from_le_bytes(chunk[6..8].try_into().unwrap());
            let unit_id = u16::from_le_bytes(chunk[8..10].try_into().unwrap());

            // 10..12 is link type, unused by BW

            let valid_state = UnitState::from_bits_truncate(u16::from_le_bytes(
                chunk[12..14].try_into().unwrap(),
            ));
            let valid_data = ValidUnitData::from_bits_truncate(u16::from_le_bytes(
                chunk[14..16].try_into().unwrap(),
            ));

            // NOTE(tec27): On melee maps (and maybe others?) the start location is often marked as
            // not having a valid owner, but start locations *must* have a valid owner, so we always
            // utilize this field for them. The description of this value of ValidUnitData may be
            // incorrect, further research is needed.
            let owner = if valid_data.contains(ValidUnitData::OWNER) || unit_id == START_LOCATION_ID
            {
                Some(chunk[16])
            } else {
                None
            };
            let hp_percent = if valid_data.contains(ValidUnitData::HP) {
                Some(chunk[17])
            } else {
                None
            };
            let shield_percent = if valid_data.contains(ValidUnitData::SHIELD) {
                Some(chunk[18])
            } else {
                None
            };
            let energy_percent = if valid_data.contains(ValidUnitData::ENERGY) {
                Some(chunk[19])
            } else {
                None
            };
            let resource_amount = if valid_data.contains(ValidUnitData::RESOURCES) {
                Some(u32::from_le_bytes(chunk[20..24].try_into().unwrap()))
            } else {
                None
            };
            let hangar_count = if valid_data.contains(ValidUnitData::HANGAR_COUNT) {
                Some(u16::from_le_bytes(chunk[24..26].try_into().unwrap()))
            } else {
                None
            };

            let state = UnitState::from_bits_truncate(u16::from_le_bytes(
                chunk[26..28].try_into().unwrap(),
            )) & valid_state;

            // Bytes 28..32 are unused

            let linked_id_value = u32::from_le_bytes(chunk[32..36].try_into().unwrap());
            let linked_id = if linked_id_value == 0 {
                None
            } else {
                Some(linked_id_value.into())
            };

            PlacedUnit {
                instance_id: instance_id.into(),
                x,
                y,
                unit_id,
                owner,
                hp_percent,
                shield_percent,
                energy_percent,
                resource_amount,
                hangar_count,
                state,
                linked_id,
            }
        })
        .collect();

    Ok(result)
}
