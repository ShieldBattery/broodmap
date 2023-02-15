use crate::chk::strings::{StringId, UsedChkStrings};
use nom::bytes::complete::take;
use nom::combinator::map;
use nom::multi::count;
use nom::number::complete::{le_u16, le_u32};
use nom::IResult;
use std::borrow::Cow;
use thiserror::Error;

#[derive(Debug, Clone)]
pub enum WeaponSettings {
    /// Weapon settings for the original/vanilla version of StarCraft.
    Original {
        /// Base (un-upgraded) weapon damage, indexed by weapon ID.
        base_damage: [u16; 100],
        /// Upgrade bonus to weapon damage, indexed by weapon ID.
        upgrade_bonus: [u16; 100],
    },
    /// Weapon settings for Brood War.
    Expanded {
        /// Base (un-upgraded) weapon damage, indexed by weapon ID.
        base_damage: [u16; 130],
        /// Upgrade bonus to weapon damage, indexed by weapon ID.
        upgrade_bonus: [u16; 130],
    },
}

fn original_weapon_settings(input: &[u8]) -> IResult<&[u8], WeaponSettings> {
    let (input, base_damage) = count(le_u16, 100usize)(input)?;
    let (input, upgrade_bonus) = count(le_u16, 100usize)(input)?;
    Ok((
        input,
        WeaponSettings::Original {
            base_damage: base_damage.try_into().unwrap(),
            upgrade_bonus: upgrade_bonus.try_into().unwrap(),
        },
    ))
}

fn expanded_weapon_settings(input: &[u8]) -> IResult<&[u8], WeaponSettings> {
    let (input, base_damage) = count(le_u16, 130usize)(input)?;
    let (input, upgrade_bonus) = count(le_u16, 130usize)(input)?;
    Ok((
        input,
        WeaponSettings::Expanded {
            base_damage: base_damage.try_into().unwrap(),
            upgrade_bonus: upgrade_bonus.try_into().unwrap(),
        },
    ))
}

#[derive(Debug, Clone)]
pub struct RawUnitSettings {
    /// `true` means the unit uses default settings, indexed by unit ID.
    pub use_defaults: [bool; 228],
    /// HP for a unit, indexed by unit ID. Note that the display value is 1/256th of this value,
    /// with the low byte being a fractional HP value.
    pub hp: [u32; 228],
    /// Shield points for a unit, indexed by unit ID.
    pub shield: [u16; 228],
    /// Armor points for a unit, indexed by unit ID.
    pub armor: [u8; 228],
    /// Build time for a unit, indexed by unit ID. These are in units of 1/60th of a second.
    pub build_time: [u16; 228],
    /// Mineral cost for a unit, indexed by unit ID.
    pub mineral_cost: [u16; 228],
    /// Vespene gas cost for a unit, indexed by unit ID.
    pub gas_cost: [u16; 228],
    /// The [StringId] for the name of a unit, indexed by unit ID.
    pub name_id: [StringId; 228],
    /// Weapon settings for the map.
    pub weapon_settings: WeaponSettings,
}

fn use_defaults(input: &[u8]) -> IResult<&[u8], [bool; 228]> {
    map(take(228usize), |use_defaults_byte: &[u8]| {
        use_defaults_byte
            .iter()
            .map(|&b| b != 0)
            .collect::<Vec<_>>()
            .try_into()
            .unwrap()
    })(input)
}

fn hp(input: &[u8]) -> IResult<&[u8], [u32; 228]> {
    let (input, hp_values) = count(le_u32, 228usize)(input)?;
    Ok((input, hp_values.try_into().unwrap()))
}

fn shield(input: &[u8]) -> IResult<&[u8], [u16; 228]> {
    let (input, shield_values) = count(le_u16, 228usize)(input)?;
    Ok((input, shield_values.try_into().unwrap()))
}

fn armor(input: &[u8]) -> IResult<&[u8], [u8; 228]> {
    let (input, armor_values) = take(228usize)(input)?;
    Ok((input, armor_values.try_into().unwrap()))
}

fn build_time(input: &[u8]) -> IResult<&[u8], [u16; 228]> {
    let (input, build_time_values) = count(le_u16, 228usize)(input)?;
    Ok((input, build_time_values.try_into().unwrap()))
}

fn mineral_cost(input: &[u8]) -> IResult<&[u8], [u16; 228]> {
    let (input, mineral_cost_values) = count(le_u16, 228usize)(input)?;
    Ok((input, mineral_cost_values.try_into().unwrap()))
}

fn gas_cost(input: &[u8]) -> IResult<&[u8], [u16; 228]> {
    let (input, gas_cost_values) = count(le_u16, 228usize)(input)?;
    Ok((input, gas_cost_values.try_into().unwrap()))
}

fn name_id(input: &[u8]) -> IResult<&[u8], [StringId; 228]> {
    map(count(le_u16, 228), |name_ids: Vec<u16>| {
        name_ids
            .iter()
            .map(|&id| id.into())
            .collect::<Vec<_>>()
            .try_into()
            .unwrap()
    })(input)
}

fn raw_unit_settings(input: &[u8], is_expanded: bool) -> IResult<&[u8], RawUnitSettings> {
    let (input, use_defaults) = use_defaults(input)?;
    let (input, hp) = hp(input)?;
    let (input, shield) = shield(input)?;
    let (input, armor) = armor(input)?;
    let (input, build_time) = build_time(input)?;
    let (input, mineral_cost) = mineral_cost(input)?;
    let (input, gas_cost) = gas_cost(input)?;
    let (input, name_id) = name_id(input)?;
    let (input, weapon_settings) = if is_expanded {
        expanded_weapon_settings(input)
    } else {
        original_weapon_settings(input)
    }?;

    Ok((
        input,
        RawUnitSettings {
            use_defaults,
            hp,
            shield,
            armor,
            build_time,
            mineral_cost,
            gas_cost,
            name_id,
            weapon_settings,
        },
    ))
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnitSettingsError {
    #[error("Error parsing unit settings")]
    ParseError(nom::error::ErrorKind),
}

impl RawUnitSettings {
    pub fn from_bytes(
        original_bytes: Option<Cow<'_, [u8]>>,
        expansion_bytes: Option<Cow<'_, [u8]>>,
    ) -> Result<RawUnitSettings, UnitSettingsError> {
        // TODO(tec27): Should we only use the extended section if the map format is BW? Some other
        // logic?
        let (_, result) = match (original_bytes, expansion_bytes) {
            (None, None) => {
                return Ok(RawUnitSettings {
                    use_defaults: [true; 228],
                    hp: [0; 228],
                    shield: [0; 228],
                    armor: [0; 228],
                    build_time: [0; 228],
                    mineral_cost: [0; 228],
                    gas_cost: [0; 228],
                    name_id: [0u16.into(); 228],
                    weapon_settings: WeaponSettings::Expanded {
                        base_damage: [0; 130],
                        upgrade_bonus: [0; 130],
                    },
                })
            }
            (_, Some(ref expansion_bytes)) => raw_unit_settings(expansion_bytes, true),
            (Some(ref legacy_bytes), None) => raw_unit_settings(legacy_bytes, false),
        }
        .map_err(|e| match e {
            nom::Err::Error(e) | nom::Err::Failure(e) => UnitSettingsError::ParseError(e.code),
            nom::Err::Incomplete(_) => unreachable!(),
        })?;

        Ok(result)
    }
}

impl UsedChkStrings for RawUnitSettings {
    fn used_string_ids(&self) -> Box<dyn Iterator<Item = StringId> + '_> {
        Box::new(self.name_id.iter().enumerate().filter_map(|(i, &id)| {
            if self.use_defaults[i] {
                None
            } else {
                Some(id)
            }
        }))
    }
}
