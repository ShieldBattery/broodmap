use crate::chk::strings::{StringId, UsedChkStrings};
use bitflags::bitflags;
use nom::bytes::complete::take;
use nom::combinator::map;
use nom::multi::{count, many0};
use nom::number::complete::{le_u16, le_u32, le_u8};
use nom::sequence::tuple;
use nom::IResult;
use std::time::Duration;
use thiserror::Error;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[repr(u8)]
pub enum NumericComparison {
    AtLeast = 0,
    AtMost = 1,
    Exactly = 10,
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum NumericComparisonConversionError {
    #[error("Invalid NumericComparison value: {0}")]
    InvalidValue(u8),
}

impl TryFrom<u8> for NumericComparison {
    type Error = NumericComparisonConversionError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            x if x == NumericComparison::AtLeast as u8 => Ok(NumericComparison::AtLeast),
            x if x == NumericComparison::AtMost as u8 => Ok(NumericComparison::AtMost),
            x if x == NumericComparison::Exactly as u8 => Ok(NumericComparison::Exactly),
            x => Err(NumericComparisonConversionError::InvalidValue(x)),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[repr(u8)]
pub enum SwitchState {
    Set = 2,
    Cleared = 3,
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum SwitchStateConversionError {
    #[error("Invalid SwitchState value: {0}")]
    InvalidValue(u8),
}

impl TryFrom<u8> for SwitchState {
    type Error = SwitchStateConversionError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            x if x == SwitchState::Set as u8 => Ok(SwitchState::Set),
            x if x == SwitchState::Cleared as u8 => Ok(SwitchState::Cleared),
            x => Err(SwitchStateConversionError::InvalidValue(x)),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[repr(u32)]
pub enum PlayerGroup {
    Player1 = 0,
    Player2 = 1,
    Player3 = 2,
    Player4 = 3,
    Player5 = 4,
    Player6 = 5,
    Player7 = 6,
    Player8 = 7,
    Player9 = 8,
    Player10 = 9,
    Player11 = 10,
    Player12 = 11,
    None = 12,
    CurrentPlayer = 13,
    Foes = 14,
    Allies = 15,
    NeutralPlayers = 16,
    AllPlayers = 17,
    Force1 = 18,
    Force2 = 19,
    Force3 = 20,
    Force4 = 21,
    Unused1 = 22,
    Unused2 = 23,
    Unused3 = 24,
    Unused4 = 25,
    NonAlliedVictoryPlayers = 26,
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum PlayerGroupConversionError {
    #[error("Invalid PlayerGroup value: {0}")]
    InvalidValue(u32),
}

impl TryFrom<u32> for PlayerGroup {
    type Error = PlayerGroupConversionError;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            x if x == PlayerGroup::Player1 as u32 => Ok(PlayerGroup::Player1),
            x if x == PlayerGroup::Player2 as u32 => Ok(PlayerGroup::Player2),
            x if x == PlayerGroup::Player3 as u32 => Ok(PlayerGroup::Player3),
            x if x == PlayerGroup::Player4 as u32 => Ok(PlayerGroup::Player4),
            x if x == PlayerGroup::Player5 as u32 => Ok(PlayerGroup::Player5),
            x if x == PlayerGroup::Player6 as u32 => Ok(PlayerGroup::Player6),
            x if x == PlayerGroup::Player7 as u32 => Ok(PlayerGroup::Player7),
            x if x == PlayerGroup::Player8 as u32 => Ok(PlayerGroup::Player8),
            x if x == PlayerGroup::Player9 as u32 => Ok(PlayerGroup::Player9),
            x if x == PlayerGroup::Player10 as u32 => Ok(PlayerGroup::Player10),
            x if x == PlayerGroup::Player11 as u32 => Ok(PlayerGroup::Player11),
            x if x == PlayerGroup::Player12 as u32 => Ok(PlayerGroup::Player12),
            x if x == PlayerGroup::None as u32 => Ok(PlayerGroup::None),
            x if x == PlayerGroup::CurrentPlayer as u32 => Ok(PlayerGroup::CurrentPlayer),
            x if x == PlayerGroup::Foes as u32 => Ok(PlayerGroup::Foes),
            x if x == PlayerGroup::Allies as u32 => Ok(PlayerGroup::Allies),
            x if x == PlayerGroup::NeutralPlayers as u32 => Ok(PlayerGroup::NeutralPlayers),
            x if x == PlayerGroup::AllPlayers as u32 => Ok(PlayerGroup::AllPlayers),
            x if x == PlayerGroup::Force1 as u32 => Ok(PlayerGroup::Force1),
            x if x == PlayerGroup::Force2 as u32 => Ok(PlayerGroup::Force2),
            x if x == PlayerGroup::Force3 as u32 => Ok(PlayerGroup::Force3),
            x if x == PlayerGroup::Force4 as u32 => Ok(PlayerGroup::Force4),
            x if x == PlayerGroup::Unused1 as u32 => Ok(PlayerGroup::Unused1),
            x if x == PlayerGroup::Unused2 as u32 => Ok(PlayerGroup::Unused2),
            x if x == PlayerGroup::Unused3 as u32 => Ok(PlayerGroup::Unused3),
            x if x == PlayerGroup::Unused4 as u32 => Ok(PlayerGroup::Unused4),
            x if x == PlayerGroup::NonAlliedVictoryPlayers as u32 => {
                Ok(PlayerGroup::NonAlliedVictoryPlayers)
            }
            x => Err(PlayerGroupConversionError::InvalidValue(x)),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[repr(u8)]
pub enum ResourceKind {
    Ore = 0,
    Gas = 1,
    OreAndGas = 2,
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum ResourceKindConversionError {
    #[error("Invalid ResourceKind value: {0}")]
    InvalidValue(u16),
}

impl TryFrom<u8> for ResourceKind {
    type Error = ResourceKindConversionError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            x if x == ResourceKind::Ore as u8 => Ok(ResourceKind::Ore),
            x if x == ResourceKind::Gas as u8 => Ok(ResourceKind::Gas),
            x if x == ResourceKind::OreAndGas as u8 => Ok(ResourceKind::OreAndGas),
            x => Err(ResourceKindConversionError::InvalidValue(x.into())),
        }
    }
}

impl TryFrom<u16> for ResourceKind {
    type Error = ResourceKindConversionError;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        match value {
            x if x == ResourceKind::Ore as u16 => Ok(ResourceKind::Ore),
            x if x == ResourceKind::Gas as u16 => Ok(ResourceKind::Gas),
            x if x == ResourceKind::OreAndGas as u16 => Ok(ResourceKind::OreAndGas),
            x => Err(ResourceKindConversionError::InvalidValue(x)),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[repr(u8)]
pub enum ScoreKind {
    Total = 0,
    Units = 1,
    Buildings = 2,
    UnitsAndBuildings = 3,
    Kills = 4,
    Razings = 5,
    KillsAndRazings = 6,
    Custom = 7,
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum ScoreKindConversionError {
    #[error("Invalid ScoreKind value: {0}")]
    InvalidValue(u16),
}

impl TryFrom<u8> for ScoreKind {
    type Error = ScoreKindConversionError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            x if x == ScoreKind::Total as u8 => Ok(ScoreKind::Total),
            x if x == ScoreKind::Units as u8 => Ok(ScoreKind::Units),
            x if x == ScoreKind::Buildings as u8 => Ok(ScoreKind::Buildings),
            x if x == ScoreKind::UnitsAndBuildings as u8 => Ok(ScoreKind::UnitsAndBuildings),
            x if x == ScoreKind::Kills as u8 => Ok(ScoreKind::Kills),
            x if x == ScoreKind::Razings as u8 => Ok(ScoreKind::Razings),
            x if x == ScoreKind::KillsAndRazings as u8 => Ok(ScoreKind::KillsAndRazings),
            x if x == ScoreKind::Custom as u8 => Ok(ScoreKind::Custom),
            _ => Err(ScoreKindConversionError::InvalidValue(value.into())),
        }
    }
}

impl TryFrom<u16> for ScoreKind {
    type Error = ScoreKindConversionError;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        match value {
            x if x == ScoreKind::Total as u16 => Ok(ScoreKind::Total),
            x if x == ScoreKind::Units as u16 => Ok(ScoreKind::Units),
            x if x == ScoreKind::Buildings as u16 => Ok(ScoreKind::Buildings),
            x if x == ScoreKind::UnitsAndBuildings as u16 => Ok(ScoreKind::UnitsAndBuildings),
            x if x == ScoreKind::Kills as u16 => Ok(ScoreKind::Kills),
            x if x == ScoreKind::Razings as u16 => Ok(ScoreKind::Razings),
            x if x == ScoreKind::KillsAndRazings as u16 => Ok(ScoreKind::KillsAndRazings),
            x if x == ScoreKind::Custom as u16 => Ok(ScoreKind::Custom),
            x => Err(ScoreKindConversionError::InvalidValue(x)),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum DeathsTriggerCondition {
    Normal {
        player_group: PlayerGroup,
        comparison: NumericComparison,
        unit_id: u16,
        amount: u32,
    },
    ExtendedUnit {
        player_group: PlayerGroup,
        comparison: NumericComparison,
        extended_unit_id: u16,
        amount: u32,
        mask: Option<u32>,
    },
    ExtendedPlayer {
        extended_player: u32,
        comparison: NumericComparison,
        unit_id: u16,
        amount: u32,
        mask: Option<u32>,
    },
    // TODO(tec27): Check that this is actually necessary
    ExtendedBoth {
        extended_player: u32,
        comparison: NumericComparison,
        extended_unit_id: u16,
        amount: u32,
        mask: Option<u32>,
    },
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum TriggerCondition {
    None,
    CountdownTimer {
        comparison: NumericComparison,
        time: Duration,
    },
    Command {
        player_group: PlayerGroup,
        comparison: NumericComparison,
        unit_id: u16,
        amount: u32,
    },
    Bring {
        player_group: PlayerGroup,
        comparison: NumericComparison,
        unit_id: u16,
        location: u32,
        amount: u32,
    },
    Accumulate {
        player_group: PlayerGroup,
        comparison: NumericComparison,
        amount: u32,
        resource: ResourceKind,
    },
    Kill {
        player_group: PlayerGroup,
        comparison: NumericComparison,
        unit_id: u16,
        amount: u32,
    },
    CommandTheMost {
        unit_id: u16,
    },
    CommandTheMostAt {
        unit_id: u16,
        location: u32,
    },
    MostKills {
        unit_id: u16,
    },
    HighestScore(ScoreKind),
    MostResources(ResourceKind),
    /// Switch `switch` is `state`.
    Switch {
        switch: u8,
        state: SwitchState,
    },
    ElapsedTime {
        comparison: NumericComparison,
        time: Duration,
    },
    /// This data is for a mission briefing. Functionally equivalent to `Never`.
    MissionBriefing,
    /// `player_group` has `comparison` `amount` opponents remaining in the game.
    Opponents {
        player_group: PlayerGroup,
        comparison: NumericComparison,
        amount: u32,
    },
    Deaths(DeathsTriggerCondition),
    CommandTheLeast {
        unit_id: u16,
    },
    CommandTheLeastAt {
        unit_id: u16,
        location: u32,
    },
    LeastKills {
        unit_id: u16,
    },
    LowestScore(ScoreKind),
    LeastResources(ResourceKind),
    Score {
        player_group: PlayerGroup,
        comparison: NumericComparison,
        score_kind: ScoreKind,
        amount: u32,
    },
    /// Same as `None`.
    Always,
    Never,
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum ParseTriggerConditionError {
    #[error("Unknown condition type: {0}")]
    UnknownType(u8),
}

impl TriggerCondition {
    // Blame Blizzard for the arguments :( And returning an error here is a pain given how many
    // different things would fail, and we'd just discard it anyway. Ideally we'd add some
    // (optional) verbose logging around this stuff I think
    #[allow(clippy::too_many_arguments)]
    pub fn from_raw_info(
        condition: u8,
        location_or_bitmask: u32,
        player_group: u32,
        amount: u32,
        unit_id: u16,
        comparison_or_switch_state: u8,
        resource_score_switch: u8,
        eud_mask: bool,
    ) -> Result<TriggerCondition, Box<dyn std::error::Error>> {
        let c = match condition {
            0 => TriggerCondition::None,
            1 => TriggerCondition::CountdownTimer {
                comparison: comparison_or_switch_state.try_into()?,
                time: Duration::from_secs(amount as u64),
            },
            2 => TriggerCondition::Command {
                player_group: player_group.try_into()?,
                comparison: comparison_or_switch_state.try_into()?,
                unit_id,
                amount,
            },
            3 => TriggerCondition::Bring {
                player_group: player_group.try_into()?,
                comparison: comparison_or_switch_state.try_into()?,
                unit_id,
                location: location_or_bitmask,
                amount,
            },
            4 => TriggerCondition::Accumulate {
                player_group: player_group.try_into()?,
                comparison: comparison_or_switch_state.try_into()?,
                amount,
                resource: resource_score_switch.try_into()?,
            },
            5 => TriggerCondition::Kill {
                player_group: player_group.try_into()?,
                comparison: comparison_or_switch_state.try_into()?,
                unit_id,
                amount,
            },
            6 => TriggerCondition::CommandTheMost { unit_id },
            7 => TriggerCondition::CommandTheMostAt {
                unit_id,
                location: location_or_bitmask,
            },
            8 => TriggerCondition::MostKills { unit_id },
            9 => TriggerCondition::HighestScore(resource_score_switch.try_into()?),
            10 => TriggerCondition::MostResources(resource_score_switch.try_into()?),
            11 => TriggerCondition::Switch {
                switch: resource_score_switch,
                state: comparison_or_switch_state.try_into()?,
            },
            12 => TriggerCondition::ElapsedTime {
                comparison: comparison_or_switch_state.try_into()?,
                time: Duration::from_secs(amount as u64),
            },
            13 => TriggerCondition::MissionBriefing,
            14 => TriggerCondition::Opponents {
                player_group: player_group.try_into()?,
                comparison: comparison_or_switch_state.try_into()?,
                amount,
            },
            15 => {
                let player_extended = player_group == 12 || player_group > 26;
                let unit_extended = unit_id > 232;
                let mask = if eud_mask {
                    Some(location_or_bitmask)
                } else {
                    None
                };

                if player_extended && unit_extended {
                    TriggerCondition::Deaths(DeathsTriggerCondition::ExtendedBoth {
                        extended_player: player_group,
                        extended_unit_id: unit_id,
                        comparison: comparison_or_switch_state.try_into()?,
                        amount,
                        mask,
                    })
                } else if player_extended {
                    TriggerCondition::Deaths(DeathsTriggerCondition::ExtendedPlayer {
                        extended_player: player_group,
                        unit_id,
                        comparison: comparison_or_switch_state.try_into()?,
                        amount,
                        mask,
                    })
                } else if unit_extended {
                    TriggerCondition::Deaths(DeathsTriggerCondition::ExtendedUnit {
                        player_group: player_group.try_into()?,
                        extended_unit_id: unit_id,
                        comparison: comparison_or_switch_state.try_into()?,
                        amount,
                        mask,
                    })
                } else {
                    TriggerCondition::Deaths(DeathsTriggerCondition::Normal {
                        player_group: player_group.try_into()?,
                        unit_id,
                        comparison: comparison_or_switch_state.try_into()?,
                        amount,
                    })
                }
            }
            16 => TriggerCondition::CommandTheLeast { unit_id },
            17 => TriggerCondition::CommandTheLeastAt {
                unit_id,
                location: location_or_bitmask,
            },
            18 => TriggerCondition::LeastKills { unit_id },
            19 => TriggerCondition::LowestScore(resource_score_switch.try_into()?),
            20 => TriggerCondition::LeastResources(resource_score_switch.try_into()?),
            21 => TriggerCondition::Score {
                player_group: player_group.try_into()?,
                comparison: comparison_or_switch_state.try_into()?,
                score_kind: resource_score_switch.try_into()?,
                amount,
            },
            22 => TriggerCondition::Always,
            23 => TriggerCondition::Never,
            value => return Err(ParseTriggerConditionError::UnknownType(value).into()),
        };

        Ok(c)
    }
}

bitflags! {
    pub struct ConditionFlags: u8 {
        /// Trigger is disabled, will not be run.
        const DISABLED = 0x02;
        // TODO(tec27): Does this make sense for conditions? SEN Wiki has this but seems weird
        const ALWAYS_DISPLAY = 0x04;
        const USE_UNIT_PROPERTIES = 0x08;
        const USE_UNIT_TYPE = 0x10;
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct TriggerConditionData {
    pub condition: TriggerCondition,
    pub flags: ConditionFlags,
}

pub(crate) fn trigger_condition_data(input: &[u8]) -> IResult<&[u8], Option<TriggerConditionData>> {
    map(
        tuple((
            le_u32,
            le_u32,
            le_u32,
            le_u16,
            le_u8,
            le_u8,
            le_u8,
            le_u8,
            take(2usize),
        )),
        |(
            location_or_bitmask,
            player_group,
            amount,
            unit_id,
            comparison_or_switch_state,
            condition,
            resource_score_switch,
            flags,
            mask_flag,
        )| {
            let trigger_condition = TriggerCondition::from_raw_info(
                condition,
                location_or_bitmask,
                player_group,
                amount,
                unit_id,
                comparison_or_switch_state,
                resource_score_switch,
                mask_flag == b"SC",
            );

            trigger_condition.ok().map(|c| TriggerConditionData {
                condition: c,
                flags: ConditionFlags::from_bits_truncate(flags),
            })
        },
    )(input)
}

// NOTE(tec27): SEN Wiki refers to this as "Number Modifiers"
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[repr(u8)]
pub enum NumberOperation {
    SetTo = 7,
    Add = 8,
    Subtract = 9,
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum NumberOperationConversionError {
    #[error("Invalid NumberOperation value: {0}")]
    InvalidValue(u8),
}

impl TryFrom<u8> for NumberOperation {
    type Error = NumberOperationConversionError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            x if x == NumberOperation::SetTo as u8 => Ok(NumberOperation::SetTo),
            x if x == NumberOperation::Add as u8 => Ok(NumberOperation::Add),
            x if x == NumberOperation::Subtract as u8 => Ok(NumberOperation::Subtract),
            x => Err(NumberOperationConversionError::InvalidValue(x)),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[repr(u8)]
pub enum ActionState {
    Set = 4,
    Clear = 5,
    Toggle = 6,
    Randomize = 11,
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum ActionStateConversionError {
    #[error("Invalid ActionState value: {0}")]
    InvalidValue(u8),
}

impl TryFrom<u8> for ActionState {
    type Error = ActionStateConversionError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            x if x == ActionState::Set as u8 => Ok(ActionState::Set),
            x if x == ActionState::Clear as u8 => Ok(ActionState::Clear),
            x if x == ActionState::Toggle as u8 => Ok(ActionState::Toggle),
            x if x == ActionState::Randomize as u8 => Ok(ActionState::Randomize),
            x => Err(ActionStateConversionError::InvalidValue(x)),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[repr(u8)]
pub enum UnitOrder {
    Move = 0,
    Patrol = 1,
    Attack = 2,
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnitOrderConversionError {
    #[error("Invalid UnitOrder value: {0}")]
    InvalidValue(u8),
}

impl TryFrom<u8> for UnitOrder {
    type Error = UnitOrderConversionError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            x if x == UnitOrder::Move as u8 => Ok(UnitOrder::Move),
            x if x == UnitOrder::Patrol as u8 => Ok(UnitOrder::Patrol),
            x if x == UnitOrder::Attack as u8 => Ok(UnitOrder::Attack),
            x => Err(UnitOrderConversionError::InvalidValue(x)),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[repr(u16)]
pub enum AllianceStatus {
    Enemy = 0,
    Ally = 1,
    AlliedVictory = 2,
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum AllianceStatusConversionError {
    #[error("Invalid AllianceStatus value: {0}")]
    InvalidValue(u16),
}

impl TryFrom<u16> for AllianceStatus {
    type Error = AllianceStatusConversionError;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        match value {
            x if x == AllianceStatus::Enemy as u16 => Ok(AllianceStatus::Enemy),
            x if x == AllianceStatus::Ally as u16 => Ok(AllianceStatus::Ally),
            x if x == AllianceStatus::AlliedVictory as u16 => Ok(AllianceStatus::AlliedVictory),
            x => Err(AllianceStatusConversionError::InvalidValue(x)),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum SetDeathsAction {
    Normal {
        player_group: PlayerGroup,
        unit_id: u16,
        operation: NumberOperation,
        amount: u32,
    },
    ExtendedUnit {
        player_group: PlayerGroup,
        extended_unit_id: u16,
        operation: NumberOperation,
        amount: u32,
        mask: Option<u32>,
    },
    ExtendedPlayer {
        extended_player: u32,
        unit_id: u16,
        operation: NumberOperation,
        amount: u32,
        mask: Option<u32>,
    },
    // TODO(tec27): Check that this is actually necessary
    ExtendedBoth {
        extended_player: u32,
        extended_unit_id: u16,
        operation: NumberOperation,
        amount: u32,
        mask: Option<u32>,
    },
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RawTriggerAction {
    NoAction,
    Victory,
    Defeat,
    PreserveTrigger,
    Wait {
        time: Duration,
    },
    /// Pause the game (single player only).
    PauseGame,
    /// Unpause the game (single player only).
    UnpauseGame,
    Transmission {
        unit_id: u16,
        location: u32,
        duration_op: NumberOperation,
        // NOTE(tec27): this corresponds to 0x14 in the data
        duration_amount: Duration,
        text: StringId,
        sound: StringId,
        // TODO(tec27): What is this for? ScmDraft doesn't seem to have this value as modifiable,
        // maybe it's auto-set to the duration of the sound file?
        time: u32,
    },
    PlaySound {
        sound: StringId,
        // TODO(tec27): What is this for? ScmDraft doesn't seem to have this value as modifiable,
        // maybe it's auto-set to the duration of the sound file?
        time: u32,
    },
    DisplayTextMessage {
        text: StringId,
    },
    CenterView {
        location: u32,
    },
    CreateUnitWithProperties {
        player_group: PlayerGroup,
        unit_id: u16,
        // NOTE(tec27): This corresponds to 0x19 in the data
        amount: u8,
        location: u32,
        prop_num: u32,
    },
    SetMissionObjectives {
        text: StringId,
    },
    ModifySwitch {
        switch: u32,
        operation: ActionState,
    },
    ModifyCountdownTimer {
        amount: Duration,
        operation: NumberOperation,
    },
    RunAiScript {
        script_id: u32,
    },
    RunAiScriptAtLocation {
        script_id: u32,
        location: u32,
    },
    LeaderboardControl {
        text: StringId,
        unit_id: u16,
    },
    LeaderboardControlAtLocation {
        text: StringId,
        unit_id: u16,
        location: u32,
    },
    LeaderboardResources {
        text: StringId,
        resource_kind: ResourceKind,
    },
    LeaderboardKills {
        text: StringId,
        unit_id: u16,
    },
    LeaderboardScore {
        text: StringId,
        score_kind: ScoreKind,
    },
    KillUnit {
        player_group: PlayerGroup,
        unit_id: u16,
    },
    KillUnitAtLocation {
        player_group: PlayerGroup,
        unit_id: u16,
        location: u32,
    },
    RemoveUnit {
        player_group: PlayerGroup,
        unit_id: u16,
    },
    RemoveUnitAtLocation {
        player_group: PlayerGroup,
        unit_id: u16,
        location: u32,
    },
    ModifyResources {
        player_group: PlayerGroup,
        resource_kind: ResourceKind,
        amount: u32,
        operation: NumberOperation,
    },
    ModifyScore {
        player_group: PlayerGroup,
        score_kind: ScoreKind,
        amount: u32,
        operation: NumberOperation,
    },
    MinimapPing {
        location: u32,
    },
    TalkingPortrait {
        unit_id: u16,
        time: Duration,
    },
    MuteUnitSpeech,
    UnmuteUnitSpeech,
    /// Set the "show computers on leaderboard" state. NOTE(tec27): Randomize isn't valid for this
    /// I think (at least ScmDraft doesn't show it as an option).
    LeaderboardComputers(ActionState),
    LeaderboardGoalControl {
        text: StringId,
        unit_id: u16,
        amount: u32,
    },
    LeaderboardGoalControlAtLocation {
        text: StringId,
        unit_id: u16,
        amount: u32,
        location: u32,
    },
    LeaderboardGoalResources {
        text: StringId,
        resource_kind: ResourceKind,
        amount: u32,
    },
    LeaderboardGoalKills {
        text: StringId,
        unit_id: u16,
        amount: u32,
    },
    LeaderboardGoalScore {
        text: StringId,
        score_kind: ScoreKind,
        amount: u32,
    },
    CenterLocationOn {
        moved_location: u32,
        player_group: PlayerGroup,
        unit_id: u16,
        unit_location: u32,
    },
    MoveUnit {
        player_group: PlayerGroup,
        unit_id: u16,
        amount: u8,
        from_location: u32,
        to_location: u32,
    },
    LeaderboardGreed(u32),
    /// Set the next scenario (single player only).
    SetNextScenario(StringId),
    SetDoodadState {
        player_group: PlayerGroup,
        unit_id: u16,
        location: u32,
        state: ActionState,
    },
    SetInvincibility {
        player_group: PlayerGroup,
        unit_id: u16,
        location: u32,
        state: ActionState,
    },
    CreateUnit {
        player_group: PlayerGroup,
        unit_id: u16,
        amount: u8,
        location: u32,
    },
    SetDeaths(SetDeathsAction),
    Order {
        player_group: PlayerGroup,
        unit_id: u16,
        source_location: u32,
        order: UnitOrder,
        target_location: u32,
    },
    Comment(StringId),
    GiveUnitsToPlayer {
        from_player_group: PlayerGroup,
        to_player_group: PlayerGroup,
        location: u32,
        unit_id: u16,
        amount: u8,
    },
    SetUnitHp {
        player_group: PlayerGroup,
        unit_id: u16,
        location: u32,
        unit_amount: u8,
        hp_amount: u32,
    },
    SetUnitEnergy {
        player_group: PlayerGroup,
        unit_id: u16,
        location: u32,
        unit_amount: u8,
        energy_amount: u32,
    },
    SetUnitShields {
        player_group: PlayerGroup,
        unit_id: u16,
        location: u32,
        unit_amount: u8,
        shield_amount: u32,
    },
    SetUnitResources {
        player_group: PlayerGroup,
        location: u32,
        unit_amount: u8,
        resource_amount: u32,
    },
    SetUnitHangarCount {
        player_group: PlayerGroup,
        unit_id: u16,
        location: u32,
        unit_amount: u8,
        hangar_count: u32,
    },
    PauseTimer,
    UnpauseTimer,
    Draw,
    SetAlliance {
        player_group: PlayerGroup,
        status: AllianceStatus,
    },
    // TODO(tec27): What do these do?!
    /// Disable debug mode (single player only).
    DisableDebugMode,
    /// Enable debug mode (single player only).
    EnableDebugMode,
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum ParseRawTriggerActionError {
    #[error("Unknown action type: {0}")]
    UnknownType(u8),
}

impl RawTriggerAction {
    // Blame Blizzard for the arguments :( And returning an error here is a pain given how many
    // different things would fail, and we'd just discard it anyway. Ideally we'd add some
    // (optional) verbose logging around this stuff I think
    #[allow(clippy::too_many_arguments)]
    pub fn from_raw_info(
        action: u8,
        location_or_bitmask: u32,
        text: u32,
        sound: u32,
        time: u32,
        player_group_a: u32,
        secondary_info: u32,
        unit_id_or_type: u16,
        unit_amount_or_state_or_op: u8,
        eud_mask: bool,
    ) -> Result<RawTriggerAction, Box<dyn std::error::Error>> {
        let a = match action {
            0 => RawTriggerAction::NoAction,
            1 => RawTriggerAction::Victory,
            2 => RawTriggerAction::Defeat,
            3 => RawTriggerAction::PreserveTrigger,
            4 => RawTriggerAction::Wait {
                time: Duration::from_millis(time as u64),
            },
            5 => RawTriggerAction::PauseGame,
            6 => RawTriggerAction::UnpauseGame,
            7 => RawTriggerAction::Transmission {
                unit_id: unit_id_or_type,
                location: location_or_bitmask,
                duration_op: unit_amount_or_state_or_op.try_into()?,
                duration_amount: Duration::from_millis(secondary_info as u64),
                text: text.into(),
                sound: sound.into(),
                time,
            },
            8 => RawTriggerAction::PlaySound {
                sound: sound.into(),
                time,
            },
            9 => RawTriggerAction::DisplayTextMessage { text: text.into() },
            10 => RawTriggerAction::CenterView {
                location: location_or_bitmask,
            },
            11 => RawTriggerAction::CreateUnitWithProperties {
                player_group: player_group_a.try_into()?,
                unit_id: unit_id_or_type,
                amount: unit_amount_or_state_or_op,
                location: location_or_bitmask,
                prop_num: secondary_info,
            },
            12 => RawTriggerAction::SetMissionObjectives { text: text.into() },
            13 => RawTriggerAction::ModifySwitch {
                switch: secondary_info,
                operation: unit_amount_or_state_or_op.try_into()?,
            },
            14 => RawTriggerAction::ModifyCountdownTimer {
                amount: Duration::from_secs(time as u64),
                operation: unit_amount_or_state_or_op.try_into()?,
            },
            15 => RawTriggerAction::RunAiScript {
                script_id: secondary_info,
            },
            16 => RawTriggerAction::RunAiScriptAtLocation {
                script_id: secondary_info,
                location: location_or_bitmask,
            },
            17 => RawTriggerAction::LeaderboardControl {
                text: text.into(),
                unit_id: unit_id_or_type,
            },
            18 => RawTriggerAction::LeaderboardControlAtLocation {
                text: text.into(),
                unit_id: unit_id_or_type,
                location: location_or_bitmask,
            },
            19 => RawTriggerAction::LeaderboardResources {
                text: text.into(),
                resource_kind: unit_id_or_type.try_into()?,
            },
            20 => RawTriggerAction::LeaderboardKills {
                text: text.into(),
                unit_id: unit_id_or_type,
            },
            21 => RawTriggerAction::LeaderboardScore {
                text: text.into(),
                score_kind: unit_id_or_type.try_into()?,
            },
            22 => RawTriggerAction::KillUnit {
                player_group: player_group_a.try_into()?,
                unit_id: unit_id_or_type,
            },
            23 => RawTriggerAction::KillUnitAtLocation {
                player_group: player_group_a.try_into()?,
                unit_id: unit_id_or_type,
                location: location_or_bitmask,
            },
            24 => RawTriggerAction::RemoveUnit {
                player_group: player_group_a.try_into()?,
                unit_id: unit_id_or_type,
            },
            25 => RawTriggerAction::RemoveUnitAtLocation {
                player_group: player_group_a.try_into()?,
                unit_id: unit_id_or_type,
                location: location_or_bitmask,
            },
            26 => RawTriggerAction::ModifyResources {
                player_group: player_group_a.try_into()?,
                resource_kind: unit_id_or_type.try_into()?,
                amount: secondary_info,
                operation: unit_amount_or_state_or_op.try_into()?,
            },
            27 => RawTriggerAction::ModifyScore {
                player_group: player_group_a.try_into()?,
                score_kind: unit_id_or_type.try_into()?,
                amount: secondary_info,
                operation: unit_amount_or_state_or_op.try_into()?,
            },
            28 => RawTriggerAction::MinimapPing {
                location: location_or_bitmask,
            },
            29 => RawTriggerAction::TalkingPortrait {
                unit_id: unit_id_or_type,
                time: Duration::from_millis(time as u64),
            },
            30 => RawTriggerAction::MuteUnitSpeech,
            31 => RawTriggerAction::UnmuteUnitSpeech,
            32 => RawTriggerAction::LeaderboardComputers(unit_amount_or_state_or_op.try_into()?),
            33 => RawTriggerAction::LeaderboardGoalControl {
                text: text.into(),
                unit_id: unit_id_or_type,
                amount: secondary_info,
            },
            34 => RawTriggerAction::LeaderboardGoalControlAtLocation {
                text: text.into(),
                unit_id: unit_id_or_type,
                amount: secondary_info,
                location: location_or_bitmask,
            },
            35 => RawTriggerAction::LeaderboardGoalResources {
                text: text.into(),
                resource_kind: unit_id_or_type.try_into()?,
                amount: secondary_info,
            },
            36 => RawTriggerAction::LeaderboardGoalKills {
                text: text.into(),
                unit_id: unit_id_or_type,
                amount: secondary_info,
            },
            37 => RawTriggerAction::LeaderboardGoalScore {
                text: text.into(),
                score_kind: unit_id_or_type.try_into()?,
                amount: secondary_info,
            },
            38 => RawTriggerAction::CenterLocationOn {
                moved_location: secondary_info,
                player_group: player_group_a.try_into()?,
                unit_id: unit_id_or_type,
                unit_location: location_or_bitmask,
            },
            39 => RawTriggerAction::MoveUnit {
                player_group: player_group_a.try_into()?,
                unit_id: unit_id_or_type,
                amount: unit_amount_or_state_or_op,
                from_location: location_or_bitmask,
                to_location: secondary_info,
            },
            40 => RawTriggerAction::LeaderboardGreed(secondary_info),
            41 => RawTriggerAction::SetNextScenario(text.into()),
            42 => RawTriggerAction::SetDoodadState {
                player_group: player_group_a.try_into()?,
                unit_id: unit_id_or_type,
                location: location_or_bitmask,
                state: unit_amount_or_state_or_op.try_into()?,
            },
            43 => RawTriggerAction::SetInvincibility {
                player_group: player_group_a.try_into()?,
                unit_id: unit_id_or_type,
                location: location_or_bitmask,
                state: unit_amount_or_state_or_op.try_into()?,
            },
            44 => RawTriggerAction::CreateUnit {
                player_group: player_group_a.try_into()?,
                unit_id: unit_id_or_type,
                amount: unit_amount_or_state_or_op,
                location: location_or_bitmask,
            },
            45 => {
                let player_extended = player_group_a == 12 || player_group_a > 26;
                let unit_extended = unit_id_or_type > 232;
                let mask = if eud_mask {
                    Some(location_or_bitmask)
                } else {
                    None
                };

                if player_extended && unit_extended {
                    RawTriggerAction::SetDeaths(SetDeathsAction::ExtendedBoth {
                        extended_player: player_group_a,
                        extended_unit_id: unit_id_or_type,
                        operation: unit_amount_or_state_or_op.try_into()?,
                        amount: secondary_info,
                        mask,
                    })
                } else if player_extended {
                    RawTriggerAction::SetDeaths(SetDeathsAction::ExtendedPlayer {
                        extended_player: player_group_a,
                        unit_id: unit_id_or_type,
                        operation: unit_amount_or_state_or_op.try_into()?,
                        amount: secondary_info,
                        mask,
                    })
                } else if unit_extended {
                    RawTriggerAction::SetDeaths(SetDeathsAction::ExtendedUnit {
                        player_group: player_group_a.try_into()?,
                        extended_unit_id: unit_id_or_type,
                        operation: unit_amount_or_state_or_op.try_into()?,
                        amount: secondary_info,
                        mask,
                    })
                } else {
                    RawTriggerAction::SetDeaths(SetDeathsAction::Normal {
                        player_group: player_group_a.try_into()?,
                        unit_id: unit_id_or_type,
                        operation: unit_amount_or_state_or_op.try_into()?,
                        amount: secondary_info,
                    })
                }
            }
            46 => RawTriggerAction::Order {
                player_group: player_group_a.try_into()?,
                unit_id: unit_id_or_type,
                source_location: location_or_bitmask,
                order: unit_amount_or_state_or_op.try_into()?,
                target_location: secondary_info,
            },
            47 => RawTriggerAction::Comment(text.into()),
            48 => RawTriggerAction::GiveUnitsToPlayer {
                from_player_group: player_group_a.try_into()?,
                to_player_group: secondary_info.try_into()?,
                location: location_or_bitmask,
                unit_id: unit_id_or_type,
                amount: unit_amount_or_state_or_op,
            },
            49 => RawTriggerAction::SetUnitHp {
                player_group: player_group_a.try_into()?,
                unit_id: unit_id_or_type,
                location: location_or_bitmask,
                unit_amount: unit_amount_or_state_or_op,
                hp_amount: secondary_info,
            },
            50 => RawTriggerAction::SetUnitEnergy {
                player_group: player_group_a.try_into()?,
                unit_id: unit_id_or_type,
                location: location_or_bitmask,
                unit_amount: unit_amount_or_state_or_op,
                energy_amount: secondary_info,
            },
            51 => RawTriggerAction::SetUnitShields {
                player_group: player_group_a.try_into()?,
                unit_id: unit_id_or_type,
                location: location_or_bitmask,
                unit_amount: unit_amount_or_state_or_op,
                shield_amount: secondary_info,
            },
            52 => RawTriggerAction::SetUnitResources {
                player_group: player_group_a.try_into()?,
                location: location_or_bitmask,
                unit_amount: unit_amount_or_state_or_op,
                resource_amount: secondary_info,
            },
            53 => RawTriggerAction::SetUnitHangarCount {
                player_group: player_group_a.try_into()?,
                unit_id: unit_id_or_type,
                location: location_or_bitmask,
                unit_amount: unit_amount_or_state_or_op,
                hangar_count: secondary_info,
            },
            54 => RawTriggerAction::PauseTimer,
            55 => RawTriggerAction::UnpauseTimer,
            56 => RawTriggerAction::Draw,
            57 => RawTriggerAction::SetAlliance {
                player_group: player_group_a.try_into()?,
                status: unit_id_or_type.try_into()?,
            },
            58 => RawTriggerAction::DisableDebugMode,
            59 => RawTriggerAction::EnableDebugMode,
            value => return Err(ParseRawTriggerActionError::UnknownType(value).into()),
        };

        Ok(a)
    }
}

bitflags! {
    pub struct ActionFlags: u8 {
        /// Ignore a wait/transmission once.
        const IGNORE_ONCE = 0x01;
        /// Trigger is disabled, will not run.
        const DISABLED = 0x02;
        /// Always show text (otherwise it will only show if subtitles are on).
        const ALWAYS_DISPLAY = 0x04;
        const USE_UNIT_PROPERTIES = 0x08;
        const USE_UNIT_TYPE = 0x10;
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct RawTriggerActionData {
    pub action: RawTriggerAction,
    pub flags: ActionFlags,
}

fn trigger_action_data(input: &[u8]) -> IResult<&[u8], Option<RawTriggerActionData>> {
    map(
        tuple((
            le_u32,
            le_u32,
            le_u32,
            le_u32,
            le_u32,
            le_u32,
            le_u16,
            le_u8,
            le_u8,
            le_u8,
            le_u8,
            take(2usize),
        )),
        |(
            location_or_bitmask,
            text,
            sound,
            time,
            player_group_a,
            secondary_info,
            unit_id_or_type,
            action,
            unit_amount_or_state_or_op,
            flags,
            _padding,
            mask_flag,
        )| {
            let trigger_action = RawTriggerAction::from_raw_info(
                action,
                location_or_bitmask,
                text,
                sound,
                time,
                player_group_a,
                secondary_info,
                unit_id_or_type,
                unit_amount_or_state_or_op,
                mask_flag == b"SC",
            );

            trigger_action.ok().map(|a| RawTriggerActionData {
                action: a,
                flags: ActionFlags::from_bits_truncate(flags),
            })
        },
    )(input)
}

// TODO(tec27): Figure out if we need to keep the "execution flags" part of the struct after
// actions. It seems to be a weird mix of bookkeeping things that BW presumably uses (or used)
// at runtime and I guess didn't want to have to allocate elsewhere? but also some stuff that
// might be used in more advanced maps (e.g. the preserve trigger flag).
#[derive(Debug, Clone)]
pub struct RawTrigger {
    pub conditions: Vec<TriggerConditionData>,
    pub actions: Vec<RawTriggerActionData>,
    /// Which players this trigger executes for. This can be indexed by the values of [PlayerGroup].
    pub enabled_for: [bool; 27],
}

fn raw_trigger(input: &[u8]) -> IResult<&[u8], RawTrigger> {
    let (input, conditions) = count(trigger_condition_data, 16)(input)?;
    let (input, actions) = count(trigger_action_data, 64)(input)?;
    let (input, _execution_flags) = le_u32(input)?;
    let (input, enabled_for) = map(take(27usize), |enabled_for: &[u8]| {
        enabled_for
            .iter()
            .map(|&b| b != 0)
            .collect::<Vec<_>>()
            .try_into()
            .unwrap()
    })(input)?;
    let (input, _action_index) = le_u8(input)?;

    Ok((
        input,
        RawTrigger {
            conditions: conditions.into_iter().flatten().collect(),
            actions: actions.into_iter().flatten().collect(),
            enabled_for,
        },
    ))
}

impl UsedChkStrings for Vec<RawTrigger> {
    fn used_string_ids(&self) -> Box<dyn Iterator<Item = StringId> + '_> {
        Box::new(self.iter().flat_map(|trigger| {
            trigger
                .actions
                .iter()
                .flat_map(|action| match action.action {
                    RawTriggerAction::Transmission { text, .. } => Some(text),
                    RawTriggerAction::DisplayTextMessage { text, .. } => Some(text),
                    RawTriggerAction::SetMissionObjectives { text, .. } => Some(text),
                    RawTriggerAction::LeaderboardControl { text, .. } => Some(text),
                    RawTriggerAction::LeaderboardControlAtLocation { text, .. } => Some(text),
                    RawTriggerAction::LeaderboardResources { text, .. } => Some(text),
                    RawTriggerAction::LeaderboardKills { text, .. } => Some(text),
                    RawTriggerAction::LeaderboardScore { text, .. } => Some(text),
                    RawTriggerAction::LeaderboardGoalControl { text, .. } => Some(text),
                    RawTriggerAction::LeaderboardGoalControlAtLocation { text, .. } => Some(text),
                    RawTriggerAction::LeaderboardGoalResources { text, .. } => Some(text),
                    RawTriggerAction::LeaderboardGoalKills { text, .. } => Some(text),
                    RawTriggerAction::LeaderboardGoalScore { text, .. } => Some(text),
                    RawTriggerAction::SetNextScenario(scenario) => Some(scenario),
                    RawTriggerAction::Comment(text) => Some(text),
                    _ => None,
                })
        }))
    }
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum TriggersError {
    #[error("Error parsing triggers")]
    ParseError(nom::error::ErrorKind),
}

pub fn read_triggers(data: &[u8]) -> Result<Vec<RawTrigger>, TriggersError> {
    let (_, triggers) = many0(raw_trigger)(data).map_err(|e| match e {
        nom::Err::Error(e) | nom::Err::Failure(e) => TriggersError::ParseError(e.code),
        nom::Err::Incomplete(_) => unreachable!(),
    })?;

    Ok(triggers)
}
