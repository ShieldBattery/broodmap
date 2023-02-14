use crate::chk::strings::{StringId, UsedChkStrings};
use crate::chk::triggers::{
    trigger_condition_data, ActionFlags, NumberOperation, TriggerConditionData,
};
use nom::bytes::complete::take;
use nom::combinator::map;
use nom::multi::{count, many0};
use nom::number::complete::{le_u16, le_u32, le_u8};
use nom::sequence::tuple;
use nom::IResult;
use std::time::Duration;
use thiserror::Error;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RawBriefingAction {
    NoAction,
    Wait {
        time: Duration,
    },
    PlaySound {
        sound: StringId,
        // TODO(tec27): What is this for? ScmDraft doesn't seem to have this value as modifiable,
        // maybe it's auto-set to the duration of the sound file?
        time: u32,
    },
    DisplayTextMessage {
        text: StringId,
        time: Duration,
    },
    MissionObjectives {
        text: StringId,
    },
    ShowPortrait {
        unit_id: u16,
        slot: u32,
    },
    HidePortrait {
        slot: u32,
    },
    DisplaySpeakingPortrait {
        slot: u32,
        time: Duration,
    },
    Transmission {
        text: StringId,
        slot: u32,
        duration_op: NumberOperation,
        duration_amount: Duration,
        sound: StringId,
        // TODO(tec27): What is this for? ScmDraft doesn't seem to have this value as modifiable,
        // maybe it's auto-set to the duration of the sound file?
        time: u32,
    },
    SkipTutorialEnabled,
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum ParseRawBriefingActionError {
    #[error("Unknown briefing action type: {0}")]
    UnknownType(u8),
}

impl RawBriefingAction {
    // Blame Blizzard for this nonsense :(
    #[allow(clippy::too_many_arguments)]
    fn from_raw_info(
        action: u8,
        text: u32,
        sound: u32,
        time: u32,
        slot: u32,
        secondary_info: u32,
        unit_id: u16,
        operation: u8,
    ) -> Result<RawBriefingAction, Box<dyn std::error::Error>> {
        let a = match action {
            0 => RawBriefingAction::NoAction,
            1 => RawBriefingAction::Wait {
                time: Duration::from_millis(time as u64),
            },
            2 => RawBriefingAction::PlaySound {
                sound: sound.into(),
                time,
            },
            3 => RawBriefingAction::DisplayTextMessage {
                text: text.into(),
                time: Duration::from_millis(time as u64),
            },
            4 => RawBriefingAction::MissionObjectives { text: text.into() },
            5 => RawBriefingAction::ShowPortrait { unit_id, slot },
            6 => RawBriefingAction::HidePortrait { slot },
            7 => RawBriefingAction::DisplaySpeakingPortrait {
                slot,
                time: Duration::from_millis(time as u64),
            },
            8 => RawBriefingAction::Transmission {
                text: text.into(),
                slot,
                duration_op: operation.try_into()?,
                duration_amount: Duration::from_millis(secondary_info as u64),
                sound: sound.into(),
                time,
            },
            9 => RawBriefingAction::SkipTutorialEnabled,
            value => return Err(ParseRawBriefingActionError::UnknownType(value).into()),
        };

        Ok(a)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct RawBriefingActionData {
    pub action: RawBriefingAction,
    pub flags: ActionFlags,
}

fn briefing_action_data(input: &[u8]) -> IResult<&[u8], Option<RawBriefingActionData>> {
    map(
        tuple((
            take(4usize),
            le_u32,
            le_u32,
            le_u32,
            le_u32,
            le_u32,
            le_u16,
            le_u8,
            le_u8,
            le_u8,
            take(3usize),
        )),
        |(
            _location,
            text,
            sound,
            time,
            slot,
            secondary_info,
            unit_id,
            action,
            operation,
            flags,
            _padding,
        )| {
            let briefing_action = RawBriefingAction::from_raw_info(
                action,
                text,
                sound,
                time,
                slot,
                secondary_info,
                unit_id,
                operation,
            );

            briefing_action.ok().map(|a| RawBriefingActionData {
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
pub struct RawBriefingTrigger {
    pub conditions: Vec<TriggerConditionData>,
    pub actions: Vec<RawBriefingActionData>,
    /// Which players this trigger executes for. This can be indexed by the values of [PlayerGroup].
    pub enabled_for: [bool; 27],
}

fn raw_briefing_trigger(input: &[u8]) -> IResult<&[u8], RawBriefingTrigger> {
    let (input, conditions) = count(trigger_condition_data, 16)(input)?;
    let (input, actions) = count(briefing_action_data, 64)(input)?;
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
        RawBriefingTrigger {
            conditions: conditions.into_iter().flatten().collect(),
            actions: actions.into_iter().flatten().collect(),
            enabled_for,
        },
    ))
}

impl UsedChkStrings for Vec<RawBriefingTrigger> {
    fn used_string_ids(&self) -> Box<dyn Iterator<Item = StringId> + '_> {
        Box::new(self.iter().flat_map(|trigger| {
            trigger
                .actions
                .iter()
                .flat_map(|action| match action.action {
                    RawBriefingAction::DisplayTextMessage { text, .. } => Some(text),
                    RawBriefingAction::MissionObjectives { text } => Some(text),
                    RawBriefingAction::Transmission { text, .. } => Some(text),
                    _ => None,
                })
        }))
    }
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum BriefingError {
    #[error("Error parsing briefing")]
    ParseError(nom::error::ErrorKind),
}

pub fn read_briefing(data: &[u8]) -> Result<Vec<RawBriefingTrigger>, BriefingError> {
    let (_, triggers) = many0(raw_briefing_trigger)(data).map_err(|e| match e {
        nom::Err::Error(e) | nom::Err::Failure(e) => BriefingError::ParseError(e.code),
        nom::Err::Incomplete(_) => unreachable!(),
    })?;

    Ok(triggers)
}
