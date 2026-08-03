use crate::chk::strings::{StringId, UsedChkStrings};
use crate::chk::triggers::{
    ActionFlags, NumberOperation, TriggerConditionData, trigger_condition_data,
};
use nom::bytes::complete::take;
use nom::combinator::map;
use nom::multi::many0;
use nom::number::complete::{le_u8, le_u16, le_u32};
use nom::{IResult, Parser};
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
        (
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
        ),
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
    )
    .parse(input)
}

// TODO(tec27): Figure out if we need to keep the "execution flags" part of the struct after
// actions. It seems to be a weird mix of bookkeeping things that BW presumably uses (or used)
// at runtime and I guess didn't want to have to allocate elsewhere? but also some stuff that
// might be used in more advanced maps (e.g. the preserve trigger flag).
#[derive(Debug, Clone)]
pub struct RawBriefingTrigger {
    pub conditions: Vec<TriggerConditionData>,
    pub actions: Vec<RawBriefingActionData>,
    /// Which players this trigger executes for. This can be indexed by the values of
    /// [`PlayerGroup`](crate::chk::triggers::PlayerGroup).
    pub enabled_for: [bool; 27],
}

fn raw_briefing_trigger(input: &[u8]) -> IResult<&[u8], RawBriefingTrigger> {
    let mut input = input;
    let mut conditions = Vec::new();
    for _ in 0..16 {
        let (rest, condition) = trigger_condition_data(input)?;
        input = rest;
        if let Some(condition) = condition {
            conditions.push(condition);
        }
    }

    let mut actions = Vec::new();
    for _ in 0..64 {
        let (rest, action) = briefing_action_data(input)?;
        input = rest;
        if let Some(action) = action {
            actions.push(action);
        }
    }

    let (input, _execution_flags) = le_u32(input)?;
    let (input, enabled_for_bytes) = take(27usize).parse(input)?;
    let enabled_for = std::array::from_fn(|index| enabled_for_bytes[index] != 0);
    let (input, _action_index) = le_u8(input)?;

    Ok((
        input,
        RawBriefingTrigger {
            conditions,
            actions,
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
                .filter_map(|action| briefing_action_string_id(action.action))
        }))
    }
}

fn briefing_action_string_id(action: RawBriefingAction) -> Option<StringId> {
    match action {
        RawBriefingAction::DisplayTextMessage { text, .. }
        | RawBriefingAction::MissionObjectives { text }
        | RawBriefingAction::Transmission { text, .. } => Some(text),
        _ => None,
    }
}

fn scan_action_string_id(action: &[u8]) -> Option<StringId> {
    debug_assert_eq!(action.len(), 32);
    let valid_string_action = match action[26] {
        3 | 4 => true,
        8 => NumberOperation::try_from(action[27]).is_ok(),
        _ => false,
    };

    valid_string_action
        .then(|| StringId::from(u32::from_le_bytes(action[4..8].try_into().unwrap())))
}

/// Finds the string IDs used by complete briefing trigger records without constructing briefing
/// trigger objects.
///
/// The action-specific validity checks mirror [`briefing_action_data`], so malformed actions are
/// excluded in the same way as [`read_briefing`].
pub(crate) fn scan_used_string_ids(data: &[u8]) -> impl Iterator<Item = StringId> + '_ {
    const TRIGGER_SIZE: usize = 2400;
    const CONDITIONS_SIZE: usize = 16 * 20;
    const ACTION_SIZE: usize = 32;
    const ACTIONS_SIZE: usize = 64 * ACTION_SIZE;

    data.chunks_exact(TRIGGER_SIZE).flat_map(|trigger| {
        trigger[CONDITIONS_SIZE..CONDITIONS_SIZE + ACTIONS_SIZE]
            .chunks_exact(ACTION_SIZE)
            .filter_map(scan_action_string_id)
    })
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum BriefingError {
    #[error("Error parsing briefing")]
    ParseError(nom::error::ErrorKind),
}

pub fn read_briefing(data: &[u8]) -> Result<Vec<RawBriefingTrigger>, BriefingError> {
    let (_, triggers) = many0(raw_briefing_trigger)
        .parse(data)
        .map_err(|e| match e {
            nom::Err::Error(e) | nom::Err::Failure(e) => BriefingError::ParseError(e.code),
            nom::Err::Incomplete(_) => unreachable!(),
        })?;

    Ok(triggers)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fast_string_scanner_matches_typed_action_parser() {
        for action_type in u8::MIN..=u8::MAX {
            for operation in [0, 7, 8, 9, u8::MAX] {
                let mut action = [0u8; 32];
                action[4..8].copy_from_slice(&1234u32.to_le_bytes());
                action[26] = action_type;
                action[27] = operation;

                let typed = briefing_action_data(&action)
                    .unwrap()
                    .1
                    .and_then(|action| briefing_action_string_id(action.action));
                assert_eq!(scan_action_string_id(&action), typed);
            }
        }
    }
}
