//! A map's scenario facts (name, size, slots, forces, triggers) without rendering anything, for
//! listing and filtering map files cheaply. Needs no game assets.

use std::collections::BTreeSet;

use serde::Serialize;
use wasm_bindgen::prelude::*;

use broodmap::chk::strings::{LegacyCodePage, StringEncoding};
use broodmap::extract_chk_from_map;

/// The unit placed to mark a start location.
const START_LOCATION_UNIT: u16 = 214;
/// The number of slots that can hold players (the rest are neutral/extended slots).
const PLAYER_SLOTS: usize = 8;

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct MapSummary {
    /// The scenario name, decoded but otherwise as stored (color codes included). `None` means
    /// BW shows the file name instead.
    name: Option<String>,
    /// The scenario description, decoded but otherwise as stored.
    description: Option<String>,
    /// The width in tiles.
    width: usize,
    /// The height in tiles.
    height: usize,
    /// The ERA tileset id.
    tileset: u8,
    /// How the map's strings were decoded: `utf8`, `utf8-cp949`, `utf8-cp1252`, `cp949` or
    /// `cp1252`.
    string_encoding: &'static str,
    /// The 8 player slots, in slot order.
    players: Vec<SlotSummary>,
    /// The 4 forces, in force order.
    forces: Vec<ForceSummary>,
    /// Whether the map's triggers read or write memory outside the death table (EUDs).
    uses_eud: bool,
    /// The distinct kinds of trigger action the map uses (each action's variant name, e.g.
    /// `CreateUnit`), sorted. `None` if the triggers couldn't be read.
    trigger_action_kinds: Option<Vec<String>>,
    /// The number of triggers, if they could be read.
    trigger_count: Option<usize>,
    /// Placed units, not counting start locations.
    placed_units: usize,
    /// Placed units owned by one of the 8 player slots, not counting start locations.
    player_units: usize,
    /// The number of start locations placed.
    start_locations: usize,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct SlotSummary {
    /// The slot's `OWNR` controller value (e.g. 6 for a human slot, 5 for a computer).
    controller: u8,
    /// The slot's `SIDE` race value (e.g. 5 for user-selectable).
    race: u8,
    /// The index of the force the slot is assigned to. Values of 4 or more are outside every
    /// force.
    force: u8,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct ForceSummary {
    name: Option<String>,
}

fn string_encoding_name(encoding: StringEncoding) -> &'static str {
    match encoding {
        StringEncoding::Utf8 => "utf8",
        StringEncoding::Utf8WithFallback(LegacyCodePage::Korean) => "utf8-cp949",
        StringEncoding::Utf8WithFallback(LegacyCodePage::Latin) => "utf8-cp1252",
        StringEncoding::Legacy(LegacyCodePage::Korean) => "cp949",
        StringEncoding::Legacy(LegacyCodePage::Latin) => "cp1252",
    }
}

/// The variant name of a trigger action, from its `Debug` output (which starts with it).
fn action_kind_name(action: &impl std::fmt::Debug) -> String {
    format!("{action:?}")
        .split(|c: char| !c.is_ascii_alphanumeric())
        .next()
        .unwrap_or_default()
        .to_string()
}

fn summarize(map_bytes: &[u8]) -> Result<MapSummary, String> {
    let (chk, _mpq) = extract_chk_from_map(map_bytes, None, None).map_err(|e| e.to_string())?;

    let props = chk.scenario_props().map_err(|e| format!("SPRP: {e}"))?;
    let controllers = chk.player_types().map_err(|e| format!("OWNR: {e}"))?;
    let races = chk.player_races().map_err(|e| format!("SIDE: {e}"))?;
    let force_settings = chk.force_settings().map_err(|e| format!("FORC: {e}"))?;

    let players = (0..PLAYER_SLOTS)
        .map(|slot| SlotSummary {
            controller: u8::from(controllers[slot]),
            race: u8::from(races[slot]),
            force: force_settings.assigned_forces[slot],
        })
        .collect();
    let forces = force_settings
        .forces
        .iter()
        .map(|force| ForceSummary {
            name: force.name.clone(),
        })
        .collect();

    let (trigger_action_kinds, trigger_count) = match chk.raw_triggers() {
        Ok(triggers) => {
            let kinds = triggers
                .iter()
                .flat_map(|t| &t.actions)
                .map(|action| action_kind_name(&action.action))
                .collect::<BTreeSet<_>>();
            (Some(kinds.into_iter().collect()), Some(triggers.len()))
        }
        Err(_) => (None, None),
    };

    let mut placed_units = 0;
    let mut player_units = 0;
    let mut start_locations = 0;
    for unit in chk.placed_units().map(|u| u.as_slice()).unwrap_or(&[]) {
        if unit.unit_id == START_LOCATION_UNIT {
            start_locations += 1;
            continue;
        }
        placed_units += 1;
        if unit.owner.is_some_and(|o| usize::from(o) < PLAYER_SLOTS) {
            player_units += 1;
        }
    }

    Ok(MapSummary {
        name: props.name.clone(),
        description: props.description.clone(),
        width: chk.width(),
        height: chk.height(),
        tileset: chk.tileset() as u8,
        string_encoding: string_encoding_name(chk.strings().encoding),
        players,
        forces,
        uses_eud: chk.uses_eud(),
        trigger_action_kinds,
        trigger_count,
        placed_units,
        player_units,
        start_locations,
    })
}

/// Summarizes a map file's scenario as JSON (see the README for the fields). Fails if the file
/// isn't a readable map, or lacks the scenario name, player or force sections every playable map
/// has.
#[wasm_bindgen(js_name = summarizeMapJson)]
pub fn summarize_map_json(map_bytes: &[u8]) -> Result<String, String> {
    let summary = summarize(map_bytes)?;
    serde_json::to_string(&summary).map_err(|e| e.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn summarizes_a_melee_map() {
        let summary = summarize(include_bytes!("../../broodmap/assets/lt.scm")).unwrap();
        assert_eq!((summary.width, summary.height), (128, 128));
        assert!(summary.name.is_some());
        assert_eq!(summary.players.len(), 8);
        assert_eq!(summary.forces.len(), 4);
        assert_eq!(summary.start_locations, 4);
        assert!(!summary.uses_eud);
        assert!(summary.trigger_action_kinds.is_some());
        assert!(summary.players.iter().filter(|p| p.controller == 6).count() >= 2);
    }

    #[test]
    fn summarizes_a_ums_map_with_its_trigger_kinds() {
        let summary =
            summarize(include_bytes!("../../broodmap/assets/Sniper_Seed_vA.scx")).unwrap();
        let kinds = summary.trigger_action_kinds.unwrap();
        assert!(!kinds.is_empty());
        assert!(kinds.windows(2).all(|w| w[0] < w[1]), "sorted and distinct");
        assert!(kinds.iter().all(|k| !k.is_empty() && !k.contains(' ')));
    }

    #[test]
    fn rejects_files_that_arent_maps() {
        assert!(summarize(b"not a map").is_err());
        assert!(summarize_map_json(b"").is_err());
    }

    #[test]
    fn json_uses_camel_case() {
        let json = summarize_map_json(include_bytes!("../../broodmap/assets/lt.scm")).unwrap();
        for key in [
            "\"stringEncoding\"",
            "\"usesEud\"",
            "\"triggerActionKinds\"",
            "\"playerUnits\"",
            "\"startLocations\"",
            "\"controller\"",
        ] {
            assert!(json.contains(key), "{key} in {json}");
        }
    }
}
