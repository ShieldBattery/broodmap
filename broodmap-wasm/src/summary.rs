//! A map's scenario facts (name, size, slots, forces, triggers) without rendering anything, for
//! listing and filtering map files cheaply. Needs no game assets.

use serde::Serialize;
use wasm_bindgen::prelude::*;

use broodmap::chk::Chk;
use broodmap::chk::placed_units::START_LOCATION_ID;
use broodmap::chk::strings::{LegacyCodePage, StringEncoding};

use crate::parse_map;

/// The number of slots that can hold players (the rest are neutral/extended slots).
const PLAYER_SLOTS: usize = 8;
/// The type id of the action that fills empty action slots.
const NO_ACTION: u8 = 0;

#[wasm_bindgen(typescript_custom_section)]
const MAP_SUMMARY_TYPES: &str = r#"
/** The JSON `summarizeMapJson` and `MapRenderer.summaryJson` return. See the README. */
export interface MapSummary {
    name: string | null;
    description: string | null;
    width: number;
    height: number;
    tileset: number;
    stringEncoding: "utf8" | "utf8-cp949" | "utf8-cp1252" | "cp949" | "cp1252";
    players: MapSlotSummary[];
    forces: MapForceSummary[];
    usesEud: boolean;
    triggerActionKinds: string[];
    triggerCount: number;
    placedUnits: number;
    playerUnits: number;
    startLocations: number;
}

export interface MapSlotSummary {
    controller: number;
    race: number;
    force: number;
}

export interface MapForceSummary {
    name: string | null;
}

/** Overrides of broodmap's default resource limits, passed as JSON when parsing a map. */
export interface MapLimitOptions {
    maxMapBytes?: number;
    maxMpqFileBytes?: number;
    maxChkBytes?: number;
    maxMergedChunkBytes?: number;
}
"#;

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct MapSummary {
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
    /// The distinct kinds of trigger action the map uses (see `broodmap::chk::triggers::
    /// action_type_name`, e.g. `CreateUnit`), sorted. Empty action slots don't count.
    trigger_action_kinds: Vec<&'static str>,
    /// The number of triggers.
    trigger_count: usize,
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

pub(crate) fn summarize(chk: &Chk) -> Result<MapSummary, String> {
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

    let trigger_stats = chk.trigger_action_stats();
    let mut action_types = trigger_stats.action_types;
    action_types.remove(NO_ACTION);
    let mut trigger_action_kinds = action_types.names().collect::<Vec<_>>();
    trigger_action_kinds.sort_unstable();

    let mut placed_units = 0;
    let mut player_units = 0;
    let mut start_locations = 0;
    for unit in chk.placed_units().map(|u| u.as_slice()).unwrap_or(&[]) {
        if unit.unit_id == START_LOCATION_ID {
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
        trigger_count: trigger_stats.trigger_count,
        placed_units,
        player_units,
        start_locations,
    })
}

pub(crate) fn summary_json(chk: &Chk) -> Result<String, String> {
    serde_json::to_string(&summarize(chk)?).map_err(|e| e.to_string())
}

/// Summarizes a map file's scenario as a JSON `MapSummary` (see the README for the fields).
/// `options` is optional `MapLimitOptions` JSON raising (or lowering) broodmap's default resource
/// limits. Fails if the file isn't a readable map within those limits, or lacks the scenario name,
/// player or force sections every playable map has. To also render the map, construct a
/// `MapRenderer` and call its `summaryJson()` instead, which parses the map only once.
#[wasm_bindgen(js_name = summarizeMapJson)]
pub fn summarize_map_json(map_bytes: &[u8], options: Option<String>) -> Result<String, String> {
    summary_json(&parse_map(map_bytes, options.as_deref())?)
}

#[cfg(test)]
mod tests {
    use super::*;

    const LT: &[u8] = include_bytes!("../../broodmap/assets/lt.scm");

    fn summary(map_bytes: &[u8]) -> Result<MapSummary, String> {
        summarize(&parse_map(map_bytes, None)?)
    }

    #[test]
    fn summarizes_a_melee_map() {
        let summary = summary(LT).unwrap();
        assert_eq!((summary.width, summary.height), (128, 128));
        assert!(summary.name.is_some());
        assert_eq!(summary.players.len(), 8);
        assert_eq!(summary.forces.len(), 4);
        assert_eq!(summary.start_locations, 4);
        assert!(!summary.uses_eud);
        assert_eq!(summary.trigger_count, 3);
        assert!(!summary.trigger_action_kinds.contains(&"NoAction"));
        assert!(summary.players.iter().filter(|p| p.controller == 6).count() >= 2);
    }

    #[test]
    fn summarizes_a_ums_map_with_its_trigger_kinds() {
        let summary = summary(include_bytes!("../../broodmap/assets/Sniper_Seed_vA.scx")).unwrap();
        let kinds = summary.trigger_action_kinds;
        assert!(!kinds.is_empty());
        assert!(kinds.windows(2).all(|w| w[0] < w[1]), "sorted and distinct");
        assert!(kinds.contains(&"CreateUnit"));
        assert!(!kinds.contains(&"NoAction"));
    }

    #[test]
    fn rejects_files_that_arent_maps() {
        assert!(summary(b"not a map").is_err());
        assert!(summarize_map_json(b"", None).is_err());
    }

    #[test]
    fn options_set_resource_limits() {
        assert!(summarize_map_json(LT, Some("{}".into())).is_ok());
        assert!(summarize_map_json(LT, Some(r#"{"maxChkBytes":16}"#.into())).is_err());
        assert!(summarize_map_json(LT, Some(r#"{"maxChkByte":16}"#.into())).is_err());
    }

    #[test]
    fn json_uses_camel_case() {
        let json = summarize_map_json(LT, None).unwrap();
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
