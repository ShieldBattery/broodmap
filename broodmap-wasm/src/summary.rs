//! A map's scenario facts (name, size, slots, forces, triggers) without rendering anything, for
//! listing and filtering map files cheaply. Needs no game assets.

use std::collections::BTreeSet;

use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

use broodmap::chk::strings::{LegacyCodePage, StringEncoding};
use broodmap::{ResourceLimits, extract_chk_from_map_with_limits};

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

/// Overrides of broodmap's default [`ResourceLimits`], as JSON. Every field is optional.
#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "camelCase", default, deny_unknown_fields)]
struct SummaryOptions {
    max_map_bytes: Option<usize>,
    max_mpq_file_bytes: Option<usize>,
    max_chk_bytes: Option<usize>,
    max_merged_chunk_bytes: Option<usize>,
}

impl SummaryOptions {
    fn parse(json: Option<&str>) -> Result<SummaryOptions, String> {
        match json.map(str::trim) {
            None | Some("") => Ok(SummaryOptions::default()),
            Some(json) => serde_json::from_str(json).map_err(|e| format!("invalid options: {e}")),
        }
    }

    fn limits(&self) -> ResourceLimits {
        let mut limits = ResourceLimits::default();
        if let Some(bytes) = self.max_map_bytes {
            limits.max_map_bytes = bytes;
        }
        if let Some(bytes) = self.max_mpq_file_bytes {
            limits.max_mpq_file_bytes = bytes;
        }
        if let Some(bytes) = self.max_chk_bytes {
            limits.max_chk_bytes = bytes;
        }
        if let Some(bytes) = self.max_merged_chunk_bytes {
            limits.max_merged_chunk_bytes = bytes;
        }
        limits
    }
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

fn summarize(map_bytes: &[u8], limits: &ResourceLimits) -> Result<MapSummary, String> {
    let (chk, _mpq) = extract_chk_from_map_with_limits(map_bytes, None, None, limits)
        .map_err(|e| e.to_string())?;

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

/// Summarizes a map file's scenario as JSON (see the README for the fields). `options` is an
/// optional JSON object raising (or lowering) broodmap's default resource limits:
/// `{ "maxMapBytes", "maxMpqFileBytes", "maxChkBytes", "maxMergedChunkBytes" }`. Fails if the file
/// isn't a readable map within those limits, or lacks the scenario name, player or force sections
/// every playable map has.
#[wasm_bindgen(js_name = summarizeMapJson)]
pub fn summarize_map_json(map_bytes: &[u8], options: Option<String>) -> Result<String, String> {
    let limits = SummaryOptions::parse(options.as_deref())?.limits();
    let summary = summarize(map_bytes, &limits)?;
    serde_json::to_string(&summary).map_err(|e| e.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn default_summary(map_bytes: &[u8]) -> Result<MapSummary, String> {
        summarize(map_bytes, &ResourceLimits::default())
    }

    #[test]
    fn summarizes_a_melee_map() {
        let summary = default_summary(include_bytes!("../../broodmap/assets/lt.scm")).unwrap();
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
            default_summary(include_bytes!("../../broodmap/assets/Sniper_Seed_vA.scx")).unwrap();
        let kinds = summary.trigger_action_kinds.unwrap();
        assert!(!kinds.is_empty());
        assert!(kinds.windows(2).all(|w| w[0] < w[1]), "sorted and distinct");
        assert!(kinds.iter().all(|k| !k.is_empty() && !k.contains(' ')));
    }

    #[test]
    fn rejects_files_that_arent_maps() {
        assert!(default_summary(b"not a map").is_err());
        assert!(summarize_map_json(b"", None).is_err());
    }

    #[test]
    fn options_set_resource_limits() {
        let map_bytes = include_bytes!("../../broodmap/assets/lt.scm");
        assert!(summarize_map_json(map_bytes, Some("{}".into())).is_ok());
        assert!(summarize_map_json(map_bytes, Some(r#"{"maxChkBytes":16}"#.into())).is_err());
        assert!(summarize_map_json(map_bytes, Some(r#"{"maxChkByte":16}"#.into())).is_err());

        let limits = SummaryOptions::parse(Some(r#"{"maxChkBytes":5,"maxMapBytes":7}"#))
            .unwrap()
            .limits();
        assert_eq!((limits.max_chk_bytes, limits.max_map_bytes), (5, 7));
        assert_eq!(
            limits.max_mpq_file_bytes,
            ResourceLimits::default().max_mpq_file_bytes
        );
    }

    #[test]
    fn json_uses_camel_case() {
        let json =
            summarize_map_json(include_bytes!("../../broodmap/assets/lt.scm"), None).unwrap();
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
