//! Static pixel-rectangle wall checking, independent of topology selection.
use super::TerrainAnalysis;
use broodmap_analysis::walls::{
    CollisionBounds, PlacementIssue, WallBuilding, WallPassage, WallQuery, check_wall,
    search::{
        BuildingSpec, WallPlacementSite, WallSearch, WallSearchQuery, WallSearchReason,
        suggest_wall_endpoints, wall_placement_reach,
    },
};
use broodmap_analysis::{DepotFootprint, PixelPosition, PixelRect, TilePosition};
use broodmap_formats::UnitsDat;
use serde::{Deserialize, Serialize};
use serde_json::json;
use wasm_bindgen::prelude::*;

#[derive(Default, Serialize)]
pub(super) struct WallProfiles {
    buildings: Vec<BuildingProfile>,
    movers: Vec<MoverProfile>,
}
#[derive(Serialize)]
struct BuildingProfile {
    id: u16,
    name: &'static str,
    footprint: [u32; 2],
    collision: [u32; 4],
}
#[derive(Serialize)]
struct MoverProfile {
    id: u16,
    name: &'static str,
    collision: [u32; 4],
}
impl WallProfiles {
    pub(super) fn from_definitions(defs: &UnitsDat) -> Self {
        // Names/IDs are conventional melee conveniences only. Dimensions always come from DAT.
        let mut result = Self::default();
        for (id, name) in [
            (109, "Supply Depot"),
            (111, "Barracks"),
            (113, "Factory"),
            (122, "Engineering Bay"),
            (125, "Bunker"),
            (156, "Pylon"),
            (160, "Gateway"),
            (166, "Forge"),
        ] {
            let Some(entry) = defs.entry(id) else {
                continue;
            };
            let (w, h) = entry.placebox;
            let bounds = [
                entry.bounds.0,
                entry.bounds.1,
                entry.bounds.2,
                entry.bounds.3,
            ];
            if entry.special_ability_flags & 1 == 0
                || w <= 0
                || h <= 0
                || w > 8192
                || h > 8192
                || w % 32 != 0
                || h % 32 != 0
                || bounds.iter().any(|&v| !(0..=8192).contains(&v))
            {
                continue;
            }
            result.buildings.push(BuildingProfile {
                id,
                name,
                footprint: [w as u32 / 32, h as u32 / 32],
                collision: bounds.map(|v| v as u32),
            });
        }
        for (id, name) in [
            (0, "Marine"),
            (7, "SCV"),
            (37, "Zergling"),
            (38, "Hydralisk"),
            (65, "Zealot"),
            (66, "Dragoon"),
        ] {
            let Some(entry) = defs.entry(id) else {
                continue;
            };
            let bounds = [
                entry.bounds.0,
                entry.bounds.1,
                entry.bounds.2,
                entry.bounds.3,
            ];
            if entry.special_ability_flags & 5 != 0
                || bounds.iter().any(|&v| !(0..=8192).contains(&v))
            {
                continue;
            }
            result.movers.push(MoverProfile {
                id,
                name,
                collision: bounds.map(|v| v as u32),
            });
        }
        result
    }
}
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct QueryInput {
    window: [i32; 4],
    start: [u32; 2],
    end: [u32; 2],
    mover: [u32; 4],
    buildings: Vec<BuildingInput>,
}
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct BuildingInput {
    tile: [u32; 2],
    footprint: [u32; 2],
    collision: [u32; 4],
}

/// Caller-selected inventory entry for incremental wall search. Buildings are placed at most
/// once per entry, so duplicate profiles are represented by duplicate entries.
#[derive(Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct SearchBuildingInput {
    footprint: [u32; 2],
    collision: [u32; 4],
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct PlacementSiteInput {
    entrance: [[u32; 2]; 2],
    max_distance_pixels: u32,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct SearchQueryInput {
    window: [i32; 4],
    placement_area: [i32; 4],
    placement_elevation: Option<u8>,
    placement_site: Option<PlacementSiteInput>,
    start: [u32; 2],
    end: [u32; 2],
    block: [u32; 4],
    allow: Option<[u32; 4]>,
    buildings: Vec<SearchBuildingInput>,
    max_states: u32,
    max_results: usize,
}
impl From<PlacementSiteInput> for WallPlacementSite {
    fn from(site: PlacementSiteInput) -> Self {
        Self {
            entrance: site.entrance.map(|[x, y]| PixelPosition { x, y }),
            max_distance_pixels: site.max_distance_pixels,
        }
    }
}
impl From<SearchQueryInput> for WallSearchQuery {
    fn from(input: SearchQueryInput) -> Self {
        Self {
            window: rect(input.window),
            placement_area: rect(input.placement_area),
            placement_elevation: input.placement_elevation,
            placement_site: input.placement_site.map(|site| WallPlacementSite {
                entrance: site.entrance.map(|[x, y]| PixelPosition { x, y }),
                max_distance_pixels: site.max_distance_pixels,
            }),
            start: PixelPosition {
                x: input.start[0],
                y: input.start[1],
            },
            end: PixelPosition {
                x: input.end[0],
                y: input.end[1],
            },
            block: bounds(input.block),
            allow: input.allow.map(bounds),
            buildings: input
                .buildings
                .into_iter()
                .map(|building| BuildingSpec {
                    footprint: DepotFootprint {
                        width_tiles: building.footprint[0],
                        height_tiles: building.footprint[1],
                    },
                    collision: bounds(building.collision),
                })
                .collect(),
            max_states: input.max_states,
            max_results: input.max_results,
        }
    }
}

/// Incremental bounded wall search. It owns the core search snapshot, so it can safely outlive
/// its TerrainAnalysis creator. Dropping/freeing it abandons unfinished work.
#[wasm_bindgen]
pub struct WallSearchJob {
    job: WallSearch,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct PlacementReachInput {
    window: [i32; 4],
    placement_site: PlacementSiteInput,
}
const MAX_WALL_SEARCH_STEP_STATES: u32 = 16;
fn bounds([left, top, right, bottom]: [u32; 4]) -> CollisionBounds {
    CollisionBounds {
        left,
        top,
        right,
        bottom,
    }
}
fn rect([left, top, right, bottom]: [i32; 4]) -> PixelRect {
    PixelRect {
        left,
        top,
        right,
        bottom,
    }
}
fn rect_array(r: PixelRect) -> [i32; 4] {
    [r.left, r.top, r.right, r.bottom]
}

#[wasm_bindgen]
impl TerrainAnalysis {
    /// Returns terrain-only walk-cell connectivity reachable from an entrance near a bounded
    /// pixel window. The result is local to the returned rectangle and ignores objects,
    /// buildability, and terrain height.
    #[wasm_bindgen(js_name = wallPlacementReachJson)]
    pub fn wall_placement_reach_json(&self, input_json: &str) -> Result<String, String> {
        if input_json.len() > 4_096 {
            return Err("wall placement reach input exceeds 4 KiB".into());
        }
        let input: PlacementReachInput = serde_json::from_str(input_json)
            .map_err(|e| format!("invalid wall placement reach input: {e}"))?;
        let reach =
            wall_placement_reach(&self.grid, rect(input.window), input.placement_site.into())
                .map_err(|e| e.to_string())?;
        serde_json::to_string(&json!({
            "origin": [reach.origin.x, reach.origin.y],
            "width": reach.width,
            "height": reach.height,
            "cells": reach.cells.into_iter().map(u8::from).collect::<Vec<_>>(),
        }))
        .map_err(|e| e.to_string())
    }
    /// Conventional melee palette with dimensions from the caller's units.dat. Empty for terrain-only snapshots.
    #[wasm_bindgen(js_name = wallProfilesJson)]
    pub fn wall_profiles_json(&self) -> Result<String, String> {
        serde_json::to_string(&self.wall_profiles).map_err(|e| e.to_string())
    }
    /// Check proposed building rectangles and mover passage in an explicitly bounded pixel window.
    /// Uses original terrain plus exact static object rectangles when the obstacle toggle is enabled.
    /// Static placement checks omit race rules, power, creep, resource spacing, and dynamic state.
    /// A blocked result is local to this window and model, not a certified game wall.
    #[wasm_bindgen(js_name = checkWallJson)]
    pub fn check_wall_json(&self, query_json: &str) -> Result<String, String> {
        if query_json.len() > 32_768 {
            return Err("wall query exceeds 32 KiB".into());
        }
        let input: QueryInput =
            serde_json::from_str(query_json).map_err(|e| format!("invalid wall query: {e}"))?;
        let query = WallQuery {
            window: rect(input.window),
            start: PixelPosition {
                x: input.start[0],
                y: input.start[1],
            },
            end: PixelPosition {
                x: input.end[0],
                y: input.end[1],
            },
            mover: bounds(input.mover),
            buildings: input
                .buildings
                .into_iter()
                .map(|b| WallBuilding {
                    tile: TilePosition {
                        x: b.tile[0],
                        y: b.tile[1],
                    },
                    footprint: DepotFootprint {
                        width_tiles: b.footprint[0],
                        height_tiles: b.footprint[1],
                    },
                    collision: bounds(b.collision),
                })
                .collect(),
        };
        let obstacles = if self.respect_obstacles {
            self.wall_obstacles.as_slice()
        } else {
            &[]
        };
        let result = check_wall(&self.grid, obstacles, &query).map_err(|e| e.to_string())?;
        let passage = match result.passage {
            WallPassage::Open => "open",
            WallPassage::BlockedWithinWindow => "blockedWithinWindow",
            WallPassage::AlreadyDisconnected => "alreadyDisconnected",
            WallPassage::StartBlocked => "startBlocked",
            WallPassage::EndBlocked => "endBlocked",
        };
        let placements: Vec<_> = result.placements.iter().map(|p| json!({
            "footprint": rect_array(p.footprint), "collision": rect_array(p.collision),
            "issues": p.issues.iter().map(|i| match i {
                PlacementIssue::OutOfMap => json!({"kind":"outOfMap"}),
                PlacementIssue::UnbuildableTerrain => json!({"kind":"unbuildableTerrain"}),
                PlacementIssue::OverlapsObstacle { index } => json!({"kind":"overlapsObstacle","index":index}),
                PlacementIssue::OverlapsBuilding { index } => json!({"kind":"overlapsBuilding","index":index}),
            }).collect::<Vec<_>>(),
        })).collect();
        serde_json::to_string(&json!({
            "model": "pixel-rect-v1", "window": rect_array(result.window),
            "baselinePassable": result.baseline_passable, "passage": passage,
            "route": result.route.iter().map(|p| [p.x,p.y]).collect::<Vec<_>>(),
            "staticPlacementValid": result.placements.iter().all(|p| p.issues.is_empty()), "placements": placements,
        })).map_err(|e| e.to_string())
    }
}

#[wasm_bindgen]
impl TerrainAnalysis {
    /// Starts a bounded, resumable search for static wall layouts. Construction prepares terrain
    /// and candidates synchronously, so its bounded preprocessing can still occupy one event-loop turn.
    /// The job snapshots the selected
    /// terrain/obstacle model, so later obstacle-toggle changes do not affect it.
    #[wasm_bindgen(js_name = startWallSearch)]
    pub fn start_wall_search(&self, query_json: &str) -> Result<WallSearchJob, String> {
        if query_json.len() > 32_768 {
            return Err("wall search query exceeds 32 KiB".into());
        }
        let input: SearchQueryInput = serde_json::from_str(query_json)
            .map_err(|e| format!("invalid wall search query: {e}"))?;
        let query: WallSearchQuery = input.into();
        let obstacles = if self.respect_obstacles {
            self.wall_obstacles.as_slice()
        } else {
            &[]
        };
        let job = WallSearch::new(&self.grid, obstacles, query).map_err(|e| e.to_string())?;
        Ok(WallSearchJob { job })
    }
    /// Chooses collision-safe walk-cell-center endpoints around a selected entrance. Search query
    /// start/end are placeholders here; the explicit entrance controls the direction.
    #[wasm_bindgen(js_name = suggestWallEndpointsJson)]
    pub fn suggest_wall_endpoints_json(
        &self,
        query_json: &str,
        entrance_json: &str,
    ) -> Result<String, String> {
        if query_json.len() > 32_768 || entrance_json.len() > 1_024 {
            return Err("wall endpoint suggestion input exceeds its size cap".into());
        }
        let input: SearchQueryInput = serde_json::from_str(query_json)
            .map_err(|e| format!("invalid wall search query: {e}"))?;
        let entrance: [[u32; 2]; 2] = serde_json::from_str(entrance_json)
            .map_err(|e| format!("invalid wall entrance: {e}"))?;
        let query: WallSearchQuery = input.into();
        let obstacles = if self.respect_obstacles {
            self.wall_obstacles.as_slice()
        } else {
            &[]
        };
        let [start, end] = suggest_wall_endpoints(
            &self.grid,
            obstacles,
            &query,
            [
                PixelPosition {
                    x: entrance[0][0],
                    y: entrance[0][1],
                },
                PixelPosition {
                    x: entrance[1][0],
                    y: entrance[1][1],
                },
            ],
        )
        .map_err(|e| e.to_string())?;
        serde_json::to_string(&json!({"start": [start.x, start.y], "end": [end.x, end.y]}))
            .map_err(|e| e.to_string())
    }
}

#[wasm_bindgen]
impl WallSearchJob {
    /// Advances at most 16 search states. Call between browser event-loop yields.
    /// Returns true when a terminal result is available.
    pub fn step(&mut self, max_states: f64) -> Result<bool, String> {
        if !max_states.is_finite()
            || max_states.fract() != 0.0
            || !(1.0..=f64::from(MAX_WALL_SEARCH_STEP_STATES)).contains(&max_states)
        {
            return Err("wall search step must be an integer from 1 through 16".into());
        }
        self.job.step(max_states as u32);
        Ok(self.job.is_done())
    }

    #[wasm_bindgen(getter, js_name = exploredStates)]
    pub fn explored_states(&self) -> u32 {
        self.job.result().explored_states
    }

    #[wasm_bindgen(getter, js_name = layoutCount)]
    pub fn layout_count(&self) -> usize {
        self.job.result().layouts.len()
    }

    /// Serializes the completed result. Calling this before a terminal step is an error so callers
    /// do not accidentally copy route data while reporting progress.
    #[wasm_bindgen(js_name = resultJson)]
    pub fn result_json(&self) -> Result<String, String> {
        if !self.job.is_done() {
            return Err("wall search is not complete".into());
        }
        let result = self.job.result();
        let reason = match result.reason {
            WallSearchReason::Running => "running",
            WallSearchReason::Exhausted => "exhausted",
            WallSearchReason::StateLimit => "stateLimit",
            WallSearchReason::ResultLimit => "resultLimit",
        };
        let layouts: Vec<_> = result
            .layouts
            .iter()
            .map(|layout| {
                let buildings: Vec<_> = layout
                    .buildings
                    .iter()
                    .map(|placement| {
                        let building = placement.building;
                        json!({
                            "specIndex": placement.spec_index,
                            "tile": [building.tile.x, building.tile.y],
                            "footprint": [
                                building.footprint.width_tiles,
                                building.footprint.height_tiles,
                            ],
                            "collision": [
                                building.collision.left,
                                building.collision.top,
                                building.collision.right,
                                building.collision.bottom,
                            ],
                        })
                    })
                    .collect();
                json!({
                    "buildings": buildings,
                    "allowRoute": layout
                        .allow_route
                        .iter()
                        .map(|point| [point.x, point.y])
                        .collect::<Vec<_>>(),
                })
            })
            .collect();
        serde_json::to_string(&json!({
            "model": "pixel-rect-v1", "searchAlgorithm": "bounded-state-v1",
            "reason": reason,
            "exploredStates": result.explored_states,
            "candidatePlacements": result.candidate_placements,
            "layouts": layouts,
        }))
        .map_err(|e| e.to_string())
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use broodmap_analysis::{TerrainCell, TerrainGrid};
    fn analysis() -> TerrainAnalysis {
        TerrainAnalysis::terrain_only(
            TerrainGrid::from_cells(
                16,
                16,
                vec![
                    TerrainCell {
                        walkable: true,
                        terrain_buildable: true,
                        ramp: false,
                        elevation: 0,
                    };
                    256
                ],
            )
            .unwrap(),
        )
    }
    #[test]
    fn placement_reach_serializes_local_origin_and_stride_and_rejects_bad_input() {
        let analysis = analysis();
        let input = json!({
            "window": [9, 17, 41, 33],
            "placementSite": { "entrance": [[16, 20], [16, 28]], "maxDistancePixels": 64 },
        });
        let result: serde_json::Value = serde_json::from_str(
            &analysis
                .wall_placement_reach_json(&input.to_string())
                .unwrap(),
        )
        .unwrap();
        assert_eq!(result["origin"], json!([1, 2]));
        assert_eq!(result["width"], json!(5));
        assert_eq!(result["height"], json!(3));
        assert_eq!(result["cells"], json!(vec![1; 15]));

        assert!(
            analysis
                .wall_placement_reach_json(&"x".repeat(4_097))
                .is_err()
        );
        for invalid in [
            json!({
                "window": [0, 0, 0, 128],
                "placementSite": { "entrance": [[16, 20], [16, 28]], "maxDistancePixels": 64 },
            }),
            json!({
                "window": [0, 0, 128, 128],
                "placementSite": { "entrance": [[16, 20], [16, 28]], "maxDistancePixels": 1_025 },
            }),
            json!({
                "window": [0, 0, 128, 128],
                "placementSite": { "entrance": [[16, 20], [16, 20]], "maxDistancePixels": 64 },
            }),
            json!({
                "window": [0, 0, 128, 128],
                "placementSite": { "entrance": [[16, 20], [16, 28]], "maxDistancePixels": 64 },
                "extra": true,
            }),
        ] {
            assert!(
                analysis
                    .wall_placement_reach_json(&invalid.to_string())
                    .is_err()
            );
        }
    }
    fn query() -> serde_json::Value {
        json!({"window":[0,0,128,128], "start":[8,64], "end":[120,64], "mover":[0,0,0,0], "buildings":[]})
    }
    fn search_query() -> serde_json::Value {
        json!({
            "window": [0, 0, 128, 128],
            "placementArea": [0, 0, 32, 32],
            "start": [8, 64],
            "end": [120, 64],
            "block": [0, 0, 0, 0],
            "allow": null,
            "buildings": [],
            "maxStates": 1,
            "maxResults": 1,
        })
    }

    fn corridor_analysis() -> TerrainAnalysis {
        TerrainAnalysis::terrain_only(
            TerrainGrid::from_cells(
                16,
                12,
                (0..12)
                    .flat_map(|y| {
                        (0..16).map(move |_| TerrainCell {
                            walkable: (4..9).contains(&y),
                            terrain_buildable: (4..9).contains(&y),
                            ramp: false,
                            elevation: 0,
                        })
                    })
                    .collect(),
            )
            .unwrap(),
        )
    }

    fn corridor_search_query() -> serde_json::Value {
        json!({
            "window": [0, 0, 128, 96],
            "placementArea": [0, 32, 128, 64],
            "start": [4, 48],
            "end": [123, 48],
            "block": [4, 4, 4, 4],
            "allow": [0, 0, 0, 0],
            "buildings": [{"footprint": [1, 1], "collision": [16, 16, 15, 15]}],
            "maxStates": 1000,
            "maxResults": 16,
        })
    }

    #[test]
    fn incremental_search_serializes_real_layouts_and_allow_routes() {
        let terrain = corridor_analysis();
        let mut job = terrain
            .start_wall_search(&corridor_search_query().to_string())
            .unwrap();
        while !job.step(16.0).unwrap() {}
        let result: serde_json::Value = serde_json::from_str(&job.result_json().unwrap()).unwrap();
        assert_eq!(result["reason"], "exhausted");
        assert!(job.layout_count() > 0);
        let layout = &result["layouts"][0];
        assert_eq!(layout["buildings"][0]["specIndex"], 0);
        assert_eq!(layout["buildings"][0]["footprint"], json!([1, 1]));
        assert_eq!(layout["buildings"][0]["collision"], json!([16, 16, 15, 15]));
        assert!(!layout["allowRoute"].as_array().unwrap().is_empty());
    }

    #[test]
    fn incremental_search_has_owned_snapshot_bounded_steps_and_json_contract() {
        let terrain = analysis();
        let mut job = terrain
            .start_wall_search(&search_query().to_string())
            .unwrap();
        assert!(job.result_json().is_err());
        assert_eq!(job.explored_states(), 0);
        assert_eq!(job.layout_count(), 0);
        for invalid_step in [-1.0, 0.0, 1.5, 17.0, f64::NAN] {
            assert!(job.step(invalid_step).is_err());
        }
        drop(terrain);
        assert!(job.step(1.0).unwrap());
        let result: serde_json::Value = serde_json::from_str(&job.result_json().unwrap()).unwrap();
        assert_eq!(result["model"], "pixel-rect-v1");
        assert_eq!(result["searchAlgorithm"], "bounded-state-v1");
        assert_eq!(result["exploredStates"], 1);
        assert_eq!(result["candidatePlacements"], 0);
        assert_eq!(result["layouts"], json!([]));
        assert!(job.step(1.0).unwrap());
    }

    #[test]
    fn incremental_search_rejects_hostile_json_without_poisoning_snapshot() {
        let terrain = analysis();
        assert!(terrain.start_wall_search("{}").is_err());
        assert!(
            terrain
                .start_wall_search(r#"{"window":[0,0,128,128],"unknown":true}"#)
                .is_err()
        );
        assert!(terrain.start_wall_search(&" ".repeat(32_769)).is_err());
        let mut invalid_limits = search_query();
        invalid_limits["maxStates"] = json!(0);
        assert!(
            terrain
                .start_wall_search(&invalid_limits.to_string())
                .is_err()
        );
        let mut job = terrain
            .start_wall_search(&search_query().to_string())
            .unwrap();
        assert!(job.step(1.0).unwrap());
        assert!(job.result_json().is_ok());
    }

    #[test]
    fn entrance_relative_search_json_filters_centers_and_rejects_invalid_sites() {
        let terrain = analysis();
        let mut query = search_query();
        query["placementArea"] = json!([0, 0, 128, 128]);
        query["placementSite"] = json!({"entrance": [[32, 64], [96, 64]], "maxDistancePixels": 0});
        query["buildings"] = json!([{"footprint": [2, 2], "collision": [16, 16, 15, 15]}]);
        let mut job = terrain.start_wall_search(&query.to_string()).unwrap();
        while !job.step(16.0).unwrap() {}
        let result: serde_json::Value = serde_json::from_str(&job.result_json().unwrap()).unwrap();
        assert_eq!(result["candidatePlacements"], 3);
        // Even with the whole window as the footprint bound, unused space can host test points.
        assert!(
            terrain
                .suggest_wall_endpoints_json(&query.to_string(), "[[32,64],[96,64]]")
                .is_ok()
        );
        query["placementElevation"] = json!(1);
        let mut job = terrain.start_wall_search(&query.to_string()).unwrap();
        assert!(job.step(16.0).unwrap());
        let result: serde_json::Value = serde_json::from_str(&job.result_json().unwrap()).unwrap();
        assert_eq!(result["candidatePlacements"], 0);
        query["placementSite"]["maxDistancePixels"] = json!(1025);
        assert!(terrain.start_wall_search(&query.to_string()).is_err());
        query["placementSite"] = json!({"entrance": [[32, 64], [32, 64]], "maxDistancePixels": 32});
        assert!(terrain.start_wall_search(&query.to_string()).is_err());
        query["placementSite"] =
            json!({"entrance": [[32, 64], [129, 64]], "maxDistancePixels": 32});
        assert!(terrain.start_wall_search(&query.to_string()).is_err());
    }

    #[test]
    fn endpoint_suggestion_json_returns_mover_fit_safe_centers() {
        let terrain = analysis();
        let mut query = search_query();
        query["placementArea"] = json!([32, 32, 96, 96]);
        query["block"] = json!([4, 4, 4, 4]);
        query["buildings"] = json!([{"footprint": [1, 1], "collision": [16, 16, 15, 15]}]);
        let result: serde_json::Value = serde_json::from_str(
            &terrain
                .suggest_wall_endpoints_json(&query.to_string(), "[[64,40],[64,56]]")
                .unwrap(),
        )
        .unwrap();
        let start = result["start"].as_array().unwrap();
        let end = result["end"].as_array().unwrap();
        assert_ne!(start, end);
        for point in [start, end] {
            let x = point[0].as_i64().unwrap();
            let y = point[1].as_i64().unwrap();
            assert!(!(28..100).contains(&x) || !(28..100).contains(&y));
            assert!((4..124).contains(&x) && (4..124).contains(&y));
        }
        query["placementElevation"] = json!(3);
        assert!(
            terrain
                .suggest_wall_endpoints_json(&query.to_string(), "[[64,40],[64,56]]")
                .is_err()
        );
    }
    #[test]
    fn precise_object_rectangles_follow_toggle_without_changing_terrain() {
        let mut a = analysis();
        a.wall_obstacles = vec![rect([63, 0, 65, 128])];
        a.respect_obstacles = true;
        let q = query().to_string();
        let result: serde_json::Value =
            serde_json::from_str(&a.check_wall_json(&q).unwrap()).unwrap();
        assert_eq!(result["passage"], "alreadyDisconnected");
        a.set_obstacles_enabled(false);
        let result: serde_json::Value =
            serde_json::from_str(&a.check_wall_json(&q).unwrap()).unwrap();
        assert_eq!(result["passage"], "open");
        assert_eq!(result["route"][0], json!([8, 64]));
        assert_eq!(a.grid.cells().len(), 256);
    }
    #[test]
    fn rejects_bad_json_and_sizes_without_poisoning_the_analysis() {
        let a = analysis();
        for q in ["{}".to_owned(), " ".repeat(32769), json!({"window":[0,0,128,128],"start":[0,0],"end":[127,127],"mover":[4294967295_u32,0,0,0],"buildings":[]}).to_string()] {
            assert!(a.check_wall_json(&q).is_err());
        }
        assert!(a.check_wall_json(&query().to_string()).is_ok());
    }
    #[test]
    fn custom_building_dimensions_reach_the_solver_and_report_placement_separately() {
        let a = analysis();
        let mut q = query();
        q["buildings"] = json!([{"tile":[1,1],"footprint":[1,1],"collision":[0,48,0,79]}]);
        let result: serde_json::Value =
            serde_json::from_str(&a.check_wall_json(&q.to_string()).unwrap()).unwrap();
        assert_eq!(result["passage"], "blockedWithinWindow");
        assert_eq!(result["baselinePassable"], true);
        assert_eq!(result["staticPlacementValid"], true);
        assert_eq!(
            result["placements"][0]["footprint"],
            json!([32, 32, 64, 64])
        );
        assert_eq!(
            result["placements"][0]["collision"],
            json!([48, 0, 49, 128])
        );
        let duplicate = q["buildings"][0].clone();
        q["buildings"].as_array_mut().unwrap().push(duplicate);
        let result: serde_json::Value =
            serde_json::from_str(&a.check_wall_json(&q.to_string()).unwrap()).unwrap();
        assert_eq!(result["passage"], "blockedWithinWindow");
        assert_eq!(result["staticPlacementValid"], false);
        assert_eq!(
            result["placements"][0]["issues"],
            json!([{"kind":"overlapsBuilding","index":1}])
        );
    }
    #[test]
    fn profiles_use_supplied_dat_bounds_and_placeboxes() {
        let mut data = vec![0_u8; 19876];
        let id = 109;
        data[7032 + id * 4..7036 + id * 4].copy_from_slice(&1_u32.to_le_bytes());
        data[11284 + id * 4..11286 + id * 4].copy_from_slice(&96_i16.to_le_bytes());
        data[11286 + id * 4..11288 + id * 4].copy_from_slice(&64_i16.to_le_bytes());
        for (side, extent) in [38_i16, 22, 37, 21].into_iter().enumerate() {
            data[12580 + id * 8 + side * 2..12582 + id * 8 + side * 2]
                .copy_from_slice(&extent.to_le_bytes());
        }
        let profiles = WallProfiles::from_definitions(&broodmap_formats::parse_units_dat(&data));
        assert_eq!(profiles.buildings.len(), 1);
        assert_eq!(profiles.buildings[0].footprint, [3, 2]);
        assert_eq!(profiles.buildings[0].collision, [38, 22, 37, 21]);
    }
}
