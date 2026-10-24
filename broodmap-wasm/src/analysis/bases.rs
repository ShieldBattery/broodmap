//! Serialized base queries over the shared melee adapter. The spatial solver stays game-agnostic.

use broodmap_analysis::{
    BaseDiscovery, BaseSearchOptions, DepotFootprint, ResourceKind, ResourceNode, StaticObstacle,
    discover_bases,
};
use wasm_bindgen::prelude::*;

use super::TerrainAnalysis;

#[derive(serde::Deserialize, Copy, Clone, Eq, PartialEq)]
#[serde(default, deny_unknown_fields, rename_all = "camelCase")]
struct Options {
    depot_width_tiles: u32,
    depot_height_tiles: u32,
    max_mineral_blocker_amount: Option<u32>,
    allow_destructible_clearing: bool,
    allow_start_obstacle_clearing: bool,
}

impl Default for Options {
    fn default() -> Self {
        let depot = DepotFootprint::default();
        Self {
            depot_width_tiles: depot.width_tiles,
            depot_height_tiles: depot.height_tiles,
            max_mineral_blocker_amount: Some(8),
            allow_destructible_clearing: true,
            allow_start_obstacle_clearing: true,
        }
    }
}

pub(super) struct BaseCatalog {
    options: Options,
    discovery: BaseDiscovery,
    json: String,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct CatalogJson {
    depot_width_tiles: u32,
    depot_height_tiles: u32,
    unplaced_clusters: usize,
    ignored_resources: usize,
    bases: Vec<BaseJson>,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct BaseJson {
    id: usize,
    depot_tile: [u32; 2],
    route_anchor: Option<[u32; 2]>,
    mineral_count: usize,
    gas_count: usize,
    start_players: Vec<u8>,
    resources: Vec<ResourceJson>,
    required_minerals: Vec<ResourceJson>,
    required_obstacles: Vec<[i32; 4]>,
    start_cleared_obstacles: Vec<[i32; 4]>,
}

#[derive(serde::Serialize)]
struct ResourceJson {
    bounds: [i32; 4],
    kind: &'static str,
    amount: Option<u32>,
}

fn obstacle_json(obstacle: StaticObstacle) -> [i32; 4] {
    let r = obstacle.bounds;
    [r.left, r.top, r.right, r.bottom]
}

fn resource_json(resource: ResourceNode) -> ResourceJson {
    let bounds = resource.bounds;
    ResourceJson {
        bounds: [bounds.left, bounds.top, bounds.right, bounds.bottom],
        kind: match resource.kind {
            ResourceKind::Mineral => "mineral",
            ResourceKind::Gas => "gas",
        },
        amount: resource.amount,
    }
}

#[wasm_bindgen]
impl TerrainAnalysis {
    /// Discovers and caches resource-base candidates in a snapshot made by `analyzeMap`.
    /// Options: `{depotWidthTiles:4,depotHeightTiles:3,maxMineralBlockerAmount:8}`.
    /// Set maxMineralBlockerAmount to null to disallow clearing; unknown amounts never qualify.
    /// allowDestructibleClearing and allowStartObstacleClearing default to true.
    /// requiredObstacles lists overlapping buildings requiring destruction; startClearedObstacles
    /// lists overlaps removed automatically if that exact start is occupied (including invincible ones).
    /// Candidate requiredMinerals lists direct placement prerequisites. All objects remain
    /// blockers for current route queries; routeAnchor is null if the whole depot area is blocked.
    /// IDs are zero-based in top-to-bottom, left-to-right depot order. Discovery always uses
    /// the initial static obstacles, independently of the route toggle. This is a heuristic:
    /// stock resource IDs, no creep/power or complete placement rules, and no main/natural roles.
    /// Unknown options and unsupported inputs fail without replacing the last successful catalog.
    #[wasm_bindgen(js_name = findBasesJson)]
    pub fn find_bases_json(&mut self, options_json: &str) -> Result<String, String> {
        let options: Options =
            serde_json::from_str(options_json).map_err(|e| format!("base options: {e}"))?;
        if let Some(catalog) = &self.base_catalog
            && catalog.options == options
        {
            return Ok(catalog.json.clone());
        }
        let inputs = self
            .base_inputs
            .as_ref()
            .ok_or("Base discovery needs map objects: create this snapshot with analyzeMap.")?;
        let discovery = discover_bases(
            &self.grid,
            &inputs.resources,
            &inputs.obstacles,
            &inputs.start_positions(),
            &BaseSearchOptions {
                depot: DepotFootprint {
                    width_tiles: options.depot_width_tiles,
                    height_tiles: options.depot_height_tiles,
                },
                max_mineral_blocker_amount: options.max_mineral_blocker_amount,
                allow_destructible_clearing: options.allow_destructible_clearing,
                allow_start_obstacle_clearing: options.allow_start_obstacle_clearing,
            },
        )
        .map_err(|e| format!("base discovery: {e}"))?;
        let bases = discovery
            .bases
            .iter()
            .enumerate()
            .map(|(id, base)| {
                let resources: Vec<_> = base
                    .resource_indices
                    .iter()
                    .map(|&index| resource_json(inputs.resources[index]))
                    .collect();
                let mineral_count = resources
                    .iter()
                    .filter(|resource| resource.kind == "mineral")
                    .count();
                let mut start_players: Vec<_> = base
                    .start_indices
                    .iter()
                    .filter_map(|&index| {
                        let owner = inputs.starts[index].owner?;
                        (owner < 8).then(|| owner + 1)
                    })
                    .collect();
                start_players.sort_unstable();
                start_players.dedup();
                BaseJson {
                    id,
                    depot_tile: [base.depot_tile.x, base.depot_tile.y],
                    route_anchor: base.route_anchor.map(|anchor| [anchor.x, anchor.y]),
                    mineral_count,
                    gas_count: resources.len() - mineral_count,
                    start_players,
                    resources,
                    required_obstacles: base
                        .required_obstacle_indices
                        .iter()
                        .map(|&i| obstacle_json(inputs.obstacles[i]))
                        .collect(),
                    start_cleared_obstacles: base
                        .start_cleared_obstacle_indices
                        .iter()
                        .map(|&i| obstacle_json(inputs.obstacles[i]))
                        .collect(),
                    required_minerals: base
                        .required_mineral_indices
                        .iter()
                        .map(|&index| resource_json(inputs.resources[index]))
                        .collect(),
                }
            })
            .collect();
        let json = serde_json::to_string(&CatalogJson {
            depot_width_tiles: options.depot_width_tiles,
            depot_height_tiles: options.depot_height_tiles,
            unplaced_clusters: discovery.unplaced_clusters,
            ignored_resources: discovery.ignored_resources + inputs.ignored_resources,
            bases,
        })
        .map_err(|e| format!("base serialization: {e}"))?;
        self.base_catalog = Some(BaseCatalog {
            options,
            discovery,
            json: json.clone(),
        });
        Ok(json)
    }

    /// Ground and air distances between the selected candidates' near-center walk anchors.
    /// Respects the current obstacle toggle. Hypothetical depots are not added as obstacles.
    /// Disconnected bases return null ground distance; undiscovered/out-of-range IDs are errors.
    #[wasm_bindgen(js_name = baseRouteJson)]
    pub fn base_route_json(&self, start_id: usize, end_id: usize) -> Result<String, String> {
        let catalog = self
            .base_catalog
            .as_ref()
            .ok_or("Find bases before comparing them.")?;
        let start = catalog
            .discovery
            .bases
            .get(start_id)
            .ok_or("Unknown start base ID.")?
            .route_anchor.ok_or("Start base footprint is blocked; its clearing prerequisites must be resolved before routing.")?;
        let end = catalog
            .discovery
            .bases
            .get(end_id)
            .ok_or("Unknown destination base ID.")?
            .route_anchor.ok_or("Destination base footprint is blocked; its clearing prerequisites must be resolved before routing.")?;
        self.route_json(start.x, start.y, end.x, end.y)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::MapRenderer;
    use broodmap_analysis::{MeleeBaseInputs, PixelPosition, PixelRect, StartLocation};

    fn definitions() -> Vec<u8> {
        let mut bytes = vec![0; 19876];
        for id in [176, 177, 178, 188] {
            bytes[7032 + id * 4..7036 + id * 4].copy_from_slice(&0x2000u32.to_le_bytes());
            for side in 0..4 {
                let offset = 12580 + id * 8 + side * 2;
                bytes[offset..offset + 2].copy_from_slice(&15i16.to_le_bytes());
            }
        }
        bytes
    }

    fn start_players(map: MapRenderer) -> Vec<serde_json::Value> {
        let cv5 = vec![0; 52 * 2048];
        let vf4: Vec<u8> = (0..16).flat_map(|_| 1u16.to_le_bytes()).collect();
        let mut analysis = map.analyze_map(&cv5, &vf4, &definitions()).unwrap();
        let catalog: serde_json::Value =
            serde_json::from_str(&analysis.find_bases_json("{}").unwrap()).unwrap();
        let bases = catalog["bases"].as_array().unwrap();
        assert!(!bases.is_empty());
        bases
            .iter()
            .flat_map(|base| base["startPlayers"].as_array().unwrap().clone())
            .collect()
    }

    #[test]
    fn out_of_range_start_owners_are_omitted_from_start_players() {
        let lt = include_bytes!("../../../broodmap/assets/lt.chk");
        let parse = |bytes: Vec<u8>| MapRenderer {
            chk: broodmap::chk::Chk::from_bytes(bytes, None).unwrap(),
            source: broodmap_render::MemorySource::new(),
            known: Default::default(),
        };
        assert!(!start_players(parse(lt.to_vec())).is_empty());

        let mut bytes = lt.to_vec();
        let mut offset = 0;
        let mut patched = 0;
        while offset + 8 <= bytes.len() {
            let len =
                u32::from_le_bytes(bytes[offset + 4..offset + 8].try_into().unwrap()) as usize;
            let body = offset + 8;
            if &bytes[offset..offset + 4] == b"UNIT" {
                for record in bytes[body..body + len].as_chunks_mut::<36>().0 {
                    if u16::from_le_bytes([record[8], record[9]]) == 214 {
                        record[16] = 255;
                        patched += 1;
                    }
                }
            }
            offset = body + len;
        }
        assert!(patched > 0);
        assert!(start_players(parse(bytes)).is_empty());
    }

    #[test]
    fn discovery_json_cache_and_routes_are_snapshot_owned() {
        let map =
            MapRenderer::new(include_bytes!("../../../broodmap/assets/lt.scm"), None).unwrap();
        let cv5 = vec![0; 52 * 2048];
        let vf4: Vec<u8> = (0..16).flat_map(|_| 1u16.to_le_bytes()).collect();
        let mut terrain_only = map.analyze_terrain(&cv5, &vf4).unwrap();
        assert!(
            terrain_only
                .find_bases_json("{}")
                .unwrap_err()
                .contains("analyzeMap")
        );
        let mut analysis = map.analyze_map(&cv5, &vf4, &definitions()).unwrap();
        drop(map);
        assert!(analysis.base_route_json(0, 0).is_err());
        let catalog = analysis.find_bases_json("{}").unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&catalog).unwrap();
        assert!(!parsed["bases"].as_array().unwrap().is_empty());
        let same: serde_json::Value =
            serde_json::from_str(&analysis.base_route_json(0, 0).unwrap()).unwrap();
        assert_eq!(same["groundDistancePixels"], 0.0);
        assert!(analysis.base_route_json(usize::MAX, 0).is_err());
        assert!(
            analysis
                .find_bases_json(r#"{"depotWidthTiles":0}"#)
                .is_err()
        );
        assert!(analysis.find_bases_json(r#"{"unknown":1}"#).is_err());
        analysis.set_obstacles_enabled(false);
        assert_eq!(analysis.find_bases_json("{}").unwrap(), catalog);
        assert!(analysis.base_route_json(0, 0).is_ok());
    }
    #[test]
    fn conditional_catalog_does_not_remove_minerals_from_current_routes() {
        use broodmap_analysis::{TerrainCell, TerrainGrid};
        let grid = TerrainGrid::from_cells(
            96,
            80,
            vec![
                TerrainCell {
                    walkable: true,
                    terrain_buildable: true,
                    elevation: 0,
                    ramp: false,
                };
                96 * 80
            ],
        )
        .unwrap();
        let mut resources: Vec<_> = [(176, 112), (208, 112), (176, 144), (208, 144)]
            .into_iter()
            .map(|(left, top)| ResourceNode {
                bounds: PixelRect {
                    left,
                    top,
                    right: left + 16,
                    bottom: top + 16,
                },
                kind: ResourceKind::Mineral,
                amount: Some(1500),
            })
            .collect();
        resources.push(ResourceNode {
            bounds: PixelRect {
                left: 376,
                top: 296,
                right: 392,
                bottom: 312,
            },
            kind: ResourceKind::Mineral,
            amount: Some(1),
        });
        let all_bounds: Vec<_> = resources.iter().map(|node| node.bounds).collect();
        let obstructed = grid.with_obstacles(&all_bounds);
        let mut analysis = TerrainAnalysis::terrain_only(grid);
        analysis.obstructed = Some(obstructed);
        analysis.respect_obstacles = true;
        analysis.obstacle_count = 5;
        analysis.base_inputs = Some(MeleeBaseInputs {
            resources,
            starts: vec![StartLocation {
                position: PixelPosition { x: 384, y: 304 },
                owner: Some(0),
            }],
            obstacles: Vec::new(),
            ignored_resources: 0,
        });
        let flags = analysis.cell_flags();
        let catalog: serde_json::Value =
            serde_json::from_str(&analysis.find_bases_json("{}").unwrap()).unwrap();
        assert_eq!(catalog["bases"][0]["depotTile"], serde_json::json!([10, 8]));
        assert_eq!(catalog["bases"][0]["mineralCount"], 4);
        assert_eq!(catalog["bases"][0]["requiredMinerals"][0]["amount"], 1);
        let anchor = catalog["bases"][0]["routeAnchor"].as_array().unwrap();
        let x = anchor[0].as_u64().unwrap() as usize;
        let y = anchor[1].as_u64().unwrap() as usize;
        assert_eq!(flags[y * 96 + x] & 1, 1);
        assert_eq!(analysis.cell_flags(), flags);
        assert!(analysis.route_json(48, 38, 48, 38).is_err()); // tiny mineral remains physically present
        assert!(analysis.base_route_json(0, 0).is_ok());
        let strict: serde_json::Value = serde_json::from_str(
            &analysis
                .find_bases_json(r#"{"maxMineralBlockerAmount":null}"#)
                .unwrap(),
        )
        .unwrap();
        assert!(
            strict["bases"]
                .as_array()
                .unwrap()
                .iter()
                .all(|base| base["requiredMinerals"].as_array().unwrap().is_empty())
        );
        assert!(
            strict["bases"]
                .as_array()
                .unwrap()
                .iter()
                .all(|base| base["depotTile"] != serde_json::json!([10, 8]))
        );
        assert_eq!(analysis.cell_flags(), flags);
    }
    #[test]
    fn building_clearance_is_conditional_and_preserves_current_routes() {
        use broodmap_analysis::{TerrainCell, TerrainGrid};
        let grid = TerrainGrid::from_cells(
            96,
            80,
            vec![
                TerrainCell {
                    walkable: true,
                    terrain_buildable: true,
                    elevation: 0,
                    ramp: false,
                };
                96 * 80
            ],
        )
        .unwrap();
        let resources: Vec<_> = [(176, 112), (208, 112), (176, 144), (208, 144)]
            .into_iter()
            .map(|(left, top)| ResourceNode {
                bounds: PixelRect {
                    left,
                    top,
                    right: left + 16,
                    bottom: top + 16,
                },
                kind: ResourceKind::Mineral,
                amount: Some(1500),
            })
            .collect();
        let building = StaticObstacle {
            bounds: PixelRect {
                left: 320,
                top: 256,
                right: 448,
                bottom: 352,
            },
            destructible: false,
        };
        let mut rectangles: Vec<_> = resources.iter().map(|r| r.bounds).collect();
        rectangles.push(building.bounds);
        let obstructed = grid.with_obstacles(&rectangles);
        let mut analysis = TerrainAnalysis::terrain_only(grid);
        analysis.obstructed = Some(obstructed);
        analysis.respect_obstacles = true;
        analysis.obstacle_count = 5;
        analysis.base_inputs = Some(MeleeBaseInputs {
            resources,
            starts: vec![StartLocation {
                position: PixelPosition { x: 384, y: 304 },
                owner: Some(0),
            }],
            obstacles: vec![building],
            ignored_resources: 0,
        });
        let before = analysis.cell_flags();
        let catalog: serde_json::Value =
            serde_json::from_str(&analysis.find_bases_json("{}").unwrap()).unwrap();
        let base = &catalog["bases"][0];
        assert_eq!(base["depotTile"], serde_json::json!([10, 8]));
        assert_eq!(base["startPlayers"], serde_json::json!([1]));
        assert_eq!(
            base["startClearedObstacles"],
            serde_json::json!([[320, 256, 448, 352]])
        );
        assert_eq!(base["requiredObstacles"], serde_json::json!([]));
        assert!(base["routeAnchor"].is_null());
        assert!(analysis.base_route_json(0, 0).is_err());
        assert_eq!(analysis.cell_flags(), before);
        let strict: serde_json::Value = serde_json::from_str(
            &analysis
                .find_bases_json(r#"{"allowStartObstacleClearing":false}"#)
                .unwrap(),
        )
        .unwrap();
        assert!(
            strict["bases"]
                .as_array()
                .unwrap()
                .iter()
                .all(|base| base["depotTile"] != serde_json::json!([10, 8]))
        );

        analysis.base_inputs.as_mut().unwrap().obstacles[0].destructible = true;
        analysis.base_catalog = None;
        let catalog: serde_json::Value = serde_json::from_str(
            &analysis
                .find_bases_json(r#"{"allowStartObstacleClearing":false}"#)
                .unwrap(),
        )
        .unwrap();
        assert_eq!(catalog["bases"][0]["depotTile"], serde_json::json!([10, 8]));
        assert_eq!(
            catalog["bases"][0]["requiredObstacles"],
            serde_json::json!([[320, 256, 448, 352]])
        );
        assert_eq!(
            catalog["bases"][0]["startClearedObstacles"],
            serde_json::json!([])
        );
        assert_eq!(analysis.cell_flags(), before);
        let strict: serde_json::Value = serde_json::from_str(
            &analysis
                .find_bases_json(
                    r#"{"allowStartObstacleClearing":false,"allowDestructibleClearing":false}"#,
                )
                .unwrap(),
        )
        .unwrap();
        assert!(
            strict["bases"]
                .as_array()
                .unwrap()
                .iter()
                .all(|base| base["depotTile"] != serde_json::json!([10, 8]))
        );
    }
}
