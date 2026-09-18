//! Compact, graphics-independent bindings for the terrain analysis experiment.

use broodmap::chk::{placed_units::PlacedUnitsError, sprites::SpriteError};
use broodmap_analysis::{TerrainGrid, WalkPosition, melee_obstacles};
use broodmap_formats::{parse_cv5, parse_units_dat, parse_vf4};
use broodmap_render::{AssetRequest, DatKind};
use wasm_bindgen::prelude::*;

use crate::MapRenderer;

mod bases;

#[wasm_bindgen]
impl MapRenderer {
    /// The CV5 and VF4 paths, in that order. Fetch these and pass their bytes to
    /// `analyzeTerrain`; they do not need to be registered with `addAsset`.
    #[wasm_bindgen(js_name = requiredAnalysisAssets)]
    pub fn required_analysis_assets(&self) -> Vec<String> {
        vec![
            AssetRequest::Cv5(self.chk.tileset()).casc_path(),
            AssetRequest::Vf4(self.chk.tileset()).casc_path(),
        ]
    }

    /// CV5, VF4, and units.dat, in that order, for `analyzeMap`.
    #[wasm_bindgen(js_name = requiredMapAnalysisAssets)]
    pub fn required_map_analysis_assets(&self) -> Vec<String> {
        let mut paths = self.required_analysis_assets();
        paths.push(AssetRequest::Dat(DatKind::Units).casc_path());
        paths
    }

    /// Resolves terrain and initial static melee obstacles from UNIT/THG2.
    /// Caller-supplied units.dat defines collision bounds; no artwork is needed.
    /// Neutral buildings and resources block routes by default. This is a conservative
    /// 8-pixel approximation, not a simulation of moving units or destruction. Missing UNIT
    /// or THG2 chunks contribute no objects. Their parsers are permissive: incomplete trailing
    /// records are ignored, and obstacleCount is not a map-integrity check.
    #[wasm_bindgen(js_name = analyzeMap)]
    pub fn analyze_map(
        &self,
        cv5: &[u8],
        vf4: &[u8],
        units: &[u8],
    ) -> Result<TerrainAnalysis, String> {
        if units.len() != 19876 {
            return Err("units.dat must contain exactly 19876 bytes (228 unit definitions)".into());
        }
        let mut analysis = self.analyze_terrain(cv5, vf4)?;
        let definitions = parse_units_dat(units);
        // Missing optional object chunks are empty. Match explicitly so future parser error
        // variants cannot silently become obstacle-free maps.
        let units = match self.chk.placed_units() {
            Ok(units) => units.as_slice(),
            Err(PlacedUnitsError::ChunkMissing) => &[],
        };
        let sprites = match self.chk.sprites() {
            Ok(sprites) => sprites.as_slice(),
            Err(SpriteError::ChunkMissing) => &[],
        };
        let obstacles = melee_obstacles(units, sprites, &definitions);
        analysis.obstructed = Some(analysis.grid.with_obstacles(&obstacles));
        analysis.obstacle_count = obstacles.len();
        analysis.respect_obstacles = true;
        analysis.base_inputs = Some(bases::BaseInputs::new(units, sprites, &definitions));
        Ok(analysis)
    }

    /// Resolves terrain without loading any artwork or unit data. The returned snapshot
    /// owns its grid and remains valid independently of this renderer and its asset cache.
    /// Missing tile references and incomplete table records are errors: analysis must not
    /// silently turn absent terrain metadata into traversable ground.
    #[wasm_bindgen(js_name = analyzeTerrain)]
    pub fn analyze_terrain(&self, cv5: &[u8], vf4: &[u8]) -> Result<TerrainAnalysis, String> {
        if cv5.is_empty() || cv5.len() > 2048 * 52 || !cv5.len().is_multiple_of(52) {
            return Err("CV5 must contain 1..2048 complete 52-byte tile-group records".into());
        }
        if vf4.is_empty() || vf4.len() > 65536 * 32 || !vf4.len().is_multiple_of(32) {
            return Err("VF4 must contain 1..65536 complete 32-byte minitile-flag records".into());
        }
        let terrain = self
            .chk
            .terrain()
            .map_err(|e| format!("terrain unavailable: {e}"))?;
        let grid = TerrainGrid::from_terrain(terrain, &parse_cv5(cv5), &parse_vf4(vf4))
            .map_err(|e| format!("terrain analysis failed: {e}"))?;
        Ok(TerrainAnalysis {
            grid,
            obstructed: None,
            respect_obstacles: false,
            obstacle_count: 0,
            base_inputs: None,
            base_catalog: None,
        })
    }
}

/// An 8-pixel point-movement snapshot, optionally including initial static melee obstacles.
/// Moving units and mover size are not represented. This does not certify engine pathing or walls.
#[wasm_bindgen]
pub struct TerrainAnalysis {
    grid: TerrainGrid,
    obstructed: Option<TerrainGrid>,
    respect_obstacles: bool,
    obstacle_count: usize,
    base_inputs: Option<bases::BaseInputs>,
    base_catalog: Option<bases::BaseCatalog>,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct RouteResult {
    points: Vec<[u32; 2]>,
    ground_distance_pixels: Option<f64>,
    air_distance_pixels: f64,
}

#[wasm_bindgen]
impl TerrainAnalysis {
    /// Number of static obstacle rectangles identified in this snapshot.
    #[wasm_bindgen(getter, js_name = obstacleCount)]
    pub fn obstacle_count(&self) -> usize {
        self.obstacle_count
    }

    /// Choose object-aware routing or the original terrain-only model. Turning this off
    /// ignores every obstacle, including invincible buildings; it does not simulate removal.
    #[wasm_bindgen(js_name = setObstaclesEnabled)]
    pub fn set_obstacles_enabled(&mut self, enabled: bool) {
        self.respect_obstacles = enabled;
    }

    #[wasm_bindgen(getter, js_name = widthWalkTiles)]
    pub fn width_walk_tiles(&self) -> u32 {
        self.grid.width()
    }

    #[wasm_bindgen(getter, js_name = heightWalkTiles)]
    pub fn height_walk_tiles(&self) -> u32 {
        self.grid.height()
    }

    /// Row-major bytes: bit 0 walkable, bit 1 CV5 terrain-buildability hint,
    /// bit 2 ramp, bits 3..4 minitile elevation (0..2), bit 5 terrain cell obstructed by a
    /// static map object (retained even when objects are ignored). Bit 0 respects the current
    /// obstacle toggle. Buildability excludes occupancy,
    /// footprint, creep/power, resources, and other placement restrictions.
    #[wasm_bindgen(js_name = cellFlags)]
    pub fn cell_flags(&self) -> Vec<u8> {
        let active = self.active_grid();
        self.grid
            .cells()
            .iter()
            .enumerate()
            .map(|(index, cell)| {
                let obstructed = cell.walkable
                    && self
                        .obstructed
                        .as_ref()
                        .is_some_and(|grid| !grid.cells()[index].walkable);
                u8::from(active.cells()[index].walkable)
                    | (u8::from(cell.terrain_buildable) << 1)
                    | (u8::from(cell.ramp) << 2)
                    | (cell.elevation << 3)
                    | (u8::from(obstructed) << 5)
            })
            .collect()
    }

    /// Routes between 8-pixel cell centers. Both endpoints must be walkable; they are
    /// never silently moved. Points include both endpoints. Disconnected endpoints return
    /// an empty point list and null groundDistancePixels. Distances use logical pixels,
    /// independent of preview resolution. Ground costs are 8 / 11.312 pixels per step
    /// (fixed-point octile approximation), with no diagonal corner cutting.
    #[wasm_bindgen(js_name = routeJson)]
    pub fn route_json(
        &self,
        start_x: u32,
        start_y: u32,
        end_x: u32,
        end_y: u32,
    ) -> Result<String, String> {
        let start = WalkPosition {
            x: start_x,
            y: start_y,
        };
        let end = WalkPosition { x: end_x, y: end_y };
        let route = self
            .active_grid()
            .route(start, end)
            .map_err(|e| e.to_string())?;
        let dx = f64::from(start_x) - f64::from(end_x);
        let dy = f64::from(start_y) - f64::from(end_y);
        let result = RouteResult {
            air_distance_pixels: dx.hypot(dy) * 8.0,
            ground_distance_pixels: route.as_ref().map(|r| r.distance_pixels),
            points: route
                .map(|r| r.points.into_iter().map(|p| [p.x, p.y]).collect())
                .unwrap_or_default(),
        };
        serde_json::to_string(&result).map_err(|e| format!("route serialization: {e}"))
    }
}

impl TerrainAnalysis {
    fn active_grid(&self) -> &TerrainGrid {
        if self.respect_obstacles {
            self.obstructed.as_ref().unwrap_or(&self.grid)
        } else {
            &self.grid
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use broodmap_analysis::TerrainCell;

    #[test]
    fn real_map_can_analyze_without_render_assets() {
        let map = MapRenderer::new(include_bytes!("../../broodmap/assets/lt.scm")).unwrap();
        assert_eq!(
            map.required_analysis_assets(),
            ["TileSet/jungle.cv5", "TileSet/jungle.vf4"]
        );
        // Synthetic metadata for every possible group; every tile maps to megatile zero.
        let cv5 = vec![0; 52 * 2048];
        let vf4: Vec<u8> = (0..16).flat_map(|_| 1u16.to_le_bytes()).collect();
        let analysis = map.analyze_terrain(&cv5, &vf4).unwrap();
        assert_eq!(
            (analysis.width_walk_tiles(), analysis.height_walk_tiles()),
            (512, 512)
        );
        assert_eq!(analysis.cell_flags(), vec![3; 512 * 512]);
        assert!(map.analyze_terrain(&[], &vf4).is_err());
        assert!(map.analyze_terrain(&vec![0; 52 * 2049], &vf4).is_err());
        assert!(map.analyze_terrain(&cv5, &vec![0; 32 * 65537]).is_err());
        assert!(map.analyze_terrain(&cv5, &vf4[..31]).is_err());
        assert!(map.analyze_terrain(&cv5[..52], &vf4).is_err());
    }

    #[test]
    fn map_obstacles_block_routes_and_toggle_without_losing_terrain() {
        let map = MapRenderer::new(include_bytes!("../../broodmap/assets/lt.scm")).unwrap();
        assert_eq!(
            map.required_map_analysis_assets(),
            ["TileSet/jungle.cv5", "TileSet/jungle.vf4", "arr/units.dat"]
        );
        let cv5 = vec![0; 52 * 2048];
        let vf4: Vec<u8> = (0..16).flat_map(|_| 1u16.to_le_bytes()).collect();
        let mut units = vec![0; 19876];
        for id in 176..=178 {
            units[7032 + id * 4..7036 + id * 4].copy_from_slice(&0x2000u32.to_le_bytes());
            for side in 0..4 {
                let offset = 12580 + id * 8 + side * 2;
                units[offset..offset + 2].copy_from_slice(&16i16.to_le_bytes());
            }
        }
        let mineral = map
            .chk
            .placed_units()
            .unwrap()
            .iter()
            .find(|unit| {
                (176..=178).contains(&unit.unit_id) && unit.owner.is_none_or(|owner| owner >= 8)
            })
            .unwrap();
        let x = u32::from(mineral.x) / 8;
        let y = u32::from(mineral.y) / 8;
        let mut analysis = map.analyze_map(&cv5, &vf4, &units).unwrap();
        assert!(analysis.obstacle_count() > 0);
        let index = (y * analysis.width_walk_tiles() + x) as usize;
        let flags = analysis.cell_flags();
        assert_eq!(flags[index] & 33, 32);
        assert!(analysis.route_json(x, y, x, y).is_err());
        analysis.set_obstacles_enabled(false);
        assert_eq!(analysis.cell_flags()[index] & 33, 33);
        let route: serde_json::Value =
            serde_json::from_str(&analysis.route_json(x, y, x, y).unwrap()).unwrap();
        assert_eq!(route["groundDistancePixels"], 0.0);
        analysis.set_obstacles_enabled(true);
        assert_eq!(analysis.cell_flags(), flags);
        assert!(map.analyze_map(&cv5, &vf4, &units[..19875]).is_err());
        units.push(0);
        assert!(map.analyze_map(&cv5, &vf4, &units).is_err());
    }

    #[test]
    fn route_json_distinguishes_unreachable_invalid_and_zero_distance() {
        let cell = TerrainCell {
            walkable: true,
            terrain_buildable: true,
            elevation: 2,
            ramp: true,
        };
        let mut cells = vec![cell; 3];
        cells[1].walkable = false;
        let analysis = TerrainAnalysis {
            grid: TerrainGrid::from_cells(3, 1, cells).unwrap(),
            obstructed: None,
            respect_obstacles: false,
            obstacle_count: 0,
            base_inputs: None,
            base_catalog: None,
        };
        assert_eq!(analysis.cell_flags(), [23, 22, 23]);
        let unreachable: serde_json::Value =
            serde_json::from_str(&analysis.route_json(0, 0, 2, 0).unwrap()).unwrap();
        assert!(unreachable["groundDistancePixels"].is_null());
        assert_eq!(unreachable["airDistancePixels"], 16.0);
        assert_eq!(unreachable["points"], serde_json::json!([]));
        let same: serde_json::Value =
            serde_json::from_str(&analysis.route_json(0, 0, 0, 0).unwrap()).unwrap();
        assert_eq!(same["groundDistancePixels"], 0.0);
        assert_eq!(same["points"], serde_json::json!([[0, 0]]));
        assert!(analysis.route_json(0, 0, 1, 0).is_err());
        assert!(analysis.route_json(0, 0, u32::MAX, 0).is_err());
    }
}
