//! Terrain-only ramp evidence, independent of route-local widening.
use broodmap_analysis::{Ramp, RampEnd};
use wasm_bindgen::prelude::*;

use super::TerrainAnalysis;

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct RampResult {
    terrain_only: bool,
    ramps: Vec<RampJson>,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct RampJson {
    id: u32,
    lower_elevation: u8,
    upper_elevation: u8,
    cell_count: u32,
    lower: RampEndJson,
    upper: RampEndJson,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct RampEndJson {
    position: [u32; 2],
    endpoints: [[u32; 2]; 2],
    width_pixels: f64,
}

impl From<&RampEnd> for RampEndJson {
    fn from(end: &RampEnd) -> Self {
        Self {
            position: [end.position.x, end.position.y],
            endpoints: end.endpoints.map(|point| [point.x, point.y]),
            width_pixels: end.width_pixels,
        }
    }
}

impl From<&Ramp> for RampJson {
    fn from(ramp: &Ramp) -> Self {
        Self {
            id: ramp.id,
            lower_elevation: ramp.lower_elevation,
            upper_elevation: ramp.upper_elevation,
            cell_count: ramp.cell_count,
            lower: (&ramp.lower).into(),
            upper: (&ramp.upper).into(),
        }
    }
}

#[wasm_bindgen]
impl TerrainAnalysis {
    /// Two wall-contact spans at the ends of each recognized terrain ramp.
    ///
    /// Always uses the original terrain, independent of obstacle mode. Positions are walk cells;
    /// span endpoints and widths are logical pixels. These are elevation/ramp-flag observations,
    /// not route-local widening claims or certified movement/building-placement boundaries.
    #[wasm_bindgen(js_name = rampsJson)]
    pub fn ramps_json(&self) -> Result<String, String> {
        let ramps = self.ramps.get_or_init(|| self.grid.ramps());
        serde_json::to_string(&RampResult {
            terrain_only: true,
            ramps: ramps.iter().map(RampJson::from).collect(),
        })
        .map_err(|error| format!("ramp serialization failed: {error}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use broodmap_analysis::{PixelRect, TerrainCell, TerrainGrid};

    #[test]
    fn ramp_json_has_two_pixel_spans_and_ignores_obstacle_mode() {
        let cells = (0..40)
            .flat_map(|y| {
                (0..40).map(move |x| TerrainCell {
                    walkable: (16..=24).contains(&x) && (2..=38).contains(&y),
                    terrain_buildable: true,
                    elevation: u8::from(y >= 20),
                    ramp: (16..=23).contains(&y),
                })
            })
            .collect();
        let grid = TerrainGrid::from_cells(40, 40, cells).unwrap();
        let obstructed = grid.with_obstacles(&[PixelRect {
            left: 128,
            top: 128,
            right: 200,
            bottom: 192,
        }]);
        let mut analysis = TerrainAnalysis::terrain_only(grid);
        analysis.obstructed = Some(obstructed);
        analysis.set_obstacles_enabled(true);
        let json = analysis.ramps_json().unwrap();
        let value: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(value["terrainOnly"], true);
        let ramps = value["ramps"].as_array().unwrap();
        assert_eq!(ramps.len(), 1);
        let ramp = &ramps[0];
        assert_eq!(ramp["lowerElevation"], 0);
        assert_eq!(ramp["upperElevation"], 1);
        assert_eq!(ramp["cellCount"], 72);
        for end in ["lower", "upper"] {
            assert_eq!(ramp[end]["widthPixels"], 72.0);
            assert!(ramp[end]["endpoints"][0][0].as_u64().unwrap() >= 128);
            assert!(ramp[end]["position"][0].as_u64().unwrap() < 40);
        }
        assert_ne!(ramp["lower"]["endpoints"], ramp["upper"]["endpoints"]);
        assert!(analysis.ramps.get().is_some());
        analysis.set_obstacles_enabled(false);
        assert_eq!(analysis.ramps_json().unwrap(), json);
    }

    #[test]
    fn flat_terrain_has_no_ramp_evidence() {
        let grid = TerrainGrid::from_cells(
            1,
            1,
            vec![TerrainCell {
                walkable: true,
                terrain_buildable: true,
                elevation: 0,
                ramp: false,
            }],
        )
        .unwrap();
        let analysis = TerrainAnalysis::terrain_only(grid);
        let value: serde_json::Value =
            serde_json::from_str(&analysis.ramps_json().unwrap()).unwrap();
        assert_eq!(value["ramps"], serde_json::json!([]));
    }
}
