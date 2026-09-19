//! Owned WASM snapshots for clearance-derived terrain regions.

use broodmap_analysis::{RegionAnalysis, RegionOptions};
use wasm_bindgen::prelude::*;

use super::TerrainAnalysis;

/// An immutable region result detached from its source [`TerrainAnalysis`] snapshot.
///
/// Labels are row-major 8-pixel walk cells. Zero denotes blocked terrain; positive labels are
/// one-based region IDs. Passages describe one representative connection per adjacent region pair.
#[wasm_bindgen]
pub struct RegionSnapshot {
    analysis: RegionAnalysis,
    min_prominence_pixels: u16,
    min_relative_prominence_percent: u8,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct RegionMetadata {
    min_prominence_pixels: u16,
    min_relative_prominence_percent: u8,
    regions: Vec<RegionMetadataEntry>,
    passages: Vec<PassageMetadataEntry>,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct RegionMetadataEntry {
    id: u32,
    peak: [u32; 2],
    peak_clearance_pixels: u16,
    cell_count: u32,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct PassageMetadataEntry {
    regions: [u32; 2],
    endpoints: [[u32; 2]; 2],
    clearance_radius_pixels: u16,
}

#[wasm_bindgen]
impl RegionSnapshot {
    /// Returns an independently owned row-major `Uint32Array` of region labels.
    ///
    /// Zero is blocked terrain; region IDs begin at one and correspond to `metadataJson` entries.
    #[wasm_bindgen(js_name = labels)]
    pub fn labels(&self) -> Vec<u32> {
        self.analysis.labels().to_vec()
    }

    /// Serializes compact region and passage metadata, without duplicating the full label buffer.
    #[wasm_bindgen(js_name = metadataJson)]
    pub fn metadata_json(&self) -> Result<String, String> {
        let metadata = RegionMetadata {
            min_prominence_pixels: self.min_prominence_pixels,
            min_relative_prominence_percent: self.min_relative_prominence_percent,
            regions: self
                .analysis
                .regions()
                .iter()
                .map(|region| RegionMetadataEntry {
                    id: region.id,
                    peak: [region.peak.x, region.peak.y],
                    peak_clearance_pixels: region.peak_clearance_pixels,
                    cell_count: region.cell_count,
                })
                .collect(),
            passages: self
                .analysis
                .passages()
                .iter()
                .map(|passage| PassageMetadataEntry {
                    regions: passage.regions,
                    endpoints: [
                        [passage.endpoints[0].x, passage.endpoints[0].y],
                        [passage.endpoints[1].x, passage.endpoints[1].y],
                    ],
                    clearance_radius_pixels: passage.clearance_radius_pixels,
                })
                .collect(),
        };
        serde_json::to_string(&metadata).map_err(|error| format!("region serialization: {error}"))
    }
}

#[wasm_bindgen]
impl TerrainAnalysis {
    /// Computes clearance-derived regions for the currently active terrain/obstacle grid.
    ///
    /// `minProminencePixels` must be from 8 through 4096. An omitted
    /// `minRelativeProminencePercent` uses the core default of 40; supplied values must be from 0
    /// through 100. The returned result owns its labels and metadata, so it remains valid after
    /// this analysis snapshot is freed or its obstacle toggle changes. Regions are a static
    /// square-clearance graph, not unit-fit or engine pathing data.
    #[wasm_bindgen(js_name = analyzeRegions)]
    pub fn analyze_regions(
        &self,
        min_prominence_pixels: f64,
        min_relative_prominence_percent: Option<f64>,
    ) -> Result<RegionSnapshot, String> {
        if !min_prominence_pixels.is_finite()
            || min_prominence_pixels.fract() != 0.0
            || !(8.0..=4096.0).contains(&min_prominence_pixels)
        {
            return Err("minProminencePixels must be a finite integer from 8 through 4096".into());
        }
        let min_prominence_pixels = min_prominence_pixels as u16;
        let min_relative_prominence_percent = match min_relative_prominence_percent {
            Some(value)
                if !value.is_finite()
                    || value.fract() != 0.0
                    || !(0.0..=100.0).contains(&value) =>
            {
                return Err(
                    "minRelativeProminencePercent must be a finite integer from 0 through 100"
                        .into(),
                );
            }
            Some(value) => value as u8,
            None => RegionOptions::default().min_relative_prominence_percent,
        };
        let analysis = self
            .active_grid()
            .regions(&RegionOptions {
                min_prominence_pixels,
                min_relative_prominence_percent,
            })
            .map_err(|error| format!("region analysis: {error}"))?;
        Ok(RegionSnapshot {
            analysis,
            min_prominence_pixels,
            min_relative_prominence_percent,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use broodmap_analysis::{PixelRect, TerrainCell, TerrainGrid};

    fn terrain(with_obstacle: bool) -> TerrainAnalysis {
        let grid = TerrainGrid::from_cells(
            9,
            9,
            vec![
                TerrainCell {
                    walkable: true,
                    terrain_buildable: true,
                    elevation: 0,
                    ramp: false,
                };
                81
            ],
        )
        .unwrap();
        let obstructed = with_obstacle.then(|| {
            grid.with_obstacles(&[PixelRect {
                left: 32,
                top: 32,
                right: 40,
                bottom: 40,
            }])
        });
        let mut analysis = TerrainAnalysis::terrain_only(grid);
        analysis.obstructed = obstructed;
        analysis.respect_obstacles = with_obstacle;
        analysis.obstacle_count = usize::from(with_obstacle);
        analysis
    }

    #[test]
    fn snapshots_are_owned_and_follow_the_active_obstacle_mode() {
        let mut analysis = terrain(true);
        let blocked = analysis.analyze_regions(8.0, None).unwrap();
        let blocked_labels = blocked.labels();
        let mut caller_labels = blocked.labels();
        caller_labels[0] = u32::MAX;
        assert_eq!(blocked.labels(), blocked_labels);
        let obstacle_index = 4 * 9 + 4;
        assert_eq!(blocked_labels[obstacle_index], 0);

        analysis.set_obstacles_enabled(false);
        let terrain_only = analysis.analyze_regions(8.0, None).unwrap();
        assert_ne!(terrain_only.labels()[obstacle_index], 0);
        assert_eq!(blocked.labels(), blocked_labels);

        drop(analysis);
        assert_eq!(blocked.labels(), blocked_labels);
        assert!(blocked.metadata_json().is_ok());
    }

    #[test]
    fn invalid_prominence_does_not_mutate_routes_or_base_inputs() {
        let mut analysis = terrain(false);
        let route_before = analysis.route_json(0, 0, 8, 8).unwrap();
        let bases_before = analysis.find_bases_json("{}").unwrap_err();

        assert!(analysis.analyze_regions(7.0, None).is_err());
        assert!(analysis.analyze_regions(4097.0, None).is_err());
        assert!(analysis.analyze_regions(f64::NAN, None).is_err());
        assert!(analysis.analyze_regions(f64::INFINITY, None).is_err());
        assert!(analysis.analyze_regions(f64::NEG_INFINITY, None).is_err());
        assert!(analysis.analyze_regions(32.5, None).is_err());
        assert!(analysis.analyze_regions(4_294_967_328.0, None).is_err());
        assert!(analysis.analyze_regions(32.0, Some(f64::NAN)).is_err());
        assert!(analysis.analyze_regions(32.0, Some(f64::INFINITY)).is_err());
        assert!(
            analysis
                .analyze_regions(32.0, Some(f64::NEG_INFINITY))
                .is_err()
        );
        assert!(analysis.analyze_regions(32.0, Some(-1.0)).is_err());
        assert!(analysis.analyze_regions(32.0, Some(32.5)).is_err());
        assert!(analysis.analyze_regions(32.0, Some(101.0)).is_err());
        assert!(
            analysis
                .analyze_regions(32.0, Some(4_294_967_328.0))
                .is_err()
        );
        assert_eq!(analysis.route_json(0, 0, 8, 8).unwrap(), route_before);
        assert_eq!(analysis.find_bases_json("{}").unwrap_err(), bases_before);
    }

    #[test]
    fn labels_and_metadata_have_the_documented_shape() {
        let snapshot = terrain(false).analyze_regions(32.0, None).unwrap();
        let labels = snapshot.labels();
        let metadata: serde_json::Value =
            serde_json::from_str(&snapshot.metadata_json().unwrap()).unwrap();
        assert_eq!(metadata["minProminencePixels"], 32);
        assert_eq!(metadata["minRelativeProminencePercent"], 40);
        assert!(metadata["regions"].is_array());
        assert!(metadata["passages"].is_array());
        assert_eq!(labels.len(), 81);
        for region in metadata["regions"].as_array().unwrap() {
            assert!(region["id"].as_u64().is_some_and(|id| id > 0));
            assert_eq!(region["peak"].as_array().unwrap().len(), 2);
            assert!(region["peakClearancePixels"].is_u64());
            assert!(region["cellCount"].is_u64());
        }
        for passage in metadata["passages"].as_array().unwrap() {
            assert_eq!(passage["regions"].as_array().unwrap().len(), 2);
            assert_eq!(passage["endpoints"].as_array().unwrap().len(), 2);
            assert!(passage["clearanceRadiusPixels"].is_u64());
        }

        for (option, expected) in [(Some(0.0), 0), (Some(100.0), 100)] {
            let snapshot = terrain(false).analyze_regions(32.0, option).unwrap();
            let metadata: serde_json::Value =
                serde_json::from_str(&snapshot.metadata_json().unwrap()).unwrap();
            assert_eq!(metadata["minRelativeProminencePercent"], expected);
        }
    }
}
