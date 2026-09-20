//! Terrain-only boundary span partition snapshots.

use std::sync::Arc;

use broodmap_analysis::{BoundarySpan, PixelPosition, SpanPartition};
use serde::Deserialize;
use wasm_bindgen::prelude::*;

use super::TerrainAnalysis;

const MAX_SPANS: usize = 256;
const MAX_INPUT_LENGTH: usize = 32_768;

#[derive(Debug, Deserialize)]
struct AreaSpan([[u32; 2]; 2]);

impl AreaSpan {
    fn boundary(&self) -> BoundarySpan {
        BoundarySpan {
            endpoints: [
                PixelPosition {
                    x: self.0[0][0],
                    y: self.0[0][1],
                },
                PixelPosition {
                    x: self.0[1][0],
                    y: self.0[1][1],
                },
            ],
        }
    }
}

/// Owned terrain-only components and evidence for the supplied boundary spans.
/// Labels remain valid after the source analysis is freed or its obstacle mode changes.
#[wasm_bindgen]
pub struct AreaSnapshot {
    partition: Arc<SpanPartition>,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct AreaMetadata<'a> {
    terrain_only: bool,
    areas: Vec<AreaMetadataEntry>,
    boundaries: Vec<BoundaryMetadataEntry<'a>>,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct AreaMetadataEntry {
    id: u32,
    cell_count: u32,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct BoundaryMetadataEntry<'a> {
    removed_edge_count: u32,
    separated_edge_count: u32,
    region_pairs: &'a [[u32; 2]],
    incident_area_ids: &'a [u32],
}

#[wasm_bindgen]
impl AreaSnapshot {
    /// Returns an owned row-major Uint32Array: zero for blocked terrain, positive component IDs
    /// for every original walkable cell. Spans remove graph crossings, not cells.
    #[wasm_bindgen(js_name = labels)]
    pub fn labels(&self) -> Vec<u32> {
        self.partition.labels().to_vec()
    }

    /// Per-component cell counts and per-input-span effects with all supplied cuts applied.
    /// A separating edge count is not evidence that this span alone disconnects the terrain.
    #[wasm_bindgen(js_name = metadataJson)]
    pub fn metadata_json(&self) -> Result<String, String> {
        serde_json::to_string(&AreaMetadata {
            terrain_only: true,
            areas: self
                .partition
                .areas()
                .iter()
                .map(|area| AreaMetadataEntry {
                    id: area.id,
                    cell_count: area.cell_count,
                })
                .collect(),
            boundaries: self
                .partition
                .boundaries()
                .iter()
                .map(|boundary| BoundaryMetadataEntry {
                    removed_edge_count: boundary.removed_edge_count,
                    separated_edge_count: boundary.separated_edge_count,
                    region_pairs: &boundary.region_pairs,
                    incident_area_ids: &boundary.incident_area_ids,
                })
                .collect(),
        })
        .map_err(|error| format!("area serialization: {error}"))
    }
}

#[wasm_bindgen]
impl TerrainAnalysis {
    /// Partitions terrain alone using up to 256 finite pixel-coordinate spans.
    ///
    /// Input is JSON `[[[x1,y1],[x2,y2]], ...]`, at most 32 KiB; coordinates must be
    /// nonnegative integers within inclusive map edges, and each span must have distinct ends.
    /// An empty list returns ordinary terrain components. This does not mutate routing,
    /// the base catalog, or obstacle mode, and does not validate wall placement.
    #[wasm_bindgen(js_name = partitionAreas)]
    pub fn partition_areas(&self, spans_json: &str) -> Result<AreaSnapshot, String> {
        if spans_json.len() > MAX_INPUT_LENGTH {
            return Err("spans JSON exceeds maximum input length".into());
        }
        let spans: Vec<AreaSpan> = serde_json::from_str(spans_json)
            .map_err(|error| format!("invalid spans JSON: {error}"))?;
        if spans.len() > MAX_SPANS {
            return Err("too many spans".into());
        }
        let spans = spans.iter().map(AreaSpan::boundary).collect::<Vec<_>>();
        let partition = if spans.is_empty() {
            self.empty_partition()?
        } else {
            Arc::new(
                self.grid
                    .partition_by_spans(&spans)
                    .map_err(|error| format!("area partition: {error}"))?,
            )
        };
        Ok(AreaSnapshot { partition })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use broodmap_analysis::{PixelRect, TerrainCell, TerrainGrid};
    fn terrain(obs: bool) -> TerrainAnalysis {
        let grid = TerrainGrid::from_cells(
            9,
            9,
            vec![
                TerrainCell {
                    walkable: true,
                    terrain_buildable: true,
                    elevation: 0,
                    ramp: false
                };
                81
            ],
        )
        .unwrap();
        let obstructed = obs.then(|| {
            grid.with_obstacles(&[PixelRect {
                left: 32,
                top: 32,
                right: 40,
                bottom: 40,
            }])
        });
        let mut analysis = TerrainAnalysis::terrain_only(grid);
        analysis.obstructed = obstructed;
        analysis.respect_obstacles = obs;
        analysis.obstacle_count = usize::from(obs);
        analysis
    }
    #[test]
    fn straight_span_splits_open_terrain_and_reports_metadata() {
        let s = terrain(false)
            .partition_areas(r#"[[[32,0],[32,72]]]"#)
            .unwrap();
        assert_eq!(s.labels().len(), 81);
        let m: serde_json::Value = serde_json::from_str(&s.metadata_json().unwrap()).unwrap();
        assert_eq!(m["terrainOnly"], true);
        assert_eq!(m["areas"].as_array().unwrap().len(), 2);
        assert_eq!(m["boundaries"][0]["separatedEdgeCount"], 25);
    }
    #[test]
    fn terrain_only_routes_unchanged() {
        let mut a = terrain(true);
        let route = a.route_json(0, 0, 8, 8).unwrap();
        let expected = terrain(false)
            .partition_areas(r#"[[[32,0],[32,72]]]"#)
            .unwrap()
            .labels();
        assert_eq!(
            a.partition_areas(r#"[[[32,0],[32,72]]]"#).unwrap().labels(),
            expected
        );
        assert_eq!(a.route_json(0, 0, 8, 8).unwrap(), route);
        a.set_obstacles_enabled(false);
        let route_clear = terrain(false).route_json(0, 0, 8, 8).unwrap();
        assert_eq!(a.route_json(0, 0, 8, 8).unwrap(), route_clear);
        assert_eq!(
            a.partition_areas(r#"[[[32,0],[32,72]]]"#).unwrap().labels(),
            expected
        );
        assert_eq!(a.route_json(0, 0, 8, 8).unwrap(), route_clear);
    }
    #[test]
    fn invalid_inputs_rejected() {
        let a = terrain(false);
        for x in [
            "{}",
            "[[[-1,0],[1,1]]]",
            "[[[[1,0],[1,1]]]]",
            "[[[1.5,0],[1,1]]]",
            "[[[4294967296,0],[1,1]]]",
            "[[[1,1],[1,1]]]",
            "[[[73,0],[73,72]]]",
            "[[[0,73],[72,73]]]",
        ] {
            assert!(a.partition_areas(x).is_err(), "accepted {x}");
        }
        let many = serde_json::to_string(&vec![[[0_u32, 0], [1, 1]]; 257]).unwrap();
        assert!(a.partition_areas(&many).is_err());
        assert!(a.partition_areas(&" ".repeat(32769)).is_err());
    }
    #[test]
    fn empty_partition_reuses_the_terrain_only_snapshot() {
        let mut analysis = terrain(true);
        let first = analysis.partition_areas("[]").unwrap();
        let second = analysis.partition_areas("[]").unwrap();
        assert!(Arc::ptr_eq(&first.partition, &second.partition));

        analysis.set_obstacles_enabled(false);
        let after_toggle = analysis.partition_areas("[]").unwrap();
        assert!(Arc::ptr_eq(&first.partition, &after_toggle.partition));
        assert_eq!(first.labels(), after_toggle.labels());

        let cut = analysis.partition_areas(r#"[[[32,0],[32,72]]]"#).unwrap();
        assert!(!Arc::ptr_eq(&first.partition, &cut.partition));

        let owned = {
            let analysis = terrain(false);
            analysis.partition_areas("[]").unwrap()
        };
        assert_eq!(owned.labels().len(), 81);
        assert!(owned.metadata_json().is_ok());
    }
    #[test]
    fn snapshot_owns_data() {
        let s = {
            let a = terrain(false);
            a.partition_areas(r#"[[[32,0],[32,72]]]"#).unwrap()
        };
        assert_eq!(s.labels().len(), 81);
        assert!(s.metadata_json().is_ok());
    }
}
