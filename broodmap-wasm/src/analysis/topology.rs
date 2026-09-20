//! Owned incremental wrapper around the reusable Rust topology analyzer.
use super::TerrainAnalysis;
use broodmap_analysis::{
    WalkPosition,
    topology::{RampSide, TopologyBase, TopologyJob, TopologyOptions},
};
use serde::Deserialize;
use serde_json::{Value, json};
use std::sync::Arc;
use wasm_bindgen::prelude::*;

#[derive(Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct BaseInput {
    id: u32,
    route_anchor: Option<[u32; 2]>,
}

/// Incremental terrain-only topology. Owns its grid; dropping/freeing cancels unfinished work.
#[wasm_bindgen]
pub struct BaseTopologyJob {
    job: TopologyJob,
}

#[wasm_bindgen]
impl TerrainAnalysis {
    /// Starts reusable map-wide analysis from JSON [{id,routeAnchor:[walkX,walkY]|null}].
    /// At most 256 unique IDs and 32KiB JSON. Uses terrain alone, independent of obstacle mode.
    /// A missing routeAnchor is skipped like null. Advance between event-loop yields;
    /// completion is required for results.
    #[wasm_bindgen(js_name=beginBaseTopology)]
    pub fn begin_base_topology(
        &self,
        bases_json: &str,
        min_widening_percent: Option<f64>,
    ) -> Result<BaseTopologyJob, String> {
        if bases_json.len() > 32_768 {
            return Err("base anchors JSON exceeds maximum input length".into());
        }
        let widening = min_widening_percent.unwrap_or(25.0);
        if !widening.is_finite() || widening.fract() != 0.0 || !(0.0..=200.0).contains(&widening) {
            return Err("minimum widening must be an integer from 0 through 200".into());
        }
        let bases: Vec<BaseInput> =
            serde_json::from_str(bases_json).map_err(|e| format!("invalid base anchors: {e}"))?;
        let bases = bases
            .into_iter()
            .map(|b| TopologyBase {
                id: b.id,
                route_anchor: b.route_anchor.map(|[x, y]| WalkPosition { x, y }),
            })
            .collect();
        let mut options = TopologyOptions::default();
        options.entrances.min_widening_percent = widening as u16;
        let job =
            TopologyJob::new(Arc::clone(&self.grid), bases, options).map_err(|e| e.to_string())?;
        Ok(BaseTopologyJob { job })
    }
}

#[wasm_bindgen]
impl BaseTopologyJob {
    /// Prepares clearance or executes one origin/consolidation pass. No hard CPU-time budget.
    /// Returns completion status; completed jobs can be advanced again without changing results.
    pub fn advance(&mut self) -> Result<bool, String> {
        self.job
            .advance()
            .map(|p| p.complete)
            .map_err(|e| e.to_string())
    }
    #[wasm_bindgen(js_name=progressJson)]
    pub fn progress_json(&self) -> Result<String, String> {
        let p = self.job.progress();
        serde_json::to_string(&json!({
            "completedOrigins": p.completed_origins,
            "totalOrigins": p.total_origins,
            "surveyCount": p.survey_count,
            "complete": p.complete,
        }))
        .map_err(|e| e.to_string())
    }

    /// Owned final labels; errors before successful completion, including after any failed step.
    pub fn labels(&self) -> Result<Vec<u32>, String> {
        self.job
            .result()
            .map(|r| r.partition.labels().to_vec())
            .ok_or_else(|| "topology analysis is not complete".into())
    }
    #[wasm_bindgen(js_name=metadataJson)]
    pub fn metadata_json(&self) -> Result<String, String> {
        let result = self
            .job
            .result()
            .ok_or("topology analysis is not complete")?;
        let observations =
            |values: &[broodmap_analysis::topology::TopologyObservation]| -> Vec<Value> {
                values
                    .iter()
                    .map(|o| {
                        json!({
                            "endpoints": o.endpoints.map(|p| [p.x, p.y]),
                            "widthPixels": o.width_pixels,
                            "sources": o.sources,
                        })
                    })
                    .collect()
            };
        let boundaries: Vec<Value> = result
            .boundaries
            .iter()
            .map(|b| {
                let candidate = &b.boundary;
                let mut value = json!({
                    "endpoints": candidate.endpoints.map(|p| [p.x, p.y]),
                    "widthPixels": candidate.width_pixels,
                    "sources": candidate.sources,
                    "observations": observations(&candidate.observations),
                    "removedEdgeCount": b.assessment.removed_edge_count,
                    "separatedEdgeCount": b.assessment.separated_edge_count,
                    "regionPairs": b.assessment.region_pairs,
                    "incidentAreaIds": b.assessment.incident_area_ids,
                });
                if let Some(ramp) = candidate.ramp {
                    value["kind"] = json!("ramp");
                    value["rampId"] = json!(ramp.id);
                    value["rampEnd"] = json!(match ramp.end {
                        RampSide::Lower => "lower",
                        RampSide::Upper => "upper",
                    });
                    value["lowerElevation"] = json!(ramp.lower_elevation);
                    value["upperElevation"] = json!(ramp.upper_elevation);
                }
                value
            })
            .collect();
        let bases: Vec<_> = result
            .bases
            .iter()
            .map(|b| {
                json!({
                    "baseId": b.base_id,
                    "areaId": b.area_id,
                    "cellCount": b.cell_count,
                    "originalCellCount": b.original_cell_count,
                    "baseIds": b.base_ids,
                })
            })
            .collect();
        let areas: Vec<_> = result
            .partition
            .areas()
            .iter()
            .map(|a| {
                json!({
                    "id": a.id,
                    "cellCount": a.cell_count,
                })
            })
            .collect();
        let stats = &result.statistics;
        serde_json::to_string(&json!({
            "terrainOnly": true,
            "areas": areas,
            "boundaries": boundaries,
            "baseAreas": bases,
            "observations": observations(&result.observations),
            "surveyCount": stats.survey_count,
            "skippedAnchorCount": stats.skipped_anchor_count,
            "observationCount": stats.observation_count,
            "consolidatedCount": result.boundaries.len(),
            "rampCount": stats.ramp_count,
            "rampMouthsRemoved": stats.ramp_mouths_removed,
            "rampInteriorCutsRemoved": stats.ramp_interior_cuts_removed,
            "crossingCutsRemoved": stats.crossing_cuts_removed,
            "serialCutsRemoved": stats.serial_and_flat_cuts_removed,
            "junctionCutsRemoved": stats.junction_cuts_removed,
        }))
        .map_err(|e| e.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use broodmap_analysis::{TerrainCell, TerrainGrid};
    fn terrain() -> TerrainAnalysis {
        TerrainAnalysis::terrain_only(
            TerrainGrid::from_cells(
                20,
                20,
                vec![
                    TerrainCell {
                        walkable: true,
                        terrain_buildable: true,
                        elevation: 0,
                        ramp: false
                    };
                    400
                ],
            )
            .unwrap(),
        )
    }
    #[test]
    fn job_owns_snapshot_and_rejects_early_results() {
        let terrain = terrain();
        let mut job = terrain
            .begin_base_topology(
                r#"[{"id":3,"routeAnchor":[2,2]},{"id":9,"routeAnchor":[17,17]}]"#,
                None,
            )
            .unwrap();
        assert!(job.labels().is_err());
        assert!(job.metadata_json().is_err());
        drop(terrain);
        while !job.advance().unwrap() {}
        let mut labels = job.labels().unwrap();
        labels.fill(0);
        assert!(job.labels().unwrap().iter().all(|&v| v > 0));
        let meta: Value = serde_json::from_str(&job.metadata_json().unwrap()).unwrap();
        assert_eq!(meta["baseAreas"][0]["baseId"], 3);
        assert_eq!(meta["baseAreas"][1]["baseId"], 9);
        assert!(job.advance().unwrap());
    }
    #[test]
    fn rejects_invalid_input_without_poisoning_analysis() {
        let terrain = terrain();
        for value in [
            r#"[{"id":1,"routeAnchor":[20,0]}]"#,
            r#"[{"id":1},{"id":1}]"#,
            r#"[{"id":1,"routeAnchor":[-1,0]}]"#,
        ] {
            assert!(terrain.begin_base_topology(value, None).is_err());
        }
        for value in [-1.0, 201.0, f64::NAN, 0.5] {
            assert!(terrain.begin_base_topology("[]", Some(value)).is_err());
        }
        assert!(terrain.begin_base_topology("[]", None).is_ok());
    }
}
