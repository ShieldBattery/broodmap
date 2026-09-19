//! One-sided width-transition evidence along terrain-only survey routes.

use broodmap_analysis::{EntranceAnalysis, EntranceOptions, WalkPosition};
use serde::Deserialize;
use wasm_bindgen::prelude::*;

use super::TerrainAnalysis;

const MAX_BATCH_QUERIES: usize = 256;
const MAX_BATCH_INPUT_LENGTH: usize = 32_768;

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct EntranceResult {
    terrain_only: bool,
    max_distance_pixels: u32,
    min_widening_percent: u16,
    route: Option<SurveyRoute>,
    candidates: Vec<Candidate>,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct SurveyRoute {
    points: Vec<[u32; 2]>,
    ground_distance_pixels: f64,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct Candidate {
    position: [u32; 2],
    endpoints: [[u32; 2]; 2],
    width_pixels: f64,
    approach_max_width_pixels: f64,
    outward_min_width_pixels: f64,
    outward_width_is_lower_bound: bool,
    distance_from_start_pixels: f64,
}

#[derive(Debug, Deserialize)]
struct BatchQuery([[u32; 2]; 2]);

fn coordinate(value: f64, bound: u32) -> Result<u32, String> {
    if !value.is_finite() || value.fract() != 0.0 || value < 0.0 || value >= f64::from(bound) {
        return Err(
            "entrance endpoints must be finite integer walk coordinates within the map".into(),
        );
    }
    Ok(value as u32)
}

fn entrance_options(min_widening_percent: Option<f64>) -> Result<EntranceOptions, String> {
    let mut options = EntranceOptions::default();
    if let Some(value) = min_widening_percent {
        if !value.is_finite() || value.fract() != 0.0 || !(0.0..=200.0).contains(&value) {
            return Err("minWideningPercent must be a finite integer from 0 through 200".into());
        }
        options.min_widening_percent = value as u16;
    }
    Ok(options)
}

fn entrance_result(options: EntranceOptions, analysis: EntranceAnalysis) -> EntranceResult {
    EntranceResult {
        terrain_only: true,
        max_distance_pixels: options.max_distance_pixels,
        min_widening_percent: options.min_widening_percent,
        route: analysis.route.map(|route| SurveyRoute {
            points: route
                .points
                .into_iter()
                .map(|point| [point.x, point.y])
                .collect(),
            ground_distance_pixels: route.distance_pixels,
        }),
        candidates: analysis
            .candidates
            .into_iter()
            .map(|candidate| Candidate {
                position: [candidate.position.x, candidate.position.y],
                endpoints: candidate.endpoints.map(|point| [point.x, point.y]),
                width_pixels: candidate.width_pixels,
                approach_max_width_pixels: candidate.approach_max_width_pixels,
                outward_min_width_pixels: candidate.outward_min_width_pixels,
                outward_width_is_lower_bound: candidate.outward_width_is_lower_bound,
                distance_from_start_pixels: candidate.distance_from_start_pixels,
            })
            .collect(),
    }
}

fn single_analysis(mut analyses: Vec<EntranceAnalysis>) -> Result<EntranceAnalysis, String> {
    if analyses.len() != 1 {
        return Err(format!(
            "entrance batch returned {} results for one query",
            analyses.len()
        ));
    }
    analyses
        .pop()
        .ok_or_else(|| "entrance batch returned no result for one query".into())
}
fn serialize_results(results: impl IntoIterator<Item = EntranceResult>) -> Result<String, String> {
    serde_json::to_string(&results.into_iter().collect::<Vec<_>>())
        .map_err(|error| format!("entrance serialization: {error}"))
}

#[wasm_bindgen]
impl TerrainAnalysis {
    /// Finds local width transitions along a clearance-favoring route from start to end.
    ///
    /// Coordinates are 8-pixel walk cells. This always surveys terrain alone, regardless of the
    /// obstacle toggle: resources and neutral objects do not define geological entrances.
    /// `minWideningPercent` defaults to 25 and accepts integral values from 0 through 200.
    /// Candidate centers lie within the first 1024 logical pixels; profiles may look 192px farther.
    /// Cross-section endpoints in the JSON are logical pixels; route points are walk cells. Spans
    /// are raster approximations, not certified chokepoints, construction legality, unit fit, or
    /// shortest movement routes.
    #[wasm_bindgen(js_name = entrancesJson)]
    pub fn entrances_json(
        &self,
        start_x: f64,
        start_y: f64,
        end_x: f64,
        end_y: f64,
        min_widening_percent: Option<f64>,
    ) -> Result<String, String> {
        let start = WalkPosition {
            x: coordinate(start_x, self.grid.width())?,
            y: coordinate(start_y, self.grid.height())?,
        };
        let end = WalkPosition {
            x: coordinate(end_x, self.grid.width())?,
            y: coordinate(end_y, self.grid.height())?,
        };
        let options = entrance_options(min_widening_percent)?;
        let analyses = self
            .entrance_survey()
            .batch(&[(start, end)], &options)
            .map_err(|error| error.to_string())?;
        let analysis = single_analysis(analyses)?;
        serde_json::to_string(&entrance_result(options, analysis))
            .map_err(|error| format!("entrance serialization: {error}"))
    }

    /// Batches up to 256 terrain-only entrance surveys without rebuilding the immutable terrain
    /// preparation. Input is JSON `[[[startX,startY],[endX,endY]], ...]` with strict unsigned
    /// integer coordinates and a 32 KiB limit. Results preserve input order and use the same
    /// objects returned by `entrancesJson`. Aggregate route output is capped at 2,097,152 points.
    /// These limits do not bound CPU time: each distinct origin can require a full-grid search.
    /// Callers needing responsive cancellation should submit smaller origin groups and yield
    /// between synchronous calls. An empty batch returns `[]` without preparing a survey.
    #[wasm_bindgen(js_name = entrancesBatchJson)]
    pub fn entrances_batch_json(
        &self,
        queries_json: &str,
        min_widening_percent: Option<f64>,
    ) -> Result<String, String> {
        if queries_json.len() > MAX_BATCH_INPUT_LENGTH {
            return Err("entrance queries JSON exceeds maximum input length".into());
        }
        let queries: Vec<BatchQuery> = serde_json::from_str(queries_json)
            .map_err(|error| format!("invalid entrance queries JSON: {error}"))?;
        if queries.len() > MAX_BATCH_QUERIES {
            return Err("too many entrance queries".into());
        }
        let options = entrance_options(min_widening_percent)?;
        if queries.is_empty() {
            return Ok("[]".into());
        }
        let positions = queries
            .into_iter()
            .map(|query| {
                (
                    WalkPosition {
                        x: query.0[0][0],
                        y: query.0[0][1],
                    },
                    WalkPosition {
                        x: query.0[1][0],
                        y: query.0[1][1],
                    },
                )
            })
            .collect::<Vec<_>>();
        let analyses = self
            .entrance_survey()
            .batch(&positions, &options)
            .map_err(|error| error.to_string())?;
        serialize_results(
            analyses
                .into_iter()
                .map(|analysis| entrance_result(options, analysis)),
        )
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use broodmap_analysis::{PixelRect, TerrainCell, TerrainGrid};

    fn analysis() -> TerrainAnalysis {
        let width = 100;
        let height = 90;
        let cells = (0..height)
            .flat_map(|y| {
                (0..width).map(move |x| TerrainCell {
                    walkable: ((1..=40).contains(&x) && (37..=52).contains(&y))
                        || ((41..=98).contains(&x) && (5..=84).contains(&y)),
                    terrain_buildable: true,
                    elevation: 0,
                    ramp: false,
                })
            })
            .collect();
        let grid = TerrainGrid::from_cells(width, height, cells).unwrap();
        let obstructed = Some(grid.with_obstacles(&[PixelRect {
            left: 200,
            top: 0,
            right: 216,
            bottom: height as i32 * 8,
        }]));
        let mut analysis = TerrainAnalysis::terrain_only(grid);
        analysis.obstructed = obstructed;
        analysis.respect_obstacles = true;
        analysis.obstacle_count = 1;
        analysis
    }

    fn disconnected_analysis() -> TerrainAnalysis {
        let width = 100;
        let height = 90;
        let cells = (0..height)
            .flat_map(|_y| {
                (0..width).map(move |x| TerrainCell {
                    walkable: x != 50,
                    terrain_buildable: true,
                    elevation: 0,
                    ramp: false,
                })
            })
            .collect();
        TerrainAnalysis::terrain_only(TerrainGrid::from_cells(width, height, cells).unwrap())
    }
    #[test]
    fn single_analysis_rejects_missing_or_extra_batch_results() {
        assert!(single_analysis(Vec::new()).is_err());
        let analysis = EntranceAnalysis {
            route: None,
            candidates: Vec::new(),
        };
        assert!(single_analysis(vec![analysis.clone(), analysis]).is_err());
    }
    #[test]
    fn terrain_only_evidence_has_explicit_units_and_is_independent_of_obstacle_mode() {
        let mut analysis = analysis();
        let first = analysis
            .entrances_json(10.0, 44.0, 80.0, 44.0, None)
            .unwrap();
        let json: serde_json::Value = serde_json::from_str(&first).unwrap();
        assert_eq!(json["terrainOnly"], true);
        assert_eq!(json["maxDistancePixels"], 1024);
        assert_eq!(json["minWideningPercent"], 25);
        assert_eq!(json["route"]["points"][0], serde_json::json!([10, 44]));
        assert!(!json["candidates"].as_array().unwrap().is_empty());
        let candidate = &json["candidates"][0];
        assert!(candidate["widthPixels"].as_f64().unwrap() >= 64.0);
        assert!(
            candidate["endpoints"][0][0].as_u64().unwrap() > 100,
            "cross-section coordinates are pixels, not walk cells"
        );
        analysis.set_obstacles_enabled(false);
        assert_eq!(
            first,
            analysis
                .entrances_json(10.0, 44.0, 80.0, 44.0, Some(25.0))
                .unwrap()
        );
    }

    #[test]
    fn rejects_noninteger_wrapping_and_nonfinite_inputs_without_changing_routes() {
        let analysis = analysis();
        let before = analysis.route_json(10, 44, 80, 44).unwrap();
        for bad in [f64::NAN, f64::INFINITY, -1.0, 0.5, 100.0, 4_294_967_306.0] {
            assert!(
                analysis
                    .entrances_json(bad, 44.0, 80.0, 44.0, None)
                    .is_err()
            );
        }
        for bad in [
            f64::NAN,
            f64::NEG_INFINITY,
            -1.0,
            0.5,
            201.0,
            4_294_967_321.0,
        ] {
            assert!(
                analysis
                    .entrances_json(10.0, 44.0, 80.0, 44.0, Some(bad))
                    .is_err()
            );
        }
        assert!(analysis.entrances_json(0.0, 0.0, 80.0, 44.0, None).is_err());
        assert_eq!(before, analysis.route_json(10, 44, 80, 44).unwrap());
    }
    #[test]
    fn batch_matches_single_preserves_order_and_reuses_the_terrain_cache() {
        let mut analysis = analysis();
        assert!(analysis.entrance_survey.get().is_none());
        assert_eq!(analysis.entrances_batch_json("[]", None).unwrap(), "[]");
        assert!(analysis.entrance_survey.get().is_none());

        let single = analysis
            .entrances_json(10.0, 44.0, 80.0, 44.0, Some(25.0))
            .unwrap();
        let cached = analysis.entrance_survey.get().unwrap() as *const _;
        let batch: Vec<serde_json::Value> = serde_json::from_str(
            &analysis
                .entrances_batch_json(
                    r#"[[[10,44],[80,44]],[[80,44],[10,44]],[[10,44],[80,44]]]"#,
                    Some(25.0),
                )
                .unwrap(),
        )
        .unwrap();
        assert_eq!(batch.len(), 3);
        assert_eq!(
            batch[0],
            serde_json::from_str::<serde_json::Value>(&single).unwrap()
        );
        assert_eq!(batch[2], batch[0]);
        assert!(!batch[0]["candidates"].as_array().unwrap().is_empty());
        assert_eq!(batch[1]["route"]["points"][0], serde_json::json!([80, 44]));

        analysis.set_obstacles_enabled(false);
        assert!(std::ptr::eq(
            cached,
            analysis.entrance_survey.get().unwrap()
        ));
        assert_eq!(
            single,
            analysis
                .entrances_json(10.0, 44.0, 80.0, 44.0, Some(25.0))
                .unwrap()
        );
    }

    #[test]
    fn batch_rejects_invalid_json_coordinates_limits_and_options() {
        let analysis = analysis();
        for input in [
            "{}",
            "[[[10,44],[80]]]",
            "[[[-1,44],[80,44]]]",
            "[[[10.5,44],[80,44]]]",
            "[[[4294967296,44],[80,44]]]",
            "[[[100,44],[80,44]]]",
        ] {
            assert!(
                analysis.entrances_batch_json(input, None).is_err(),
                "accepted {input}"
            );
        }
        let many = serde_json::to_string(&vec![[[10_u32, 44], [80, 44]]; 257]).unwrap();
        assert!(analysis.entrances_batch_json(&many, None).is_err());
        assert!(
            analysis
                .entrances_batch_json(&" ".repeat(32_769), None)
                .is_err()
        );
        for bad in [f64::NAN, f64::NEG_INFINITY, -1.0, 0.5, 201.0] {
            assert!(
                analysis
                    .entrances_batch_json(r#"[[[10,44],[80,44]]]"#, Some(bad))
                    .is_err()
            );
        }
    }

    #[test]
    fn terrain_snapshots_keep_separate_batch_preparation() {
        let first = analysis();
        let second = disconnected_analysis();
        let first_result = first
            .entrances_batch_json(r#"[[[10,44],[80,44]]]"#, None)
            .unwrap();
        let second_result = second
            .entrances_batch_json(r#"[[[10,44],[80,44]]]"#, None)
            .unwrap();
        let first_json: serde_json::Value = serde_json::from_str(&first_result).unwrap();
        let second_json: serde_json::Value = serde_json::from_str(&second_result).unwrap();
        assert!(!first_json[0]["route"].is_null());
        assert!(second_json[0]["route"].is_null());
        assert_ne!(first_json, second_json);
        assert!(!std::ptr::eq(
            first.entrance_survey.get().unwrap(),
            second.entrance_survey.get().unwrap(),
        ));
        assert_eq!(
            first_result,
            first
                .entrances_batch_json(r#"[[[10,44],[80,44]]]"#, None)
                .unwrap()
        );
    }
}
