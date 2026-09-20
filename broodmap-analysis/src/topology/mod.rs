//! Experimental, reusable base-area topology analysis.
//!
//! Results describe static terrain entrances and finite-span partitions, not legal building
//! placements or walls. The analyzer accepts caller-owned base identities and route anchors.

use crate::{
    AreaError, BoundaryAssessment, BoundarySpan, EntranceError, EntranceOptions, EntranceSurvey,
    PixelPosition, SpanPartition, TerrainGrid, WalkPosition,
};
use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};
use thiserror::Error;

mod context;
mod junctions;
mod observations;
mod ramp_boundaries;
mod serial;

use context::{PartitionContext, canonical};

/// Caller identity and optional walk-grid route anchor for a base.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TopologyBase {
    pub id: u32,
    pub route_anchor: Option<WalkPosition>,
}

/// Which end of a structural ramp supplied a boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum RampSide {
    Lower,
    Upper,
}

/// Structural evidence retained independently from route-local observations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RampBoundary {
    pub id: u32,
    pub end: RampSide,
    pub lower_elevation: u8,
    pub upper_elevation: u8,
}

/// A route-local observation, with directed caller base-ID pairs as provenance.
#[derive(Debug, Clone, PartialEq)]
pub struct TopologyObservation {
    pub endpoints: [PixelPosition; 2],
    pub width_pixels: f64,
    pub sources: Vec<[u32; 2]>,
}

/// A retained candidate section and its supporting evidence.
#[derive(Debug, Clone, PartialEq)]
pub struct TopologyBoundary {
    pub endpoints: [PixelPosition; 2],
    pub width_pixels: f64,
    pub sources: Vec<[u32; 2]>,
    pub observations: Vec<TopologyObservation>,
    pub ramp: Option<RampBoundary>,
}

/// Effects of one retained section with all final sections applied.
#[derive(Debug, Clone, PartialEq)]
pub struct AnalyzedBoundary {
    pub boundary: TopologyBoundary,
    pub assessment: BoundaryAssessment,
}

const MAX_BASES: usize = 256;
const MAX_BOUNDARIES: usize = 256;
const MAX_OBSERVATIONS: usize = 8192;

/// Explicit work policy. Nearest-neighbor surveys are evidence sampling, not exhaustive exits.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TopologyOptions {
    pub entrances: EntranceOptions,
    /// Number of nearest same-component bases; reciprocal surveys are added automatically.
    pub nearby_base_count: usize,
}
impl Default for TopologyOptions {
    fn default() -> Self {
        Self {
            entrances: EntranceOptions::default(),
            nearby_base_count: 3,
        }
    }
}

#[derive(Debug, Error)]
pub enum TopologyError {
    #[error("at most {MAX_BASES} bases are supported")]
    TooManyBases,
    #[error("base ID {0} occurs more than once")]
    DuplicateBase(u32),
    #[error("base {0} has an out-of-bounds or blocked terrain anchor")]
    InvalidAnchor(u32),
    #[error("nearby_base_count must be in 1..=16")]
    InvalidNearbyCount,
    #[error("at most {MAX_OBSERVATIONS} distinct entrance observations are supported")]
    TooManyObservations,
    #[error("at most {MAX_BOUNDARIES} candidate boundaries are supported")]
    TooManyBoundaries,
    #[error("topology job previously failed")]
    Failed,
    #[error(transparent)]
    Entrances(#[from] EntranceError),
    #[error(transparent)]
    Partition(#[from] AreaError),
}

/// Base membership in the final terrain partition. IDs come from the caller.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BaseArea {
    pub base_id: u32,
    pub area_id: u32,
    pub cell_count: u32,
    pub original_cell_count: u32,
    pub base_ids: Vec<u32>,
}

/// Diagnostic counts for the selected policy; removed cuts include both serial and flat duplicates.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TopologyStatistics {
    pub survey_count: usize,
    pub skipped_anchor_count: usize,
    pub observation_count: usize,
    pub ramp_count: usize,
    pub crossing_cuts_removed: usize,
    pub ramp_mouths_removed: usize,
    pub ramp_interior_cuts_removed: usize,
    pub serial_and_flat_cuts_removed: usize,
    pub junction_cuts_removed: usize,
}

/// Owned completed terrain topology, usable without a renderer or WASM runtime.
#[derive(Debug, Clone, PartialEq)]
pub struct BaseTopology {
    pub partition: SpanPartition,
    pub boundaries: Vec<AnalyzedBoundary>,
    pub bases: Vec<BaseArea>,
    /// All distinct route observations before consolidation, including discarded sections.
    pub observations: Vec<TopologyObservation>,
    pub statistics: TopologyStatistics,
}
impl BaseTopology {
    pub fn base(&self, id: u32) -> Option<&BaseArea> {
        self.bases.iter().find(|b| b.base_id == id)
    }
    /// Retained sections incident to a base's final area. Includes shared and bypassable sections.
    /// An empty result does not prove that the base has no exits.
    pub fn entrances_for_base(&self, id: u32) -> impl Iterator<Item = &AnalyzedBoundary> {
        let area = self.base(id).map(|b| b.area_id);
        self.boundaries
            .iter()
            .filter(move |b| area.is_some_and(|a| b.assessment.incident_area_ids.contains(&a)))
    }
}

/// A scheduling checkpoint, not a hard time budget: a survey step may search one full component.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TopologyProgress {
    pub completed_origins: usize,
    pub total_origins: usize,
    pub survey_count: usize,
    pub complete: bool,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Stage {
    Prepare,
    Survey,
    Candidates,
    Mouth(u8),
    Interiors,
    Serial,
    Junctions,
    Finish,
    Complete,
    Failed,
}

/// Incremental analysis on an immutable terrain snapshot. Drop the job to cancel it.
/// Construction labels original terrain components and plans base pairs. The first advance
/// prepares clearance; later advances perform one origin survey or a consolidation/repartition pass.
/// It never exposes an incomplete result and does not retain destination route trees between steps.
pub struct TopologyJob {
    grid: Arc<TerrainGrid>,
    survey: Option<EntranceSurvey>,
    bases: Vec<TopologyBase>,
    skipped: usize,
    options: TopologyOptions,
    destinations: BTreeMap<u32, Vec<TopologyBase>>,
    original: SpanPartition,
    partition: SpanPartition,
    raw: BTreeMap<[PixelPosition; 2], TopologyObservation>,
    boundaries: Vec<TopologyBoundary>,
    statistics: TopologyStatistics,
    completed_origins: usize,
    stage: Stage,
    result: Option<BaseTopology>,
}
impl TopologyJob {
    /// Accepts up to 256 distinct caller base IDs. Missing anchors are reported as skipped;
    /// present anchors must be walkable in this terrain snapshot. Options are validated eagerly.
    /// Construction computes terrain connectivity; clearance and surveys are deferred to advance.
    pub fn new(
        grid: Arc<TerrainGrid>,
        mut bases: Vec<TopologyBase>,
        options: TopologyOptions,
    ) -> Result<Self, TopologyError> {
        if bases.len() > MAX_BASES {
            return Err(TopologyError::TooManyBases);
        }
        if !(1..=16).contains(&options.nearby_base_count) {
            return Err(TopologyError::InvalidNearbyCount);
        }
        bases.sort_by_key(|b| b.id);
        for pair in bases.windows(2) {
            if pair[0].id == pair[1].id {
                return Err(TopologyError::DuplicateBase(pair[0].id));
            }
        }
        for base in &bases {
            if base
                .route_anchor
                .is_some_and(|p| !grid.cell(p).is_some_and(|c| c.walkable))
            {
                return Err(TopologyError::InvalidAnchor(base.id));
            }
        }
        // Reuse the low-level contract rather than duplicate its option ranges.
        grid.entrances_batch(&[], &options.entrances)?;
        let skipped = bases.iter().filter(|b| b.route_anchor.is_none()).count();
        bases.retain(|b| b.route_anchor.is_some());
        let original = grid.partition_by_spans(&[])?;
        let label = |b: &TopologyBase| {
            let p = b.route_anchor.unwrap();
            original.labels()[(p.y * grid.width() + p.x) as usize]
        };
        let mut pairs = BTreeSet::new();
        for base in &bases {
            let p = base.route_anchor.unwrap();
            let mut others: Vec<_> = bases
                .iter()
                .filter(|b| b.id != base.id && label(b) == label(base))
                .collect();
            others.sort_by_key(|b| {
                let q = b.route_anchor.unwrap();
                (
                    i64::from(p.x).abs_diff(i64::from(q.x)).pow(2)
                        + i64::from(p.y).abs_diff(i64::from(q.y)).pow(2),
                    b.id,
                )
            });
            for other in others.into_iter().take(options.nearby_base_count) {
                pairs.insert((base.id, other.id));
                pairs.insert((other.id, base.id));
            }
        }
        let destinations = bases
            .iter()
            .map(|b| {
                (
                    b.id,
                    bases
                        .iter()
                        .filter(|o| pairs.contains(&(b.id, o.id)))
                        .copied()
                        .collect(),
                )
            })
            .collect();
        Ok(Self {
            grid,
            survey: None,
            bases,
            skipped,
            options,
            destinations,
            partition: original.clone(),
            original,
            raw: BTreeMap::new(),
            boundaries: Vec::new(),
            statistics: TopologyStatistics::default(),
            completed_origins: 0,
            stage: Stage::Prepare,
            result: None,
        })
    }
    pub fn progress(&self) -> TopologyProgress {
        TopologyProgress {
            completed_origins: self.completed_origins,
            total_origins: self.bases.len(),
            survey_count: self.statistics.survey_count,
            complete: self.stage == Stage::Complete,
        }
    }
    pub fn result(&self) -> Option<&BaseTopology> {
        self.result.as_ref()
    }
    pub fn advance(&mut self) -> Result<TopologyProgress, TopologyError> {
        if self.stage == Stage::Failed {
            return Err(TopologyError::Failed);
        }
        if let Err(error) = self.advance_inner() {
            self.stage = Stage::Failed;
            return Err(error);
        }
        Ok(self.progress())
    }
    /// Runs remaining steps synchronously. Interactive callers can use advance instead.
    pub fn finish(mut self) -> Result<BaseTopology, TopologyError> {
        while self.stage != Stage::Complete {
            self.advance()?;
        }
        self.result.take().ok_or(TopologyError::Failed)
    }
    fn repartition(&mut self) -> Result<(), TopologyError> {
        let spans: Vec<_> = self
            .boundaries
            .iter()
            .map(|b| BoundarySpan {
                endpoints: b.endpoints,
            })
            .collect();
        self.partition = self.grid.partition_by_spans(&spans)?;
        Ok(())
    }
    fn advance_inner(&mut self) -> Result<(), TopologyError> {
        match self.stage {
            Stage::Prepare => {
                self.survey = Some(EntranceSurvey::new(Arc::clone(&self.grid)));
                self.stage = Stage::Survey;
            }
            Stage::Survey => {
                if self.completed_origins == self.bases.len() {
                    self.stage = Stage::Candidates;
                    return Ok(());
                }
                let base = self.bases[self.completed_origins];
                let destinations = &self.destinations[&base.id];
                let queries: Vec<_> = destinations
                    .iter()
                    .map(|d| (base.route_anchor.unwrap(), d.route_anchor.unwrap()))
                    .collect();
                let results = self
                    .survey
                    .as_ref()
                    .ok_or(TopologyError::Failed)?
                    .batch(&queries, &self.options.entrances)?;
                self.statistics.survey_count += queries.len();
                for (survey, destination) in results.into_iter().zip(destinations) {
                    for candidate in survey.candidates {
                        let endpoints = canonical(candidate.endpoints);
                        let observation =
                            self.raw
                                .entry(endpoints)
                                .or_insert_with(|| TopologyObservation {
                                    endpoints,
                                    width_pixels: candidate.width_pixels,
                                    sources: Vec::new(),
                                });
                        observation.width_pixels =
                            observation.width_pixels.min(candidate.width_pixels);
                        observation.sources.push([base.id, destination.id]);
                    }
                }
                if self.raw.len() > MAX_OBSERVATIONS {
                    return Err(TopologyError::TooManyObservations);
                }
                self.completed_origins += 1;
            }
            Stage::Candidates => {
                for observation in self.raw.values_mut() {
                    observation.sources.sort();
                    observation.sources.dedup();
                }
                self.statistics.observation_count = self.raw.len();
                self.boundaries =
                    observations::consolidate(self.raw.values().cloned().collect(), &self.bases);
                if self.boundaries.len() > MAX_BOUNDARIES {
                    return Err(TopologyError::TooManyBoundaries);
                }
                let ramps = self.grid.ramps();
                self.statistics.ramp_count = ramps.len();
                for ramp in ramps {
                    for (end, span) in
                        [(RampSide::Lower, ramp.lower), (RampSide::Upper, ramp.upper)]
                    {
                        self.boundaries.push(TopologyBoundary {
                            endpoints: canonical(span.endpoints),
                            width_pixels: span.width_pixels,
                            sources: vec![],
                            observations: vec![],
                            ramp: Some(RampBoundary {
                                id: ramp.id,
                                end,
                                lower_elevation: ramp.lower_elevation,
                                upper_elevation: ramp.upper_elevation,
                            }),
                        });
                    }
                }
                if self.boundaries.len() > MAX_BOUNDARIES {
                    return Err(TopologyError::TooManyBoundaries);
                }
                let count = self.boundaries.len();
                self.boundaries = observations::non_crossing(std::mem::take(&mut self.boundaries));
                self.statistics.crossing_cuts_removed = count - self.boundaries.len();
                self.repartition()?;
                self.stage = Stage::Mouth(0);
            }
            Stage::Mouth(pass) => {
                let ctx = PartitionContext::new(&self.grid, &self.partition, &self.bases);
                let next = ramp_boundaries::replace_ramp_mouths(&self.boundaries, &ctx);
                let removed = self.boundaries.len() - next.len();
                self.boundaries = next;
                self.statistics.ramp_mouths_removed += removed;
                if removed > 0 {
                    self.repartition()?;
                }
                self.stage = if removed > 0 && pass == 0 {
                    Stage::Mouth(1)
                } else {
                    Stage::Interiors
                };
            }
            Stage::Interiors | Stage::Serial | Stage::Junctions => {
                let ctx = PartitionContext::new(&self.grid, &self.partition, &self.bases);
                let (next, stage) = match self.stage {
                    Stage::Interiors => (
                        ramp_boundaries::collapse_ramp_interiors(&self.boundaries, &ctx),
                        Stage::Serial,
                    ),
                    Stage::Serial => (
                        serial::collapse_serial_boundaries(&self.boundaries, &ctx),
                        Stage::Junctions,
                    ),
                    _ => (
                        junctions::remove_junction_boundaries(&self.boundaries, &ctx),
                        Stage::Finish,
                    ),
                };
                let removed = self.boundaries.len() - next.len();
                self.boundaries = next;
                match self.stage {
                    Stage::Interiors => self.statistics.ramp_interior_cuts_removed = removed,
                    Stage::Serial => self.statistics.serial_and_flat_cuts_removed = removed,
                    _ => self.statistics.junction_cuts_removed = removed,
                }
                if removed > 0 {
                    self.repartition()?;
                }
                self.stage = stage;
            }
            Stage::Finish => {
                let component = |partition: &SpanPartition, b: &TopologyBase| {
                    let p = b.route_anchor.unwrap();
                    partition.labels()[(p.y * self.grid.width() + p.x) as usize]
                };
                let counts: BTreeMap<_, _> = self
                    .partition
                    .areas()
                    .iter()
                    .map(|a| (a.id, a.cell_count))
                    .collect();
                let original_counts: BTreeMap<_, _> = self
                    .original
                    .areas()
                    .iter()
                    .map(|a| (a.id, a.cell_count))
                    .collect();
                let bases = self
                    .bases
                    .iter()
                    .map(|b| {
                        let area_id = component(&self.partition, b);
                        BaseArea {
                            base_id: b.id,
                            area_id,
                            cell_count: counts[&area_id],
                            original_cell_count: original_counts[&component(&self.original, b)],
                            base_ids: self
                                .bases
                                .iter()
                                .filter(|o| component(&self.partition, o) == area_id)
                                .map(|o| o.id)
                                .collect(),
                        }
                    })
                    .collect();
                self.statistics.skipped_anchor_count = self.skipped;
                let boundaries = self
                    .boundaries
                    .iter()
                    .cloned()
                    .zip(self.partition.boundaries().iter().cloned())
                    .map(|(boundary, assessment)| AnalyzedBoundary {
                        boundary,
                        assessment,
                    })
                    .collect();
                self.result = Some(BaseTopology {
                    partition: self.partition.clone(),
                    boundaries,
                    bases,
                    observations: self.raw.values().cloned().collect(),
                    statistics: self.statistics.clone(),
                });
                self.stage = Stage::Complete;
            }
            Stage::Complete => {}
            Stage::Failed => return Err(TopologyError::Failed),
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TerrainCell;
    fn hallway() -> Arc<TerrainGrid> {
        let cells = (0..90)
            .flat_map(|y| {
                (0..100).map(move |x| TerrainCell {
                    walkable: ((1..=40).contains(&x) && (37..=52).contains(&y))
                        || ((41..=98).contains(&x) && (5..=84).contains(&y)),
                    terrain_buildable: true,
                    elevation: 0,
                    ramp: false,
                })
            })
            .collect();
        Arc::new(TerrainGrid::from_cells(100, 90, cells).unwrap())
    }
    fn bases() -> Vec<TopologyBase> {
        vec![
            TopologyBase {
                id: 42,
                route_anchor: Some(WalkPosition { x: 10, y: 44 }),
            },
            TopologyBase {
                id: 9,
                route_anchor: Some(WalkPosition { x: 80, y: 44 }),
            },
            TopologyBase {
                id: 81,
                route_anchor: None,
            },
        ]
    }
    #[test]
    fn full_pipeline_keeps_real_entrance_evidence_and_queries_by_caller_base_id() {
        let grid = hallway();
        let result = TopologyJob::new(Arc::clone(&grid), bases(), TopologyOptions::default())
            .unwrap()
            .finish()
            .unwrap();
        assert!(!result.boundaries.is_empty());
        assert!(!result.observations.is_empty());
        assert_eq!(result.statistics.survey_count, 2);
        assert_eq!(result.statistics.skipped_anchor_count, 1);
        assert_ne!(
            result.base(9).unwrap().area_id,
            result.base(42).unwrap().area_id
        );
        assert!(result.base(81).is_none());
        assert!(result.entrances_for_base(42).next().is_some());
        assert!(result.entrances_for_base(800).next().is_none());
        for boundary in &result.boundaries {
            assert_eq!(boundary.boundary.sources, vec![[42, 9]]);
        }
        let repartition = grid
            .partition_by_spans(
                &result
                    .boundaries
                    .iter()
                    .map(|b| BoundarySpan {
                        endpoints: b.boundary.endpoints,
                    })
                    .collect::<Vec<_>>(),
            )
            .unwrap();
        assert_eq!(result.partition, repartition);
        let mut reversed = bases();
        reversed.reverse();
        let mut job = TopologyJob::new(grid, reversed, TopologyOptions::default()).unwrap();
        assert!(job.result().is_none());
        let mut previous = 0;
        while !job.advance().unwrap().complete {
            assert!(job.progress().completed_origins >= previous);
            previous = job.progress().completed_origins;
            assert!(job.result().is_none());
        }
        assert_eq!(job.result(), Some(&result));
        assert!(job.advance().unwrap().complete);
        assert_eq!(job.finish().unwrap(), result);
    }
    #[test]
    fn base_query_includes_bypassed_sections_but_not_unrelated_or_empty_cuts() {
        let grid = TerrainGrid::from_cells(
            8,
            4,
            vec![
                TerrainCell {
                    walkable: true,
                    terrain_buildable: true,
                    elevation: 0,
                    ramp: false,
                };
                32
            ],
        )
        .unwrap();
        let spans = [
            BoundarySpan {
                endpoints: [
                    PixelPosition { x: 32, y: 0 },
                    PixelPosition { x: 32, y: 32 },
                ],
            },
            BoundarySpan {
                endpoints: [
                    PixelPosition { x: 16, y: 8 },
                    PixelPosition { x: 16, y: 24 },
                ],
            },
            BoundarySpan {
                endpoints: [PixelPosition { x: 0, y: 0 }, PixelPosition { x: 0, y: 32 }],
            },
        ];
        let partition = grid.partition_by_spans(&spans).unwrap();
        assert_eq!(partition.areas().len(), 2);
        assert!(partition.boundaries()[1].removed_edge_count > 0);
        assert!(partition.boundaries()[1].region_pairs.is_empty());
        assert_eq!(partition.boundaries()[2].removed_edge_count, 0);
        let boundaries = spans
            .iter()
            .zip(partition.boundaries())
            .map(|(span, assessment)| AnalyzedBoundary {
                boundary: TopologyBoundary {
                    endpoints: span.endpoints,
                    width_pixels: 32.0,
                    sources: vec![],
                    observations: vec![],
                    ramp: None,
                },
                assessment: assessment.clone(),
            })
            .collect();
        let bases = [(10, 1), (20, 6)]
            .into_iter()
            .map(|(base_id, x)| {
                let area_id = partition.labels()[8 + x];
                BaseArea {
                    base_id,
                    area_id,
                    cell_count: 16,
                    original_cell_count: 32,
                    base_ids: vec![base_id],
                }
            })
            .collect();
        let result = BaseTopology {
            partition,
            boundaries,
            bases,
            observations: vec![],
            statistics: TopologyStatistics::default(),
        };
        assert_eq!(
            result
                .entrances_for_base(10)
                .map(|b| b.boundary.endpoints)
                .collect::<Vec<_>>(),
            vec![spans[0].endpoints, spans[1].endpoints]
        );
        assert_eq!(
            result
                .entrances_for_base(20)
                .map(|b| b.boundary.endpoints)
                .collect::<Vec<_>>(),
            vec![spans[0].endpoints]
        );
        assert_eq!(result.entrances_for_base(99).count(), 0);
    }
    #[test]
    fn validates_base_ids_anchors_and_options_before_scheduling() {
        let grid = hallway();
        let mut duplicated = bases();
        duplicated.push(duplicated[0]);
        assert!(matches!(
            TopologyJob::new(Arc::clone(&grid), duplicated, TopologyOptions::default()),
            Err(TopologyError::DuplicateBase(42))
        ));
        let mut bad = bases();
        bad[0].route_anchor = Some(WalkPosition { x: 0, y: 0 });
        assert!(matches!(
            TopologyJob::new(Arc::clone(&grid), bad, TopologyOptions::default()),
            Err(TopologyError::InvalidAnchor(42))
        ));
        assert!(matches!(
            TopologyJob::new(
                Arc::clone(&grid),
                vec![bases()[0]; 257],
                TopologyOptions::default()
            ),
            Err(TopologyError::TooManyBases)
        ));
        for nearby_base_count in [0, 17] {
            assert!(matches!(
                TopologyJob::new(
                    Arc::clone(&grid),
                    bases(),
                    TopologyOptions {
                        nearby_base_count,
                        ..Default::default()
                    }
                ),
                Err(TopologyError::InvalidNearbyCount)
            ));
        }
        let mut options = TopologyOptions::default();
        options.entrances.min_widening_percent = 201;
        assert!(matches!(
            TopologyJob::new(grid, bases(), options),
            Err(TopologyError::Entrances(_))
        ));
    }
    #[test]
    fn disconnected_bases_are_not_surveyed() {
        let cells = (0..20)
            .flat_map(|_| {
                (0..20).map(|x| TerrainCell {
                    walkable: x != 10,
                    terrain_buildable: true,
                    elevation: 0,
                    ramp: false,
                })
            })
            .collect();
        let grid = Arc::new(TerrainGrid::from_cells(20, 20, cells).unwrap());
        let result = TopologyJob::new(
            grid,
            vec![
                TopologyBase {
                    id: 3,
                    route_anchor: Some(WalkPosition { x: 2, y: 2 }),
                },
                TopologyBase {
                    id: 4,
                    route_anchor: Some(WalkPosition { x: 15, y: 2 }),
                },
            ],
            TopologyOptions::default(),
        )
        .unwrap()
        .finish()
        .unwrap();
        assert_eq!(result.statistics.survey_count, 0);
        assert!(result.boundaries.is_empty());
        assert_ne!(
            result.base(3).unwrap().area_id,
            result.base(4).unwrap().area_id
        );
    }
}
