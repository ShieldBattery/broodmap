//! Exhaustive scoring of a candidate group with lane-parallel proximity sweeps.

use super::{
    ResourceKind, ResourceNode, TilePosition,
    geometry::{footprint_cells, footprint_perimeter},
    proximity::{CellBox, ProximityField, half_cost},
    scoring::ScoringContext,
    search::ScoringScratch,
    selection::{CandidateKey, CandidateLocation, key_distance, tie_of},
};
use crate::{PixelRect, TerrainGrid, WalkPosition};

impl ScoringContext<'_> {
    /// Scores every member of one removal-set group exactly, as the reference algorithm does, with
    /// lane-parallel sweeps, and marks the members it retains. Returns the best selection key
    /// among fully connected ordinary members.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn sweep_group(
        &self,
        scoring: &mut ScoringScratch,
        cluster: &[usize],
        weights: &[u64],
        blockers: &[PixelRect],
        members: &[usize],
        candidates: &mut [CandidateLocation],
        retained: &mut [bool],
    ) -> Option<CandidateKey> {
        let depot = self.options.depot;
        let targets = members
            .iter()
            .map(|&index| {
                let (x0, y0, x1, y1) = footprint_cells(candidates[index].tile, depot);
                CellBox {
                    x0: i64::from(x0),
                    y0: i64::from(y0),
                    x1: i64::from(x1),
                    y1: i64::from(y1),
                }
            })
            .reduce(CellBox::union)
            .expect("nonempty group");
        let resources: Vec<_> = cluster
            .iter()
            .zip(weights)
            .map(|(&index, &weight)| (self.node(index), weight))
            .collect();
        let perimeter = footprint_perimeter(TilePosition { x: 0, y: 0 }, depot);
        if resources.len() <= 8 {
            sweep_chunks(
                &mut scoring.narrow,
                self.resource_grid,
                blockers,
                &resources,
                targets,
                &perimeter,
                members,
                candidates,
            );
        } else {
            sweep_chunks(
                &mut scoring.wide,
                self.resource_grid,
                blockers,
                &resources,
                targets,
                &perimeter,
                members,
                candidates,
            );
        }
        let (mineral_total, gas_total) =
            resources
                .iter()
                .fold((0, 0), |(m, g), (node, w)| match node.kind {
                    ResourceKind::Mineral => (m + w, g),
                    ResourceKind::Gas => (m, g + w),
                });
        let mut best: Option<CandidateKey> = None;
        for &index in members {
            let candidate = &candidates[index];
            let full = candidate.connected == cluster.len();
            retained[index] = (candidate.exact_start && candidate.connected > 0) || full;
            if full && !candidate.exact_start {
                let (deviation, y, x) = tie_of(candidate);
                let key = (
                    key_distance(candidate, mineral_total, gas_total),
                    deviation,
                    y,
                    x,
                );
                best = Some(best.map_or(key, |old| old.min(key)));
            }
        }
        best
    }
}

/// Accumulates each resource's exact capped distance to each candidate's perimeter, `L`
/// resources per sweep.
#[allow(clippy::too_many_arguments)]
fn sweep_chunks<const L: usize>(
    field: &mut ProximityField<L>,
    grid: &TerrainGrid,
    blockers: &[PixelRect],
    resources: &[(ResourceNode, u64)],
    targets: CellBox,
    perimeter: &[WalkPosition],
    members: &[usize],
    candidates: &mut [CandidateLocation],
) {
    for chunk in resources.chunks(L) {
        let bounds: Vec<_> = chunk.iter().map(|(node, _)| node.bounds).collect();
        field.compute(grid, blockers, &bounds, targets);
        for &index in members {
            let candidate = &mut candidates[index];
            let mut best = [i16::MAX; L];
            let (left, top) = (candidate.tile.x * 4, candidate.tile.y * 4);
            for point in perimeter {
                if let Some(lanes) = field.at(left + point.x, top + point.y) {
                    for (best, &lane) in best.iter_mut().zip(lanes) {
                        *best = (*best).min(lane);
                    }
                }
            }
            for (&lane, &(node, weight)) in best.iter().zip(chunk) {
                let Some(half) = half_cost(lane) else {
                    continue;
                };
                let distance = u64::from(half) * 2;
                candidate.connected += 1;
                match node.kind {
                    ResourceKind::Mineral => {
                        candidate.mineral_distance += distance * weight;
                        candidate.reachable_mineral_weight += weight;
                    }
                    ResourceKind::Gas => {
                        candidate.gas_distance += distance * weight;
                        candidate.reachable_gas_weight += weight;
                    }
                }
            }
        }
    }
}
