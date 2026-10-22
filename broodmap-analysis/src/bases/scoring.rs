//! Pruned scoring of a field's candidate depots with bounded, targeted floods.

use std::collections::BTreeMap;

use super::{
    BaseSearchOptions, PROXIMITY_MAX_COST, PixelPosition, ResourceKind, ResourceNode,
    candidates::{
        category_balanced_center_deviation, category_reference, is_clearable, resource_weight,
    },
    clustering::perimeter_access_cells,
    geometry::{
        CellWindow, access_ring, footprint_cells, footprint_perimeter, octile_gap, stamp_obstacles,
    },
    search::{FloodEnd, ScoringScratch},
    selection::{
        CandidateKey, CandidateLocation, CatalogResource, MiningResource, PrunedCandidate,
        ScoredCluster,
    },
};
use crate::{StaticObstacle, TerrainGrid};

/// Ordinary candidates are pruned against an incumbent, a fully scored candidate. Before one
/// exists, the most promising candidates by lower bound are probed, one per attempt.
const PROBE_ATTEMPTS: usize = 3;
/// A probe is abandoned past 1.5 times its straight-line bound plus this detour.
const PROBE_SLACK_COST: u32 = 8_000;
/// Cells a group's pruned floods may settle before the group is swept exhaustively instead.
const FLOOD_POP_BUDGET: usize = 2_000;

pub(super) struct ScoringContext<'a> {
    pub(super) catalog: &'a [CatalogResource],
    pub(super) mining: &'a [MiningResource],
    pub(super) obstacles: &'a [StaticObstacle],
    pub(super) valid_starts: &'a [(usize, PixelPosition)],
    pub(super) options: &'a BaseSearchOptions,
    pub(super) resource_grid: &'a TerrainGrid,
    pub(super) map_width_tiles: u32,
    pub(super) map_height_tiles: u32,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum MemberStatus {
    Live,
    /// An ordinary candidate that some mining resource cannot reach; it is never retained.
    Dead,
    /// Moved to the cluster's pruned list with a lower bound.
    Pruned,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Known {
    /// `value` is a lower bound on the distance.
    Bound,
    Distance,
    Unreachable,
}

/// Per-group scoring state: for each member candidate and cluster resource, what is known about
/// the distance between them, plus each member's resulting lower bound on its key distance.
struct GroupScores {
    resources: usize,
    values: Vec<u32>,
    known: Vec<Known>,
    settled: Vec<usize>,
    bounds: Vec<u128>,
    status: Vec<MemberStatus>,
}

impl GroupScores {
    fn slot(&self, member: usize, resource: usize) -> usize {
        member * self.resources + resource
    }

    fn settle(&mut self, member: usize, resource: usize, distance: u32, key_weight: u128) {
        let slot = self.slot(member, resource);
        self.bounds[member] = self.bounds[member] - key_weight * u128::from(self.values[slot])
            + key_weight * u128::from(distance);
        self.values[slot] = distance;
        self.known[slot] = Known::Distance;
        self.settled[member] += 1;
    }

    fn raise(&mut self, member: usize, resource: usize, bound: u32, key_weight: u128) {
        let slot = self.slot(member, resource);
        if bound > self.values[slot] {
            self.bounds[member] += key_weight * u128::from(bound - self.values[slot]);
            self.values[slot] = bound;
        }
    }
}

impl ScoringContext<'_> {
    pub(super) fn node(&self, mining_index: usize) -> ResourceNode {
        self.catalog[self.mining[mining_index].catalog_index].node
    }

    /// Scores a field's candidates. Without `prune`, every group is swept exhaustively, as the
    /// reference algorithm scores it. With it, ordinary candidates that provably cannot beat the
    /// best fully scored candidate are left in `pruned` instead; a group where pruning does not
    /// pay off within the flood budget is swept exhaustively too.
    pub(super) fn score_cluster(
        &self,
        cluster: Vec<usize>,
        scoring: &mut ScoringScratch,
        prune: bool,
    ) -> ScoredCluster {
        let depot = self.options.depot;
        let mut candidates = self.candidates(&cluster);
        let mineral_reference =
            category_reference(&cluster, self.mining, self.catalog, ResourceKind::Mineral);
        let gas_reference =
            category_reference(&cluster, self.mining, self.catalog, ResourceKind::Gas);
        let weights: Vec<_> = cluster
            .iter()
            .map(|&mining_index| {
                let node = self.node(mining_index);
                let reference = match node.kind {
                    ResourceKind::Mineral => mineral_reference,
                    ResourceKind::Gas => gas_reference,
                };
                u64::from(resource_weight(node, reference))
            })
            .collect();
        let mut mineral_weight_total = 0_u64;
        let mut gas_weight_total = 0_u64;
        for (&mining_index, &weight) in cluster.iter().zip(&weights) {
            match self.node(mining_index).kind {
                ResourceKind::Mineral => mineral_weight_total += weight,
                ResourceKind::Gas => gas_weight_total += weight,
            }
        }
        for candidate in &mut candidates {
            candidate.center_deviation = category_balanced_center_deviation(
                candidate.tile,
                depot,
                &cluster,
                self.mining,
                self.catalog,
                &weights,
            );
        }
        // A fully connected candidate's key distance is the sum of these times its distances.
        let key_weights: Vec<_> = cluster
            .iter()
            .zip(&weights)
            .map(|(&mining_index, &weight)| {
                let factor = match (gas_weight_total, self.node(mining_index).kind) {
                    (0, _) => 1,
                    (_, ResourceKind::Mineral) => gas_weight_total,
                    (_, ResourceKind::Gas) => mineral_weight_total,
                };
                u128::from(weight) * u128::from(factor)
            })
            .collect();
        let rings: Vec<_> = cluster
            .iter()
            .map(|&mining_index| {
                access_ring(
                    self.node(mining_index).bounds,
                    self.resource_grid.width(),
                    self.resource_grid.height(),
                )
            })
            .collect();
        // Floods never leave the walk cells within `PROXIMITY_RADIUS_CELLS` of the access rings,
        // so scoring grids only cover that window.
        let window = CellWindow::around(
            &rings,
            self.resource_grid.width(),
            self.resource_grid.height(),
        );

        // Only clear objects intersecting this footprint, never unrelated walls elsewhere.
        // Candidates with identical removal sets share one grid and one flood per resource.
        let mut groups: BTreeMap<(Vec<usize>, Vec<usize>), Vec<usize>> = BTreeMap::new();
        for (index, candidate) in candidates.iter().enumerate() {
            groups
                .entry((
                    candidate.cleared_obstacle_indices.clone(),
                    candidate.required_mineral_indices.clone(),
                ))
                .or_default()
                .push(index);
        }
        let resource_count = cluster.len();
        let tie = |candidate: &CandidateLocation| {
            (
                candidate.center_deviation,
                candidate.tile.y,
                candidate.tile.x,
            )
        };
        let mut incumbent: Option<CandidateKey> = None;
        let mut pruned = Vec::new();
        let mut retained = vec![false; candidates.len()];
        for ((cleared, cleared_minerals), members) in groups {
            let member_count = members.len();
            let pruned_before = pruned.len();
            let mut scores = GroupScores {
                resources: resource_count,
                values: Vec::with_capacity(member_count * resource_count),
                known: vec![Known::Bound; member_count * resource_count],
                settled: vec![0; member_count],
                bounds: Vec::with_capacity(member_count),
                status: vec![MemberStatus::Live; member_count],
            };
            for &candidate_index in &members {
                let footprint = footprint_cells(candidates[candidate_index].tile, depot);
                let mut bound = 0;
                for (ring, &key_weight) in rings.iter().zip(&key_weights) {
                    let lower = octile_gap(*ring, footprint);
                    scores.values.push(lower);
                    bound += key_weight * u128::from(lower);
                }
                scores.bounds.push(bound);
            }
            let prune_pass = |scores: &mut GroupScores,
                              incumbent: Option<CandidateKey>,
                              pruned: &mut Vec<PrunedCandidate>| {
                let Some(incumbent) = incumbent else {
                    return;
                };
                for (member, &candidate_index) in members.iter().enumerate() {
                    let candidate = &candidates[candidate_index];
                    if scores.status[member] != MemberStatus::Live || candidate.exact_start {
                        continue;
                    }
                    let (deviation, y, x) = tie(candidate);
                    let bound = (scores.bounds[member], deviation, y, x);
                    if bound > incumbent {
                        scores.status[member] = MemberStatus::Pruned;
                        pruned.push(PrunedCandidate {
                            tile: candidate.tile,
                            bound,
                        });
                    }
                }
            };
            if prune {
                prune_pass(&mut scores, incumbent, &mut pruned);
                if !scores.status.contains(&MemberStatus::Live) {
                    continue;
                }
            }

            let mut blockers: Vec<_> = self
                .obstacles
                .iter()
                .enumerate()
                .filter(|(index, _)| cleared.binary_search(index).is_err())
                .map(|(_, obstacle)| obstacle.bounds)
                .collect();
            blockers.extend(
                self.catalog
                    .iter()
                    .filter(|resource| {
                        is_clearable(resource.node, self.options)
                            && cleared_minerals
                                .binary_search(&resource.original_index)
                                .is_err()
                    })
                    .map(|resource| resource.node.bounds),
            );
            // Exhaustive scoring sweeps every group: its cost does not depend on pruning.
            if !prune {
                self.sweep_group(
                    scoring,
                    &cluster,
                    &weights,
                    &blockers,
                    &members,
                    &mut candidates,
                    &mut retained,
                );
                continue;
            }
            let remaining: Vec<_> = blockers
                .iter()
                .map(|&bounds| window.local_rect(bounds))
                .collect();
            let scoring_grid = stamp_obstacles(window.extract(self.resource_grid), &remaining);
            let accesses: Vec<_> = cluster
                .iter()
                .map(|&mining_index| {
                    perimeter_access_cells(
                        &scoring_grid,
                        window.local_rect(self.node(mining_index).bounds),
                    )
                })
                .collect();
            // An ordinary candidate survives only if every mining resource reaches it; only exact
            // starts need every distance. Dropped candidates' partial scores are never read.
            for (resource, access) in accesses.iter().enumerate() {
                if access.is_some() {
                    continue;
                }
                for (member, &candidate_index) in members.iter().enumerate() {
                    if candidates[candidate_index].exact_start {
                        let slot = scores.slot(member, resource);
                        scores.known[slot] = Known::Unreachable;
                    } else if scores.status[member] == MemberStatus::Live {
                        scores.status[member] = MemberStatus::Dead;
                    }
                }
            }
            if !scores.status.contains(&MemberStatus::Live) {
                continue;
            }
            // Registers the wanted members' perimeters before each flood.
            let register = |scoring: &mut ScoringScratch, wanted: &[usize]| {
                scoring.targets.set(
                    &scoring_grid,
                    wanted.iter().map(|&member| {
                        let tile = candidates[members[member]].tile;
                        let cells = footprint_perimeter(tile, depot)
                            .into_iter()
                            .filter_map(move |point| window.local_point(point));
                        (member, cells)
                    }),
                );
            };
            let mut wanted = Vec::new();
            let mut give_up = vec![u32::MAX; member_count];

            // Probe the most promising ordinary candidate so later floods have a bound. A probe
            // is only a heuristic, so give up on one far beyond its straight-line distance (likely
            // on another level) rather than exhausting the flood, and try the next one.
            // Pruning can fail to bite (probes on other levels, far exact starts, weak bounds).
            // Past a pop budget the group's floods would cost more than sweeping it exhaustively.
            let mut budget = FLOOD_POP_BUDGET;
            let mut over_budget = false;
            let mut probed = vec![false; member_count];
            'probes: for _ in 0..PROBE_ATTEMPTS {
                if incumbent.is_some() {
                    break;
                }
                let Some(probe) = (0..member_count)
                    .filter(|&member| {
                        scores.status[member] == MemberStatus::Live
                            && !probed[member]
                            && !candidates[members[member]].exact_start
                    })
                    .min_by_key(|&member| {
                        (scores.bounds[member], tie(&candidates[members[member]]))
                    })
                else {
                    break;
                };
                probed[probe] = true;
                for (resource, access) in accesses.iter().enumerate() {
                    let Some(access) = access else {
                        continue;
                    };
                    let slot = scores.slot(probe, resource);
                    let lower = scores.values[slot];
                    give_up[probe] = lower
                        .saturating_add(lower / 2)
                        .saturating_add(PROBE_SLACK_COST);
                    wanted.clear();
                    wanted.push(probe);
                    register(scoring, &wanted);
                    let end = scoring.flood_targets(
                        &scoring_grid,
                        access.iter().copied(),
                        PROXIMITY_MAX_COST,
                        &wanted,
                        &give_up,
                        budget,
                    );
                    if end == FloodEnd::OverBudget {
                        over_budget = true;
                        break 'probes;
                    }
                    budget -= scoring.pops;
                    self.record_flood(
                        &mut scores,
                        &members,
                        &candidates,
                        resource,
                        key_weights[resource],
                        &wanted,
                        scoring,
                        end,
                        None,
                    );
                    if scores.status[probe] != MemberStatus::Live
                        || scores.known[slot] != Known::Distance
                    {
                        break; // Dead or abandoned, so it cannot become the incumbent.
                    }
                }
                if scores.status[probe] == MemberStatus::Live
                    && scores.settled[probe] == resource_count
                {
                    let (deviation, y, x) = tie(&candidates[members[probe]]);
                    incumbent = Some((scores.bounds[probe], deviation, y, x));
                }
                prune_pass(&mut scores, incumbent, &mut pruned);
            }

            // Without an incumbent nothing can be pruned, so floods would cost more than a sweep.
            if incumbent.is_none() {
                over_budget = true;
            }
            for (resource, access) in accesses.iter().enumerate() {
                if over_budget {
                    break;
                }
                let Some(access) = access else {
                    continue;
                };
                let key_weight = key_weights[resource];
                wanted.clear();
                for member in 0..member_count {
                    let slot = scores.slot(member, resource);
                    if scores.status[member] != MemberStatus::Live
                        || scores.known[slot] != Known::Bound
                    {
                        continue;
                    }
                    give_up[member] = u32::MAX;
                    let candidate = &candidates[members[member]];
                    if candidate.exact_start {
                        wanted.push(member);
                        continue;
                    }
                    let Some((best, best_deviation, best_y, best_x)) = incumbent else {
                        wanted.push(member);
                        continue;
                    };
                    // Pruned once its key bound exceeds the incumbent: the distance from this
                    // resource may grow while `base + key_weight * distance <= allowed`.
                    let allowed = if tie(candidate) < (best_deviation, best_y, best_x) {
                        Some(best)
                    } else {
                        best.checked_sub(1)
                    };
                    let current = key_weight * u128::from(scores.values[slot]);
                    let base = scores.bounds[member] - current;
                    match allowed {
                        Some(allowed) if base + current <= allowed => {
                            wanted.push(member);
                            give_up[member] =
                                u32::try_from((allowed - base) / key_weight).unwrap_or(u32::MAX);
                        }
                        _ => {
                            scores.status[member] = MemberStatus::Pruned;
                            let (deviation, y, x) = tie(candidate);
                            pruned.push(PrunedCandidate {
                                tile: candidate.tile,
                                bound: (scores.bounds[member], deviation, y, x),
                            });
                        }
                    }
                }
                register(scoring, &wanted);
                let end = scoring.flood_targets(
                    &scoring_grid,
                    access.iter().copied(),
                    PROXIMITY_MAX_COST,
                    &wanted,
                    &give_up,
                    budget,
                );
                if end == FloodEnd::OverBudget {
                    over_budget = true;
                    break;
                }
                budget -= scoring.pops;
                self.record_flood(
                    &mut scores,
                    &members,
                    &candidates,
                    resource,
                    key_weight,
                    &wanted,
                    scoring,
                    end,
                    Some(&mut pruned),
                );
                for &member in &wanted {
                    let candidate = &candidates[members[member]];
                    if scores.status[member] == MemberStatus::Live
                        && !candidate.exact_start
                        && scores.settled[member] == resource_count
                    {
                        let (deviation, y, x) = tie(candidate);
                        let key = (scores.bounds[member], deviation, y, x);
                        incumbent = Some(incumbent.map_or(key, |best| best.min(key)));
                    }
                }
                prune_pass(&mut scores, incumbent, &mut pruned);
            }
            scoring.targets.clear();
            if over_budget {
                // Score the whole group exactly instead. Its pruned entries are superseded, and
                // its exact keys are valid incumbents.
                pruned.truncate(pruned_before);
                let best = self.sweep_group(
                    scoring,
                    &cluster,
                    &weights,
                    &blockers,
                    &members,
                    &mut candidates,
                    &mut retained,
                );
                if let Some(best) = best {
                    incumbent = Some(incumbent.map_or(best, |old| old.min(best)));
                }
                continue;
            }

            for (member, &candidate_index) in members.iter().enumerate() {
                if scores.status[member] != MemberStatus::Live {
                    continue;
                }
                let candidate = &mut candidates[candidate_index];
                for (resource, (&mining_index, &weight)) in cluster.iter().zip(&weights).enumerate()
                {
                    let slot = scores.slot(member, resource);
                    if scores.known[slot] != Known::Distance {
                        continue;
                    }
                    let distance = scores.values[slot];
                    candidate.connected += 1;
                    match self.node(mining_index).kind {
                        ResourceKind::Mineral => {
                            candidate.mineral_distance += u64::from(distance) * weight;
                            candidate.reachable_mineral_weight += weight;
                        }
                        ResourceKind::Gas => {
                            candidate.gas_distance += u64::from(distance) * weight;
                            candidate.reachable_gas_weight += weight;
                        }
                    }
                }
                // Exact starts can legitimately contain a buried resource patch. Keep those
                // mapped placements even when current static objects leave a mining perimeter
                // unreachable; ordinary heuristic candidates still require every mining resource
                // to connect.
                retained[candidate_index] = (candidate.exact_start && candidate.connected > 0)
                    || candidate.connected == resource_count;
            }
        }
        let candidates = candidates
            .into_iter()
            .zip(retained)
            .filter_map(|(candidate, retained)| retained.then_some(candidate))
            .collect();
        let resource_indices = cluster
            .iter()
            .map(|&index| self.catalog[self.mining[index].catalog_index].original_index)
            .collect();
        ScoredCluster {
            cluster,
            resource_indices,
            candidates,
            pruned,
            mineral_weight_total,
            gas_weight_total,
        }
    }

    /// Applies one flood's outcome to every member it wanted.
    #[allow(clippy::too_many_arguments)]
    fn record_flood(
        &self,
        scores: &mut GroupScores,
        members: &[usize],
        candidates: &[CandidateLocation],
        resource: usize,
        key_weight: u128,
        wanted: &[usize],
        scoring: &ScoringScratch,
        end: FloodEnd,
        pruned: Option<&mut Vec<PrunedCandidate>>,
    ) {
        let mut pruned = pruned;
        for &member in wanted {
            let candidate = &candidates[members[member]];
            if let Some(distance) = scoring.target_distance(member) {
                scores.settle(member, resource, distance, key_weight);
                continue;
            }
            match end {
                FloodEnd::GaveUp(cost) => {
                    // Every unsettled cell is at least `cost` away, past this member's limit.
                    // Probes stay live; a bounded member is pruned with its raised bound.
                    scores.raise(member, resource, cost, key_weight);
                    let Some(pruned) = pruned.as_deref_mut() else {
                        continue;
                    };
                    scores.status[member] = MemberStatus::Pruned;
                    pruned.push(PrunedCandidate {
                        tile: candidate.tile,
                        bound: (
                            scores.bounds[member],
                            candidate.center_deviation,
                            candidate.tile.y,
                            candidate.tile.x,
                        ),
                    });
                }
                FloodEnd::Settled | FloodEnd::Exhausted | FloodEnd::OverBudget => {
                    if candidate.exact_start {
                        let slot = scores.slot(member, resource);
                        scores.known[slot] = Known::Unreachable;
                    } else {
                        scores.status[member] = MemberStatus::Dead;
                    }
                }
            }
        }
    }
}
