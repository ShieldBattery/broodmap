//! Per-field candidate records and the final selection among them.

use std::{
    cmp::Reverse,
    collections::{BTreeMap, HashSet},
};

use super::{PROXIMITY_MAX_COST, ResourceNode, TilePosition};
use crate::WalkPosition;

#[derive(Debug, Copy, Clone)]
pub(super) struct CatalogResource {
    pub(super) original_index: usize,
    pub(super) node: ResourceNode,
}

#[derive(Debug, Clone)]
pub(super) struct MiningResource {
    pub(super) catalog_index: usize,
    pub(super) access: Vec<WalkPosition>,
}

#[derive(Debug, Clone)]
pub(super) struct CandidateLocation {
    pub(super) tile: TilePosition,
    pub(super) exact_start: bool,
    pub(super) required_mineral_indices: Vec<usize>,
    pub(super) cleared_obstacle_indices: Vec<usize>,
    pub(super) required_obstacle_indices: Vec<usize>,
    pub(super) start_cleared_obstacle_indices: Vec<usize>,
    pub(super) mineral_distance: u64,
    pub(super) gas_distance: u64,
    pub(super) center_deviation: u128,
    pub(super) connected: usize,
    pub(super) reachable_mineral_weight: u64,
    pub(super) reachable_gas_weight: u64,
}

impl CandidateLocation {
    pub(super) fn association_distance(&self, has_gas: bool) -> u128 {
        // Millionths of a fixed-point walk cost keep cross-cluster comparisons deterministic.
        // Exclude buried patches from the mean, but an entirely inaccessible category is costly.
        let mean = |distance: u64, weight: u64| {
            if weight == 0 {
                u128::from(PROXIMITY_MAX_COST) * 1_000_000
            } else {
                u128::from(distance) * 1_000_000 / u128::from(weight)
            }
        };
        let mineral = mean(self.mineral_distance, self.reachable_mineral_weight);
        if has_gas {
            (mineral + mean(self.gas_distance, self.reachable_gas_weight)) / 2
        } else {
            mineral
        }
    }
}

pub(super) struct ScoredCluster {
    /// Mining indices, kept so selection can rescore the field.
    pub(super) cluster: Vec<usize>,
    pub(super) resource_indices: Vec<usize>,
    pub(super) candidates: Vec<CandidateLocation>,
    pub(super) pruned: Vec<PrunedCandidate>,
    pub(super) mineral_weight_total: u64,
    pub(super) gas_weight_total: u64,
}

impl ScoredCluster {
    pub(super) fn score_denominator(&self) -> u128 {
        u128::from(self.mineral_weight_total)
            * u128::from(self.gas_weight_total.max(1))
            * if self.gas_weight_total > 0 { 2 } else { 1 }
    }

    /// Picks this field's site among the available candidates, or reports that a pruned
    /// candidate might beat every available scored one.
    pub(super) fn choose(
        &self,
        cluster_index: usize,
        used_tiles: &HashSet<TilePosition>,
        start_owners: &BTreeMap<TilePosition, (u128, u128, usize)>,
    ) -> Result<Option<usize>, Uncertain> {
        let available =
            |candidate: &(usize, &CandidateLocation)| !used_tiles.contains(&candidate.1.tile);
        let owned_start = self
            .candidates
            .iter()
            .enumerate()
            .filter(available)
            .filter(|(_, candidate)| {
                candidate.exact_start
                    && start_owners
                        .get(&candidate.tile)
                        .is_some_and(|owner| owner.2 == cluster_index)
            })
            .min_by_key(|(_, candidate)| {
                (Reverse(candidate.connected), self.candidate_key(candidate))
            });
        if let Some((index, _)) = owned_start {
            return Ok(Some(index));
        }
        let ordinary = self
            .candidates
            .iter()
            .enumerate()
            .filter(available)
            .filter(|(_, candidate)| {
                (!candidate.exact_start || !start_owners.contains_key(&candidate.tile))
                    && candidate.connected == self.resource_indices.len()
            })
            .min_by_key(|(_, candidate)| self.candidate_key(candidate));
        let key = ordinary.map(|(_, candidate)| self.candidate_key(candidate));
        let certain = self
            .pruned
            .iter()
            .filter(|pruned| !used_tiles.contains(&pruned.tile))
            .all(|pruned| key.is_some_and(|key| key < pruned.bound));
        if certain {
            Ok(ordinary.map(|(index, _)| index))
        } else {
            Err(Uncertain)
        }
    }

    fn candidate_key(&self, candidate: &CandidateLocation) -> CandidateKey {
        let distance = if self.gas_weight_total == 0 {
            u128::from(candidate.mineral_distance)
        } else {
            u128::from(candidate.mineral_distance) * u128::from(self.gas_weight_total)
                + u128::from(candidate.gas_distance) * u128::from(self.mineral_weight_total)
        };
        (
            distance,
            candidate.center_deviation,
            candidate.tile.y,
            candidate.tile.x,
        )
    }
}

/// `(distance, center deviation, tile y, tile x)`, the ordinary selection order.
pub(super) type CandidateKey = (u128, u128, u32, u32);

#[derive(Debug, Copy, Clone)]
pub(super) struct PrunedCandidate {
    pub(super) tile: TilePosition,
    /// A lower bound on the candidate's ordinary selection key.
    pub(super) bound: CandidateKey,
}

/// A pruned candidate may beat every scored alternative that is still available.
#[derive(Debug, Copy, Clone)]
pub(super) struct Uncertain;

/// The distance part of the ordinary selection key, as `ScoredCluster::candidate_key` builds it.
pub(super) fn key_distance(
    candidate: &CandidateLocation,
    mineral_total: u64,
    gas_total: u64,
) -> u128 {
    if gas_total == 0 {
        u128::from(candidate.mineral_distance)
    } else {
        u128::from(candidate.mineral_distance) * u128::from(gas_total)
            + u128::from(candidate.gas_distance) * u128::from(mineral_total)
    }
}

pub(super) fn tie_of(candidate: &CandidateLocation) -> (u128, u32, u32) {
    (
        candidate.center_deviation,
        candidate.tile.y,
        candidate.tile.x,
    )
}
