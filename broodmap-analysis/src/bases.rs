//! Deterministic, terrain-aware resource-base discovery.
//!
//! Placement is a static heuristic. It deliberately reports mineral blockers that must be cleared
//! instead of pretending that a conditional location is an immediately usable depot.

use std::{
    cmp::Reverse,
    collections::{BTreeMap, BinaryHeap, HashSet, VecDeque},
};

use thiserror::Error;

use crate::{PixelRect, StaticObstacle, TerrainGrid, WalkPosition};

const WALK_CELLS_PER_TILE: u32 = 4;
const PIXELS_PER_TILE: i64 = 32;
const PIXELS_PER_WALK_CELL: i64 = 8;
const MAX_RESOURCES: usize = 512;
const MAX_STARTS: usize = 64;
const CLUSTER_RADIUS_CELLS: u16 = 32;
const PROXIMITY_RADIUS_CELLS: u16 = 96;
// 96 orthogonal 8px walk-cell steps; scoring uses 1000/1414 octile costs.
const PROXIMITY_MAX_COST: u32 = PROXIMITY_RADIUS_CELLS as u32 * 1_000;
const START_RADIUS_CELLS: u16 = 48;
const START_ASSOCIATION_PIXELS: i64 = 6 * PIXELS_PER_TILE;
const SEARCH_RADIUS_TILES: i64 = 12;
const RESOURCE_GAP_PIXELS: i64 = 3 * PIXELS_PER_TILE;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
/// Logical map pixels, with the origin at the top-left corner.
pub struct PixelPosition {
    pub x: u32,
    pub y: u32,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
/// Build-tile coordinates (32 logical pixels per tile).
pub struct TilePosition {
    pub x: u32,
    pub y: u32,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum ResourceKind {
    Mineral,
    Gas,
}

/// Caller-normalized, half-open resource collision bounds. `amount` is remaining mineral count
/// when available; unknown amounts stay permanent for discovery purposes.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ResourceNode {
    pub bounds: PixelRect,
    pub kind: ResourceKind,
    pub amount: Option<u32>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct DepotFootprint {
    pub width_tiles: u32,
    pub height_tiles: u32,
}

impl Default for DepotFootprint {
    fn default() -> Self {
        Self {
            width_tiles: 4,
            height_tiles: 3,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct BaseSearchOptions {
    pub depot: DepotFootprint,
    /// Known mineral patches at or below this amount may be treated as clearance prerequisites.
    /// `None` disables conditional mineral clearing entirely.
    pub max_mineral_blocker_amount: Option<u32>,
    /// Permit destruction of non-resource objects overlapping the proposed footprint.
    pub allow_destructible_clearing: bool,
    /// At an exact start, permit the engine's automatic removal of overlapping non-resource objects.
    /// This is conditional on the start being occupied; current route occupancy is unchanged.
    pub allow_start_obstacle_clearing: bool,
}

impl Default for BaseSearchOptions {
    fn default() -> Self {
        Self {
            depot: DepotFootprint::default(),
            max_mineral_blocker_amount: Some(8),
            allow_destructible_clearing: true,
            allow_start_obstacle_clearing: true,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BaseCandidate {
    /// Top-left build tile of the proposed depot.
    pub depot_tile: TilePosition,
    /// The current all-object route grid can leave a conditional depot completely blocked.
    pub route_anchor: Option<WalkPosition>,
    /// Mining resources, indexed into the original resource slice.
    pub resource_indices: Vec<usize>,
    /// Low, known mineral patches directly preventing this placement.
    pub required_mineral_indices: Vec<usize>,
    /// Destructible objects overlapping this footprint, indexed into the input obstacle slice.
    pub required_obstacle_indices: Vec<usize>,
    /// Objects removed automatically if this exact start is occupied, indexed into obstacles.
    pub start_cleared_obstacle_indices: Vec<usize>,
    /// Nearby connected starts, indexed into the original start slice.
    pub start_indices: Vec<usize>,
}

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct BaseDiscovery {
    pub bases: Vec<BaseCandidate>,
    pub unplaced_clusters: usize,
    pub ignored_resources: usize,
}

#[derive(Debug, Error, Copy, Clone, Eq, PartialEq)]
pub enum BaseError {
    #[error("base discovery requires a tile-aligned walk grid, got {width}x{height} walk cells")]
    GridNotTileAligned { width: u32, height: u32 },
    #[error(
        "depot footprint must be nonzero and fit the map, got {width_tiles}x{height_tiles} tiles on {map_width_tiles}x{map_height_tiles}"
    )]
    InvalidFootprint {
        width_tiles: u32,
        height_tiles: u32,
        map_width_tiles: u32,
        map_height_tiles: u32,
    },
    #[error("base discovery accepts at most {maximum} resources, got {actual}")]
    TooManyResources { maximum: usize, actual: usize },
    #[error("base discovery accepts at most {maximum} start positions, got {actual}")]
    TooManyStarts { maximum: usize, actual: usize },
}

/// Discovers mineral fields with bounded searches.
///
/// `terrain` is the unmodified terrain grid; `obstacles` contains only non-resource objects.
/// Resource occupancy is added internally. Returned indices refer to the original input slices.
///
/// Cluster identity is terrain-only, so a nearby cliff does not merge fields. Scoring instead uses
/// a static grid with non-resource objects and permanent resources stamped into it. Low, known
/// mineral patches at the configured threshold are omitted only when listed in that candidate's
/// `required_mineral_indices`. Other patches remain blocked. Starts are assigned to one reachable
/// cluster before selection, and output depot tiles and start associations are unique.
pub fn discover_bases(
    terrain: &TerrainGrid,
    resources: &[ResourceNode],
    obstacles: &[StaticObstacle],
    starts: &[PixelPosition],
    options: &BaseSearchOptions,
) -> Result<BaseDiscovery, BaseError> {
    if resources.len() > MAX_RESOURCES {
        return Err(BaseError::TooManyResources {
            maximum: MAX_RESOURCES,
            actual: resources.len(),
        });
    }
    if starts.len() > MAX_STARTS {
        return Err(BaseError::TooManyStarts {
            maximum: MAX_STARTS,
            actual: starts.len(),
        });
    }
    if !terrain.width().is_multiple_of(WALK_CELLS_PER_TILE)
        || !terrain.height().is_multiple_of(WALK_CELLS_PER_TILE)
    {
        return Err(BaseError::GridNotTileAligned {
            width: terrain.width(),
            height: terrain.height(),
        });
    }
    let map_width_tiles = terrain.width() / WALK_CELLS_PER_TILE;
    let map_height_tiles = terrain.height() / WALK_CELLS_PER_TILE;
    let depot = options.depot;
    if depot.width_tiles == 0
        || depot.height_tiles == 0
        || depot.width_tiles > map_width_tiles
        || depot.height_tiles > map_height_tiles
    {
        return Err(BaseError::InvalidFootprint {
            width_tiles: depot.width_tiles,
            height_tiles: depot.height_tiles,
            map_width_tiles,
            map_height_tiles,
        });
    }

    let map_width_pixels = i64::from(terrain.width()) * PIXELS_PER_WALK_CELL;
    let map_height_pixels = i64::from(terrain.height()) * PIXELS_PER_WALK_CELL;
    let valid_starts: Vec<_> = starts
        .iter()
        .copied()
        .enumerate()
        .filter(|(_, point)| {
            i64::from(point.x) < map_width_pixels && i64::from(point.y) < map_height_pixels
        })
        .collect();

    let mut ignored_resources = 0;
    let mut seen_bounds = HashSet::with_capacity(resources.len());
    let mut catalog = Vec::with_capacity(resources.len());
    for (original_index, node) in resources.iter().copied().enumerate() {
        if !rect_is_in_map(node.bounds, map_width_pixels, map_height_pixels)
            || !seen_bounds.insert(node.bounds)
        {
            ignored_resources += 1;
            continue;
        }
        catalog.push(CatalogResource {
            original_index,
            node,
        });
    }
    if catalog.is_empty() {
        return Ok(BaseDiscovery {
            bases: Vec::new(),
            unplaced_clusters: 0,
            ignored_resources,
        });
    }

    let all_resource_bounds: Vec<_> = catalog
        .iter()
        .map(|resource| resource.node.bounds)
        .collect();
    let permanent_bounds: Vec<_> = catalog
        .iter()
        .filter(|resource| !is_clearable(resource.node, options))
        .map(|resource| resource.node.bounds)
        .collect();
    let obstacle_bounds: Vec<_> = obstacles.iter().map(|obstacle| obstacle.bounds).collect();
    let current_grid =
        terrain.with_obstacles(&combined_rects(&obstacle_bounds, &all_resource_bounds));
    let resource_grid = terrain.with_obstacles(&permanent_bounds);
    let availability = AvailabilityPrefix::new(&resource_grid);

    // Low known minerals never enter a mining cluster and therefore cannot bridge normal fields.
    let mut mining = Vec::new();
    for (catalog_index, resource) in catalog.iter().enumerate() {
        if is_clearable(resource.node, options) {
            continue;
        }
        if let Some(access) = perimeter_access_cells(terrain, resource.node.bounds) {
            mining.push(MiningResource {
                catalog_index,
                access,
            });
        }
    }
    if mining.is_empty() {
        return Ok(BaseDiscovery {
            bases: Vec::new(),
            unplaced_clusters: 0,
            ignored_resources,
        });
    }

    let mut scratch = SearchScratch::new(terrain.cells().len());
    let mut sets = DisjointSet::new(mining.len());
    for source in 0..mining.len() {
        scratch.flood(
            terrain,
            mining[source].access.iter().copied(),
            CLUSTER_RADIUS_CELLS,
        );
        for (target, candidate) in mining.iter().enumerate().skip(source + 1) {
            if candidate
                .access
                .iter()
                .any(|point| scratch.reached(terrain.index(*point).expect("access in map")))
            {
                sets.union(source, target);
            }
        }
    }
    let clusters = clusters_from_sets(&mut sets, mining.len());
    let mut result = BaseDiscovery {
        bases: Vec::new(),
        unplaced_clusters: 0,
        ignored_resources,
    };

    let mut scoring = ScoringScratch::new(terrain.cells().len());
    let mut scored_clusters = Vec::new();
    for cluster in clusters {
        let mineral_count = cluster
            .iter()
            .filter(|&&index| {
                catalog[mining[index].catalog_index].node.kind == ResourceKind::Mineral
            })
            .count();
        if mineral_count < 4 {
            continue;
        }
        let center = cluster_center(&cluster, &mining, &catalog);
        let mut candidates: Vec<_> =
            candidate_tiles(center, depot, map_width_tiles, map_height_tiles)
                .into_iter()
                .filter_map(|tile| {
                    let required = required_clearances(tile, depot, &catalog, options);
                    let no_permanent_resource = catalog.iter().all(|resource| {
                        is_clearable(resource.node, options)
                            || !intersects_gap(tile, depot, resource.node.bounds)
                    });
                    if !no_permanent_resource || !footprint_is_available(tile, depot, &availability)
                    {
                        return None;
                    }
                    let exact_start = valid_starts
                        .iter()
                        .any(|(_, start)| start_depot_tile(*start, depot) == Some(tile));
                    let mut cleared_obstacle_indices = Vec::new();
                    let mut required_obstacle_indices = Vec::new();
                    let mut start_cleared_obstacle_indices = Vec::new();
                    for (index, obstacle) in obstacles.iter().enumerate() {
                        if !intersects_footprint(tile, depot, obstacle.bounds) {
                            continue;
                        }
                        if exact_start && options.allow_start_obstacle_clearing {
                            start_cleared_obstacle_indices.push(index);
                        } else if obstacle.destructible && options.allow_destructible_clearing {
                            required_obstacle_indices.push(index);
                        } else {
                            return None;
                        }
                        cleared_obstacle_indices.push(index);
                    }
                    Some(CandidateLocation {
                        tile,
                        exact_start,
                        perimeter: footprint_perimeter(tile, depot),
                        required_mineral_indices: required,
                        cleared_obstacle_indices,
                        required_obstacle_indices,
                        start_cleared_obstacle_indices,
                        mineral_distance: 0,
                        gas_distance: 0,
                        center_deviation: 0,
                        connected: 0,
                        reachable_mineral_weight: 0,
                        reachable_gas_weight: 0,
                    })
                })
                .collect();

        let mineral_reference =
            category_reference(&cluster, &mining, &catalog, ResourceKind::Mineral);
        let gas_reference = category_reference(&cluster, &mining, &catalog, ResourceKind::Gas);
        let weights: Vec<_> = cluster
            .iter()
            .map(|&mining_index| {
                let node = catalog[mining[mining_index].catalog_index].node;
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
            match catalog[mining[mining_index].catalog_index].node.kind {
                ResourceKind::Mineral => mineral_weight_total += weight,
                ResourceKind::Gas => gas_weight_total += weight,
            }
        }
        for candidate in &mut candidates {
            candidate.center_deviation = category_balanced_center_deviation(
                candidate.tile,
                depot,
                &cluster,
                &mining,
                &catalog,
                &weights,
            );
        }

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
        for ((cleared, cleared_minerals), candidate_indices) in groups {
            let remaining: Vec<_> = obstacles
                .iter()
                .enumerate()
                .filter(|(index, _)| cleared.binary_search(index).is_err())
                .map(|(_, obstacle)| obstacle.bounds)
                .collect();
            let mut remaining = remaining;
            remaining.extend(
                catalog
                    .iter()
                    .filter(|resource| {
                        is_clearable(resource.node, options)
                            && cleared_minerals
                                .binary_search(&resource.original_index)
                                .is_err()
                    })
                    .map(|resource| resource.node.bounds),
            );
            let scoring_grid = resource_grid.with_obstacles(&remaining);
            for (&mining_index, &weight) in cluster.iter().zip(&weights) {
                let resource = &catalog[mining[mining_index].catalog_index];
                let Some(access) = perimeter_access_cells(&scoring_grid, resource.node.bounds)
                else {
                    continue;
                };
                scoring.flood(&scoring_grid, access, PROXIMITY_MAX_COST);
                for &candidate_index in &candidate_indices {
                    let candidate = &mut candidates[candidate_index];
                    let distance = candidate
                        .perimeter
                        .iter()
                        .filter_map(|point| {
                            scoring.distance(
                                scoring_grid
                                    .index(*point)
                                    .expect("footprint perimeter in map"),
                            )
                        })
                        .min();
                    if let Some(distance) = distance {
                        candidate.connected += 1;
                        match resource.node.kind {
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
                }
            }
        }
        // Exact starts can legitimately contain a buried resource patch. Keep those mapped
        // placements even when current static objects leave a mining perimeter unreachable;
        // ordinary heuristic candidates still require every mining resource to connect.
        candidates.retain(|candidate| {
            (candidate.exact_start && candidate.connected > 0)
                || candidate.connected == cluster.len()
        });
        for candidate in &mut candidates {
            candidate.perimeter = Vec::new(); // Selection no longer needs per-cell scoring data.
        }
        let resource_indices: Vec<_> = cluster
            .iter()
            .map(|&index| catalog[mining[index].catalog_index].original_index)
            .collect();
        scored_clusters.push(ScoredCluster {
            resource_indices,
            candidates,
            mineral_weight_total,
            gas_weight_total,
        });
    }

    // A start belongs to the closest currently accessible field, even when another field's
    // candidate window contains it. Normalize across fields with different sizes/amounts.
    scored_clusters.sort_by_key(|cluster| {
        cluster
            .resource_indices
            .iter()
            .map(|&i| {
                let r = resources[i].bounds;
                (r.top, r.left, r.bottom, r.right)
            })
            .min()
            .expect("nonempty cluster")
    });
    let mut start_owners: BTreeMap<TilePosition, (u128, u128, usize)> = BTreeMap::new();
    for (cluster_index, cluster) in scored_clusters.iter().enumerate() {
        for candidate in cluster.candidates.iter().filter(|c| c.exact_start) {
            let key = (
                candidate.association_distance(cluster.gas_weight_total > 0),
                candidate.center_deviation / cluster.score_denominator(),
                cluster_index,
            );
            let owner = start_owners.entry(candidate.tile).or_insert(key);
            if key < *owner {
                *owner = key;
            }
        }
    }
    // Reserve start-owned sites before choosing ordinary sites. Distinct fields never emit the
    // same depot tile; if an ordinary optimum is taken, try its next fully reachable candidate.
    let mut cluster_order: Vec<_> = (0..scored_clusters.len()).collect();
    cluster_order
        .sort_by_key(|&index| (!start_owners.values().any(|owner| owner.2 == index), index));
    let mut used_tiles = HashSet::new();
    let mut selected_cluster_indices = Vec::new();
    for cluster_index in cluster_order {
        let cluster = &scored_clusters[cluster_index];
        let available = |candidate: &&CandidateLocation| !used_tiles.contains(&candidate.tile);
        let owned_start = |candidate: &&CandidateLocation| {
            candidate.exact_start
                && start_owners
                    .get(&candidate.tile)
                    .is_some_and(|owner| owner.2 == cluster_index)
        };
        let chosen = cluster
            .candidates
            .iter()
            .filter(available)
            .filter(owned_start)
            .min_by_key(|candidate| {
                (
                    Reverse(candidate.connected),
                    cluster.candidate_key(candidate),
                )
            })
            .or_else(|| {
                cluster
                    .candidates
                    .iter()
                    .filter(available)
                    .filter(|candidate| {
                        (!candidate.exact_start || !start_owners.contains_key(&candidate.tile))
                            && candidate.connected == cluster.resource_indices.len()
                    })
                    .min_by_key(|candidate| cluster.candidate_key(candidate))
            });
        let Some(chosen) = chosen else {
            result.unplaced_clusters += 1;
            continue;
        };
        // A cluster emits one site. Release its unused start alternatives so a later fully
        // connected field can use them, rather than reserving starts that were never selected.
        start_owners.retain(|&tile, owner| owner.2 != cluster_index || tile == chosen.tile);
        if chosen.exact_start {
            start_owners.insert(chosen.tile, (0, 0, cluster_index));
        }
        used_tiles.insert(chosen.tile);
        selected_cluster_indices.push(cluster_index);
        result.bases.push(BaseCandidate {
            depot_tile: chosen.tile,
            route_anchor: current_footprint_anchor(chosen.tile, depot, &current_grid),
            resource_indices: cluster.resource_indices.clone(),
            required_mineral_indices: chosen.required_mineral_indices.clone(),
            required_obstacle_indices: chosen.required_obstacle_indices.clone(),
            start_cleared_obstacle_indices: chosen.start_cleared_obstacle_indices.clone(),
            start_indices: Vec::new(),
        });
    }
    // A nearby-start label is also exclusive. Exact ownership takes priority; otherwise choose
    // the nearest connected site within the existing association radius.
    let mut associations = vec![None; starts.len()];
    for (base_index, base) in result.bases.iter().enumerate() {
        if let Some(anchor) = base.route_anchor {
            scratch.flood(&current_grid, [anchor], START_RADIUS_CELLS);
        }
        for &(start_index, start) in &valid_starts {
            let tile = start_depot_tile(start, depot);
            if tile
                .and_then(|tile| start_owners.get(&tile))
                .is_some_and(|owner| owner.2 != selected_cluster_indices[base_index])
            {
                continue;
            }
            let exact = tile == Some(base.depot_tile);
            let connected = base.route_anchor.is_some()
                && start_near_depot(start, base.depot_tile, depot)
                && nearest_walkable(&current_grid, start).is_some_and(|point| {
                    scratch.reached(current_grid.index(point).expect("start in map"))
                });
            if exact || connected {
                let dx = i64::from(start.x)
                    - (i64::from(base.depot_tile.x) * 32 + i64::from(depot.width_tiles) * 16);
                let dy = i64::from(start.y)
                    - (i64::from(base.depot_tile.y) * 32 + i64::from(depot.height_tiles) * 16);
                let key = (
                    !exact,
                    dx * dx + dy * dy,
                    base.depot_tile.y,
                    base.depot_tile.x,
                    base_index,
                );
                if associations[start_index].is_none_or(|old| key < old) {
                    associations[start_index] = Some(key);
                }
            }
        }
    }
    for (start_index, association) in associations.into_iter().enumerate() {
        if let Some((_, _, _, _, base_index)) = association {
            result.bases[base_index].start_indices.push(start_index);
        }
    }

    result
        .bases
        .sort_by_key(|base| (base.depot_tile.y, base.depot_tile.x));
    Ok(result)
}

#[derive(Debug, Copy, Clone)]
struct CatalogResource {
    original_index: usize,
    node: ResourceNode,
}

#[derive(Debug, Clone)]
struct MiningResource {
    catalog_index: usize,
    access: Vec<WalkPosition>,
}

#[derive(Debug, Clone)]
struct CandidateLocation {
    tile: TilePosition,
    exact_start: bool,
    perimeter: Vec<WalkPosition>,
    required_mineral_indices: Vec<usize>,
    cleared_obstacle_indices: Vec<usize>,
    required_obstacle_indices: Vec<usize>,
    start_cleared_obstacle_indices: Vec<usize>,
    mineral_distance: u64,
    gas_distance: u64,
    center_deviation: u128,
    connected: usize,
    reachable_mineral_weight: u64,
    reachable_gas_weight: u64,
}

impl CandidateLocation {
    fn association_distance(&self, has_gas: bool) -> u128 {
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

struct ScoredCluster {
    resource_indices: Vec<usize>,
    candidates: Vec<CandidateLocation>,
    mineral_weight_total: u64,
    gas_weight_total: u64,
}

impl ScoredCluster {
    fn score_denominator(&self) -> u128 {
        u128::from(self.mineral_weight_total)
            * u128::from(self.gas_weight_total.max(1))
            * if self.gas_weight_total > 0 { 2 } else { 1 }
    }

    fn candidate_key(&self, candidate: &CandidateLocation) -> (u128, u128, u32, u32) {
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

fn category_balanced_center_deviation(
    tile: TilePosition,
    depot: DepotFootprint,
    cluster: &[usize],
    mining: &[MiningResource],
    catalog: &[CatalogResource],
    weights: &[u64],
) -> u128 {
    let depot_x_twice = i128::from(tile.x) * 64 + i128::from(depot.width_tiles) * 32;
    let depot_y_twice = i128::from(tile.y) * 64 + i128::from(depot.height_tiles) * 32;
    let mut mineral = 0_u128;
    let mut gas = 0_u128;
    let mut mineral_weight_total = 0_u64;
    let mut gas_weight_total = 0_u64;
    for (&mining_index, &weight) in cluster.iter().zip(weights) {
        let node = catalog[mining[mining_index].catalog_index].node;
        let dx = i128::from(node.bounds.left) + i128::from(node.bounds.right) - depot_x_twice;
        let dy = i128::from(node.bounds.top) + i128::from(node.bounds.bottom) - depot_y_twice;
        let squared = (dx * dx + dy * dy) as u128;
        let weighted = squared.saturating_mul(u128::from(weight));
        match node.kind {
            ResourceKind::Mineral => {
                mineral = mineral.saturating_add(weighted);
                mineral_weight_total += weight;
            }
            ResourceKind::Gas => {
                gas = gas.saturating_add(weighted);
                gas_weight_total += weight;
            }
        }
    }
    if gas_weight_total == 0 {
        mineral
    } else {
        mineral
            .saturating_mul(u128::from(gas_weight_total))
            .saturating_add(gas.saturating_mul(u128::from(mineral_weight_total)))
    }
}

fn category_reference(
    cluster: &[usize],
    mining: &[MiningResource],
    catalog: &[CatalogResource],
    kind: ResourceKind,
) -> u32 {
    let mut amounts: Vec<_> = cluster
        .iter()
        .filter_map(|&index| {
            let node = catalog[mining[index].catalog_index].node;
            (node.kind == kind)
                .then_some(node.amount)
                .flatten()
                .filter(|&amount| amount > 0)
        })
        .collect();
    amounts.sort_unstable();
    amounts.get(amounts.len() / 2).copied().unwrap_or(1)
}

fn resource_weight(node: ResourceNode, reference: u32) -> u32 {
    node.amount.unwrap_or(reference).min(reference).max(1)
}

fn is_clearable(node: ResourceNode, options: &BaseSearchOptions) -> bool {
    node.kind == ResourceKind::Mineral
        && node
            .amount
            .zip(options.max_mineral_blocker_amount)
            .is_some_and(|(amount, maximum)| amount <= maximum)
}

fn combined_rects(objects: &[PixelRect], resources: &[PixelRect]) -> Vec<PixelRect> {
    let mut result = Vec::with_capacity(objects.len() + resources.len());
    result.extend_from_slice(objects);
    result.extend_from_slice(resources);
    result
}

fn rect_is_in_map(bounds: PixelRect, width: i64, height: i64) -> bool {
    i64::from(bounds.left) >= 0
        && i64::from(bounds.top) >= 0
        && i64::from(bounds.right) > i64::from(bounds.left)
        && i64::from(bounds.bottom) > i64::from(bounds.top)
        && i64::from(bounds.right) <= width
        && i64::from(bounds.bottom) <= height
}

fn clusters_from_sets(sets: &mut DisjointSet, count: usize) -> Vec<Vec<usize>> {
    let mut roots = vec![None; count];
    let mut clusters = Vec::new();
    for index in 0..count {
        let root = sets.find(index);
        let slot = match roots[root] {
            Some(slot) => slot,
            None => {
                let slot = clusters.len();
                clusters.push(Vec::new());
                roots[root] = Some(slot);
                slot
            }
        };
        clusters[slot].push(index);
    }
    clusters
}

fn perimeter_access_cells(terrain: &TerrainGrid, bounds: PixelRect) -> Option<Vec<WalkPosition>> {
    let left = u32::try_from(bounds.left).ok()? / 8;
    let right = u32::try_from(bounds.right - 1).ok()? / 8;
    let top = u32::try_from(bounds.top).ok()? / 8;
    let bottom = u32::try_from(bounds.bottom - 1).ok()? / 8;
    let mut result = Vec::new();
    for y in top.saturating_sub(1)..=bottom.saturating_add(1).min(terrain.height() - 1) {
        for x in left.saturating_sub(1)..=right.saturating_add(1).min(terrain.width() - 1) {
            if (left..=right).contains(&x) && (top..=bottom).contains(&y) {
                continue;
            }
            let point = WalkPosition { x, y };
            if terrain.cell(point).is_some_and(|cell| cell.walkable) {
                result.push(point);
            }
        }
    }
    (!result.is_empty()).then_some(result)
}

fn cluster_center(
    cluster: &[usize],
    mining: &[MiningResource],
    catalog: &[CatalogResource],
) -> PixelPosition {
    let (sum_x, sum_y) = cluster.iter().fold((0_i64, 0_i64), |(x, y), &index| {
        let bounds = catalog[mining[index].catalog_index].node.bounds;
        (
            x + i64::from(bounds.left) + i64::from(bounds.right),
            y + i64::from(bounds.top) + i64::from(bounds.bottom),
        )
    });
    let divisor = i64::try_from(cluster.len()).expect("cluster length fits i64");
    PixelPosition {
        x: u32::try_from(sum_x / divisor / 2).expect("center in map"),
        y: u32::try_from(sum_y / divisor / 2).expect("center in map"),
    }
}

fn candidate_tiles(
    center: PixelPosition,
    depot: DepotFootprint,
    map_width: u32,
    map_height: u32,
) -> Vec<TilePosition> {
    let center_x =
        (i64::from(center.x) - i64::from(depot.width_tiles) * 16).div_euclid(PIXELS_PER_TILE);
    let center_y =
        (i64::from(center.y) - i64::from(depot.height_tiles) * 16).div_euclid(PIXELS_PER_TILE);
    let min_x = (center_x - SEARCH_RADIUS_TILES).max(0);
    let max_x = (center_x + SEARCH_RADIUS_TILES).min(i64::from(map_width - depot.width_tiles));
    let min_y = (center_y - SEARCH_RADIUS_TILES).max(0);
    let max_y = (center_y + SEARCH_RADIUS_TILES).min(i64::from(map_height - depot.height_tiles));
    let mut result = Vec::new();
    for y in min_y..=max_y {
        for x in min_x..=max_x {
            result.push(TilePosition {
                x: u32::try_from(x).expect("clamped"),
                y: u32::try_from(y).expect("clamped"),
            });
        }
    }
    result
}

fn footprint_is_available(
    tile: TilePosition,
    depot: DepotFootprint,
    prefix: &AvailabilityPrefix,
) -> bool {
    let left = tile.x as usize * 4;
    let top = tile.y as usize * 4;
    let right = left + depot.width_tiles as usize * 4;
    let bottom = top + depot.height_tiles as usize * 4;
    prefix.count(left, top, right, bottom) == (right - left) * (bottom - top)
}

fn intersects_footprint(tile: TilePosition, depot: DepotFootprint, bounds: PixelRect) -> bool {
    let left = i64::from(tile.x) * 32;
    let top = i64::from(tile.y) * 32;
    let right = (i64::from(tile.x) + i64::from(depot.width_tiles)) * 32;
    let bottom = (i64::from(tile.y) + i64::from(depot.height_tiles)) * 32;
    bounds.left < bounds.right
        && bounds.top < bounds.bottom
        && i64::from(bounds.left) < right
        && i64::from(bounds.right) > left
        && i64::from(bounds.top) < bottom
        && i64::from(bounds.bottom) > top
}

fn intersects_gap(tile: TilePosition, depot: DepotFootprint, bounds: PixelRect) -> bool {
    let left = i64::from(tile.x) * 32 - RESOURCE_GAP_PIXELS;
    let top = i64::from(tile.y) * 32 - RESOURCE_GAP_PIXELS;
    // SC:R queries half-open bounds; touching this right/bottom endpoint is legal.
    let right = (i64::from(tile.x) + i64::from(depot.width_tiles)) * 32 + RESOURCE_GAP_PIXELS;
    let bottom = (i64::from(tile.y) + i64::from(depot.height_tiles)) * 32 + RESOURCE_GAP_PIXELS;
    i64::from(bounds.left) < right
        && i64::from(bounds.right) > left
        && i64::from(bounds.top) < bottom
        && i64::from(bounds.bottom) > top
}

fn required_clearances(
    tile: TilePosition,
    depot: DepotFootprint,
    catalog: &[CatalogResource],
    options: &BaseSearchOptions,
) -> Vec<usize> {
    catalog
        .iter()
        .filter(|resource| {
            is_clearable(resource.node, options)
                && intersects_gap(tile, depot, resource.node.bounds)
        })
        .map(|resource| resource.original_index)
        .collect()
}

fn footprint_perimeter(tile: TilePosition, depot: DepotFootprint) -> Vec<WalkPosition> {
    let left = tile.x * 4;
    let top = tile.y * 4;
    let right = left + depot.width_tiles * 4;
    let bottom = top + depot.height_tiles * 4;
    let mut result = Vec::with_capacity((right - left + bottom - top).saturating_mul(2) as usize);
    for x in left..right {
        result.push(WalkPosition { x, y: top });
    }
    for y in top + 1..bottom {
        result.push(WalkPosition { x: right - 1, y });
    }
    if bottom > top + 1 {
        for x in (left..right - 1).rev() {
            result.push(WalkPosition { x, y: bottom - 1 });
        }
    }
    if right > left + 1 {
        for y in (top + 1..bottom - 1).rev() {
            result.push(WalkPosition { x: left, y });
        }
    }
    result
}

fn current_footprint_anchor(
    tile: TilePosition,
    depot: DepotFootprint,
    terrain: &TerrainGrid,
) -> Option<WalkPosition> {
    let left = tile.x * 4;
    let top = tile.y * 4;
    let width = depot.width_tiles * 4;
    let height = depot.height_tiles * 4;
    let center_x = i64::from(left) * 2 + i64::from(width);
    let center_y = i64::from(top) * 2 + i64::from(height);
    let mut best = None;
    for y in top..top + height {
        for x in left..left + width {
            let point = WalkPosition { x, y };
            if !terrain.cell(point).is_some_and(|cell| cell.walkable) {
                continue;
            }
            let dx = i64::from(x) * 2 + 1 - center_x;
            let dy = i64::from(y) * 2 + 1 - center_y;
            let score = (dx * dx + dy * dy, y, x);
            if best.is_none_or(|(prior, _)| score < prior) {
                best = Some((score, point));
            }
        }
    }
    best.map(|(_, point)| point)
}

fn start_depot_tile(start: PixelPosition, depot: DepotFootprint) -> Option<TilePosition> {
    let x = (i64::from(start.x) - i64::from(depot.width_tiles) * 16).div_euclid(32);
    let y = (i64::from(start.y) - i64::from(depot.height_tiles) * 16).div_euclid(32);
    (x >= 0 && y >= 0).then(|| TilePosition {
        x: u32::try_from(x).expect("nonnegative"),
        y: u32::try_from(y).expect("nonnegative"),
    })
}

fn start_near_depot(start: PixelPosition, tile: TilePosition, depot: DepotFootprint) -> bool {
    let dx = i64::from(start.x) - (i64::from(tile.x) * 32 + i64::from(depot.width_tiles) * 16);
    let dy = i64::from(start.y) - (i64::from(tile.y) * 32 + i64::from(depot.height_tiles) * 16);
    dx * dx + dy * dy <= START_ASSOCIATION_PIXELS * START_ASSOCIATION_PIXELS
}

fn nearest_walkable(terrain: &TerrainGrid, point: PixelPosition) -> Option<WalkPosition> {
    let center = WalkPosition {
        x: point.x / 8,
        y: point.y / 8,
    };
    let mut best = None;
    for y in center.y.saturating_sub(2)..=center.y.saturating_add(2).min(terrain.height() - 1) {
        for x in center.x.saturating_sub(2)..=center.x.saturating_add(2).min(terrain.width() - 1) {
            let candidate = WalkPosition { x, y };
            if !terrain.cell(candidate).is_some_and(|cell| cell.walkable) {
                continue;
            }
            let dx = i64::from(x) * 8 + 4 - i64::from(point.x);
            let dy = i64::from(y) * 8 + 4 - i64::from(point.y);
            let score = (dx * dx + dy * dy, y, x);
            if best.is_none_or(|(prior, _)| score < prior) {
                best = Some((score, candidate));
            }
        }
    }
    best.map(|(_, point)| point)
}

struct AvailabilityPrefix {
    width: usize,
    values: Vec<usize>,
}
impl AvailabilityPrefix {
    fn new(terrain: &TerrainGrid) -> Self {
        let width = terrain.width() as usize;
        let height = terrain.height() as usize;
        let mut values = vec![0; (width + 1) * (height + 1)];
        for y in 0..height {
            for x in 0..width {
                let available = terrain.cells()[y * width + x].walkable
                    && terrain.cells()[y * width + x].terrain_buildable;
                let index = (y + 1) * (width + 1) + x + 1;
                values[index] =
                    usize::from(available) + values[index - 1] + values[index - width - 1]
                        - values[index - width - 2];
            }
        }
        Self { width, values }
    }
    fn count(&self, left: usize, top: usize, right: usize, bottom: usize) -> usize {
        let stride = self.width + 1;
        self.values[bottom * stride + right] + self.values[top * stride + left]
            - self.values[top * stride + right]
            - self.values[bottom * stride + left]
    }
}

struct SearchScratch {
    epoch: u32,
    seen: Vec<u32>,
    distances: Vec<u16>,
    queue: VecDeque<usize>,
}
impl SearchScratch {
    fn new(count: usize) -> Self {
        Self {
            epoch: 0,
            seen: vec![0; count],
            distances: vec![0; count],
            queue: VecDeque::new(),
        }
    }
    fn flood(
        &mut self,
        terrain: &TerrainGrid,
        starts: impl IntoIterator<Item = WalkPosition>,
        maximum: u16,
    ) {
        self.epoch = self.epoch.wrapping_add(1);
        if self.epoch == 0 {
            self.seen.fill(0);
            self.epoch = 1;
        }
        self.queue.clear();
        for start in starts {
            let Some(index) = terrain.index(start) else {
                continue;
            };
            if self.seen[index] == self.epoch || !terrain.cells()[index].walkable {
                continue;
            }
            self.seen[index] = self.epoch;
            self.distances[index] = 0;
            self.queue.push_back(index);
        }
        while let Some(index) = self.queue.pop_front() {
            let distance = self.distances[index];
            if distance == maximum {
                continue;
            }
            for (next, _) in terrain.neighbors(terrain.position(index)) {
                let next = terrain.index(next).expect("neighbor in map");
                if self.seen[next] == self.epoch {
                    continue;
                }
                self.seen[next] = self.epoch;
                self.distances[next] = distance + 1;
                self.queue.push_back(next);
            }
        }
    }
    fn reached(&self, index: usize) -> bool {
        self.seen[index] == self.epoch
    }
}

struct ScoringScratch {
    epoch: u32,
    seen: Vec<u32>,
    distances: Vec<u32>,
    // Row-major cell indices preserve the (y, x) tie order without storing both coordinates.
    queue: BinaryHeap<Reverse<(u32, usize)>>,
}

impl ScoringScratch {
    fn new(count: usize) -> Self {
        Self {
            epoch: 0,
            seen: vec![0; count],
            distances: vec![0; count],
            queue: BinaryHeap::new(),
        }
    }

    fn flood(
        &mut self,
        terrain: &TerrainGrid,
        starts: impl IntoIterator<Item = WalkPosition>,
        maximum_cost: u32,
    ) {
        self.epoch = self.epoch.wrapping_add(1);
        if self.epoch == 0 {
            self.seen.fill(0);
            self.epoch = 1;
        }
        self.queue.clear();
        for start in starts {
            let Some(index) = terrain.index(start) else {
                continue;
            };
            if !terrain.cells()[index].walkable || self.seen[index] == self.epoch {
                continue;
            }
            self.seen[index] = self.epoch;
            self.distances[index] = 0;
            self.queue.push(Reverse((0, index)));
        }
        while let Some(Reverse((cost, index))) = self.queue.pop() {
            if self.seen[index] != self.epoch || self.distances[index] != cost {
                continue;
            }
            for (next, step) in terrain.neighbors(terrain.position(index)) {
                let next_cost =
                    cost.saturating_add(u32::try_from(step).expect("walk cost fits u32"));
                if next_cost > maximum_cost {
                    continue;
                }
                let next_index = terrain.index(next).expect("neighbor in map");
                if self.seen[next_index] == self.epoch && self.distances[next_index] <= next_cost {
                    continue;
                }
                self.seen[next_index] = self.epoch;
                self.distances[next_index] = next_cost;
                self.queue.push(Reverse((next_cost, next_index)));
            }
        }
    }

    fn distance(&self, index: usize) -> Option<u32> {
        (self.seen[index] == self.epoch).then_some(self.distances[index])
    }
}

struct DisjointSet {
    parents: Vec<usize>,
    ranks: Vec<u8>,
}
impl DisjointSet {
    fn new(length: usize) -> Self {
        Self {
            parents: (0..length).collect(),
            ranks: vec![0; length],
        }
    }
    fn find(&mut self, index: usize) -> usize {
        if self.parents[index] != index {
            let root = self.find(self.parents[index]);
            self.parents[index] = root;
        }
        self.parents[index]
    }
    fn union(&mut self, left: usize, right: usize) {
        let left = self.find(left);
        let right = self.find(right);
        if left == right {
            return;
        }
        if self.ranks[left] < self.ranks[right] {
            self.parents[left] = right;
        } else {
            self.parents[right] = left;
            if self.ranks[left] == self.ranks[right] {
                self.ranks[left] += 1;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TerrainCell;

    fn grid(
        width_tiles: u32,
        height_tiles: u32,
        walkable: impl Fn(u32, u32) -> bool,
    ) -> TerrainGrid {
        let width = width_tiles * 4;
        let height = height_tiles * 4;
        let mut cells = Vec::new();
        for y in 0..height {
            for x in 0..width {
                cells.push(TerrainCell {
                    walkable: walkable(x, y),
                    terrain_buildable: true,
                    elevation: 0,
                    ramp: false,
                });
            }
        }
        TerrainGrid::from_cells(width, height, cells).unwrap()
    }
    fn resource(x: i32, y: i32, kind: ResourceKind, amount: Option<u32>) -> ResourceNode {
        ResourceNode {
            bounds: PixelRect {
                left: x,
                top: y,
                right: x + 16,
                bottom: y + 16,
            },
            kind,
            amount,
        }
    }
    fn field() -> Vec<ResourceNode> {
        vec![
            resource(176, 112, ResourceKind::Mineral, None),
            resource(208, 112, ResourceKind::Mineral, None),
            resource(176, 144, ResourceKind::Mineral, None),
            resource(208, 144, ResourceKind::Mineral, None),
        ]
    }

    #[test]
    fn finds_normal_mineral_and_gas_fields() {
        let terrain = grid(24, 20, |_, _| true);
        let mut resources = field();
        resources.push(resource(240, 112, ResourceKind::Gas, Some(0)));
        let result = discover_bases(
            &terrain,
            &resources,
            &[],
            &[],
            &BaseSearchOptions::default(),
        )
        .unwrap();
        assert_eq!(result.bases.len(), 1);
        assert_eq!(result.bases[0].resource_indices, vec![0, 1, 2, 3, 4]);
        assert!(result.bases[0].route_anchor.is_some());
    }

    #[test]
    fn cliffs_and_long_detours_do_not_merge_fields() {
        let terrain = grid(24, 40, |x, y| !(32..36).contains(&x) || y >= 150);
        let mut resources = field();
        resources.extend(field().into_iter().map(|mut node| {
            node.bounds.left += 128;
            node.bounds.right += 128;
            node
        }));
        let result = discover_bases(
            &terrain,
            &resources,
            &[],
            &[],
            &BaseSearchOptions::default(),
        )
        .unwrap();
        assert_eq!(result.bases.len(), 2);
    }

    #[test]
    fn gap_boundary_is_half_open_on_both_axes() {
        let tile = TilePosition { x: 10, y: 10 };
        let depot = DepotFootprint::default();
        let touching_right = resource(544, 320, ResourceKind::Mineral, None);
        assert!(!intersects_gap(tile, depot, touching_right.bounds));
        let touching_bottom = resource(320, 512, ResourceKind::Mineral, None);
        assert!(!intersects_gap(tile, depot, touching_bottom.bounds));
        let one_pixel_right = resource(543, 320, ResourceKind::Mineral, None);
        let one_pixel_bottom = resource(320, 511, ResourceKind::Mineral, None);
        assert!(intersects_gap(tile, depot, one_pixel_right.bounds));
        assert!(intersects_gap(tile, depot, one_pixel_bottom.bounds));
    }

    #[test]
    fn valid_start_is_not_shifted_by_resource_score() {
        let terrain = grid(24, 20, |_, _| true);
        let result = discover_bases(
            &terrain,
            &field(),
            &[],
            &[PixelPosition { x: 384, y: 304 }],
            &BaseSearchOptions::default(),
        )
        .unwrap();
        assert_eq!(result.bases[0].depot_tile, TilePosition { x: 10, y: 8 });
        assert_eq!(result.bases[0].start_indices, vec![0]);
    }

    #[test]
    fn exact_start_survives_a_buried_cluster_resource() {
        let terrain = grid(24, 20, |_, _| true);
        let buried = StaticObstacle {
            bounds: PixelRect {
                left: 168,
                top: 104,
                right: 200,
                bottom: 136,
            },
            destructible: false,
        };
        let start = PixelPosition { x: 384, y: 304 };
        let with_start = discover_bases(
            &terrain,
            &field(),
            &[buried],
            &[start],
            &BaseSearchOptions::default(),
        )
        .unwrap();
        assert_eq!(with_start.bases.len(), 1);
        assert_eq!(with_start.bases[0].depot_tile, TilePosition { x: 10, y: 8 });
        assert_eq!(with_start.bases[0].start_indices, vec![0]);

        let without_start = discover_bases(
            &terrain,
            &field(),
            &[buried],
            &[],
            &BaseSearchOptions::default(),
        )
        .unwrap();
        assert!(without_start.bases.is_empty());
        assert_eq!(without_start.unplaced_clusters, 1);
    }

    #[test]
    fn low_minerals_are_conditional_and_do_not_form_or_bridge_fields() {
        let terrain = grid(24, 20, |_, _| true);
        let tiny = resource(256, 112, ResourceKind::Mineral, Some(8));
        let nine = resource(256, 112, ResourceKind::Mineral, Some(9));
        let unknown = resource(256, 112, ResourceKind::Mineral, None);
        let gas = resource(256, 112, ResourceKind::Gas, Some(0));
        assert!(is_clearable(tiny, &BaseSearchOptions::default()));
        for amount in [0, 1] {
            assert!(is_clearable(
                ResourceNode {
                    amount: Some(amount),
                    ..tiny
                },
                &BaseSearchOptions::default()
            ));
        }
        assert!(!is_clearable(nine, &BaseSearchOptions::default()));
        assert!(!is_clearable(unknown, &BaseSearchOptions::default()));
        assert!(!is_clearable(gas, &BaseSearchOptions::default()));
        let result = discover_bases(
            &terrain,
            &[tiny; 4],
            &[],
            &[],
            &BaseSearchOptions::default(),
        )
        .unwrap();
        assert!(result.bases.is_empty());
        let mut resources = field();
        resources.push(tiny);
        let result = discover_bases(
            &terrain,
            &resources,
            &[],
            &[],
            &BaseSearchOptions::default(),
        )
        .unwrap();
        assert_eq!(result.bases.len(), 1);
        assert_eq!(result.bases[0].resource_indices, vec![0, 1, 2, 3]);
        let strict = BaseSearchOptions {
            max_mineral_blocker_amount: None,
            ..BaseSearchOptions::default()
        };
        let strict_result = discover_bases(&terrain, &resources, &[], &[], &strict).unwrap();
        assert_ne!(
            result.bases[0].depot_tile,
            strict_result.bases[0].depot_tile
        );
    }

    #[test]
    fn tiny_mineral_chain_does_not_join_separate_mining_fields() {
        let terrain = grid(40, 24, |_, _| true);
        let mut resources = field();
        resources.extend(field().into_iter().map(|mut node| {
            node.bounds.left += 640;
            node.bounds.right += 640;
            node
        }));
        for x in [320, 480, 640, 768] {
            resources.push(resource(x, 112, ResourceKind::Mineral, Some(1)));
        }
        let result = discover_bases(
            &terrain,
            &resources,
            &[],
            &[],
            &BaseSearchOptions::default(),
        )
        .unwrap();
        assert_eq!(result.bases.len(), 2);
        assert!(
            result
                .bases
                .iter()
                .all(|base| base.resource_indices.len() == 4)
        );
        assert!(
            result
                .bases
                .iter()
                .flat_map(|base| &base.resource_indices)
                .all(|&index| index < 8)
        );
    }

    #[test]
    fn thin_permanent_mineral_barrier_is_not_crossed_by_scoring_search() {
        let terrain = grid(12, 12, |_, _| true);
        let wall = PixelRect {
            left: 184,
            top: 0,
            right: 200,
            bottom: 384,
        };
        let scoring = terrain.with_obstacles(&[wall]);
        let source = perimeter_access_cells(
            &scoring,
            PixelRect {
                left: 144,
                top: 160,
                right: 176,
                bottom: 192,
            },
        )
        .unwrap();
        let target = footprint_perimeter(TilePosition { x: 7, y: 5 }, DepotFootprint::default());
        let mut scratch = SearchScratch::new(scoring.cells().len());
        scratch.flood(&scoring, source, PROXIMITY_RADIUS_CELLS);
        assert!(
            target
                .iter()
                .all(|point| !scratch.reached(scoring.index(*point).unwrap()))
        );
    }

    #[test]
    fn conditional_clearance_reports_prerequisite_and_current_anchor_none() {
        let terrain = grid(24, 20, |_, _| true);
        let mut resources = field();
        let low_footprint = ResourceNode {
            bounds: PixelRect {
                left: 320,
                top: 256,
                right: 448,
                bottom: 352,
            },
            kind: ResourceKind::Mineral,
            amount: Some(8),
        };
        resources.push(low_footprint);
        let start = PixelPosition { x: 384, y: 304 };
        let result = discover_bases(
            &terrain,
            &resources,
            &[],
            &[start],
            &BaseSearchOptions::default(),
        )
        .unwrap();
        let conditional = result
            .bases
            .iter()
            .find(|base| base.depot_tile == TilePosition { x: 10, y: 8 })
            .unwrap();
        assert_eq!(conditional.required_mineral_indices, vec![4]);
        assert_eq!(conditional.route_anchor, None);

        let permanently_blocked = discover_bases(
            &terrain,
            &resources,
            &[StaticObstacle {
                bounds: low_footprint.bounds,
                destructible: false,
            }],
            &[start],
            &BaseSearchOptions {
                allow_start_obstacle_clearing: false,
                ..BaseSearchOptions::default()
            },
        )
        .unwrap();
        assert!(
            permanently_blocked
                .bases
                .iter()
                .all(|base| base.depot_tile != TilePosition { x: 10, y: 8 })
        );

        let strict = BaseSearchOptions {
            max_mineral_blocker_amount: None,
            ..BaseSearchOptions::default()
        };
        let strict_result = discover_bases(&terrain, &resources, &[], &[start], &strict).unwrap();
        assert!(
            strict_result
                .bases
                .iter()
                .all(|base| base.depot_tile != TilePosition { x: 10, y: 8 })
        );
    }

    fn node(bounds: [i32; 4], kind: ResourceKind, amount: u32) -> ResourceNode {
        ResourceNode {
            bounds: PixelRect {
                left: bounds[0],
                top: bounds[1],
                right: bounds[2],
                bottom: bounds[3],
            },
            kind,
            amount: Some(amount),
        }
    }

    #[test]
    fn python_13_stock_half_open_field_keeps_its_exact_start_depot() {
        let terrain = grid(24, 24, |_, _| true);
        let mut resources = vec![node([224, 32, 352, 96], ResourceKind::Gas, 5_000)];
        for bounds in [
            [448, 96, 512, 128],
            [480, 128, 544, 160],
            [480, 192, 544, 224],
            [448, 224, 512, 256],
            [480, 256, 544, 288],
            [448, 288, 512, 320],
            [448, 352, 512, 384],
            [416, 384, 480, 416],
            [416, 64, 480, 96],
        ] {
            resources.push(node(bounds, ResourceKind::Mineral, 1_500));
        }
        let with_start = discover_bases(
            &terrain,
            &resources,
            &[],
            &[PixelPosition { x: 288, y: 240 }],
            &BaseSearchOptions::default(),
        )
        .unwrap();
        assert!(
            with_start
                .bases
                .iter()
                .any(|base| base.depot_tile == TilePosition { x: 7, y: 6 })
        );
        let without_start = discover_bases(
            &terrain,
            &resources,
            &[],
            &[],
            &BaseSearchOptions::default(),
        )
        .unwrap();
        assert!(
            without_start
                .bases
                .iter()
                .any(|base| base.depot_tile.x < 13)
        );
    }

    #[test]
    fn hunters_bottom_start_survives_half_open_resource_edges() {
        let terrain = grid(24, 24, |_, _| true);
        let mut resources = vec![node([96, 224, 224, 288], ResourceKind::Gas, 5_000)];
        for bounds in [
            [160, 320, 224, 352],
            [544, 352, 608, 384],
            [160, 352, 224, 384],
            [544, 384, 608, 416],
            [480, 416, 544, 448],
            [384, 416, 448, 448],
            [288, 416, 352, 448],
            [224, 416, 288, 448],
            [416, 448, 480, 480],
            [352, 448, 416, 480],
        ] {
            resources.push(node(bounds, ResourceKind::Mineral, 1_500));
        }
        let result = discover_bases(
            &terrain,
            &resources,
            &[],
            &[PixelPosition { x: 384, y: 272 }],
            &BaseSearchOptions::default(),
        )
        .unwrap();
        assert!(
            result
                .bases
                .iter()
                .any(|base| base.depot_tile == TilePosition { x: 10, y: 7 })
        );
    }

    #[test]
    fn median_capped_mineral_weight_keeps_six_patch_natural_on_open_side() {
        let terrain = grid(24, 24, |_, _| true);
        let mut resources = Vec::new();
        for bounds in [
            [224, 224, 288, 256],
            [224, 64, 288, 96],
            [192, 256, 256, 288],
            [256, 96, 320, 128],
            [224, 128, 288, 160],
            [256, 160, 320, 192],
        ] {
            resources.push(node(bounds, ResourceKind::Mineral, 1_500));
        }
        resources.push(node([256, 464, 320, 496], ResourceKind::Mineral, 249));
        let result = discover_bases(
            &terrain,
            &resources,
            &[],
            &[],
            &BaseSearchOptions::default(),
        )
        .unwrap();
        assert_eq!(result.bases.len(), 1);
        assert!(result.bases[0].depot_tile.x <= 3, "{:?}", result.bases[0]);
        assert_eq!(
            category_reference(
                &(0..7).collect::<Vec<_>>(),
                &(0..7)
                    .map(|index| MiningResource {
                        catalog_index: index,
                        access: Vec::new()
                    })
                    .collect::<Vec<_>>(),
                &resources
                    .iter()
                    .enumerate()
                    .map(|(original_index, &node)| CatalogResource {
                        original_index,
                        node
                    })
                    .collect::<Vec<_>>(),
                ResourceKind::Mineral,
            ),
            1_500
        );
    }

    #[test]
    fn unbuildable_and_bounded_inputs_are_rejected() {
        let terrain = grid(24, 20, |_, _| true);
        assert!(matches!(
            discover_bases(
                &terrain,
                &vec![resource(1, 1, ResourceKind::Mineral, None); MAX_RESOURCES + 1],
                &[],
                &[],
                &BaseSearchOptions::default()
            ),
            Err(BaseError::TooManyResources { .. })
        ));
        assert!(matches!(
            discover_bases(
                &terrain,
                &field(),
                &[],
                &vec![PixelPosition { x: 0, y: 0 }; MAX_STARTS + 1],
                &BaseSearchOptions::default()
            ),
            Err(BaseError::TooManyStarts { .. })
        ));
        assert!(matches!(
            discover_bases(
                &terrain,
                &field(),
                &[],
                &[],
                &BaseSearchOptions {
                    depot: DepotFootprint {
                        width_tiles: 0,
                        height_tiles: 3
                    },
                    ..BaseSearchOptions::default()
                }
            ),
            Err(BaseError::InvalidFootprint { .. })
        ));
    }
}
