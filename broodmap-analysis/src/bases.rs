//! Deterministic, terrain-aware resource-base discovery.
//!
//! Placement is a static heuristic. It deliberately reports mineral blockers that must be cleared
//! instead of pretending that a conditional location is an immediately usable depot.

use std::{
    cmp::Reverse,
    collections::{BTreeMap, BinaryHeap, HashSet},
};

use thiserror::Error;

use crate::{
    PixelRect, StaticObstacle, TerrainGrid, WalkPosition,
    proximity::{CellBox, ProximityField, half_cost},
};

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
///
/// Grid alignment and the depot footprint are validated before the resource and start limits, so
/// a count-limit error implies the grid and options are valid.
pub fn discover_bases(
    terrain: &TerrainGrid,
    resources: &[ResourceNode],
    obstacles: &[StaticObstacle],
    starts: &[PixelPosition],
    options: &BaseSearchOptions,
) -> Result<BaseDiscovery, BaseError> {
    discover(terrain, resources, obstacles, starts, options, true)
}

/// `prune` only changes how much scoring work is done, never the result.
fn discover(
    terrain: &TerrainGrid,
    resources: &[ResourceNode],
    obstacles: &[StaticObstacle],
    starts: &[PixelPosition],
    options: &BaseSearchOptions,
    prune: bool,
) -> Result<BaseDiscovery, BaseError> {
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

    let permanent_bounds: Vec<_> = catalog
        .iter()
        .filter(|resource| !is_clearable(resource.node, options))
        .map(|resource| resource.node.bounds)
        .collect();
    let resource_grid = stamp_obstacles(terrain.clone(), &permanent_bounds);
    // The all-object grid is only queried near chosen depots, so it is rasterized in local
    // windows on top of the permanent-resource grid instead of as another full-map copy.
    let current = CurrentOccupancy::new(
        &resource_grid,
        obstacles.iter().map(|obstacle| obstacle.bounds).chain(
            catalog
                .iter()
                .filter(|resource| is_clearable(resource.node, options))
                .map(|resource| resource.node.bounds),
        ),
    );

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

    let mut scoring = ScoringScratch::new(terrain.cells().len());
    let mut sets = cluster_sets(terrain, &mining);
    let clusters = clusters_from_sets(&mut sets, mining.len());
    let mut result = BaseDiscovery {
        bases: Vec::new(),
        unplaced_clusters: 0,
        ignored_resources,
    };

    let context = ScoringContext {
        catalog: &catalog,
        mining: &mining,
        obstacles,
        valid_starts: &valid_starts,
        options,
        resource_grid: &resource_grid,
        map_width_tiles,
        map_height_tiles,
    };
    let mut scored_clusters = Vec::new();
    for cluster in clusters {
        let mineral_count = cluster
            .iter()
            .filter(|&&index| context.node(index).kind == ResourceKind::Mineral)
            .count();
        if mineral_count < 4 {
            continue;
        }
        scored_clusters.push(context.score_cluster(cluster, &mut scoring, prune));
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
        let chosen = match scored_clusters[cluster_index].choose(
            cluster_index,
            &used_tiles,
            &start_owners,
        ) {
            Ok(chosen) => chosen,
            Err(Uncertain) => {
                // A taken optimum exposed candidates that scoring skipped; score every one.
                let cluster = std::mem::take(&mut scored_clusters[cluster_index].cluster);
                scored_clusters[cluster_index] =
                    context.score_cluster(cluster, &mut scoring, false);
                scored_clusters[cluster_index]
                    .choose(cluster_index, &used_tiles, &start_owners)
                    .expect("fully scored fields are certain")
            }
        };
        let cluster = &scored_clusters[cluster_index];
        let Some(chosen) = chosen.map(|index| &cluster.candidates[index]) else {
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
            route_anchor: current_footprint_anchor(chosen.tile, depot, &current),
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
        // Only non-exact starts near the depot need the connectivity flood; most bases have none.
        let mut reach = None;
        for &(start_index, start) in &valid_starts {
            let tile = start_depot_tile(start, depot);
            if tile
                .and_then(|tile| start_owners.get(&tile))
                .is_some_and(|owner| owner.2 != selected_cluster_indices[base_index])
            {
                continue;
            }
            let exact = tile == Some(base.depot_tile);
            let connected = !exact
                && base.route_anchor.is_some_and(|anchor| {
                    start_near_depot(start, base.depot_tile, depot)
                        && nearest_walkable(&current, start).is_some_and(|point| {
                            reach
                                .get_or_insert_with(|| current.reach(anchor, START_RADIUS_CELLS))
                                .walkable(point)
                        })
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
    /// Mining indices, kept so selection can rescore the field.
    cluster: Vec<usize>,
    resource_indices: Vec<usize>,
    candidates: Vec<CandidateLocation>,
    pruned: Vec<PrunedCandidate>,
    mineral_weight_total: u64,
    gas_weight_total: u64,
}

impl ScoredCluster {
    fn score_denominator(&self) -> u128 {
        u128::from(self.mineral_weight_total)
            * u128::from(self.gas_weight_total.max(1))
            * if self.gas_weight_total > 0 { 2 } else { 1 }
    }

    /// Picks this field's site among the available candidates, or reports that a pruned
    /// candidate might beat every available scored one.
    fn choose(
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
type CandidateKey = (u128, u128, u32, u32);

/// Ordinary candidates are pruned against an incumbent, a fully scored candidate. Before one
/// exists, the most promising candidates by lower bound are probed, one per attempt.
const PROBE_ATTEMPTS: usize = 3;
/// A probe is abandoned past 1.5 times its straight-line bound plus this detour.
const PROBE_SLACK_COST: u32 = 8_000;
/// Cells a group's pruned floods may settle before the group is swept exhaustively instead.
const FLOOD_POP_BUDGET: usize = 2_000;

#[derive(Debug, Copy, Clone)]
struct PrunedCandidate {
    tile: TilePosition,
    /// A lower bound on the candidate's ordinary selection key.
    bound: CandidateKey,
}

/// A pruned candidate may beat every scored alternative that is still available.
#[derive(Debug, Copy, Clone)]
struct Uncertain;

struct ScoringContext<'a> {
    catalog: &'a [CatalogResource],
    mining: &'a [MiningResource],
    obstacles: &'a [StaticObstacle],
    valid_starts: &'a [(usize, PixelPosition)],
    options: &'a BaseSearchOptions,
    resource_grid: &'a TerrainGrid,
    map_width_tiles: u32,
    map_height_tiles: u32,
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
    fn node(&self, mining_index: usize) -> ResourceNode {
        self.catalog[self.mining[mining_index].catalog_index].node
    }

    fn candidates(&self, cluster: &[usize]) -> Vec<CandidateLocation> {
        let depot = self.options.depot;
        let center = cluster_center(cluster, self.mining, self.catalog);
        let tiles = candidate_tiles(center, depot, self.map_width_tiles, self.map_height_tiles);
        let (Some(&first), Some(&last)) = (tiles.first(), tiles.last()) else {
            return Vec::new();
        };
        // Tiles are a row-major rectangle. Anything outside the union of every footprint's gap
        // (or footprint) cannot intersect one tile's, so skip it up front, keeping input order.
        let union = |margin: i64| {
            (
                i64::from(first.x) * PIXELS_PER_TILE - margin,
                i64::from(first.y) * PIXELS_PER_TILE - margin,
                (i64::from(last.x) + i64::from(depot.width_tiles)) * PIXELS_PER_TILE + margin,
                (i64::from(last.y) + i64::from(depot.height_tiles)) * PIXELS_PER_TILE + margin,
            )
        };
        let touches = |bounds: PixelRect, (left, top, right, bottom): (i64, i64, i64, i64)| {
            i64::from(bounds.left) < right
                && i64::from(bounds.right) > left
                && i64::from(bounds.top) < bottom
                && i64::from(bounds.bottom) > top
        };
        let gap_union = union(RESOURCE_GAP_PIXELS);
        let nearby: Vec<_> = self
            .catalog
            .iter()
            .filter(|resource| touches(resource.node.bounds, gap_union))
            .copied()
            .collect();
        let footprint_union = union(0);
        let availability = TileAvailability::new(
            self.resource_grid,
            (first.x, first.y),
            (last.x + depot.width_tiles, last.y + depot.height_tiles),
        );
        let nearby_obstacles: Vec<_> = self
            .obstacles
            .iter()
            .enumerate()
            .filter(|(_, obstacle)| touches(obstacle.bounds, footprint_union))
            .collect();
        tiles
            .into_iter()
            .filter_map(|tile| {
                let required = required_clearances(tile, depot, &nearby, self.options);
                let no_permanent_resource = nearby.iter().all(|resource| {
                    is_clearable(resource.node, self.options)
                        || !intersects_gap(tile, depot, resource.node.bounds)
                });
                if !no_permanent_resource || !availability.footprint(tile, depot) {
                    return None;
                }
                let exact_start = self
                    .valid_starts
                    .iter()
                    .any(|(_, start)| start_depot_tile(*start, depot) == Some(tile));
                let mut cleared_obstacle_indices = Vec::new();
                let mut required_obstacle_indices = Vec::new();
                let mut start_cleared_obstacle_indices = Vec::new();
                for &(index, obstacle) in &nearby_obstacles {
                    if !intersects_footprint(tile, depot, obstacle.bounds) {
                        continue;
                    }
                    if exact_start && self.options.allow_start_obstacle_clearing {
                        start_cleared_obstacle_indices.push(index);
                    } else if obstacle.destructible && self.options.allow_destructible_clearing {
                        required_obstacle_indices.push(index);
                    } else {
                        return None;
                    }
                    cleared_obstacle_indices.push(index);
                }
                Some(CandidateLocation {
                    tile,
                    exact_start,
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
            .collect()
    }

    /// Scores a field's candidates. Without `prune`, every group is swept exhaustively, as the
    /// reference algorithm scores it. With it, ordinary candidates that provably cannot beat the
    /// best fully scored candidate are left in `pruned` instead; a group where pruning does not
    /// pay off within the flood budget is swept exhaustively too.
    fn score_cluster(
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

    /// Scores every member of one removal-set group exactly, as the reference algorithm does, with
    /// lane-parallel sweeps, and marks the members it retains. Returns the best selection key
    /// among fully connected ordinary members.
    #[allow(clippy::too_many_arguments)]
    fn sweep_group(
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

/// Inclusive walk-cell rectangle `(left, top, right, bottom)`.
type CellRect = (u32, u32, u32, u32);

/// Cells that can hold a resource's perimeter access cells.
fn access_ring(bounds: PixelRect, width: u32, height: u32) -> CellRect {
    let left = u32::try_from(bounds.left).unwrap_or(0) / 8;
    let top = u32::try_from(bounds.top).unwrap_or(0) / 8;
    let right = u32::try_from(bounds.right - 1).unwrap_or(0) / 8;
    let bottom = u32::try_from(bounds.bottom - 1).unwrap_or(0) / 8;
    (
        left.saturating_sub(1),
        top.saturating_sub(1),
        right.saturating_add(1).min(width - 1),
        bottom.saturating_add(1).min(height - 1),
    )
}

/// A rectangle of walk cells, with translation into its own local coordinates.
#[derive(Debug, Copy, Clone)]
struct CellWindow {
    left: u32,
    top: u32,
    width: u32,
    height: u32,
}

impl CellWindow {
    /// Every cell a scoring flood from these access rings can reach, plus a one-cell border.
    fn around(rings: &[CellRect], map_width: u32, map_height: u32) -> Self {
        let margin = u32::from(PROXIMITY_RADIUS_CELLS) + 1;
        let left = rings.iter().map(|ring| ring.0).min().unwrap_or(0);
        let top = rings.iter().map(|ring| ring.1).min().unwrap_or(0);
        let right = rings.iter().map(|ring| ring.2).max().unwrap_or(0);
        let bottom = rings.iter().map(|ring| ring.3).max().unwrap_or(0);
        let left = left.saturating_sub(margin);
        let top = top.saturating_sub(margin);
        let right = right.saturating_add(margin).min(map_width - 1);
        let bottom = bottom.saturating_add(margin).min(map_height - 1);
        Self {
            left,
            top,
            width: right - left + 1,
            height: bottom - top + 1,
        }
    }

    fn extract(&self, terrain: &TerrainGrid) -> TerrainGrid {
        let mut cells = Vec::with_capacity(self.width as usize * self.height as usize);
        for y in self.top..self.top + self.height {
            let start = y as usize * terrain.width as usize + self.left as usize;
            cells.extend_from_slice(&terrain.cells[start..start + self.width as usize]);
        }
        TerrainGrid {
            width: self.width,
            height: self.height,
            cells,
        }
    }

    /// Translates pixel bounds; rasterization clips whatever falls outside the window.
    fn local_rect(&self, rect: PixelRect) -> PixelRect {
        let x = i32::try_from(i64::from(self.left) * PIXELS_PER_WALK_CELL)
            .expect("validated grid width");
        let y = i32::try_from(i64::from(self.top) * PIXELS_PER_WALK_CELL)
            .expect("validated grid height");
        PixelRect {
            left: rect.left.saturating_sub(x),
            top: rect.top.saturating_sub(y),
            right: rect.right.saturating_sub(x),
            bottom: rect.bottom.saturating_sub(y),
        }
    }

    fn local_point(&self, point: WalkPosition) -> Option<WalkPosition> {
        let x = point.x.checked_sub(self.left).filter(|&x| x < self.width)?;
        let y = point.y.checked_sub(self.top).filter(|&y| y < self.height)?;
        Some(WalkPosition { x, y })
    }
}

fn footprint_cells(tile: TilePosition, depot: DepotFootprint) -> CellRect {
    let left = tile.x * 4;
    let top = tile.y * 4;
    (
        left,
        top,
        left + depot.width_tiles * 4 - 1,
        top + depot.height_tiles * 4 - 1,
    )
}

/// The bounds of no cells; `union_cells` with it is the identity.
const EMPTY_CELL_RECT: CellRect = (u32::MAX, u32::MAX, 0, 0);

fn union_cells(a: CellRect, b: CellRect) -> CellRect {
    (a.0.min(b.0), a.1.min(b.1), a.2.max(b.2), a.3.max(b.3))
}

fn axis_gaps(a: CellRect, b: CellRect) -> (u32, u32) {
    let gap = |a_low: u32, a_high: u32, b_low: u32, b_high: u32| {
        b_low
            .saturating_sub(a_high)
            .max(a_low.saturating_sub(b_high))
    };
    (gap(a.0, a.2, b.0, b.2), gap(a.1, a.3, b.1, b.3))
}

/// The cheapest possible octile walk between any cells of two rectangles, ignoring obstacles.
fn octile_gap(a: CellRect, b: CellRect) -> u32 {
    let (dx, dy) = axis_gaps(a, b);
    dx.max(dy).saturating_mul(1_000) + dx.min(dy).saturating_mul(414)
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

/// The same grid as `TerrainGrid::with_obstacles`, built by marking each clipped rectangle's
/// cells directly. That costs the rectangles' area rather than a sweep of every cell, so the
/// sweep is only used when the rectangles cover more cells than the grid has.
fn stamp_obstacles(mut grid: TerrainGrid, obstacles: &[PixelRect]) -> TerrainGrid {
    let cell_pixels = i32::try_from(PIXELS_PER_WALK_CELL).expect("cell size fits i32");
    let max_x = i32::try_from(grid.width).expect("validated grid width") * cell_pixels;
    let max_y = i32::try_from(grid.height).expect("validated grid height") * cell_pixels;
    let mut spans = Vec::with_capacity(obstacles.len());
    let mut area = 0_usize;
    for rect in obstacles {
        if rect.left >= rect.right || rect.top >= rect.bottom {
            continue;
        }
        // Clamp before converting to cells, exactly as the sweep does.
        let left = rect.left.clamp(0, max_x);
        let top = rect.top.clamp(0, max_y);
        let right = rect.right.clamp(0, max_x);
        let bottom = rect.bottom.clamp(0, max_y);
        if left >= right || top >= bottom {
            continue;
        }
        let x0 = (left / cell_pixels) as usize;
        let y0 = (top / cell_pixels) as usize;
        let x1 = ((right + cell_pixels - 1) / cell_pixels) as usize;
        let y1 = ((bottom + cell_pixels - 1) / cell_pixels) as usize;
        area = area.saturating_add((x1 - x0) * (y1 - y0));
        spans.push((x0, y0, x1, y1));
    }
    if area > grid.cells.len() {
        return grid.with_obstacles(obstacles);
    }
    let width = grid.width as usize;
    for (x0, y0, x1, y1) in spans {
        for y in y0..y1 {
            for cell in &mut grid.cells[y * width + x0..y * width + x1] {
                cell.walkable = false;
            }
        }
    }
    grid
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
    current: &CurrentOccupancy,
) -> Option<WalkPosition> {
    let left = tile.x * 4;
    let top = tile.y * 4;
    let width = depot.width_tiles * 4;
    let height = depot.height_tiles * 4;
    let footprint = current.window(left, top, left + width, top + height);
    let center_x = i64::from(left) * 2 + i64::from(width);
    let center_y = i64::from(top) * 2 + i64::from(height);
    let mut best = None;
    for y in top..top + height {
        for x in left..left + width {
            let point = WalkPosition { x, y };
            if !footprint.walkable(point) {
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

fn nearest_walkable(current: &CurrentOccupancy, point: PixelPosition) -> Option<WalkPosition> {
    let center = WalkPosition {
        x: point.x / 8,
        y: point.y / 8,
    };
    let left = center.x.saturating_sub(2);
    let top = center.y.saturating_sub(2);
    let right = center.x.saturating_add(2).min(current.grid.width() - 1) + 1;
    let bottom = center.y.saturating_add(2).min(current.grid.height() - 1) + 1;
    let nearby = current.window(left, top, right, bottom);
    let mut best = None;
    for y in top..bottom {
        for x in left..right {
            let candidate = WalkPosition { x, y };
            if !nearby.walkable(candidate) {
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

/// Whether each build tile in a rectangle is entirely walkable and buildable.
struct TileAvailability {
    left: u32,
    top: u32,
    width: u32,
    available: Vec<bool>,
}

impl TileAvailability {
    /// Covers the tiles from `start` (inclusive) to `end` (exclusive).
    fn new(terrain: &TerrainGrid, start: (u32, u32), end: (u32, u32)) -> Self {
        let width = end.0 - start.0;
        let mut available = Vec::with_capacity(width as usize * (end.1 - start.1) as usize);
        let stride = terrain.width() as usize;
        for tile_y in start.1..end.1 {
            for tile_x in start.0..end.0 {
                let left = tile_x as usize * 4;
                available.push((0..4).all(|row| {
                    let first = (tile_y as usize * 4 + row) * stride + left;
                    terrain.cells()[first..first + 4]
                        .iter()
                        .all(|cell| cell.walkable && cell.terrain_buildable)
                }));
            }
        }
        Self {
            left: start.0,
            top: start.1,
            width,
            available,
        }
    }

    fn footprint(&self, tile: TilePosition, depot: DepotFootprint) -> bool {
        (tile.y..tile.y + depot.height_tiles).all(|y| {
            let row = (y - self.top) * self.width;
            (tile.x..tile.x + depot.width_tiles)
                .all(|x| self.available[(row + x - self.left) as usize])
        })
    }
}

#[cfg(test)]
struct SearchScratch {
    epoch: u32,
    seen: Vec<u32>,
    distances: Vec<u16>,
}
#[cfg(test)]
impl SearchScratch {
    fn new(count: usize) -> Self {
        Self {
            epoch: 0,
            seen: vec![0; count],
            distances: vec![0; count],
        }
    }
    #[cfg(test)]
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
        let mut queue = std::collections::VecDeque::new();
        for start in starts {
            let Some(index) = terrain.index(start) else {
                continue;
            };
            if self.seen[index] == self.epoch || !terrain.cells()[index].walkable {
                continue;
            }
            self.seen[index] = self.epoch;
            self.distances[index] = 0;
            queue.push_back(index);
        }
        while let Some(index) = queue.pop_front() {
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
                queue.push_back(next);
            }
        }
    }
    #[cfg(test)]
    fn reached(&self, index: usize) -> bool {
        self.seen[index] == self.epoch
    }
}

/// Maps cells to the ids of the targets that contain them.
struct TargetIndex {
    /// Per cell, 1 + the index of its range in `ranges`; 0 when the cell is no target's.
    slots: Vec<u32>,
    cells: Vec<usize>,
    ranges: Vec<(u32, u32)>,
    /// Target ids grouped by cell, ranged by `ranges`.
    ids: Vec<u32>,
    /// Bounding cells per target id, `EMPTY_CELL_RECT` when it has none.
    boxes: Vec<CellRect>,
    pairs: Vec<(usize, u32)>,
}

impl TargetIndex {
    fn new(count: usize) -> Self {
        Self {
            slots: vec![0; count],
            cells: Vec::new(),
            ranges: Vec::new(),
            ids: Vec::new(),
            boxes: Vec::new(),
            pairs: Vec::new(),
        }
    }

    /// Registers each target's cells, replacing the previous targets.
    fn set<T: IntoIterator<Item = WalkPosition>>(
        &mut self,
        terrain: &TerrainGrid,
        targets: impl IntoIterator<Item = (usize, T)>,
    ) {
        self.clear();
        for (id, cells) in targets {
            if self.boxes.len() <= id {
                self.boxes.resize(id + 1, EMPTY_CELL_RECT);
            }
            let mut bounds = EMPTY_CELL_RECT;
            for point in cells {
                let index = terrain.index(point).expect("target in grid");
                let id = u32::try_from(id).expect("target count fits u32");
                self.pairs.push((index, id));
                bounds = union_cells(bounds, (point.x, point.y, point.x, point.y));
            }
            self.boxes[id] = bounds;
        }
        self.pairs.sort_unstable();
        self.pairs.dedup();
        for &(index, id) in &self.pairs {
            if self.cells.last() != Some(&index) {
                let start = u32::try_from(self.ids.len()).expect("target ids fit u32");
                self.ranges.push((start, start));
                self.cells.push(index);
                self.slots[index] = u32::try_from(self.ranges.len()).expect("cells fit u32");
            }
            self.ids.push(id);
            self.ranges.last_mut().expect("range pushed").1 += 1;
        }
    }

    fn clear(&mut self) {
        for &index in &self.cells {
            self.slots[index] = 0;
        }
        self.cells.clear();
        self.ranges.clear();
        self.ids.clear();
        self.boxes.clear();
        self.pairs.clear();
    }

    fn ids_at(&self, index: usize) -> &[u32] {
        match self.slots[index] {
            0 => &[],
            slot => {
                let (start, end) = self.ranges[slot as usize - 1];
                &self.ids[start as usize..end as usize]
            }
        }
    }
}

struct ScoringScratch {
    epoch: u32,
    seen: Vec<u32>,
    distances: Vec<u32>,
    // Row-major cell indices preserve the (y, x) tie order without storing both coordinates.
    queue: BinaryHeap<Reverse<(u32, usize)>>,
    targets: TargetIndex,
    /// Minimum settled distance per target from the last `flood_targets`.
    target_distances: Vec<Option<u32>>,
    give_up_order: Vec<usize>,
    /// Cells settled by the last `flood_targets`.
    pops: usize,
    narrow: ProximityField<8>,
    wide: ProximityField<16>,
}

impl ScoringScratch {
    fn new(count: usize) -> Self {
        Self {
            epoch: 0,
            // Sized on demand: scoring floods run on windows smaller than the map.
            seen: Vec::new(),
            distances: Vec::new(),
            queue: BinaryHeap::new(),
            targets: TargetIndex::new(count),
            target_distances: Vec::new(),
            give_up_order: Vec::new(),
            pops: 0,
            narrow: ProximityField::new(),
            wide: ProximityField::new(),
        }
    }

    fn target_distance(&self, id: usize) -> Option<u32> {
        self.target_distances[id]
    }

    /// Dijkstra from `starts` that stops once every `wanted` target is settled, or once every
    /// unsettled wanted target is farther than its `give_up` cost.
    ///
    /// Cells pop in nondecreasing cost order, so a target's minimum over its cells is the cost of
    /// the first of them to pop, and every unpopped cell costs at least the last popped cost.
    /// Targets never popped within `maximum_cost` are unreachable.
    fn flood_targets(
        &mut self,
        terrain: &TerrainGrid,
        starts: impl IntoIterator<Item = WalkPosition>,
        maximum_cost: u32,
        wanted: &[usize],
        give_up: &[u32],
        budget: usize,
    ) -> FloodEnd {
        self.pops = 0;
        for &id in wanted {
            if self.target_distances.len() <= id {
                self.target_distances.resize(id + 1, None);
            }
            self.target_distances[id] = None;
        }
        // Wanted targets by descending give-up cost; `next` is the first unsettled one. Only
        // wanted targets are registered in `targets`.
        self.give_up_order.clear();
        self.give_up_order.extend_from_slice(wanted);
        if self.give_up_order.is_empty() {
            return FloodEnd::Settled;
        }
        self.give_up_order
            .sort_unstable_by_key(|&id| Reverse(give_up[id]));
        let mut next = 0;
        let mut limit = give_up[self.give_up_order[0]];
        // A* toward the wanted targets' bounding box. The octile distance to it is consistent and
        // zero on every wanted target cell, so those pop in true-distance order, and every
        // unpopped wanted cell is at least the last popped priority away.
        let goal = self
            .give_up_order
            .iter()
            .map(|&id| self.targets.boxes[id])
            .fold(EMPTY_CELL_RECT, union_cells);
        let heuristic = |point: WalkPosition| {
            if goal.0 > goal.2 {
                0
            } else {
                octile_gap((point.x, point.y, point.x, point.y), goal)
            }
        };
        if self.seen.len() < terrain.cells().len() {
            self.seen.resize(terrain.cells().len(), 0);
            self.distances.resize(terrain.cells().len(), 0);
        }
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
            self.queue.push(Reverse((heuristic(start), index)));
        }
        while let Some(Reverse((priority, index))) = self.queue.pop() {
            if self.seen[index] != self.epoch {
                continue;
            }
            let position = terrain.position(index);
            let cost = self.distances[index];
            if cost + heuristic(position) != priority {
                continue;
            }
            if priority > limit {
                return FloodEnd::GaveUp(priority);
            }
            self.pops += 1;
            if self.pops > budget {
                return FloodEnd::OverBudget;
            }
            let ids = self.targets.ids_at(index);
            if !ids.is_empty() {
                for &id in ids {
                    let id = id as usize;
                    if self.target_distances[id].is_none() {
                        self.target_distances[id] = Some(cost);
                    }
                }
                while self
                    .give_up_order
                    .get(next)
                    .is_some_and(|&id| self.target_distances[id].is_some())
                {
                    next += 1;
                }
                let Some(&id) = self.give_up_order.get(next) else {
                    return FloodEnd::Settled;
                };
                limit = give_up[id];
            }
            for (next, step) in terrain.neighbors(position) {
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
                self.queue
                    .push(Reverse((next_cost + heuristic(next), next_index)));
            }
        }
        FloodEnd::Exhausted
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

/// The distance part of the ordinary selection key, as `ScoredCluster::candidate_key` builds it.
fn key_distance(candidate: &CandidateLocation, mineral_total: u64, gas_total: u64) -> u128 {
    if gas_total == 0 {
        u128::from(candidate.mineral_distance)
    } else {
        u128::from(candidate.mineral_distance) * u128::from(gas_total)
            + u128::from(candidate.gas_distance) * u128::from(mineral_total)
    }
}

fn tie_of(candidate: &CandidateLocation) -> (u128, u32, u32) {
    (
        candidate.center_deviation,
        candidate.tile.y,
        candidate.tile.x,
    )
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum FloodEnd {
    /// Every wanted target settled.
    Settled,
    /// Stopped at this cost; every unsettled target is at least this far away.
    GaveUp(u32),
    /// Every cell within the maximum cost was settled.
    Exhausted,
    /// More cells than the budget were settled.
    OverBudget,
}

/// Calls `visit` for each 8-connected walkable neighbor of `index`, matching
/// [`TerrainGrid::neighbors`]: a diagonal step needs both orthogonal side cells walkable.
#[inline(always)]
fn for_each_walk_neighbor(terrain: &TerrainGrid, index: usize, mut visit: impl FnMut(usize)) {
    let cells = terrain.cells();
    let width = terrain.width() as usize;
    let x = index % width;
    let up = index >= width && cells[index - width].walkable;
    let down = index + width < cells.len() && cells[index + width].walkable;
    let left = x > 0 && cells[index - 1].walkable;
    let right = x + 1 < width && cells[index + 1].walkable;
    if up {
        visit(index - width);
        if left && cells[index - width - 1].walkable {
            visit(index - width - 1);
        }
        if right && cells[index - width + 1].walkable {
            visit(index - width + 1);
        }
    }
    if left {
        visit(index - 1);
    }
    if right {
        visit(index + 1);
    }
    if down {
        visit(index + width);
        if left && cells[index + width - 1].walkable {
            visit(index + width - 1);
        }
        if right && cells[index + width + 1].walkable {
            visit(index + width + 1);
        }
    }
}

/// Unions every pair of mining resources whose access cell sets are within
/// `CLUSTER_RADIUS_CELLS` walk steps of each other.
///
/// Instead of one bounded flood per resource, a multi-source BFS labels each cell with a nearest
/// resource out to half the radius, and unions labels across every edge `(u, v)` with
/// `d(u) + 1 + d(v) <= CLUSTER_RADIUS_CELLS`. Such an edge closes a path of that length between
/// the two labels' access sets, so every union is a true pair. Conversely, every cell on a
/// shortest path of length `L <= CLUSTER_RADIUS_CELLS` between two access sets lies within
/// `L / 2` steps of one of them, so it is labeled, and each label change along the path is an
/// edge with `d(u) + 1 + d(v) <= L`; those unions chain the two ends together. The step graph is
/// undirected, so the components equal those of pairwise per-resource floods.
fn cluster_sets(terrain: &TerrainGrid, mining: &[MiningResource]) -> DisjointSet {
    // A cell's state packs `(mining index + 1) << DEPTH_BITS | depth`; zero means unreached.
    const DEPTH_BITS: u32 = 5;
    const DEPTH_MASK: u16 = (1 << DEPTH_BITS) - 1;
    const HALF_RADIUS: u16 = CLUSTER_RADIUS_CELLS / 2;
    const _: () = assert!(HALF_RADIUS <= DEPTH_MASK);
    const _: () = assert!((MAX_RESOURCES as u32 + 1) << DEPTH_BITS <= u16::MAX as u32);
    let cells = terrain.cells();
    let mut sets = DisjointSet::new(mining.len());
    let mut states = vec![0_u16; cells.len()];
    let mut queue = Vec::new();
    // One search per spatial group keeps each wavefront cache-local. Every step moves at most one
    // cell per axis, so resources whose access boxes are more than the radius apart (Chebyshev)
    // are never joined and their half-radius regions are disjoint: the searches don't interact.
    for group in spatial_groups(mining) {
        queue.clear();
        for resource in group {
            let label = u16::try_from(resource + 1).expect("resource count fits u16") << DEPTH_BITS;
            for &point in &mining[resource].access {
                let index = terrain.index(point).expect("access in map");
                if !cells[index].walkable {
                    continue;
                }
                match states[index] {
                    0 => {
                        states[index] = label;
                        queue.push(index);
                    }
                    other => sets.union(usize::from(other >> DEPTH_BITS) - 1, resource),
                }
            }
        }
        let mut head = 0;
        let mut last_union = (0, 0);
        while let Some(&index) = queue.get(head) {
            head += 1;
            let state = states[index];
            let (label, depth) = (state & !DEPTH_MASK, state & DEPTH_MASK);
            for_each_walk_neighbor(terrain, index, |next| match states[next] {
                0 => {
                    if depth < HALF_RADIUS {
                        states[next] = state + 1;
                        queue.push(next);
                    }
                }
                other => {
                    let other_label = other & !DEPTH_MASK;
                    if other_label != label
                        && depth + (other & DEPTH_MASK) < CLUSTER_RADIUS_CELLS
                        && last_union != (label, other_label)
                    {
                        last_union = (label, other_label);
                        sets.union(
                            usize::from(label >> DEPTH_BITS) - 1,
                            usize::from(other_label >> DEPTH_BITS) - 1,
                        );
                    }
                }
            });
        }
    }
    sets
}

/// Groups mining resources whose access bounding boxes are within `CLUSTER_RADIUS_CELLS`
/// (Chebyshev) of each other, transitively.
fn spatial_groups(mining: &[MiningResource]) -> Vec<Vec<usize>> {
    let radius = u32::from(CLUSTER_RADIUS_CELLS);
    let bounds: Vec<_> = mining
        .iter()
        .map(|resource| {
            resource.access.iter().fold(
                (u32::MAX, u32::MAX, 0, 0),
                |(left, top, right, bottom), point| {
                    (
                        left.min(point.x),
                        top.min(point.y),
                        right.max(point.x),
                        bottom.max(point.y),
                    )
                },
            )
        })
        .collect();
    let mut near = DisjointSet::new(mining.len());
    for (first, a) in bounds.iter().enumerate() {
        for (second, b) in bounds.iter().enumerate().skip(first + 1) {
            if a.0 <= b.2.saturating_add(radius)
                && b.0 <= a.2.saturating_add(radius)
                && a.1 <= b.3.saturating_add(radius)
                && b.1 <= a.3.saturating_add(radius)
            {
                near.union(first, second);
            }
        }
    }
    clusters_from_sets(&mut near, mining.len())
}

/// The walk cells [`TerrainGrid::with_obstacles`] blocks for `rect`, as a half-open
/// `(left, top, right, bottom)` range clamped to the map.
fn cell_range(grid: &TerrainGrid, rect: PixelRect) -> Option<(u32, u32, u32, u32)> {
    if rect.left >= rect.right || rect.top >= rect.bottom {
        return None;
    }
    let max_x = i64::from(grid.width()) * PIXELS_PER_WALK_CELL;
    let max_y = i64::from(grid.height()) * PIXELS_PER_WALK_CELL;
    let left = i64::from(rect.left).clamp(0, max_x);
    let top = i64::from(rect.top).clamp(0, max_y);
    let right = i64::from(rect.right).clamp(0, max_x);
    let bottom = i64::from(rect.bottom).clamp(0, max_y);
    let cell = |value: i64| u32::try_from(value / PIXELS_PER_WALK_CELL).expect("clamped");
    (left < right && top < bottom).then(|| {
        (
            cell(left),
            cell(top),
            cell(right + PIXELS_PER_WALK_CELL - 1),
            cell(bottom + PIXELS_PER_WALK_CELL - 1),
        )
    })
}

/// The current all-object grid: the permanent-resource grid plus every non-resource obstacle
/// and low mineral, rasterized exactly as [`TerrainGrid::with_obstacles`] would, on demand.
struct CurrentOccupancy<'a> {
    grid: &'a TerrainGrid,
    /// Half-open walk-cell ranges `(left, top, right, bottom)`, already clamped to the map.
    rects: Vec<(u32, u32, u32, u32)>,
}

impl<'a> CurrentOccupancy<'a> {
    fn new(grid: &'a TerrainGrid, rects: impl IntoIterator<Item = PixelRect>) -> Self {
        let rects = rects
            .into_iter()
            .filter_map(|rect| cell_range(grid, rect))
            .collect();
        Self { grid, rects }
    }

    /// Rasterizes the half-open in-map cell window `[left, right) x [top, bottom)`.
    fn window(&self, left: u32, top: u32, right: u32, bottom: u32) -> LocalGrid {
        let width = (right - left) as usize;
        let height = (bottom - top) as usize;
        let stride = width + 1;
        let mut differences = vec![0_i64; stride * (height + 1)];
        for &(x0, y0, x1, y1) in &self.rects {
            let x0 = x0.max(left);
            let y0 = y0.max(top);
            let x1 = x1.min(right);
            let y1 = y1.min(bottom);
            if x0 >= x1 || y0 >= y1 {
                continue;
            }
            let (x0, y0) = ((x0 - left) as usize, (y0 - top) as usize);
            let (x1, y1) = ((x1 - left) as usize, (y1 - top) as usize);
            differences[y0 * stride + x0] += 1;
            differences[y0 * stride + x1] -= 1;
            differences[y1 * stride + x0] -= 1;
            differences[y1 * stride + x1] += 1;
        }
        let grid_width = self.grid.width() as usize;
        let cells = self.grid.cells();
        let mut walkable = vec![false; width * height];
        for y in 0..height {
            let row = &cells[(top as usize + y) * grid_width + left as usize..][..width];
            for x in 0..width {
                let index = y * stride + x;
                let above = if y == 0 {
                    0
                } else {
                    differences[index - stride]
                };
                let before = if x == 0 { 0 } else { differences[index - 1] };
                let above_before = if x == 0 || y == 0 {
                    0
                } else {
                    differences[index - stride - 1]
                };
                differences[index] += above + before - above_before;
                walkable[y * width + x] = row[x].walkable && differences[index] <= 0;
            }
        }
        LocalGrid {
            left,
            top,
            width: width as u32,
            height: height as u32,
            walkable,
        }
    }

    /// Cells reachable from `start` within `maximum` 8-neighbor steps, matching
    /// [`SearchScratch::flood`] on the full current grid. Such a flood cannot leave the
    /// Chebyshev window around `start`, so only that window is rasterized.
    fn reach(&self, start: WalkPosition, maximum: u16) -> LocalGrid {
        let radius = u32::from(maximum);
        let left = start.x.saturating_sub(radius);
        let top = start.y.saturating_sub(radius);
        let right = start.x.saturating_add(radius).min(self.grid.width() - 1) + 1;
        let bottom = start.y.saturating_add(radius).min(self.grid.height() - 1) + 1;
        let open = self.window(left, top, right, bottom);
        let width = open.width as usize;
        let height = open.height as usize;
        let mut reached = vec![false; open.walkable.len()];
        let mut distances = vec![0_u16; open.walkable.len()];
        let mut queue = std::collections::VecDeque::new();
        let start_index = (start.y - top) as usize * width + (start.x - left) as usize;
        if open.walkable[start_index] {
            reached[start_index] = true;
            queue.push_back(start_index);
        }
        while let Some(index) = queue.pop_front() {
            let distance = distances[index];
            if distance == maximum {
                continue;
            }
            let x = index % width;
            let y = index / width;
            for (delta_x, delta_y) in NEIGHBOR_DIRECTIONS {
                let (Some(next_x), Some(next_y)) = (
                    x.checked_add_signed(delta_x).filter(|&value| value < width),
                    y.checked_add_signed(delta_y)
                        .filter(|&value| value < height),
                ) else {
                    continue;
                };
                let next = next_y * width + next_x;
                let diagonal = delta_x != 0 && delta_y != 0;
                if reached[next]
                    || !open.walkable[next]
                    || (diagonal
                        && (!open.walkable[y * width + next_x]
                            || !open.walkable[next_y * width + x]))
                {
                    continue;
                }
                reached[next] = true;
                distances[next] = distance + 1;
                queue.push_back(next);
            }
        }
        LocalGrid {
            walkable: reached,
            ..open
        }
    }
}

const NEIGHBOR_DIRECTIONS: [(isize, isize); 8] = [
    (0, -1),
    (-1, 0),
    (1, 0),
    (0, 1),
    (-1, -1),
    (1, -1),
    (-1, 1),
    (1, 1),
];

/// A rectangular window of walk-cell flags; cells outside it read as false.
struct LocalGrid {
    left: u32,
    top: u32,
    width: u32,
    height: u32,
    walkable: Vec<bool>,
}

impl LocalGrid {
    fn walkable(&self, point: WalkPosition) -> bool {
        let (Some(x), Some(y)) = (
            point.x.checked_sub(self.left),
            point.y.checked_sub(self.top),
        ) else {
            return false;
        };
        x < self.width
            && y < self.height
            && self.walkable[y as usize * self.width as usize + x as usize]
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

    #[test]
    fn stamped_obstacles_match_the_sweep() {
        let terrain = grid(8, 6, |x, y| (x + 2 * y) % 7 != 0);
        let rect = |left, top, right, bottom| PixelRect {
            left,
            top,
            right,
            bottom,
        };
        let cases = [
            vec![
                rect(9, 17, 40, 41),
                rect(0, 0, 1, 1),
                rect(255, 191, 256, 192),
            ],
            vec![rect(-50, -3, 7, 300), rect(250, 100, i32::MAX, 101)],
            vec![rect(i32::MIN, i32::MIN, i32::MAX, i32::MAX)],
            vec![
                rect(30, 30, 30, 60),
                rect(40, 50, 10, 60),
                rect(256, 0, 300, 50),
            ],
            vec![rect(8, 8, 16, 16), rect(15, 15, 17, 17), rect(-8, -8, 0, 0)],
            vec![rect(0, 0, 256, 192); 3],
        ];
        for obstacles in cases {
            assert_eq!(
                stamp_obstacles(terrain.clone(), &obstacles),
                terrain.with_obstacles(&obstacles)
            );
        }
    }

    fn pairwise_clusters(terrain: &TerrainGrid, mining: &[MiningResource]) -> Vec<Vec<usize>> {
        let mut sets = DisjointSet::new(mining.len());
        for source in 0..mining.len() {
            let mut distances = vec![None; terrain.cells().len()];
            let mut queue = std::collections::VecDeque::new();
            for &point in &mining[source].access {
                let index = terrain.index(point).unwrap();
                if distances[index].is_none() {
                    distances[index] = Some(0);
                    queue.push_back(point);
                }
            }
            while let Some(point) = queue.pop_front() {
                let distance = distances[terrain.index(point).unwrap()].unwrap();
                if distance == CLUSTER_RADIUS_CELLS {
                    continue;
                }
                for (next, _) in terrain.neighbors(point) {
                    let next_index = terrain.index(next).unwrap();
                    if distances[next_index].is_none() {
                        distances[next_index] = Some(distance + 1);
                        queue.push_back(next);
                    }
                }
            }
            for (target, candidate) in mining.iter().enumerate().skip(source + 1) {
                if candidate
                    .access
                    .iter()
                    .any(|point| distances[terrain.index(*point).unwrap()].is_some())
                {
                    sets.union(source, target);
                }
            }
        }
        clusters_from_sets(&mut sets, mining.len())
    }

    #[test]
    fn labeled_cluster_search_matches_pairwise_floods() {
        let mut seed = 0x2545_f491_4f6c_dd1d_u64;
        let mut next = move |bound: u32| {
            seed ^= seed << 13;
            seed ^= seed >> 7;
            seed ^= seed << 17;
            u32::try_from(seed % u64::from(bound)).unwrap()
        };
        let mut joined = 0;
        for trial in 0..120 {
            let tiles = 12 + next(16);
            let open = 55 + next(46);
            let blocks = next(12);
            let mut walls = Vec::new();
            for _ in 0..blocks {
                walls.push((next(tiles * 4), next(tiles * 4), 1 + next(12), 1 + next(12)));
            }
            let mut noise = Vec::new();
            for _ in 0..tiles * tiles * 16 {
                noise.push(next(100) < open);
            }
            let width = tiles * 4;
            let terrain = grid(tiles, tiles, |x, y| {
                noise[(y * width + x) as usize]
                    && !walls
                        .iter()
                        .any(|&(l, t, w, h)| (l..l + w).contains(&x) && (t..t + h).contains(&y))
            });
            let mut mining = Vec::new();
            for _ in 0..2 + next(14) {
                let left = i32::try_from(next(tiles * 32 - 8)).unwrap();
                let top = i32::try_from(next(tiles * 32 - 8)).unwrap();
                let right = (left + 1 + i32::try_from(next(64)).unwrap()).min((tiles * 32) as i32);
                let bottom = (top + 1 + i32::try_from(next(48)).unwrap()).min((tiles * 32) as i32);
                let bounds = PixelRect {
                    left,
                    top,
                    right,
                    bottom,
                };
                if let Some(access) = perimeter_access_cells(&terrain, bounds) {
                    mining.push(MiningResource {
                        catalog_index: mining.len(),
                        access,
                    });
                }
            }
            let expected = pairwise_clusters(&terrain, &mining);
            let mut sets = cluster_sets(&terrain, &mining);
            assert_eq!(
                clusters_from_sets(&mut sets, mining.len()),
                expected,
                "trial {trial}"
            );
            joined += mining.len() - expected.len();
        }
        assert!(joined > 50, "trials should exercise joins, got {joined}");
    }

    #[test]
    fn local_rasterization_matches_full_grid_obstacles() {
        let terrain = grid(16, 12, |x, y| (x * 7 + y * 3) % 11 != 0);
        let rects = [
            PixelRect {
                left: 3,
                top: 5,
                right: 21,
                bottom: 9,
            },
            PixelRect {
                left: i32::MIN,
                top: 100,
                right: 40,
                bottom: i32::MAX,
            },
            PixelRect {
                left: 300,
                top: 200,
                right: 300,
                bottom: 260,
            },
            PixelRect {
                left: 90,
                top: 90,
                right: 80,
                bottom: 120,
            },
            PixelRect {
                left: 500,
                top: -20,
                right: 513,
                bottom: 7,
            },
            PixelRect {
                left: 2_000,
                top: 0,
                right: 3_000,
                bottom: 50,
            },
            PixelRect {
                left: 250,
                top: 250,
                right: 330,
                bottom: 270,
            },
        ];
        let full = terrain.with_obstacles(&rects);
        assert_eq!(stamp_obstacles(terrain.clone(), &rects), full);
        let huge = [PixelRect {
            left: 0,
            top: 0,
            right: 512,
            bottom: 384,
        }; 2];
        assert_eq!(
            stamp_obstacles(terrain.clone(), &huge),
            terrain.with_obstacles(&huge)
        );

        let current = CurrentOccupancy::new(&terrain, rects);
        let window = current.window(5, 7, 40, 33);
        for y in 0..full.height() {
            for x in 0..full.width() {
                let point = WalkPosition { x, y };
                let inside = (5..40).contains(&x) && (7..33).contains(&y);
                assert_eq!(
                    window.walkable(point),
                    inside && full.cell(point).unwrap().walkable,
                    "{point:?}"
                );
            }
        }
        let mut scratch = SearchScratch::new(full.cells().len());
        for (start, radius) in [
            (WalkPosition { x: 30, y: 20 }, 12),
            (WalkPosition { x: 1, y: 2 }, 48),
        ] {
            scratch.flood(&full, [start], radius);
            let reach = current.reach(start, radius);
            for y in 0..full.height() {
                for x in 0..full.width() {
                    let point = WalkPosition { x, y };
                    assert_eq!(
                        reach.walkable(point),
                        scratch.reached(full.index(point).unwrap()),
                        "{start:?} {point:?}"
                    );
                }
            }
        }
    }

    /// Discovery with pruned scoring must equal discovery that scores every candidate.
    fn discover_both(
        terrain: &TerrainGrid,
        resources: &[ResourceNode],
        obstacles: &[StaticObstacle],
        starts: &[PixelPosition],
        options: &BaseSearchOptions,
    ) -> BaseDiscovery {
        let pruned = discover(terrain, resources, obstacles, starts, options, true);
        let exhaustive = discover(terrain, resources, obstacles, starts, options, false);
        assert_eq!(pruned, exhaustive);
        pruned.unwrap()
    }

    #[test]
    fn a_taken_optimum_rescores_pruned_candidates() {
        // Only two depot tiles are buildable, and two separate fields both prefer the left one.
        // The second field's alternative was pruned against that optimum and must be rescored.
        let width = 40 * 4;
        let height = 20 * 4;
        let cells = (0..height)
            .flat_map(|y| {
                (0..width).map(move |x| TerrainCell {
                    walkable: true,
                    terrain_buildable: (68..88).contains(&x) && (32..44).contains(&y),
                    elevation: 0,
                    ramp: false,
                })
            })
            .collect();
        let terrain = TerrainGrid::from_cells(width, height, cells).unwrap();
        let resources: Vec<_> = [48, 80, 560, 592]
            .into_iter()
            .flat_map(|top| [384, 416].map(|left| resource(left, top, ResourceKind::Mineral, None)))
            .collect();
        let result = discover_both(
            &terrain,
            &resources,
            &[],
            &[],
            &BaseSearchOptions::default(),
        );
        let tiles: Vec<_> = result.bases.iter().map(|base| base.depot_tile).collect();
        assert_eq!(
            tiles,
            [TilePosition { x: 17, y: 8 }, TilePosition { x: 18, y: 8 }]
        );
    }

    #[test]
    fn pruned_scoring_matches_exhaustive_scoring() {
        let mut state = 0x2545_f491_4f6c_dd1d_u64;
        let mut next = move |bound: u32| {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            (state % u64::from(bound.max(1))) as u32
        };
        let loose = BaseSearchOptions {
            max_mineral_blocker_amount: Some(10_000),
            depot: DepotFootprint {
                width_tiles: 3,
                height_tiles: 2,
            },
            ..BaseSearchOptions::default()
        };
        let strict = BaseSearchOptions {
            max_mineral_blocker_amount: None,
            allow_destructible_clearing: false,
            allow_start_obstacle_clearing: false,
            ..BaseSearchOptions::default()
        };
        for _ in 0..40 {
            let size = 32 * 4;
            let mut walls = Vec::new();
            for _ in 0..next(8) {
                let (x, y) = (next(size), next(size));
                let (w, h) = if next(2) == 0 {
                    (1 + next(40), 1 + next(4))
                } else {
                    (1 + next(4), 1 + next(40))
                };
                walls.push((x, y, x + w, y + h, next(3) == 0));
            }
            let cells = (0..size)
                .flat_map(|y| {
                    let walls = &walls;
                    (0..size).map(move |x| {
                        let hit = walls.iter().find(|&&(left, top, right, bottom, _)| {
                            (left..right).contains(&x) && (top..bottom).contains(&y)
                        });
                        TerrainCell {
                            // Some walls are only unbuildable, like low ground beside a ramp.
                            walkable: hit.is_none_or(|wall| wall.4),
                            terrain_buildable: hit.is_none(),
                            elevation: 0,
                            ramp: false,
                        }
                    })
                })
                .collect();
            let terrain = TerrainGrid::from_cells(size, size, cells).unwrap();
            let pixels = (size * 8) as i32;
            let mut resources = Vec::new();
            for _ in 0..1 + next(4) {
                let x = 64 + next(pixels as u32 - 192) as i32;
                let y = 64 + next(pixels as u32 - 192) as i32;
                for _ in 0..3 + next(7) {
                    let left = x + next(160) as i32 - 80;
                    let top = y + next(160) as i32 - 80;
                    let amount = match next(4) {
                        0 => None,
                        1 => Some(next(9)),
                        _ => Some(500 + next(1_500)),
                    };
                    resources.push(ResourceNode {
                        bounds: PixelRect {
                            left,
                            top,
                            right: left + 64,
                            bottom: top + 32,
                        },
                        kind: ResourceKind::Mineral,
                        amount,
                    });
                }
                if next(2) == 0 {
                    let left = x + next(256) as i32 - 128;
                    let top = y + next(256) as i32 - 128;
                    resources.push(ResourceNode {
                        bounds: PixelRect {
                            left,
                            top,
                            right: left + 128,
                            bottom: top + 64,
                        },
                        kind: ResourceKind::Gas,
                        amount: Some(next(5_000)),
                    });
                }
            }
            let obstacles: Vec<_> = (0..next(12))
                .map(|_| {
                    let left = next(pixels as u32) as i32;
                    let top = next(pixels as u32) as i32;
                    StaticObstacle {
                        bounds: PixelRect {
                            left,
                            top,
                            right: left + 8 + next(64) as i32,
                            bottom: top + 8 + next(64) as i32,
                        },
                        destructible: next(2) == 0,
                    }
                })
                .collect();
            let mut starts: Vec<_> = (0..next(3))
                .map(|_| PixelPosition {
                    x: next(pixels as u32),
                    y: next(pixels as u32),
                })
                .collect();
            // Put a start exactly where a depot would sit near each field's first resource.
            for resource in resources.iter().step_by(4).take(2) {
                starts.push(PixelPosition {
                    x: (resource.bounds.left.max(0) as u32 / 32) * 32 + 128 + 64,
                    y: (resource.bounds.top.max(0) as u32 / 32) * 32 + 48,
                });
            }
            for options in [&BaseSearchOptions::default(), &strict, &loose] {
                discover_both(&terrain, &resources, &obstacles, &starts, options);
            }
        }
    }
}
