//! Deterministic, terrain-aware resource-base discovery.
//!
//! Placement is a static heuristic. It deliberately reports mineral blockers that must be cleared
//! instead of pretending that a conditional location is an immediately usable depot.

mod candidates;
mod clustering;
mod geometry;
mod occupancy;
mod proximity;
mod scoring;
mod search;
mod selection;
mod sweep;
#[cfg(test)]
mod tests;

use std::collections::{BTreeMap, HashSet};

use thiserror::Error;

use crate::{PixelRect, StaticObstacle, TerrainGrid, WalkPosition};
use candidates::{is_clearable, rect_is_in_map, start_depot_tile, start_near_depot};
use clustering::{cluster_sets, clusters_from_sets, perimeter_access_cells};
use geometry::stamp_obstacles;
use occupancy::{CurrentOccupancy, current_footprint_anchor, nearest_walkable};
use scoring::ScoringContext;
use search::ScoringScratch;
use selection::{CatalogResource, MiningResource, Uncertain};

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
    discover(terrain, resources, obstacles, starts, options, true).map(|(discovery, _)| discovery)
}

/// `prune` only changes how much scoring work is done, never the result. Also returns how many
/// fields selection had to re-score without pruning because a taken optimum left them uncertain.
fn discover(
    terrain: &TerrainGrid,
    resources: &[ResourceNode],
    obstacles: &[StaticObstacle],
    starts: &[PixelPosition],
    options: &BaseSearchOptions,
    prune: bool,
) -> Result<(BaseDiscovery, usize), BaseError> {
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
        let discovery = BaseDiscovery {
            bases: Vec::new(),
            unplaced_clusters: 0,
            ignored_resources,
        };
        return Ok((discovery, 0));
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
        let discovery = BaseDiscovery {
            bases: Vec::new(),
            unplaced_clusters: 0,
            ignored_resources,
        };
        return Ok((discovery, 0));
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
    let mut uncertain_rescores = 0;
    for cluster_index in cluster_order {
        let chosen = match scored_clusters[cluster_index].choose(
            cluster_index,
            &used_tiles,
            &start_owners,
        ) {
            Ok(chosen) => chosen,
            Err(Uncertain) => {
                // A taken optimum exposed candidates that scoring skipped; score every one.
                uncertain_rescores += 1;
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
    Ok((result, uncertain_rescores))
}
