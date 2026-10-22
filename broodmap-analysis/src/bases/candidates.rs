//! Candidate depot tiles around a field, their clearance requirements, and resource weights.

use super::{
    BaseSearchOptions, DepotFootprint, PIXELS_PER_TILE, PixelPosition, RESOURCE_GAP_PIXELS,
    ResourceKind, ResourceNode, SEARCH_RADIUS_TILES, START_ASSOCIATION_PIXELS, TilePosition,
    scoring::ScoringContext,
    selection::{CandidateLocation, CatalogResource, MiningResource},
};
use crate::{PixelRect, TerrainGrid};

impl ScoringContext<'_> {
    pub(super) fn candidates(&self, cluster: &[usize]) -> Vec<CandidateLocation> {
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
}

pub(super) fn category_balanced_center_deviation(
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

pub(super) fn category_reference(
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

pub(super) fn resource_weight(node: ResourceNode, reference: u32) -> u32 {
    node.amount.unwrap_or(reference).min(reference).max(1)
}

pub(super) fn is_clearable(node: ResourceNode, options: &BaseSearchOptions) -> bool {
    node.kind == ResourceKind::Mineral
        && node
            .amount
            .zip(options.max_mineral_blocker_amount)
            .is_some_and(|(amount, maximum)| amount <= maximum)
}

pub(super) fn rect_is_in_map(bounds: PixelRect, width: i64, height: i64) -> bool {
    i64::from(bounds.left) >= 0
        && i64::from(bounds.top) >= 0
        && i64::from(bounds.right) > i64::from(bounds.left)
        && i64::from(bounds.bottom) > i64::from(bounds.top)
        && i64::from(bounds.right) <= width
        && i64::from(bounds.bottom) <= height
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

pub(super) fn intersects_gap(tile: TilePosition, depot: DepotFootprint, bounds: PixelRect) -> bool {
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

pub(super) fn start_depot_tile(
    start: PixelPosition,
    depot: DepotFootprint,
) -> Option<TilePosition> {
    let x = (i64::from(start.x) - i64::from(depot.width_tiles) * 16).div_euclid(32);
    let y = (i64::from(start.y) - i64::from(depot.height_tiles) * 16).div_euclid(32);
    (x >= 0 && y >= 0).then(|| TilePosition {
        x: u32::try_from(x).expect("nonnegative"),
        y: u32::try_from(y).expect("nonnegative"),
    })
}

pub(super) fn start_near_depot(
    start: PixelPosition,
    tile: TilePosition,
    depot: DepotFootprint,
) -> bool {
    let dx = i64::from(start.x) - (i64::from(tile.x) * 32 + i64::from(depot.width_tiles) * 16);
    let dy = i64::from(start.y) - (i64::from(tile.y) * 32 + i64::from(depot.height_tiles) * 16);
    dx * dx + dy * dy <= START_ASSOCIATION_PIXELS * START_ASSOCIATION_PIXELS
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
