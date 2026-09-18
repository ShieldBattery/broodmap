#![no_main]

use broodmap::chk::{
    placed_units::read_placed_units,
    sprites::read_sprites,
    terrain::{TerrainTileIds, TileId},
};
use broodmap_analysis::{
    BaseSearchOptions, DepotFootprint, PixelPosition, PixelRect, ResourceKind, ResourceNode,
    StaticObstacle, TerrainCell, TerrainGrid, WalkPosition, discover_bases, melee_obstacles,
};
use broodmap_formats::{parse_cv5, parse_units_dat, parse_vf4};
use libfuzzer_sys::fuzz_target;

const MAX_METADATA_BYTES: usize = 4096;
const MAX_TERRAIN_AXIS: usize = 4;
const MAX_GRID_AXIS: u32 = 32;
const MAX_ROUTE_POINTS: usize = 4096;
const MAX_OBSTACLES: usize = 64;

fn obstacle_rectangles(data: &[u8]) -> Vec<PixelRect> {
    let count = data.first().copied().unwrap_or(0) as usize % (MAX_OBSTACLES + 1);
    let mut obstacles = Vec::with_capacity(count);
    for index in 0..count {
        let offset = 1 + index * 16;
        let coordinate = |part: usize| {
            let bytes = [
                data.get(offset + part * 4).copied().unwrap_or(0),
                data.get(offset + part * 4 + 1).copied().unwrap_or(0),
                data.get(offset + part * 4 + 2).copied().unwrap_or(0),
                data.get(offset + part * 4 + 3).copied().unwrap_or(0),
            ];
            i32::from_le_bytes(bytes)
        };
        obstacles.push(PixelRect {
            left: coordinate(0),
            top: coordinate(1),
            right: coordinate(2),
            bottom: coordinate(3),
        });
    }
    obstacles
}

fn exercise_grid(grid: &TerrainGrid, data: &[u8]) {
    let width = grid.width();
    let height = grid.height();
    for y in 0..height {
        for x in 0..width {
            let _ = grid.cell(WalkPosition { x, y });
        }
    }
    let _ = grid.cell(WalkPosition {
        x: u32::MAX,
        y: u32::MAX,
    });
    let endpoints = [
        (
            WalkPosition { x: 0, y: 0 },
            WalkPosition {
                x: width.saturating_sub(1),
                y: height.saturating_sub(1),
            },
        ),
        (
            WalkPosition { x: u32::MAX, y: 0 },
            WalkPosition { x: 0, y: 0 },
        ),
        (
            WalkPosition { x: 0, y: 0 },
            WalkPosition { x: 0, y: u32::MAX },
        ),
    ];
    for (start, end) in endpoints {
        if let Ok(Some(route)) = grid.route(start, end) {
            for point in route.points.iter().take(MAX_ROUTE_POINTS) {
                let _ = grid.cell(*point);
            }
            let _ = route.distance_pixels;
        }
    }
    if data.first().copied().unwrap_or(0) & 1 != 0 {
        let end = WalkPosition {
            x: width / 2,
            y: height / 2,
        };
        let _ = grid.route(WalkPosition { x: 0, y: 0 }, end);
    }
}

fn exercise_bases(grid: &TerrainGrid, data: &[u8], obstacles: &[PixelRect]) {
    let mut resources: Vec<_> = obstacle_rectangles(data)
        .into_iter()
        .take(12)
        .map(|bounds| ResourceNode {
            bounds,
            kind: ResourceKind::Mineral,
            amount: None,
        })
        .collect();
    // Also generate small, valid geometry so the fuzzer reaches clustering and placement.
    for bytes in data.as_chunks::<4>().0.iter().take(12) {
        let left = i32::from(bytes[0]) % (grid.width() as i32 * 8);
        let top = i32::from(bytes[1]) % (grid.height() as i32 * 8);
        resources.push(ResourceNode {
            bounds: PixelRect {
                left,
                top,
                right: left + 1 + i32::from(bytes[2] % 32),
                bottom: top + 8,
            },
            kind: if bytes[3] & 3 == 0 {
                ResourceKind::Gas
            } else {
                ResourceKind::Mineral
            },
            amount: Some(u32::from(bytes[3])),
        });
    }
    let starts = [
        PixelPosition {
            x: u32::MAX,
            y: u32::MAX,
        },
        PixelPosition {
            x: grid.width() * 4,
            y: grid.height() * 4,
        },
    ];
    let options = BaseSearchOptions {
        depot: DepotFootprint {
            width_tiles: u32::from(data.first().copied().unwrap_or(4) % 6),
            height_tiles: u32::from(data.get(1).copied().unwrap_or(3) % 6),
        },
        ..BaseSearchOptions::default()
    };
    let base_obstacles: Vec<_> = obstacles
        .iter()
        .enumerate()
        .map(|(index, &bounds)| StaticObstacle {
            bounds,
            destructible: data.get(index).is_some_and(|byte| byte & 1 != 0),
        })
        .collect();
    if let Ok(result) = discover_bases(grid, &resources, &base_obstacles, &starts, &options) {
        let mut current_obstacles = obstacles.to_vec();
        current_obstacles.extend(resources.iter().filter_map(|node| {
            let bounds = node.bounds;
            (bounds.left >= 0
                && bounds.top >= 0
                && bounds.right > bounds.left
                && bounds.bottom > bounds.top
                && i64::from(bounds.right) <= i64::from(grid.width()) * 8
                && i64::from(bounds.bottom) <= i64::from(grid.height()) * 8)
                .then_some(bounds)
        }));
        let occupied = grid.with_obstacles(&current_obstacles);
        let mut depot_tiles = std::collections::HashSet::new();
        let mut associated_starts = std::collections::HashSet::new();
        for base in result.bases {
            assert!(depot_tiles.insert(base.depot_tile));
            assert!(
                base.start_indices
                    .iter()
                    .all(|&index| associated_starts.insert(index))
            );
            assert!(base.route_anchor.is_none_or(|anchor| {
                occupied
                    .cell(anchor)
                    .is_some_and(|cell| cell.walkable && cell.terrain_buildable)
            }));
            assert!(
                base.resource_indices
                    .iter()
                    .all(|&index| index < resources.len())
            );
            assert!(base.required_mineral_indices.iter().all(|&index| {
                resources.get(index).is_some_and(|resource| {
                    resource.kind == ResourceKind::Mineral
                        && resource.amount.is_some_and(|amount| amount <= 8)
                })
            }));
            assert!(
                base.required_obstacle_indices
                    .iter()
                    .all(|&index| base_obstacles.get(index).is_some_and(|o| o.destructible))
            );
            assert!(
                base.start_cleared_obstacle_indices
                    .iter()
                    .all(|&index| index < base_obstacles.len())
            );
            assert!(
                base.start_cleared_obstacle_indices.is_empty() || !base.start_indices.is_empty()
            );
            assert!(base.start_indices.iter().all(|&index| index < starts.len()));
        }
    }
}

fuzz_target!(|data: &[u8]| {
    let Some((&w_byte, rest)) = data.split_first() else {
        return;
    };
    let Some((&h_byte, rest)) = rest.split_first() else {
        return;
    };
    let records = &data[..data.len().min(2048)];
    let units = read_placed_units(records).unwrap();
    let sprites = read_sprites(records).unwrap();
    let definitions = parse_units_dat(&data[..data.len().min(19876)]);
    let mut obstacles = obstacle_rectangles(data);
    obstacles.extend(melee_obstacles(&units, &sprites, &definitions));
    let tile_width = (w_byte as usize % MAX_TERRAIN_AXIS) + 1;
    let tile_height = (h_byte as usize % MAX_TERRAIN_AXIS) + 1;
    let tile_count = tile_width * tile_height;
    let tile_bytes_len = tile_count * 2;
    let (tile_bytes, metadata) = if rest.len() >= tile_bytes_len {
        rest.split_at(tile_bytes_len)
    } else {
        (rest, &[][..])
    };
    let mut tiles = Vec::with_capacity(tile_count);
    for index in 0..tile_count {
        let offset = index * 2;
        let lo = tile_bytes.get(offset).copied().unwrap_or(0);
        let hi = tile_bytes.get(offset + 1).copied().unwrap_or(0);
        tiles.push(TileId(u16::from_le_bytes([lo, hi])));
    }
    let terrain = TerrainTileIds {
        width: tile_width,
        height: tile_height,
        tiles,
    };
    let bounded_metadata = &metadata[..metadata.len().min(MAX_METADATA_BYTES * 2)];
    let split = bounded_metadata.len() / 2;
    let (cv5_bytes, vf4_bytes) = bounded_metadata.split_at(split);
    let cv5 = parse_cv5(cv5_bytes);
    let vf4 = parse_vf4(vf4_bytes);
    if let Ok(grid) = TerrainGrid::from_terrain(&terrain, &cv5, &vf4) {
        exercise_grid(&grid, data);
        exercise_grid(&grid.with_obstacles(&obstacles), data);
    }

    let control = data.get(0..4).unwrap_or(&[0, 0, 0, 0]);
    let width = match control[0] % 4 {
        0 => 0,
        1 => 1025,
        _ => (u32::from(control[1]) % MAX_GRID_AXIS) + 1,
    };
    let height = match control[2] % 4 {
        0 => 0,
        1 => 1025,
        _ => (u32::from(control[3]) % MAX_GRID_AXIS) + 1,
    };
    let expected = width
        .checked_mul(height)
        .and_then(|n| usize::try_from(n).ok());
    let cell_count = expected.filter(|&n| n <= (MAX_GRID_AXIS * MAX_GRID_AXIS) as usize);
    let mismatch = data.get(4).copied().unwrap_or(0) & 3 == 0;
    let count = cell_count
        .map(|n| if mismatch { n.saturating_sub(1) } else { n })
        .unwrap_or(0);
    let mut cells = Vec::with_capacity(count);
    for index in 0..count {
        let byte = data.get(5 + index).copied().unwrap_or(0);
        cells.push(TerrainCell {
            walkable: byte & 1 != 0,
            terrain_buildable: byte & 2 != 0,
            elevation: if byte & 0x80 != 0 { 3 } else { (byte >> 2) % 3 },
            ramp: byte & 0x10 != 0,
        });
    }
    if let Ok(grid) = TerrainGrid::from_cells(width, height, cells) {
        exercise_bases(&grid, data, &obstacles);
        exercise_grid(&grid, data);
        exercise_grid(&grid.with_obstacles(&obstacles), data);
    }
});
