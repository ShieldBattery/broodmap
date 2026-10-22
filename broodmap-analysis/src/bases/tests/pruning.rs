use super::*;
use crate::bases::discover;

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
