use super::*;
use crate::bases::{UNCERTAIN_RESCORES, discover};

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

/// Random maps almost never make a pruned selection uncertain, which needs two fields to want
/// the same depot tile. This generator makes fields compete for it: open ground that is buildable
/// only in a few small pockets near the center, ringed by mineral lines far enough apart to stay
/// separate fields. More lines on the ring mostly merge into one field and contest less.
#[test]
fn contested_pockets_match_exhaustive_scoring() {
    // Unit directions in thousandths, every 22.5 degrees.
    const DIRECTIONS: [(i32, i32); 16] = [
        (1000, 0),
        (924, 383),
        (707, 707),
        (383, 924),
        (0, 1000),
        (-383, 924),
        (-707, 707),
        (-924, 383),
        (-1000, 0),
        (-924, -383),
        (-707, -707),
        (-383, -924),
        (0, -1000),
        (383, -924),
        (707, -707),
        (924, -383),
    ];
    let mut state = 0x9e37_79b9_7f4a_7c15_u64;
    let mut next = move |bound: u32| {
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        (state % u64::from(bound.max(1))) as u32
    };
    let tiles = 32;
    let size = tiles * 4;
    let center = (tiles * 16) as i32;
    UNCERTAIN_RESCORES.with(|count| count.set(0));
    for _ in 0..200 {
        let pockets: Vec<_> = (0..2 + next(2))
            .map(|_| {
                let (w, h) = (4 + next(3), 3 + next(2));
                let x = tiles / 2 - w / 2 + next(9) - 4;
                let y = tiles / 2 - h / 2 + next(9) - 4;
                (x, y, x + w, y + h)
            })
            .collect();
        let cells = (0..size)
            .flat_map(|y| {
                let pockets = &pockets;
                (0..size).map(move |x| TerrainCell {
                    walkable: true,
                    terrain_buildable: pockets.iter().any(|&(left, top, right, bottom)| {
                        (left..right).contains(&(x / 4)) && (top..bottom).contains(&(y / 4))
                    }),
                    elevation: 0,
                    ramp: false,
                })
            })
            .collect();
        let terrain = TerrainGrid::from_cells(size, size, cells).unwrap();
        let mut resources = Vec::new();
        let lines = 2 + next(2);
        let first = next(16);
        for line in 0..lines {
            let direction = (first + line * 16 / lines + next(2)) % 16;
            let (dx, dy) = DIRECTIONS[direction as usize];
            let radius = (200 + next(161)) as i32;
            let x = center + radius * dx / 1000;
            let y = center + radius * dy / 1000;
            // Patches run tangent to the ring, so the line faces the pockets.
            let patches = 4 + next(5) as i32;
            for patch in 0..patches {
                let offset = (patch - patches / 2) * 32;
                let left = x - offset * dy / 1000 - 32;
                let top = y + offset * dx / 1000 - 16;
                let amount = match next(6) {
                    0 => Some(next(9)),
                    1 => None,
                    _ => Some(500 + next(1_000)),
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
                // Past one end of the line, set back from the pockets.
                let along = patches * 16 + 64;
                let left = x - along * dy / 1000 + 48 * dx / 1000 - 64;
                let top = y + along * dx / 1000 + 48 * dy / 1000 - 32;
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
        // Any object in a pocket this small usually removes it, so keep them rare.
        let obstacles: Vec<_> = (0..next(2))
            .map(|_| {
                let (x, y, _, _) = pockets[next(pockets.len() as u32) as usize];
                let left = (x * 32 + next(160)) as i32;
                let top = (y * 32 + next(128)) as i32;
                StaticObstacle {
                    bounds: PixelRect {
                        left,
                        top,
                        right: left + 16 + next(48) as i32,
                        bottom: top + 16 + next(48) as i32,
                    },
                    destructible: next(2) == 0,
                }
            })
            .collect();
        // Exact starts on pocket tiles.
        let starts: Vec<_> = (0..next(4))
            .map(|_| {
                let (x, y, right, bottom) = pockets[next(pockets.len() as u32) as usize];
                PixelPosition {
                    x: (x + next(right - x)) * 32 + 64,
                    y: (y + next(bottom - y)) * 32 + 48,
                }
            })
            .collect();
        let options = BaseSearchOptions {
            max_mineral_blocker_amount: [None, Some(8), Some(600)][next(3) as usize],
            allow_destructible_clearing: next(2) == 0,
            allow_start_obstacle_clearing: next(2) == 0,
            ..BaseSearchOptions::default()
        };
        discover_both(&terrain, &resources, &obstacles, &starts, &options);
    }
    let rescores = UNCERTAIN_RESCORES.with(|count| count.get());
    assert!(rescores >= 5, "uncertain selections re-scored: {rescores}");
}
