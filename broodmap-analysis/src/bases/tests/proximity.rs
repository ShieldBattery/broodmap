use std::{cmp::Reverse, collections::BinaryHeap};

use crate::{
    PixelRect, TerrainCell, TerrainGrid, WalkPosition,
    bases::proximity::{CellBox, HALF_CAP, ProximityField, access_ring, half_cost},
};

struct Lcg(u64);

impl Lcg {
    fn next(&mut self, bound: u32) -> u32 {
        self.0 = self
            .0
            .wrapping_mul(6_364_136_223_846_793_005)
            .wrapping_add(1_442_695_040_888_963_407);
        ((self.0 >> 33) % u64::from(bound)) as u32
    }
}

/// Clumpy random walls: blocked rectangles over an open grid.
fn random_grid(rng: &mut Lcg, width: u32, height: u32) -> TerrainGrid {
    let mut walkable = vec![true; (width * height) as usize];
    for _ in 0..rng.next(60) {
        let x = rng.next(width);
        let y = rng.next(height);
        let w = 1 + rng.next(12);
        let h = 1 + rng.next(12);
        for yy in y..(y + h).min(height) {
            for xx in x..(x + w).min(width) {
                walkable[(yy * width + xx) as usize] = false;
            }
        }
    }
    let cells = walkable
        .into_iter()
        .map(|walkable| TerrainCell {
            walkable,
            terrain_buildable: true,
            elevation: 0,
            ramp: false,
        })
        .collect();
    TerrainGrid::from_cells(width, height, cells).unwrap()
}

fn random_rect(rng: &mut Lcg, width: u32, height: u32, max_size: u32) -> PixelRect {
    let left = rng.next(width * 8 - 1) as i32;
    let top = rng.next(height * 8 - 1) as i32;
    PixelRect {
        left,
        top,
        right: (left + 1 + rng.next(max_size) as i32).min(width as i32 * 8),
        bottom: (top + 1 + rng.next(max_size) as i32).min(height as i32 * 8),
    }
}

/// The per-resource search this module replaces: Dijkstra from the walkable access ring,
/// never relaxing past the cap.
fn reference_costs(grid: &TerrainGrid, bounds: PixelRect) -> Vec<Option<u32>> {
    let (ring, body) = access_ring(grid, bounds).unwrap();
    let mut costs = vec![None; grid.cells().len()];
    let mut queue = BinaryHeap::new();
    for y in ring.y0..=ring.y1 {
        for x in ring.x0..=ring.x1 {
            if (body.x0..=body.x1).contains(&x) && (body.y0..=body.y1).contains(&y) {
                continue;
            }
            let index = grid
                .index(WalkPosition {
                    x: x as u32,
                    y: y as u32,
                })
                .unwrap();
            if grid.cells()[index].walkable {
                costs[index] = Some(0);
                queue.push(Reverse((0_u32, index)));
            }
        }
    }
    while let Some(Reverse((cost, index))) = queue.pop() {
        if costs[index] != Some(cost) {
            continue;
        }
        for (next, step) in grid.neighbors(grid.position(index)) {
            let next_cost = cost + step as u32;
            let next = grid.index(next).unwrap();
            if next_cost <= HALF_CAP as u32 * 2 && costs[next].is_none_or(|old| next_cost < old) {
                costs[next] = Some(next_cost);
                queue.push(Reverse((next_cost, next)));
            }
        }
    }
    costs
}

#[test]
fn lanes_match_capped_dijkstra_within_targets() {
    let mut rng = Lcg(7);
    let mut narrow = ProximityField::<8>::new();
    let mut wide = ProximityField::<16>::new();
    for case in 0..40 {
        let (width, height) = (96 + rng.next(160), 96 + rng.next(160));
        let terrain = random_grid(&mut rng, width, height);
        let blockers: Vec<_> = (0..rng.next(20))
            .map(|_| random_rect(&mut rng, width, height, 80))
            .collect();
        let grid = terrain.with_obstacles(&blockers);
        let count = 1 + rng.next(16) as usize;
        let resources: Vec<_> = (0..count)
            .map(|_| random_rect(&mut rng, width, height, 64))
            .collect();
        let x0 = i64::from(rng.next(width));
        let y0 = i64::from(rng.next(height));
        let targets = CellBox {
            x0,
            y0,
            x1: (x0 + i64::from(rng.next(120))).min(i64::from(width) - 1),
            y1: (y0 + i64::from(rng.next(120))).min(i64::from(height) - 1),
        };
        if count <= 8 {
            narrow.compute(&terrain, &blockers, &resources, targets);
        } else {
            wide.compute(&terrain, &blockers, &resources, targets);
        }
        let lanes = |x: u32, y: u32| -> Vec<Option<u32>> {
            let to_cost = |lane: &i16| half_cost(*lane).map(|half| u32::from(half) * 2);
            if count <= 8 {
                narrow.at(x, y).map_or(vec![None; count], |lanes| {
                    lanes[..count].iter().map(to_cost).collect()
                })
            } else {
                wide.at(x, y).map_or(vec![None; count], |lanes| {
                    lanes[..count].iter().map(to_cost).collect()
                })
            }
        };
        let expected: Vec<_> = resources
            .iter()
            .map(|&bounds| reference_costs(&grid, bounds))
            .collect();
        for y in targets.y0..=targets.y1 {
            for x in targets.x0..=targets.x1 {
                let index = (y * i64::from(width) + x) as usize;
                let actual = lanes(x as u32, y as u32);
                for (lane, costs) in expected.iter().enumerate() {
                    assert_eq!(
                        actual[lane], costs[index],
                        "case {case} lane {lane} at ({x}, {y})"
                    );
                }
            }
        }
    }
}
