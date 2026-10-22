use super::*;
use crate::bases::{
    PROXIMITY_RADIUS_CELLS,
    clustering::perimeter_access_cells,
    geometry::{footprint_perimeter, stamp_obstacles},
    occupancy::CurrentOccupancy,
    search::SearchScratch,
};

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
