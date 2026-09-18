use broodmap_analysis::{
    BaseSearchOptions, PixelPosition, PixelRect, ResourceKind, ResourceNode, TerrainCell,
    TerrainGrid, discover_bases,
};

fn grid(w: u32, h: u32, wall: Option<PixelRect>) -> TerrainGrid {
    let mut cells = vec![
        TerrainCell {
            walkable: true,
            terrain_buildable: true,
            elevation: 0,
            ramp: false
        };
        (w * h * 16) as usize
    ];
    if let Some(wall) = wall {
        for y in 0..h * 4 {
            for x in 0..w * 4 {
                let p = PixelRect {
                    left: (x * 8) as i32,
                    top: (y * 8) as i32,
                    right: (x * 8 + 8) as i32,
                    bottom: (y * 8 + 8) as i32,
                };
                if p.left < wall.right
                    && p.right > wall.left
                    && p.top < wall.bottom
                    && p.bottom > wall.top
                {
                    cells[(y * w * 4 + x) as usize].walkable = false;
                }
            }
        }
    }
    TerrainGrid::from_cells(w * 4, h * 4, cells).unwrap()
}
fn field(x: i32) -> Vec<ResourceNode> {
    [(x, 112), (x + 32, 112), (x, 144), (x + 32, 144)]
        .into_iter()
        .map(|(left, top)| ResourceNode {
            bounds: PixelRect {
                left,
                top,
                right: left + 16,
                bottom: top + 16,
            },
            kind: ResourceKind::Mineral,
            amount: None,
        })
        .collect()
}
fn two_fields(reverse: bool) -> Vec<ResourceNode> {
    let mut v = field(176);
    v.extend(field(688));
    if reverse {
        v.reverse();
    }
    v
}
fn opts() -> BaseSearchOptions {
    BaseSearchOptions::default()
}
fn start_count(r: &broodmap_analysis::BaseDiscovery) -> usize {
    r.bases.iter().map(|b| b.start_indices.len()).sum()
}

#[test]
fn nearby_start_is_associated_once_and_reversed_input_keeps_field_identity() {
    for reverse in [false, true] {
        let resources = two_fields(reverse);
        let result = discover_bases(
            &grid(32, 20, None),
            &resources,
            &[],
            &[PixelPosition { x: 448, y: 128 }],
            &opts(),
        )
        .unwrap();
        assert_eq!(result.bases.len(), 2);
        assert_ne!(result.bases[0].depot_tile, result.bases[1].depot_tile);
        assert!(result.bases.iter().all(|b| b.start_indices.len() <= 1));
        assert_eq!(start_count(&result), 1);
        let associated = result
            .bases
            .iter()
            .find(|b| !b.start_indices.is_empty())
            .unwrap();
        assert!(
            associated
                .resource_indices
                .iter()
                .all(|&i| resources[i].bounds.left < 400)
        );
    }
}

#[test]
fn separating_wall_prevents_cross_field_start_association() {
    let wall = PixelRect {
        left: 560,
        top: 0,
        right: 568,
        bottom: 640,
    };
    let resources = two_fields(false);
    let result = discover_bases(
        &grid(32, 20, Some(wall)),
        &resources,
        &[],
        &[PixelPosition { x: 448, y: 128 }],
        &opts(),
    )
    .unwrap();
    assert_eq!(result.bases.len(), 2);
    assert_eq!(start_count(&result), 1);
    let associated = result
        .bases
        .iter()
        .find(|b| !b.start_indices.is_empty())
        .unwrap();
    assert!(
        associated
            .resource_indices
            .iter()
            .all(|&i| resources[i].bounds.left < 400)
    );
    assert!(result.bases.iter().any(|b| {
        b.resource_indices
            .iter()
            .all(|&i| resources[i].bounds.left >= 600)
            && b.start_indices.is_empty()
    }));
}

fn plugged_terrain() -> TerrainGrid {
    let width = 24 * 4;
    let height = 20 * 4;
    let mut cells = vec![
        TerrainCell {
            walkable: true,
            terrain_buildable: true,
            elevation: 0,
            ramp: false
        };
        (width * height) as usize
    ];
    for y in 0..height {
        for x in 0..64 {
            cells[(y * width + x) as usize].terrain_buildable = false;
        }
        cells[(y * width + 50) as usize].walkable = false;
    }
    cells[(16 * width + 50) as usize].walkable = true;
    TerrainGrid::from_cells(width, height, cells).unwrap()
}

#[test]
fn blocked_only_exit_cannot_make_far_candidate_legal() {
    let mut resources = field(176);
    let open = discover_bases(&plugged_terrain(), &resources, &[], &[], &opts()).unwrap();
    assert_eq!(
        open.bases.len(),
        1,
        "opening must permit a far-side site before adding the plug"
    );
    assert!(open.bases[0].depot_tile.x >= 16);
    resources.push(ResourceNode {
        bounds: PixelRect {
            left: 400,
            top: 128,
            right: 408,
            bottom: 136,
        },
        kind: ResourceKind::Mineral,
        amount: Some(8),
    });
    let default_result = discover_bases(&plugged_terrain(), &resources, &[], &[], &opts()).unwrap();
    assert_eq!(default_result.unplaced_clusters, 1);
    let strict = BaseSearchOptions {
        max_mineral_blocker_amount: None,
        ..opts()
    };
    let strict_result = discover_bases(&plugged_terrain(), &resources, &[], &[], &strict).unwrap();
    assert_eq!(strict_result.unplaced_clusters, 1);
    let mut nearby = field(176);
    nearby.push(ResourceNode {
        bounds: PixelRect {
            left: 320,
            top: 256,
            right: 448,
            bottom: 352,
        },
        kind: ResourceKind::Mineral,
        amount: Some(1),
    });
    let positive = discover_bases(&grid(24, 20, None), &nearby, &[], &[], &opts()).unwrap();
    assert!(
        positive
            .bases
            .iter()
            .any(|b| b.required_mineral_indices.contains(&4))
    );
}

#[test]
fn start_ownership_compares_gas_and_mineral_only_fields_fairly() {
    let mut resources = field(176);
    resources.push(ResourceNode {
        bounds: PixelRect {
            left: 240,
            top: 112,
            right: 256,
            bottom: 128,
        },
        kind: ResourceKind::Gas,
        amount: Some(5000),
    });
    resources.extend(field(688));
    let result = discover_bases(
        &grid(32, 20, None),
        &resources,
        &[],
        &[PixelPosition { x: 448, y: 128 }],
        &opts(),
    )
    .unwrap();
    let base = result
        .bases
        .iter()
        .find(|b| b.start_indices == [0])
        .unwrap();
    assert!(
        base.resource_indices
            .iter()
            .all(|&i| resources[i].bounds.left < 400)
    );
}
