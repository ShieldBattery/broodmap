use broodmap_analysis::{
    BaseSearchOptions, PixelPosition, PixelRect, ResourceKind, ResourceNode, StaticObstacle,
    TerrainCell, TerrainGrid, discover_bases,
};

fn open_grid(w: u32, h: u32) -> TerrainGrid {
    TerrainGrid::from_cells(
        w * 4,
        h * 4,
        vec![
            TerrainCell {
                walkable: true,
                terrain_buildable: true,
                elevation: 0,
                ramp: false,
            };
            (w * h * 16) as usize
        ],
    )
    .unwrap()
}
fn field() -> Vec<ResourceNode> {
    [(176, 112), (208, 112), (176, 144), (208, 144)]
        .into_iter()
        .map(|(l, t)| ResourceNode {
            bounds: PixelRect {
                left: l,
                top: t,
                right: l + 16,
                bottom: t + 16,
            },
            kind: ResourceKind::Mineral,
            amount: None,
        })
        .collect()
}
fn footprint((x, y): (u32, u32)) -> PixelRect {
    PixelRect {
        left: (x * 32) as i32,
        top: (y * 32) as i32,
        right: ((x + 4) * 32) as i32,
        bottom: ((y + 3) * 32) as i32,
    }
}
fn contains(r: PixelRect, p: PixelPosition) -> bool {
    r.left <= p.x as i32 && (p.x as i32) < r.right && r.top <= p.y as i32 && (p.y as i32) < r.bottom
}

#[test]
fn obstacle_clearance_is_conditional_and_access_scoring_keeps_walls() {
    let terrain = open_grid(24, 20);
    let resources = field();
    let options = BaseSearchOptions::default();
    let baseline = discover_bases(&terrain, &resources, &[], &[], &options).unwrap();
    let ideal = baseline
        .bases
        .first()
        .expect("fixture has a candidate")
        .depot_tile;
    let covering = footprint((ideal.x, ideal.y));
    let destructible = [StaticObstacle {
        bounds: covering,
        destructible: true,
    }];
    let cleared = discover_bases(&terrain, &resources, &destructible, &[], &options).unwrap();
    let conditional = cleared
        .bases
        .iter()
        .find(|b| b.depot_tile == ideal)
        .expect("destructible overlap keeps ideal");
    assert_eq!(conditional.required_obstacle_indices, vec![0]);
    assert!(conditional.start_cleared_obstacle_indices.is_empty());
    assert_eq!(conditional.route_anchor, None);

    let blocked = discover_bases(
        &terrain,
        &resources,
        &[
            destructible[0],
            StaticObstacle {
                bounds: covering,
                destructible: false,
            },
        ],
        &[],
        &options,
    )
    .unwrap();
    assert!(blocked.bases.iter().all(|b| b.depot_tile != ideal));

    // A wall between the resources and the ideal site must not disappear merely because it
    // is destructible. It does not touch the ideal footprint, so that site cannot clear it.
    assert!(covering.top >= 192, "fixture's ideal is below the field");
    let wall = StaticObstacle {
        bounds: PixelRect {
            left: 0,
            top: 176,
            right: 768,
            bottom: 184,
        },
        destructible: true,
    };
    let walled = discover_bases(&terrain, &resources, &[wall], &[], &options).unwrap();
    assert!(walled.bases.iter().all(|base| base.depot_tile != ideal));
    assert!(
        walled
            .bases
            .iter()
            .all(|b| b.route_anchor.is_none_or(|a| !contains(
                wall.bounds,
                PixelPosition {
                    x: a.x * 8 + 4,
                    y: a.y * 8 + 4
                }
            )))
    );
}
