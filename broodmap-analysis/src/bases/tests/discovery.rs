use super::*;
use crate::bases::{
    MAX_RESOURCES, MAX_STARTS,
    candidates::{category_reference, intersects_gap, is_clearable},
    selection::{CatalogResource, MiningResource},
};

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
