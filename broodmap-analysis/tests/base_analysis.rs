use std::path::Path;

use broodmap::{
    Chk,
    chk::{
        placed_units::{PlacedUnit, UnitInstanceId, UnitState},
        sprites::{Sprite, SpriteFlags},
    },
};
use broodmap_analysis::{
    AnalysisAssets, AnalysisError, BaseAnalysis, BaseAnalysisError, BaseDiscoveryOutcome,
    BaseError, BaseSearchOptions, DepotFootprint, DiscoverySkipReason, MeleeBaseInputs,
    PixelPosition, PixelRect, ResourceKind, ResourceNode, StartLocation, StaticObstacle,
    TerrainCell, TerrainGrid, analyze_bases, discover_bases, melee_base_inputs,
};
use broodmap_formats::{Cv5, UnitsDat, Vf4, parse_cv5, parse_units_dat, parse_vf4};

const LT_CHK: &[u8] = include_bytes!("../../broodmap/assets/lt.chk");
const UNITS_DAT_LEN: usize = 19876;
const FLAGS_OFFSET: usize = 7032;
const BOUNDS_OFFSET: usize = 12580;

/// Every CV5 group resolves to one fully walkable, buildable megatile. Repository maps carry no
/// game tables, so terrain is flat, but every UNIT/THG2 placement is real.
fn flat_tables() -> (Cv5, Vf4) {
    let cv5 = parse_cv5(&vec![0; 52 * 2048]);
    let vf4: Vec<u8> = (0..16).flat_map(|_| 1u16.to_le_bytes()).collect();
    (cv5, parse_vf4(&vf4))
}

fn set_definition(bytes: &mut [u8], id: usize, flags: u32, bounds: [i16; 4]) {
    bytes[FLAGS_OFFSET + id * 4..FLAGS_OFFSET + id * 4 + 4].copy_from_slice(&flags.to_le_bytes());
    for (side, extent) in bounds.into_iter().enumerate() {
        let offset = BOUNDS_OFFSET + id * 8 + side * 2;
        bytes[offset..offset + 2].copy_from_slice(&extent.to_le_bytes());
    }
}

/// Stock mineral field and geyser collision extents with the resource-container flag.
fn stock_definition_bytes() -> Vec<u8> {
    let mut bytes = vec![0; UNITS_DAT_LEN];
    for id in [176, 177, 178] {
        set_definition(&mut bytes, id, 0x2000, [16, 8, 15, 7]);
    }
    set_definition(&mut bytes, 188, 0x2000, [32, 16, 31, 15]);
    bytes
}

fn analyze(chk: &Chk, units: &UnitsDat) -> Result<BaseAnalysis, BaseAnalysisError> {
    let (cv5, vf4) = flat_tables();
    analyze_bases(
        chk,
        AnalysisAssets {
            cv5: &cv5,
            vf4: &vf4,
            units,
        },
        &BaseSearchOptions::default(),
    )
}

fn unit(unit_id: u16, x: u16, y: u16) -> PlacedUnit {
    PlacedUnit {
        instance_id: UnitInstanceId(1),
        x,
        y,
        unit_id,
        owner: None,
        hp_percent: None,
        shield_percent: None,
        energy_percent: None,
        resource_amount: None,
        hangar_count: None,
        state: UnitState::empty(),
        linked_id: None,
    }
}

#[test]
fn melee_map_finds_a_base_for_every_start() {
    let chk = Chk::from_bytes(LT_CHK.to_vec(), None).unwrap();
    let definitions = parse_units_dat(&stock_definition_bytes());
    let analysis = analyze(&chk, &definitions).unwrap();
    assert_eq!(analysis.terrain.width(), 128 * 4);
    assert_eq!(analysis.terrain.height(), 128 * 4);
    let inputs = &analysis.inputs;
    assert_eq!(inputs.starts.len(), 4);
    assert!(inputs.mineral_count() >= 8 * 4);
    assert!(inputs.gas_count() >= 4);
    assert_eq!(inputs.ignored_resources, 0);

    let BaseDiscoveryOutcome::Complete(discovery) = &analysis.discovery else {
        panic!("unexpected outcome {:?}", analysis.discovery);
    };
    for start in 0..inputs.starts.len() {
        let owners: Vec<_> = discovery
            .bases
            .iter()
            .filter(|base| base.start_indices.contains(&start))
            .collect();
        assert_eq!(owners.len(), 1, "start {start}");
    }
    assert!(discovery.bases.len() > inputs.starts.len());
    assert!(discovery.bases.iter().all(|base| {
        base.resource_indices
            .iter()
            .all(|&index| index < inputs.resources.len())
    }));

    // The one-call result is exactly discovery over the returned inputs.
    let direct = discover_bases(
        &analysis.terrain,
        &inputs.resources,
        &inputs.obstacles,
        &inputs.start_positions(),
        &BaseSearchOptions::default(),
    )
    .unwrap();
    assert_eq!(*discovery, direct);
}

#[test]
fn repository_maps_analyze_without_errors() {
    let definitions = parse_units_dat(&stock_definition_bytes());
    let assets = Path::new(env!("CARGO_MANIFEST_DIR")).join("../broodmap/assets");
    let mut analyzed = 0;
    for entry in std::fs::read_dir(assets).unwrap() {
        let path = entry.unwrap().path();
        if path.extension().is_none_or(|extension| extension != "chk") {
            continue;
        }
        let Ok(chk) = Chk::from_bytes(std::fs::read(&path).unwrap(), None) else {
            continue;
        };
        let analysis = analyze(&chk, &definitions)
            .unwrap_or_else(|error| panic!("{}: {error}", path.display()));
        if let Some(discovery) = analysis.discovery.discovery() {
            assert!(discovery.bases.iter().all(|base| {
                base.start_indices
                    .iter()
                    .all(|&index| index < analysis.inputs.starts.len())
            }));
        }
        analyzed += 1;
    }
    assert!(analyzed >= 10, "only {analyzed} maps analyzed");
}

#[test]
fn dense_resource_field_skips_discovery_but_reports_inputs() {
    // UNIT chunks append, so a second chunk adds a 600-patch field to a real melee map.
    const EXTRA: usize = 600;
    let mut bytes = LT_CHK.to_vec();
    bytes.extend_from_slice(b"UNIT");
    bytes.extend_from_slice(&((EXTRA * 36) as u32).to_le_bytes());
    for index in 0..EXTRA {
        let mut record = [0u8; 36];
        record[0..4].copy_from_slice(&(10_000 + index as u32).to_le_bytes());
        let x = 64 + (index % 30) as u16 * 64;
        let y = 64 + (index / 30) as u16 * 32;
        record[4..6].copy_from_slice(&x.to_le_bytes());
        record[6..8].copy_from_slice(&y.to_le_bytes());
        record[8..10].copy_from_slice(&176u16.to_le_bytes());
        // Owner and resource amount are valid; owner 11 is neutral.
        record[14..16].copy_from_slice(&0x11u16.to_le_bytes());
        record[16] = 11;
        record[20..24].copy_from_slice(&1500u32.to_le_bytes());
        bytes.extend_from_slice(&record);
    }
    let definitions = parse_units_dat(&stock_definition_bytes());
    let base_resources = analyze(
        &Chk::from_bytes(LT_CHK.to_vec(), None).unwrap(),
        &definitions,
    )
    .unwrap()
    .inputs
    .resources
    .len();

    let analysis = analyze(&Chk::from_bytes(bytes, None).unwrap(), &definitions).unwrap();
    let actual = base_resources + EXTRA;
    assert_eq!(
        analysis.discovery,
        BaseDiscoveryOutcome::Skipped(DiscoverySkipReason::DenseResourceField {
            maximum: 512,
            actual,
        })
    );
    assert_eq!(analysis.discovery.discovery(), None);
    assert_eq!(analysis.inputs.resources.len(), actual);
    assert_eq!(analysis.inputs.starts.len(), 4);
    assert_eq!(analysis.inputs.resources[actual - 1].amount, Some(1500));
}

#[test]
fn too_many_starts_skip_discovery() {
    let terrain = TerrainGrid::from_cells(
        64,
        64,
        vec![
            TerrainCell {
                walkable: true,
                terrain_buildable: true,
                elevation: 0,
                ramp: false,
            };
            64 * 64
        ],
    )
    .unwrap();
    let inputs = MeleeBaseInputs {
        starts: vec![
            StartLocation {
                position: PixelPosition { x: 64, y: 64 },
                owner: None,
            };
            65
        ],
        ..MeleeBaseInputs::default()
    };
    let analysis =
        BaseAnalysis::from_inputs(terrain.clone(), inputs, &BaseSearchOptions::default()).unwrap();
    assert_eq!(
        analysis.discovery,
        BaseDiscoveryOutcome::Skipped(DiscoverySkipReason::TooManyStarts {
            maximum: 64,
            actual: 65,
        })
    );
    assert_eq!(analysis.inputs.starts.len(), 65);

    // Invalid options remain errors rather than a skipped analysis.
    let options = BaseSearchOptions {
        depot: DepotFootprint {
            width_tiles: 0,
            height_tiles: 3,
        },
        ..BaseSearchOptions::default()
    };
    assert!(matches!(
        BaseAnalysis::from_inputs(terrain, MeleeBaseInputs::default(), &options),
        Err(BaseError::InvalidFootprint { .. })
    ));
}

#[test]
fn missing_terrain_tables_are_errors() {
    let chk = Chk::from_bytes(LT_CHK.to_vec(), None).unwrap();
    let definitions = parse_units_dat(&stock_definition_bytes());
    let (_, vf4) = flat_tables();
    let result = analyze_bases(
        &chk,
        AnalysisAssets {
            cv5: &parse_cv5(&[]),
            vf4: &vf4,
            units: &definitions,
        },
        &BaseSearchOptions::default(),
    );
    assert!(matches!(
        result,
        Err(BaseAnalysisError::Grid(
            AnalysisError::MissingCv5Group { .. }
        ))
    ));
}

#[test]
fn adapter_classifies_stock_resources_starts_and_ignored_definitions() {
    let mut bytes = stock_definition_bytes();
    // A modded geyser that is not a grounded resource container is not a resource node.
    set_definition(&mut bytes, 188, 0x2000 | 0x0004, [32, 16, 31, 15]);
    let definitions = parse_units_dat(&bytes);
    let mineral = PlacedUnit {
        resource_amount: Some(1),
        ..unit(176, 100, 200)
    };
    let start = PlacedUnit {
        owner: Some(2),
        ..unit(214, 400, 300)
    };
    let hallucinated = PlacedUnit {
        state: UnitState::HALLUCINATED,
        ..unit(177, 500, 500)
    };
    let refinery = unit(110, 600, 600);
    let geyser = unit(188, 700, 700);
    let sprites = [
        Sprite {
            id: 178,
            x: 256,
            y: 256,
            owner: 11,
            flags: SpriteFlags::empty(),
        },
        // Non-neutral and pure sprites are not resources.
        Sprite {
            id: 178,
            x: 320,
            y: 256,
            owner: 0,
            flags: SpriteFlags::empty(),
        },
        Sprite {
            id: 178,
            x: 384,
            y: 256,
            owner: 11,
            flags: SpriteFlags::DRAW_AS_SPRITE,
        },
    ];
    let inputs = melee_base_inputs(
        &[mineral, start, hallucinated, refinery, geyser],
        &sprites,
        &definitions,
    );
    assert_eq!(inputs.resources.len(), 2);
    assert_eq!(
        inputs.resources[0].bounds,
        PixelRect {
            left: 84,
            top: 192,
            right: 116,
            bottom: 208,
        }
    );
    assert_eq!(inputs.resources[0].kind, ResourceKind::Mineral);
    assert_eq!(inputs.resources[0].amount, Some(1));
    assert_eq!(inputs.resources[1].amount, None);
    assert_eq!(
        inputs.starts,
        vec![StartLocation {
            position: PixelPosition { x: 400, y: 300 },
            owner: Some(2),
        }]
    );
    assert_eq!(inputs.ignored_resources, 1);
    assert_eq!((inputs.mineral_count(), inputs.gas_count()), (2, 0));
    assert!(inputs.obstacles.is_empty());
}

#[test]
fn tiny_mineral_keeps_overlapping_non_resource_objects() {
    let mut bytes = stock_definition_bytes();
    set_definition(&mut bytes, 190, 0x0001, [15, 15, 15, 15]);
    let definitions = parse_units_dat(&bytes);
    let mineral = PlacedUnit {
        resource_amount: Some(1),
        ..unit(176, 160, 160)
    };
    let building = unit(190, 160, 160);
    let inputs = melee_base_inputs(&[mineral, building], &[], &definitions);
    assert_eq!(inputs.resources.len(), 1);
    assert_eq!(
        inputs.obstacles,
        vec![StaticObstacle {
            bounds: PixelRect {
                left: 145,
                top: 145,
                right: 176,
                bottom: 176,
            },
            destructible: true,
        }]
    );
    // A modded stock resource ID with only BUILDING remains a static object.
    set_definition(&mut bytes, 176, 0x0001, [16, 8, 15, 7]);
    let inputs = melee_base_inputs(&[mineral], &[], &parse_units_dat(&bytes));
    assert!(inputs.resources.is_empty());
    assert_eq!(inputs.ignored_resources, 1);
    assert_eq!(inputs.obstacles.len(), 1);
}

#[test]
fn invalid_options_and_grids_are_errors_even_past_count_limits() {
    let flat = |width, height| {
        TerrainGrid::from_cells(
            width,
            height,
            vec![
                TerrainCell {
                    walkable: true,
                    terrain_buildable: true,
                    elevation: 0,
                    ramp: false,
                };
                (width * height) as usize
            ],
        )
        .unwrap()
    };
    let crowded = MeleeBaseInputs {
        resources: vec![
            ResourceNode {
                bounds: PixelRect {
                    left: 64,
                    top: 64,
                    right: 96,
                    bottom: 80,
                },
                kind: ResourceKind::Mineral,
                amount: None,
            };
            513
        ],
        starts: vec![
            StartLocation {
                position: PixelPosition { x: 64, y: 64 },
                owner: None,
            };
            65
        ],
        ..MeleeBaseInputs::default()
    };
    let too_many_starts = MeleeBaseInputs {
        resources: Vec::new(),
        ..crowded.clone()
    };
    let zero_width = BaseSearchOptions {
        depot: DepotFootprint {
            width_tiles: 0,
            height_tiles: 3,
        },
        ..BaseSearchOptions::default()
    };
    for inputs in [&crowded, &too_many_starts] {
        assert!(matches!(
            BaseAnalysis::from_inputs(flat(64, 64), inputs.clone(), &zero_width),
            Err(BaseError::InvalidFootprint { .. })
        ));
        assert!(matches!(
            BaseAnalysis::from_inputs(flat(63, 64), inputs.clone(), &BaseSearchOptions::default()),
            Err(BaseError::GridNotTileAligned {
                width: 63,
                height: 64
            })
        ));
    }
}
