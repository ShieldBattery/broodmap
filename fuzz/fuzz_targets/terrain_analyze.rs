#![no_main]

use broodmap::chk::{
    placed_units::read_placed_units,
    sprites::read_sprites,
    terrain::{TerrainTileIds, TileId},
};
use broodmap_analysis::topology::{
    BaseTopology, TopologyBase, TopologyError, TopologyJob, TopologyOptions,
};
use broodmap_analysis::{
    BaseSearchOptions, BoundarySpan, DepotFootprint, EntranceError, EntranceOptions, PixelPosition,
    PixelRect, RegionOptions, ResourceKind, ResourceNode, StaticObstacle, TerrainCell, TerrainGrid,
    WalkPosition, discover_bases, melee_obstacles,
};
use broodmap_formats::{parse_cv5, parse_units_dat, parse_vf4};
use libfuzzer_sys::fuzz_target;
use std::sync::Arc;

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

// Independent graph oracle for region connectivity and passage coverage.
fn region_neighbors(grid: &TerrainGrid, index: usize) -> Vec<usize> {
    let width = grid.width() as usize;
    let x = (index % width) as u32;
    let y = (index / width) as u32;
    let mut result = Vec::new();
    for dy in -1..=1 {
        for dx in -1..=1 {
            if dx == 0 && dy == 0 {
                continue;
            }
            let (Some(nx), Some(ny)) = (x.checked_add_signed(dx), y.checked_add_signed(dy)) else {
                continue;
            };
            if !grid
                .cell(WalkPosition { x: nx, y: ny })
                .is_some_and(|c| c.walkable)
            {
                continue;
            }
            if dx != 0
                && dy != 0
                && (!grid
                    .cell(WalkPosition { x: nx, y })
                    .is_some_and(|c| c.walkable)
                    || !grid
                        .cell(WalkPosition { x, y: ny })
                        .is_some_and(|c| c.walkable))
            {
                continue;
            }
            result.push(ny as usize * width + nx as usize);
        }
    }
    result
}

fn exercise_regions(grid: &TerrainGrid, data: &[u8]) {
    let options = RegionOptions {
        min_prominence_pixels: 8 + u16::from(data.first().copied().unwrap_or(0) % 16) * 8,
        min_relative_prominence_percent: data.get(1).copied().unwrap_or(40) % 101,
    };
    let analysis = grid.regions(&options).unwrap();
    let labels = analysis.labels();
    assert_eq!(labels.len(), grid.cells().len());
    assert_eq!(
        analysis.region_at(WalkPosition {
            x: u32::MAX,
            y: u32::MAX
        }),
        None
    );
    let clearance = grid.clearance();
    let width = grid.width() as usize;
    let mut counts = vec![0; analysis.regions().len() + 1];
    let mut expected_pairs = std::collections::BTreeSet::new();
    for (index, (&label, cell)) in labels.iter().zip(grid.cells()).enumerate() {
        assert_eq!(label != 0, cell.walkable);
        assert!(label as usize <= analysis.regions().len());
        counts[label as usize] += 1;
        if label == 0 {
            continue;
        }
        for neighbor in region_neighbors(grid, index) {
            let other = labels[neighbor];
            if label != other {
                expected_pairs.insert([label.min(other), label.max(other)]);
            }
        }
    }
    let mut visited = vec![false; labels.len()];
    for (i, region) in analysis.regions().iter().enumerate() {
        assert_eq!(region.id as usize, i + 1);
        assert_eq!(analysis.region_at(region.peak), Some(region.id));
        assert_eq!(
            clearance.radius_pixels(region.peak),
            Some(region.peak_clearance_pixels)
        );
        assert_eq!(counts[region.id as usize], region.cell_count);
        let start = region.peak.y as usize * width + region.peak.x as usize;
        let mut queue = std::collections::VecDeque::from([start]);
        visited[start] = true;
        let mut reached = 0;
        while let Some(index) = queue.pop_front() {
            reached += 1;
            for next in region_neighbors(grid, index) {
                if labels[next] == region.id && !visited[next] {
                    visited[next] = true;
                    queue.push_back(next);
                }
            }
        }
        assert_eq!(reached, region.cell_count);
    }
    let mut pairs = std::collections::BTreeSet::new();
    for passage in analysis.passages() {
        assert!(pairs.insert(passage.regions));
        assert!(passage.regions[0] < passage.regions[1]);
        let [a, b] = passage.endpoints;
        assert_eq!(analysis.region_at(a), Some(passage.regions[0]));
        assert_eq!(analysis.region_at(b), Some(passage.regions[1]));
        assert!(
            region_neighbors(grid, a.y as usize * width + a.x as usize)
                .contains(&(b.y as usize * width + b.x as usize))
        );
        assert_eq!(
            passage.clearance_radius_pixels,
            clearance
                .radius_pixels(a)
                .unwrap()
                .min(clearance.radius_pixels(b).unwrap())
        );
    }
    assert_eq!(pairs, expected_pairs);
    if data.first().is_some_and(|byte| byte & 7 == 0) {
        assert_eq!(analysis, grid.regions(&options).unwrap());
        let coarser = grid
            .regions(&RegionOptions {
                min_prominence_pixels: options.min_prominence_pixels + 32,
                ..options
            })
            .unwrap();
        assert!(coarser.regions().len() <= analysis.regions().len());
        let relative_coarser = grid
            .regions(&RegionOptions {
                min_relative_prominence_percent: options
                    .min_relative_prominence_percent
                    .saturating_add(20)
                    .min(100),
                ..options
            })
            .unwrap();
        assert!(relative_coarser.regions().len() <= analysis.regions().len());
    }
}

fn exercise_ramps(grid: &TerrainGrid) {
    let ramps = grid.ramps();
    assert_eq!(ramps, grid.ramps());
    for (index, ramp) in ramps.iter().enumerate() {
        assert_eq!(ramp.id as usize, index + 1);
        assert_eq!(ramp.upper_elevation, ramp.lower_elevation + 1);
        assert!(ramp.cell_count >= 12);
        assert_ne!(ramp.lower.endpoints, ramp.upper.endpoints);
        assert_ne!(
            ramp.lower.endpoints,
            [ramp.upper.endpoints[1], ramp.upper.endpoints[0]]
        );
        for end in [&ramp.lower, &ramp.upper] {
            let cell = grid.cell(end.position).unwrap();
            assert!(cell.walkable && (cell.ramp || !cell.terrain_buildable));
            assert!(end.width_pixels.is_finite() && end.width_pixels > 0.0);
            for point in end.endpoints {
                assert!(point.x <= grid.width() * 8 && point.y <= grid.height() * 8);
            }
            let dx = end.endpoints[0].x.abs_diff(end.endpoints[1].x);
            let dy = end.endpoints[0].y.abs_diff(end.endpoints[1].y);
            assert!(dx == 0 || dy == 0 || dx == dy);
            let expected = f64::from(dx.max(dy)) * if dx != 0 && dy != 0 { 1.414 } else { 1.0 };
            assert!((end.width_pixels - expected).abs() < 0.00001);
        }
    }
}

fn exercise_entrances(grid: &TerrainGrid, data: &[u8]) {
    let options = EntranceOptions {
        max_distance_pixels: 256 + u32::from(data.first().copied().unwrap_or(0)) * 8,
        min_widening_percent: u16::from(data.get(1).copied().unwrap_or(25)) % 201,
    };
    let endpoints: Vec<_> = grid
        .cells()
        .iter()
        .enumerate()
        .filter_map(|(i, cell)| {
            cell.walkable.then_some(WalkPosition {
                x: i as u32 % grid.width(),
                y: i as u32 / grid.width(),
            })
        })
        .collect();
    let (Some(&start), Some(&end)) = (endpoints.first(), endpoints.last()) else {
        return;
    };
    let result = grid.entrances(start, end, &options).unwrap();
    assert!(result.candidates.len() <= 4);
    if let Some(route) = &result.route {
        assert_eq!(route.points.first(), Some(&start));
        assert_eq!(route.points.last(), Some(&end));
        let mut cost = 0_u64;
        for pair in route.points.windows(2) {
            let [a, b] = [pair[0], pair[1]];
            let ai = a.y as usize * grid.width() as usize + a.x as usize;
            let bi = b.y as usize * grid.width() as usize + b.x as usize;
            assert!(region_neighbors(grid, ai).contains(&bi));
            cost += if a.x == b.x || a.y == b.y { 1000 } else { 1414 };
        }
        assert!((route.distance_pixels - cost as f64 * 8.0 / 1000.0).abs() < 0.00001);
    } else {
        assert!(result.candidates.is_empty());
        assert!(grid.route(start, end).unwrap().is_none());
    }
    let mut prior = None;
    for candidate in &result.candidates {
        assert!(grid.cell(candidate.position).unwrap().walkable);
        assert!((64.0..=640.0).contains(&candidate.width_pixels));
        assert!(candidate.approach_max_width_pixels <= candidate.width_pixels * 1.3 + 0.00001);
        assert!((2..=3).contains(&candidate.outward_sample_count));
        assert!(candidate.outward_min_width_pixels + 0.00001 >= candidate.width_pixels + 64.0);
        assert!(
            candidate.outward_min_width_pixels * 100.0 + 0.00001
                >= candidate.width_pixels * f64::from(100 + options.min_widening_percent)
        );
        assert!(candidate.distance_from_start_pixels <= f64::from(options.max_distance_pixels));
        if let Some(previous) = prior {
            assert!(candidate.distance_from_start_pixels - previous >= 256.0 - 0.00001);
        }
        prior = Some(candidate.distance_from_start_pixels);
        for endpoint in candidate.endpoints {
            assert!(endpoint.x <= grid.width() * 8);
            assert!(endpoint.y <= grid.height() * 8);
        }
        let [a, b] = candidate.endpoints;
        let actual = f64::from(a.x.abs_diff(b.x)).hypot(f64::from(a.y.abs_diff(b.y)));
        assert!((actual - candidate.width_pixels).abs() < 0.2);
    }
    if data.first().is_some_and(|b| b & 15 == 0) {
        assert_eq!(result, grid.entrances(start, end, &options).unwrap());
        let middle = endpoints[endpoints.len() / 2];
        let queries = [
            (start, end),
            (start, middle),
            (end, start),
            (start, end),
            (start, start),
        ];
        let batch = grid.entrances_batch(&queries, &options).unwrap();
        for (actual, &(a, b)) in batch.iter().zip(&queries) {
            assert_eq!(*actual, grid.entrances(a, b, &options).unwrap());
        }
        let reversed: Vec<_> = queries.into_iter().rev().collect();
        assert_eq!(
            batch.into_iter().rev().collect::<Vec<_>>(),
            grid.entrances_batch(&reversed, &options).unwrap()
        );
    }
}

fn exercise_areas(grid: &TerrainGrid, data: &[u8]) {
    let original = grid.partition_by_spans(&[]).unwrap();
    let coordinate = |index: usize, bound: u32| {
        let value = u32::from(data.get(index).copied().unwrap_or(0))
            + u32::from(data.get(index + 1).copied().unwrap_or(0)) * 256;
        value % (bound + 1)
    };
    let mut spans = Vec::new();
    for index in 0..usize::from(data.first().copied().unwrap_or(0) % 5) {
        let start = index * 8 + 1;
        let endpoints = [
            PixelPosition {
                x: coordinate(start, grid.width() * 8),
                y: coordinate(start + 2, grid.height() * 8),
            },
            PixelPosition {
                x: coordinate(start + 4, grid.width() * 8),
                y: coordinate(start + 6, grid.height() * 8),
            },
        ];
        if endpoints[0] != endpoints[1] {
            spans.push(BoundarySpan { endpoints });
        }
    }
    let partition = grid.partition_by_spans(&spans).unwrap();
    let labels = partition.labels();
    assert_eq!(labels.len(), grid.cells().len());
    assert!(partition.areas().len() >= original.areas().len());
    let mut counts = vec![0_u32; partition.areas().len() + 1];
    let mut parents = vec![None; counts.len()];
    for (index, (&label, cell)) in labels.iter().zip(grid.cells()).enumerate() {
        assert_eq!(label > 0, cell.walkable);
        assert!((label as usize) < counts.len());
        if label == 0 {
            continue;
        }
        counts[label as usize] += 1;
        let parent = parents[label as usize].get_or_insert(original.labels()[index]);
        assert_eq!(
            *parent,
            original.labels()[index],
            "cuts cannot join original components"
        );
    }
    for area in partition.areas() {
        assert_eq!(counts[area.id as usize], area.cell_count);
    }
    for boundary in partition.boundaries() {
        assert!(boundary.separated_edge_count <= boundary.removed_edge_count);
        assert_eq!(
            boundary.separated_edge_count == 0,
            boundary.region_pairs.is_empty()
        );
        for &[a, b] in &boundary.region_pairs {
            assert!(a > 0 && a < b && (b as usize) < counts.len());
            assert_eq!(parents[a as usize], parents[b as usize]);
        }
    }
    if data.first().is_some_and(|value| value & 7 == 0) {
        let mut reordered = spans.clone();
        reordered.reverse();
        for span in &mut reordered {
            span.endpoints.swap(0, 1);
        }
        if let Some(span) = spans.first() {
            reordered.push(*span);
        }
        let again = grid.partition_by_spans(&reordered).unwrap();
        assert_eq!(labels, again.labels());
        assert_eq!(partition.areas(), again.areas());
    }
}

fn exercise_grid(grid: &TerrainGrid, data: &[u8]) {
    exercise_regions(grid, data);
    exercise_entrances(grid, data);
    exercise_ramps(grid);
    exercise_areas(grid, data);
    let width = grid.width();
    let height = grid.height();
    let clearance = grid.clearance();
    assert_eq!((clearance.width(), clearance.height()), (width, height));
    assert_eq!(clearance.radii_pixels().len(), grid.cells().len());
    assert_eq!(
        clearance.radius_pixels(WalkPosition { x: u32::MAX, y: 0 }),
        None
    );
    assert_eq!(
        clearance.radius_pixels(WalkPosition { x: 0, y: u32::MAX }),
        None
    );
    for y in 0..height {
        for x in 0..width {
            let point = WalkPosition { x, y };
            let cell = grid.cell(point).unwrap();
            let radius = clearance.radius_pixels(point).unwrap();
            assert_eq!(radius > 0, cell.walkable);
            let edge = (x + 1).min(y + 1).min(width - x).min(height - y) * 8 - 4;
            assert!(u32::from(radius) <= edge);
            if x > 0 {
                assert!(
                    radius.abs_diff(
                        clearance
                            .radius_pixels(WalkPosition { x: x - 1, y })
                            .unwrap()
                    ) <= 8
                );
            }
            if y > 0 {
                assert!(
                    radius.abs_diff(
                        clearance
                            .radius_pixels(WalkPosition { x, y: y - 1 })
                            .unwrap()
                    ) <= 8
                );
            }
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

fn run_topology_job(
    grid: &TerrainGrid,
    bases: Vec<TopologyBase>,
    options: TopologyOptions,
) -> BaseTopology {
    let expected_origins = bases
        .iter()
        .filter(|base| base.route_anchor.is_some())
        .count();
    let mut job = TopologyJob::new(Arc::new(grid.clone()), bases, options).unwrap();
    let mut previous = job.progress();
    assert_eq!(previous.completed_origins, 0);
    assert_eq!(previous.total_origins, expected_origins);
    for _ in 0..expected_origins + 16 {
        if previous.complete {
            break;
        }
        let current = job.advance().unwrap();
        assert!(current.completed_origins >= previous.completed_origins);
        assert!(current.completed_origins <= current.total_origins);
        assert_eq!(current.total_origins, expected_origins);
        previous = current;
    }
    assert!(previous.complete);
    job.finish().unwrap()
}

fn assert_topology_invariants(grid: &TerrainGrid, bases: &[TopologyBase], topology: &BaseTopology) {
    let spans: Vec<_> = topology
        .boundaries
        .iter()
        .map(|boundary| BoundarySpan {
            endpoints: boundary.boundary.endpoints,
        })
        .collect();
    let repartition = grid.partition_by_spans(&spans).unwrap();
    assert_eq!(topology.partition, repartition);
    assert_eq!(
        topology.boundaries.len(),
        topology.partition.boundaries().len()
    );
    for (analyzed, assessment) in topology
        .boundaries
        .iter()
        .zip(topology.partition.boundaries())
    {
        assert_eq!(&analyzed.assessment, assessment);
    }

    let mut counts = vec![0_u32; topology.partition.areas().len() + 1];
    for (&label, cell) in topology.partition.labels().iter().zip(grid.cells()) {
        assert_eq!(label != 0, cell.walkable);
        assert!((label as usize) < counts.len());
        if label != 0 {
            counts[label as usize] += 1;
        }
    }
    for area in topology.partition.areas() {
        assert_eq!(counts[area.id as usize], area.cell_count);
    }

    let original = grid.partition_by_spans(&[]).unwrap();
    let retained: Vec<_> = bases
        .iter()
        .filter_map(|base| base.route_anchor.map(|anchor| (base.id, anchor)))
        .collect();
    assert_eq!(topology.bases.len(), retained.len());
    assert_eq!(
        topology.statistics.skipped_anchor_count,
        bases
            .iter()
            .filter(|base| base.route_anchor.is_none())
            .count()
    );
    for area in &topology.bases {
        assert_eq!(topology.base(area.base_id), Some(area));
        let (_, anchor) = retained
            .iter()
            .find(|(id, _)| *id == area.base_id)
            .expect("completed topology retains every anchored base");
        let index = (anchor.y * grid.width() + anchor.x) as usize;
        assert_eq!(area.area_id, topology.partition.labels()[index]);
        assert_eq!(area.cell_count, counts[area.area_id as usize]);
        assert_eq!(
            area.original_cell_count,
            original.areas()[(original.labels()[index] - 1) as usize].cell_count
        );
        let mut expected_ids: Vec<_> = retained
            .iter()
            .filter_map(|(id, other)| {
                let other_index = (other.y * grid.width() + other.x) as usize;
                (topology.partition.labels()[other_index] == area.area_id).then_some(*id)
            })
            .collect();
        expected_ids.sort_unstable();
        assert_eq!(area.base_ids, expected_ids);
    }
}

fn exercise_topology_with_bases(
    grid: &TerrainGrid,
    bases: Vec<TopologyBase>,
    options: TopologyOptions,
) {
    assert!(grid.cells().len() <= 4096);
    let first = run_topology_job(grid, bases.clone(), options);
    assert_topology_invariants(grid, &bases, &first);
    let repeated = TopologyJob::new(Arc::new(grid.clone()), bases.clone(), options)
        .unwrap()
        .finish()
        .unwrap();
    assert_eq!(first, repeated);
    let reordered = TopologyJob::new(
        Arc::new(grid.clone()),
        bases.iter().copied().rev().collect(),
        options,
    )
    .unwrap()
    .finish()
    .unwrap();
    assert_eq!(first, reordered);

    let invalid_nearby = TopologyOptions {
        nearby_base_count: 0,
        ..options
    };
    assert!(matches!(
        TopologyJob::new(Arc::new(grid.clone()), bases.clone(), invalid_nearby),
        Err(TopologyError::InvalidNearbyCount)
    ));
    let invalid_entrances = TopologyOptions {
        entrances: EntranceOptions {
            max_distance_pixels: 255,
            ..options.entrances
        },
        ..options
    };
    assert!(matches!(
        TopologyJob::new(Arc::new(grid.clone()), bases, invalid_entrances),
        Err(TopologyError::Entrances(
            EntranceError::InvalidMaxDistance {
                max_distance_pixels: 255
            }
        ))
    ));
}

fn exercise_topology(grid: &TerrainGrid, data: &[u8]) {
    // This target is capped at 32x32 for the constructed-grid branch. The random anchor sample
    // covers disconnected and empty jobs. Coherent entrance fixtures belong in unit tests.
    let walkable: Vec<_> = grid
        .cells()
        .iter()
        .enumerate()
        .filter_map(|(index, cell)| {
            cell.walkable.then_some(WalkPosition {
                x: index as u32 % grid.width(),
                y: index as u32 / grid.width(),
            })
        })
        .collect();
    let anchor_count = walkable.len().min(4);
    let mut bases: Vec<_> = (0..anchor_count)
        .map(|index| TopologyBase {
            id: index as u32 + 1,
            route_anchor: Some(walkable[index * walkable.len() / anchor_count]),
        })
        .collect();
    if data.first().is_some_and(|byte| byte & 1 != 0) {
        bases.push(TopologyBase {
            id: 5,
            route_anchor: None,
        });
    }
    let options = TopologyOptions {
        entrances: EntranceOptions {
            max_distance_pixels: 256 + u32::from(data.get(1).copied().unwrap_or(0)) * 8,
            min_widening_percent: u16::from(data.get(2).copied().unwrap_or(0)) % 201,
        },
        nearby_base_count: 1 + usize::from(data.get(3).copied().unwrap_or(0) % 4),
    };
    exercise_topology_with_bases(grid, bases, options);
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
    // Random tiny grids mostly exercise validation and disconnected routes. This family also
    // reaches the full-width profile: a confined vertical approach opens into a larger room.
    if w_byte & 15 == 0 {
        let mouth_y = 36 + u32::from(h_byte % 17);
        let half_width = 4 + u32::from(data.get(2).copied().unwrap_or(0) % 9);
        let cells = (0..96)
            .flat_map(|y| {
                (0..96).map(move |x| TerrainCell {
                    walkable: (y >= 1
                        && y <= mouth_y
                        && x >= 48 - half_width
                        && x <= 48 + half_width)
                        || (y > mouth_y && y < 95 && (4..92).contains(&x)),
                    terrain_buildable: true,
                    elevation: 0,
                    ramp: false,
                })
            })
            .collect();
        let grid = TerrainGrid::from_cells(96, 96, cells).unwrap();
        exercise_entrances(&grid, data);
        exercise_ramps(&grid);
    }
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
        exercise_topology(&grid, data);
        exercise_grid(&grid.with_obstacles(&obstacles), data);
    }
});
