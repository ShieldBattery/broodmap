use super::*;
use crate::bases::{
    CLUSTER_RADIUS_CELLS,
    clustering::{DisjointSet, cluster_sets, clusters_from_sets, perimeter_access_cells},
    selection::MiningResource,
};

fn pairwise_clusters(terrain: &TerrainGrid, mining: &[MiningResource]) -> Vec<Vec<usize>> {
    let mut sets = DisjointSet::new(mining.len());
    for source in 0..mining.len() {
        let mut distances = vec![None; terrain.cells().len()];
        let mut queue = std::collections::VecDeque::new();
        for &point in &mining[source].access {
            let index = terrain.index(point).unwrap();
            if distances[index].is_none() {
                distances[index] = Some(0);
                queue.push_back(point);
            }
        }
        while let Some(point) = queue.pop_front() {
            let distance = distances[terrain.index(point).unwrap()].unwrap();
            if distance == CLUSTER_RADIUS_CELLS {
                continue;
            }
            for (next, _) in terrain.neighbors(point) {
                let next_index = terrain.index(next).unwrap();
                if distances[next_index].is_none() {
                    distances[next_index] = Some(distance + 1);
                    queue.push_back(next);
                }
            }
        }
        for (target, candidate) in mining.iter().enumerate().skip(source + 1) {
            if candidate
                .access
                .iter()
                .any(|point| distances[terrain.index(*point).unwrap()].is_some())
            {
                sets.union(source, target);
            }
        }
    }
    clusters_from_sets(&mut sets, mining.len())
}

#[test]
fn labeled_cluster_search_matches_pairwise_floods() {
    let mut seed = 0x2545_f491_4f6c_dd1d_u64;
    let mut next = move |bound: u32| {
        seed ^= seed << 13;
        seed ^= seed >> 7;
        seed ^= seed << 17;
        u32::try_from(seed % u64::from(bound)).unwrap()
    };
    let mut joined = 0;
    for trial in 0..120 {
        let tiles = 12 + next(16);
        let open = 55 + next(46);
        let blocks = next(12);
        let mut walls = Vec::new();
        for _ in 0..blocks {
            walls.push((next(tiles * 4), next(tiles * 4), 1 + next(12), 1 + next(12)));
        }
        let mut noise = Vec::new();
        for _ in 0..tiles * tiles * 16 {
            noise.push(next(100) < open);
        }
        let width = tiles * 4;
        let terrain = grid(tiles, tiles, |x, y| {
            noise[(y * width + x) as usize]
                && !walls
                    .iter()
                    .any(|&(l, t, w, h)| (l..l + w).contains(&x) && (t..t + h).contains(&y))
        });
        let mut mining = Vec::new();
        for _ in 0..2 + next(14) {
            let left = i32::try_from(next(tiles * 32 - 8)).unwrap();
            let top = i32::try_from(next(tiles * 32 - 8)).unwrap();
            let right = (left + 1 + i32::try_from(next(64)).unwrap()).min((tiles * 32) as i32);
            let bottom = (top + 1 + i32::try_from(next(48)).unwrap()).min((tiles * 32) as i32);
            let bounds = PixelRect {
                left,
                top,
                right,
                bottom,
            };
            if let Some(access) = perimeter_access_cells(&terrain, bounds) {
                mining.push(MiningResource {
                    catalog_index: mining.len(),
                    access,
                });
            }
        }
        let expected = pairwise_clusters(&terrain, &mining);
        let mut sets = cluster_sets(&terrain, &mining);
        assert_eq!(
            clusters_from_sets(&mut sets, mining.len()),
            expected,
            "trial {trial}"
        );
        joined += mining.len() - expected.len();
    }
    assert!(joined > 50, "trials should exercise joins, got {joined}");
}
