//! Terrain-only grouping of mining resources into fields.

use super::{CLUSTER_RADIUS_CELLS, MAX_RESOURCES, selection::MiningResource};
use crate::{PixelRect, TerrainGrid, WalkPosition};

/// Calls `visit` for each 8-connected walkable neighbor of `index`, matching
/// [`TerrainGrid::neighbors`]: a diagonal step needs both orthogonal side cells walkable.
#[inline(always)]
fn for_each_walk_neighbor(terrain: &TerrainGrid, index: usize, mut visit: impl FnMut(usize)) {
    let cells = terrain.cells();
    let width = terrain.width() as usize;
    let x = index % width;
    let up = index >= width && cells[index - width].walkable;
    let down = index + width < cells.len() && cells[index + width].walkable;
    let left = x > 0 && cells[index - 1].walkable;
    let right = x + 1 < width && cells[index + 1].walkable;
    if up {
        visit(index - width);
        if left && cells[index - width - 1].walkable {
            visit(index - width - 1);
        }
        if right && cells[index - width + 1].walkable {
            visit(index - width + 1);
        }
    }
    if left {
        visit(index - 1);
    }
    if right {
        visit(index + 1);
    }
    if down {
        visit(index + width);
        if left && cells[index + width - 1].walkable {
            visit(index + width - 1);
        }
        if right && cells[index + width + 1].walkable {
            visit(index + width + 1);
        }
    }
}

/// Unions every pair of mining resources whose access cell sets are within
/// `CLUSTER_RADIUS_CELLS` walk steps of each other.
///
/// Instead of one bounded flood per resource, a multi-source BFS labels each cell with a nearest
/// resource out to half the radius, and unions labels across every edge `(u, v)` with
/// `d(u) + 1 + d(v) <= CLUSTER_RADIUS_CELLS`. Such an edge closes a path of that length between
/// the two labels' access sets, so every union is a true pair. Conversely, every cell on a
/// shortest path of length `L <= CLUSTER_RADIUS_CELLS` between two access sets lies within
/// `L / 2` steps of one of them, so it is labeled, and each label change along the path is an
/// edge with `d(u) + 1 + d(v) <= L`; those unions chain the two ends together. The step graph is
/// undirected, so the components equal those of pairwise per-resource floods.
pub(super) fn cluster_sets(terrain: &TerrainGrid, mining: &[MiningResource]) -> DisjointSet {
    // A cell's state packs `(mining index + 1) << DEPTH_BITS | depth`; zero means unreached.
    const DEPTH_BITS: u32 = 5;
    const DEPTH_MASK: u16 = (1 << DEPTH_BITS) - 1;
    const HALF_RADIUS: u16 = CLUSTER_RADIUS_CELLS / 2;
    const _: () = assert!(HALF_RADIUS <= DEPTH_MASK);
    const _: () = assert!((MAX_RESOURCES as u32 + 1) << DEPTH_BITS <= u16::MAX as u32);
    let cells = terrain.cells();
    let mut sets = DisjointSet::new(mining.len());
    let mut states = vec![0_u16; cells.len()];
    let mut queue = Vec::new();
    // One search per spatial group keeps each wavefront cache-local. Every step moves at most one
    // cell per axis, so resources whose access boxes are more than the radius apart (Chebyshev)
    // are never joined and their half-radius regions are disjoint: the searches don't interact.
    for group in spatial_groups(mining) {
        queue.clear();
        for resource in group {
            let label = u16::try_from(resource + 1).expect("resource count fits u16") << DEPTH_BITS;
            for &point in &mining[resource].access {
                let index = terrain.index(point).expect("access in map");
                if !cells[index].walkable {
                    continue;
                }
                match states[index] {
                    0 => {
                        states[index] = label;
                        queue.push(index);
                    }
                    other => sets.union(usize::from(other >> DEPTH_BITS) - 1, resource),
                }
            }
        }
        let mut head = 0;
        let mut last_union = (0, 0);
        while let Some(&index) = queue.get(head) {
            head += 1;
            let state = states[index];
            let (label, depth) = (state & !DEPTH_MASK, state & DEPTH_MASK);
            for_each_walk_neighbor(terrain, index, |next| match states[next] {
                0 => {
                    if depth < HALF_RADIUS {
                        states[next] = state + 1;
                        queue.push(next);
                    }
                }
                other => {
                    let other_label = other & !DEPTH_MASK;
                    if other_label != label
                        && depth + (other & DEPTH_MASK) < CLUSTER_RADIUS_CELLS
                        && last_union != (label, other_label)
                    {
                        last_union = (label, other_label);
                        sets.union(
                            usize::from(label >> DEPTH_BITS) - 1,
                            usize::from(other_label >> DEPTH_BITS) - 1,
                        );
                    }
                }
            });
        }
    }
    sets
}

/// Groups mining resources whose access bounding boxes are within `CLUSTER_RADIUS_CELLS`
/// (Chebyshev) of each other, transitively.
fn spatial_groups(mining: &[MiningResource]) -> Vec<Vec<usize>> {
    let radius = u32::from(CLUSTER_RADIUS_CELLS);
    let bounds: Vec<_> = mining
        .iter()
        .map(|resource| {
            resource.access.iter().fold(
                (u32::MAX, u32::MAX, 0, 0),
                |(left, top, right, bottom), point| {
                    (
                        left.min(point.x),
                        top.min(point.y),
                        right.max(point.x),
                        bottom.max(point.y),
                    )
                },
            )
        })
        .collect();
    let mut near = DisjointSet::new(mining.len());
    for (first, a) in bounds.iter().enumerate() {
        for (second, b) in bounds.iter().enumerate().skip(first + 1) {
            if a.0 <= b.2.saturating_add(radius)
                && b.0 <= a.2.saturating_add(radius)
                && a.1 <= b.3.saturating_add(radius)
                && b.1 <= a.3.saturating_add(radius)
            {
                near.union(first, second);
            }
        }
    }
    clusters_from_sets(&mut near, mining.len())
}

pub(super) fn clusters_from_sets(sets: &mut DisjointSet, count: usize) -> Vec<Vec<usize>> {
    let mut roots = vec![None; count];
    let mut clusters = Vec::new();
    for index in 0..count {
        let root = sets.find(index);
        let slot = match roots[root] {
            Some(slot) => slot,
            None => {
                let slot = clusters.len();
                clusters.push(Vec::new());
                roots[root] = Some(slot);
                slot
            }
        };
        clusters[slot].push(index);
    }
    clusters
}

pub(super) fn perimeter_access_cells(
    terrain: &TerrainGrid,
    bounds: PixelRect,
) -> Option<Vec<WalkPosition>> {
    let left = u32::try_from(bounds.left).ok()? / 8;
    let right = u32::try_from(bounds.right - 1).ok()? / 8;
    let top = u32::try_from(bounds.top).ok()? / 8;
    let bottom = u32::try_from(bounds.bottom - 1).ok()? / 8;
    let mut result = Vec::new();
    for y in top.saturating_sub(1)..=bottom.saturating_add(1).min(terrain.height() - 1) {
        for x in left.saturating_sub(1)..=right.saturating_add(1).min(terrain.width() - 1) {
            if (left..=right).contains(&x) && (top..=bottom).contains(&y) {
                continue;
            }
            let point = WalkPosition { x, y };
            if terrain.cell(point).is_some_and(|cell| cell.walkable) {
                result.push(point);
            }
        }
    }
    (!result.is_empty()).then_some(result)
}

pub(super) struct DisjointSet {
    parents: Vec<usize>,
    ranks: Vec<u8>,
}
impl DisjointSet {
    pub(super) fn new(length: usize) -> Self {
        Self {
            parents: (0..length).collect(),
            ranks: vec![0; length],
        }
    }
    fn find(&mut self, index: usize) -> usize {
        if self.parents[index] != index {
            let root = self.find(self.parents[index]);
            self.parents[index] = root;
        }
        self.parents[index]
    }
    pub(super) fn union(&mut self, left: usize, right: usize) {
        let left = self.find(left);
        let right = self.find(right);
        if left == right {
            return;
        }
        if self.ranks[left] < self.ranks[right] {
            self.parents[left] = right;
        } else {
            self.parents[right] = left;
            if self.ranks[left] == self.ranks[right] {
                self.ranks[left] += 1;
            }
        }
    }
}
