//! Targeted scoring floods and the scratch state they reuse.

use std::{cmp::Reverse, collections::BinaryHeap};

use super::{
    geometry::{CellRect, EMPTY_CELL_RECT, octile_gap, union_cells},
    proximity::ProximityField,
};
use crate::{TerrainGrid, WalkPosition};

/// Maps cells to the ids of the targets that contain them.
pub(super) struct TargetIndex {
    /// Per cell, 1 + the index of its range in `ranges`; 0 when the cell is no target's.
    slots: Vec<u32>,
    cells: Vec<usize>,
    ranges: Vec<(u32, u32)>,
    /// Target ids grouped by cell, ranged by `ranges`.
    ids: Vec<u32>,
    /// Bounding cells per target id, `EMPTY_CELL_RECT` when it has none.
    boxes: Vec<CellRect>,
    pairs: Vec<(usize, u32)>,
}

impl TargetIndex {
    fn new(count: usize) -> Self {
        Self {
            slots: vec![0; count],
            cells: Vec::new(),
            ranges: Vec::new(),
            ids: Vec::new(),
            boxes: Vec::new(),
            pairs: Vec::new(),
        }
    }

    /// Registers each target's cells, replacing the previous targets.
    pub(super) fn set<T: IntoIterator<Item = WalkPosition>>(
        &mut self,
        terrain: &TerrainGrid,
        targets: impl IntoIterator<Item = (usize, T)>,
    ) {
        self.clear();
        for (id, cells) in targets {
            if self.boxes.len() <= id {
                self.boxes.resize(id + 1, EMPTY_CELL_RECT);
            }
            let mut bounds = EMPTY_CELL_RECT;
            for point in cells {
                let index = terrain.index(point).expect("target in grid");
                let id = u32::try_from(id).expect("target count fits u32");
                self.pairs.push((index, id));
                bounds = union_cells(bounds, (point.x, point.y, point.x, point.y));
            }
            self.boxes[id] = bounds;
        }
        self.pairs.sort_unstable();
        self.pairs.dedup();
        for &(index, id) in &self.pairs {
            if self.cells.last() != Some(&index) {
                let start = u32::try_from(self.ids.len()).expect("target ids fit u32");
                self.ranges.push((start, start));
                self.cells.push(index);
                self.slots[index] = u32::try_from(self.ranges.len()).expect("cells fit u32");
            }
            self.ids.push(id);
            self.ranges.last_mut().expect("range pushed").1 += 1;
        }
    }

    pub(super) fn clear(&mut self) {
        for &index in &self.cells {
            self.slots[index] = 0;
        }
        self.cells.clear();
        self.ranges.clear();
        self.ids.clear();
        self.boxes.clear();
        self.pairs.clear();
    }

    fn ids_at(&self, index: usize) -> &[u32] {
        match self.slots[index] {
            0 => &[],
            slot => {
                let (start, end) = self.ranges[slot as usize - 1];
                &self.ids[start as usize..end as usize]
            }
        }
    }
}

pub(super) struct ScoringScratch {
    epoch: u32,
    seen: Vec<u32>,
    distances: Vec<u32>,
    // Row-major cell indices preserve the (y, x) tie order without storing both coordinates.
    queue: BinaryHeap<Reverse<(u32, usize)>>,
    pub(super) targets: TargetIndex,
    /// Minimum settled distance per target from the last `flood_targets`.
    target_distances: Vec<Option<u32>>,
    give_up_order: Vec<usize>,
    /// Cells settled by the last `flood_targets`.
    pub(super) pops: usize,
    pub(super) narrow: ProximityField<8>,
    pub(super) wide: ProximityField<16>,
}

impl ScoringScratch {
    pub(super) fn new(count: usize) -> Self {
        Self {
            epoch: 0,
            // Sized on demand: scoring floods run on windows smaller than the map.
            seen: Vec::new(),
            distances: Vec::new(),
            queue: BinaryHeap::new(),
            targets: TargetIndex::new(count),
            target_distances: Vec::new(),
            give_up_order: Vec::new(),
            pops: 0,
            narrow: ProximityField::new(),
            wide: ProximityField::new(),
        }
    }

    pub(super) fn target_distance(&self, id: usize) -> Option<u32> {
        self.target_distances[id]
    }

    /// Dijkstra from `starts` that stops once every `wanted` target is settled, or once every
    /// unsettled wanted target is farther than its `give_up` cost.
    ///
    /// Cells pop in nondecreasing cost order, so a target's minimum over its cells is the cost of
    /// the first of them to pop, and every unpopped cell costs at least the last popped cost.
    /// Targets never popped within `maximum_cost` are unreachable.
    pub(super) fn flood_targets(
        &mut self,
        terrain: &TerrainGrid,
        starts: impl IntoIterator<Item = WalkPosition>,
        maximum_cost: u32,
        wanted: &[usize],
        give_up: &[u32],
        budget: usize,
    ) -> FloodEnd {
        self.pops = 0;
        for &id in wanted {
            if self.target_distances.len() <= id {
                self.target_distances.resize(id + 1, None);
            }
            self.target_distances[id] = None;
        }
        // Wanted targets by descending give-up cost; `next` is the first unsettled one. Only
        // wanted targets are registered in `targets`.
        self.give_up_order.clear();
        self.give_up_order.extend_from_slice(wanted);
        if self.give_up_order.is_empty() {
            return FloodEnd::Settled;
        }
        self.give_up_order
            .sort_unstable_by_key(|&id| Reverse(give_up[id]));
        let mut next = 0;
        let mut limit = give_up[self.give_up_order[0]];
        // A* toward the wanted targets' bounding box. The octile distance to it is consistent and
        // zero on every wanted target cell, so those pop in true-distance order, and every
        // unpopped wanted cell is at least the last popped priority away.
        let goal = self
            .give_up_order
            .iter()
            .map(|&id| self.targets.boxes[id])
            .fold(EMPTY_CELL_RECT, union_cells);
        let heuristic = |point: WalkPosition| {
            if goal.0 > goal.2 {
                0
            } else {
                octile_gap((point.x, point.y, point.x, point.y), goal)
            }
        };
        if self.seen.len() < terrain.cells().len() {
            self.seen.resize(terrain.cells().len(), 0);
            self.distances.resize(terrain.cells().len(), 0);
        }
        self.epoch = self.epoch.wrapping_add(1);
        if self.epoch == 0 {
            self.seen.fill(0);
            self.epoch = 1;
        }
        self.queue.clear();
        for start in starts {
            let Some(index) = terrain.index(start) else {
                continue;
            };
            if !terrain.cells()[index].walkable || self.seen[index] == self.epoch {
                continue;
            }
            self.seen[index] = self.epoch;
            self.distances[index] = 0;
            self.queue.push(Reverse((heuristic(start), index)));
        }
        while let Some(Reverse((priority, index))) = self.queue.pop() {
            if self.seen[index] != self.epoch {
                continue;
            }
            let position = terrain.position(index);
            let cost = self.distances[index];
            if cost + heuristic(position) != priority {
                continue;
            }
            if priority > limit {
                return FloodEnd::GaveUp(priority);
            }
            self.pops += 1;
            if self.pops > budget {
                return FloodEnd::OverBudget;
            }
            let ids = self.targets.ids_at(index);
            if !ids.is_empty() {
                for &id in ids {
                    let id = id as usize;
                    if self.target_distances[id].is_none() {
                        self.target_distances[id] = Some(cost);
                    }
                }
                while self
                    .give_up_order
                    .get(next)
                    .is_some_and(|&id| self.target_distances[id].is_some())
                {
                    next += 1;
                }
                let Some(&id) = self.give_up_order.get(next) else {
                    return FloodEnd::Settled;
                };
                limit = give_up[id];
            }
            for (next, step) in terrain.neighbors(position) {
                let next_cost =
                    cost.saturating_add(u32::try_from(step).expect("walk cost fits u32"));
                if next_cost > maximum_cost {
                    continue;
                }
                let next_index = terrain.index(next).expect("neighbor in map");
                if self.seen[next_index] == self.epoch && self.distances[next_index] <= next_cost {
                    continue;
                }
                self.seen[next_index] = self.epoch;
                self.distances[next_index] = next_cost;
                self.queue
                    .push(Reverse((next_cost + heuristic(next), next_index)));
            }
        }
        FloodEnd::Exhausted
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(super) enum FloodEnd {
    /// Every wanted target settled.
    Settled,
    /// Stopped at this cost; every unsettled target is at least this far away.
    GaveUp(u32),
    /// Every cell within the maximum cost was settled.
    Exhausted,
    /// More cells than the budget were settled.
    OverBudget,
}

#[cfg(test)]
pub(super) struct SearchScratch {
    epoch: u32,
    seen: Vec<u32>,
    distances: Vec<u16>,
}
#[cfg(test)]
impl SearchScratch {
    pub(super) fn new(count: usize) -> Self {
        Self {
            epoch: 0,
            seen: vec![0; count],
            distances: vec![0; count],
        }
    }
    #[cfg(test)]
    pub(super) fn flood(
        &mut self,
        terrain: &TerrainGrid,
        starts: impl IntoIterator<Item = WalkPosition>,
        maximum: u16,
    ) {
        self.epoch = self.epoch.wrapping_add(1);
        if self.epoch == 0 {
            self.seen.fill(0);
            self.epoch = 1;
        }
        let mut queue = std::collections::VecDeque::new();
        for start in starts {
            let Some(index) = terrain.index(start) else {
                continue;
            };
            if self.seen[index] == self.epoch || !terrain.cells()[index].walkable {
                continue;
            }
            self.seen[index] = self.epoch;
            self.distances[index] = 0;
            queue.push_back(index);
        }
        while let Some(index) = queue.pop_front() {
            let distance = self.distances[index];
            if distance == maximum {
                continue;
            }
            for (next, _) in terrain.neighbors(terrain.position(index)) {
                let next = terrain.index(next).expect("neighbor in map");
                if self.seen[next] == self.epoch {
                    continue;
                }
                self.seen[next] = self.epoch;
                self.distances[next] = distance + 1;
                queue.push_back(next);
            }
        }
    }
    #[cfg(test)]
    pub(super) fn reached(&self, index: usize) -> bool {
        self.seen[index] == self.epoch
    }
}
