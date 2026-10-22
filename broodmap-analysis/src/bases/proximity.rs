//! Exact bounded walk distances computed for several sources at once.
//!
//! Base scoring needs, for every resource of a field and every candidate depot, the shortest
//! 8-connected walk cost (1000 orthogonal, 1414 diagonal, no corner cutting) from the resource's
//! access ring to the depot perimeter, or nothing beyond a cap. Instead of one heap-driven
//! Dijkstra per resource, [`ProximityField`] relaxes a whole field together, one SIMD-friendly
//! `i16` lane per resource, with raster sweeps iterated to a fixed point.
//!
//! Exactness:
//! - Costs are even, so lanes hold half costs (500/707), biased so that one past the cap is
//!   `i16::MAX`. Saturating adds therefore turn every over-cap path into "unreachable" and leave
//!   every within-cap sum exact; signed min/max order the biased values like the costs.
//! - Every finite lane value is the cost of a real path and sources are zero, so a state where
//!   no edge can lower any lane is the capped shortest-path cost (Bellman-Ford): along a
//!   shortest path within the cap, every prefix is within the cap too. A top-down sweep
//!   relaxes every edge entering a row from above or along it, a bottom-up sweep every edge
//!   from below or along it, so a sweep that changes nothing after an opposite sweep proves
//!   the fixed point. Rows whose feeding neighbor row has not changed since they last pulled
//!   from it are skipped; values only fall, so the edges they satisfied still hold.
//! - A cell on a within-cap path from a source in box `S` to a target in box `T` satisfies
//!   `octile(p, S) + octile(p, T) <= cap`, because obstacle-free octile cost is a lower bound.
//!   Cells outside that region stay unreachable: excluding them changes no within-cap path.
//!   Corner-cutting checks still read the true walkability of side cells.

use crate::{PixelRect, TerrainGrid};

const ORTHOGONAL: i16 = 500;
const DIAGONAL: i16 = 707;
/// Lanes store `half_cost - BIAS`, which puts one past the largest wanted half cost exactly at
/// `i16::MAX`: saturating adds then clamp every over-cap path to "unreachable" for free.
const BIAS: i32 = HALF_CAP as i32 + 1 - i16::MAX as i32;
const INFINITE: i16 = i16::MAX;
const ZERO: i16 = (0 - BIAS) as i16;
// `max` with these keeps a lane as is or forces it unreachable.
const OPEN: i16 = i16::MIN;
const SHUT: i16 = i16::MAX;
/// The cost cap in orthogonal walk-cell steps.
pub(crate) const RADIUS_CELLS: i64 = 96;
/// The largest half cost a lane can hold.
pub(crate) const HALF_CAP: u16 = (RADIUS_CELLS * 500) as u16;

/// Converts a lane value back to a half cost, or `None` beyond the cap.
#[inline]
pub(crate) fn half_cost(lane: i16) -> Option<u16> {
    (lane != INFINITE).then(|| (i32::from(lane) + BIAS) as u16)
}
const CELL_PIXELS: i32 = 8;
const WALK: u8 = 1;
// Diagonal moves into a cell whose two orthogonal side cells are walkable.
const FROM_NW: u8 = 2;
const FROM_NE: u8 = 4;
const FROM_SW: u8 = 8;
const FROM_SE: u8 = 16;

/// Obstacle-free octile cost of a cell offset, a lower bound on any path's cost.
fn octile(dx: i64, dy: i64) -> i64 {
    dx.max(dy) * 1_000 + dx.min(dy) * 414
}

/// Inclusive walk-cell rectangle.
#[derive(Debug, Copy, Clone)]
pub(crate) struct CellBox {
    pub x0: i64,
    pub y0: i64,
    pub x1: i64,
    pub y1: i64,
}

impl CellBox {
    pub(crate) fn union(self, other: Self) -> Self {
        Self {
            x0: self.x0.min(other.x0),
            y0: self.y0.min(other.y0),
            x1: self.x1.max(other.x1),
            y1: self.y1.max(other.y1),
        }
    }
}

fn axis_gap(value: i64, low: i64, high: i64) -> i64 {
    (low - value).max(value - high).max(0)
}

/// Inclusive cell bounds of a resource's access ring, clamped to the grid as
/// `perimeter_access_cells` does. Returns the resource body bounds too.
pub(super) fn access_ring(grid: &TerrainGrid, bounds: PixelRect) -> Option<(CellBox, CellBox)> {
    let left = i64::from(u32::try_from(bounds.left).ok()? / 8);
    let right = i64::from(u32::try_from(bounds.right.checked_sub(1)?).ok()? / 8);
    let top = i64::from(u32::try_from(bounds.top).ok()? / 8);
    let bottom = i64::from(u32::try_from(bounds.bottom.checked_sub(1)?).ok()? / 8);
    let ring = CellBox {
        x0: (left - 1).max(0),
        y0: (top - 1).max(0),
        x1: (right + 1).min(i64::from(grid.width()) - 1),
        y1: (bottom + 1).min(i64::from(grid.height()) - 1),
    };
    let body = CellBox {
        x0: left,
        y0: top,
        x1: right,
        y1: bottom,
    };
    Some((ring, body))
}

pub(crate) struct ProximityField<const L: usize> {
    left: i64,
    top: i64,
    width: i64,
    height: i64,
    stride: usize,
    distances: Vec<[i16; L]>,
    walk: Vec<u8>,
    flags: Vec<u8>,
    rows: Vec<(usize, usize)>,
    /// Per padded row: bumped whenever the row changes.
    versions: Vec<u32>,
    /// Per padded row: the neighbor row's version when last relaxed from above/below.
    pulled_down: Vec<u32>,
    pulled_up: Vec<u32>,
}

impl<const L: usize> ProximityField<L> {
    pub(crate) fn new() -> Self {
        Self {
            left: 0,
            top: 0,
            width: 0,
            height: 0,
            stride: 2,
            distances: Vec::new(),
            walk: Vec::new(),
            flags: Vec::new(),
            rows: Vec::new(),
            versions: Vec::new(),
            pulled_down: Vec::new(),
            pulled_up: Vec::new(),
        }
    }

    /// Computes lane `i`'s capped costs from `resources[i]`'s walkable access ring (the cells
    /// bordering its walk-cell bounds) on `grid` with `blockers` also rasterized as unwalkable.
    /// Lanes are exact within `targets`; other cells may read as unreachable.
    pub(crate) fn compute(
        &mut self,
        grid: &TerrainGrid,
        blockers: &[PixelRect],
        resources: &[PixelRect],
        targets: CellBox,
    ) {
        let radius = RADIUS_CELLS;
        debug_assert!(resources.len() <= L);
        let rings: Vec<_> = resources
            .iter()
            .map(|&bounds| access_ring(grid, bounds))
            .collect();
        let Some(sources) = rings
            .iter()
            .flatten()
            .map(|&(ring, _)| ring)
            .reduce(CellBox::union)
        else {
            self.clear();
            return;
        };
        let grid_width = i64::from(grid.width());
        let grid_height = i64::from(grid.height());
        let x0 = ((sources.x0 + targets.x0 - radius).div_euclid(2)).max(0);
        let x1 = ((sources.x1 + targets.x1 + radius).div_euclid(2)).min(grid_width - 1);
        let y0 = ((sources.y0 + targets.y0 - radius).div_euclid(2)).max(0);
        let y1 = ((sources.y1 + targets.y1 + radius).div_euclid(2)).min(grid_height - 1);
        if x0 > x1 || y0 > y1 {
            self.clear();
            return;
        }
        self.left = x0;
        self.top = y0;
        self.width = x1 - x0 + 1;
        self.height = y1 - y0 + 1;
        self.stride = self.width as usize + 2;
        let stride = self.stride;
        let total = stride * (self.height as usize + 2);
        // Reuse the buffers and refill them as plain byte runs.
        self.distances.resize(total, [INFINITE; L]);
        self.distances.truncate(total);
        self.distances.as_flattened_mut().fill(INFINITE);
        self.walk.resize(total, 0);
        self.walk.truncate(total);
        self.walk.fill(0);

        let cells = grid.cells();
        for y in 0..self.height {
            let source = ((y0 + y) * grid_width + x0) as usize;
            let row = (y as usize + 1) * stride + 1;
            for x in 0..self.width as usize {
                self.walk[row + x] = u8::from(cells[source + x].walkable);
            }
        }
        let max_x = grid.width() as i32 * CELL_PIXELS;
        let max_y = grid.height() as i32 * CELL_PIXELS;
        for rect in blockers {
            // Matches `obstacles::apply_to_grid`'s conservative rasterization.
            let left = rect.left.clamp(0, max_x);
            let top = rect.top.clamp(0, max_y);
            let right = rect.right.clamp(0, max_x);
            let bottom = rect.bottom.clamp(0, max_y);
            if left >= right || top >= bottom {
                continue;
            }
            let bx0 = i64::from(left / CELL_PIXELS).max(x0);
            let by0 = i64::from(top / CELL_PIXELS).max(y0);
            let bx1 = i64::from((right + CELL_PIXELS - 1) / CELL_PIXELS).min(x1 + 1);
            let by1 = i64::from((bottom + CELL_PIXELS - 1) / CELL_PIXELS).min(y1 + 1);
            for y in by0..by1 {
                let row = (y - y0 + 1) as usize * stride + 1;
                for x in bx0..bx1 {
                    self.walk[row + (x - x0) as usize] = 0;
                }
            }
        }

        self.rows.clear();
        for y in y0..=y1 {
            let gap_s = axis_gap(y, sources.y0, sources.y1);
            let gap_t = axis_gap(y, targets.y0, targets.y1);
            let inside = |x: i64| {
                octile(axis_gap(x, sources.x0, sources.x1), gap_s)
                    + octile(axis_gap(x, targets.x0, targets.x1), gap_t)
                    <= radius * 1_000
            };
            let mut a = x0;
            while a <= x1 && !inside(a) {
                a += 1;
            }
            let mut b = x1;
            while b >= a && !inside(b) {
                b -= 1;
            }
            self.rows
                .push(((a - x0) as usize + 1, (b - x0 + 1) as usize + 1));
        }

        for (lane, ring) in rings.iter().enumerate() {
            let Some((ring, body)) = ring else { continue };
            for y in ring.y0..=ring.y1 {
                if y < y0 || y > y1 {
                    continue;
                }
                let (a, b) = self.rows[(y - y0) as usize];
                for x in ring.x0..=ring.x1 {
                    if (body.x0..=body.x1).contains(&x) && (body.y0..=body.y1).contains(&y) {
                        continue;
                    }
                    let column = (x - x0 + 1) as usize;
                    if x < x0 || x > x1 || column < a || column >= b {
                        continue;
                    }
                    let index = (y - y0 + 1) as usize * stride + column;
                    if self.walk[index] != 0 {
                        self.distances[index][lane] = ZERO;
                    }
                }
            }
        }

        self.flags.clear();
        self.flags.resize(total, 0);
        for y in 1..=self.height as usize {
            let row = y * stride;
            let walk = &self.walk[row - stride..row + 2 * stride];
            let flags = &mut self.flags[row + 1..row + stride - 1];
            for (x, flag) in flags.iter_mut().enumerate() {
                let cell = walk[stride + x + 1];
                let west = walk[stride + x];
                let east = walk[stride + x + 2];
                let north = walk[x + 1];
                let south = walk[2 * stride + x + 1];
                *flag = cell
                    | ((west & north) * FROM_NW)
                    | ((east & north) * FROM_NE)
                    | ((west & south) * FROM_SW)
                    | ((east & south) * FROM_SE);
            }
        }
        let padded_rows = self.height as usize + 2;
        self.versions.clear();
        self.versions.resize(padded_rows, 0);
        self.pulled_down.clear();
        self.pulled_down.resize(padded_rows, u32::MAX);
        self.pulled_up.clear();
        self.pulled_up.resize(padded_rows, u32::MAX);
        // Alternate directions until a sweep after the first changes nothing.
        let mut down = true;
        let mut first = true;
        loop {
            let changed = if down {
                self.sweep::<true>()
            } else {
                self.sweep::<false>()
            };
            if !changed && !first {
                break;
            }
            first = false;
            down = !down;
        }
    }

    fn clear(&mut self) {
        self.width = 0;
        self.height = 0;
    }

    /// Relaxes every edge entering each row from the previous row of the sweep and within the
    /// row. A row whose source neighbor is unchanged since it last pulled from it is skipped:
    /// rows only decrease, so the edges it already satisfied still hold.
    fn sweep<const DOWN: bool>(&mut self) -> bool {
        let stride = self.stride;
        let distances = &mut self.distances[..];
        let flags = &self.flags[..];
        let versions = &mut self.versions[..];
        let (pulled, left_bit, right_bit) = if DOWN {
            (&mut self.pulled_down[..], FROM_NW, FROM_NE)
        } else {
            (&mut self.pulled_up[..], FROM_SW, FROM_SE)
        };
        let mut any_changed = false;
        for step in 0..self.height as usize {
            let y = if DOWN {
                step
            } else {
                self.height as usize - 1 - step
            };
            let (a, b) = self.rows[y];
            let padded = y + 1;
            let other_row = if DOWN { padded - 1 } else { padded + 1 };
            if a >= b || pulled[padded] == versions[other_row] {
                continue;
            }
            pulled[padded] = versions[other_row];
            let mut changed = [0_i16; L];
            let row = padded * stride;
            let other = other_row * stride;
            // Cells left of the interval are never updated, so they stay infinite.
            let mut previous = [INFINITE; L];
            for column in a..b {
                let index = row + column;
                let cell = flags[index];
                let old = distances[index];
                let mut value = old;
                relax(&mut value, &distances[other + column], ORTHOGONAL);
                let left = if cell & left_bit != 0 { OPEN } else { SHUT };
                relax_gated(&mut value, &distances[other + column - 1], left, DIAGONAL);
                let right = if cell & right_bit != 0 { OPEN } else { SHUT };
                relax_gated(&mut value, &distances[other + column + 1], right, DIAGONAL);
                relax(&mut value, &previous, ORTHOGONAL);
                block(&mut value, cell);
                track(&mut changed, &old, &value);
                distances[index] = value;
                previous = value;
            }
            let mut next = [INFINITE; L];
            for column in (a..b).rev() {
                let index = row + column;
                let cell = flags[index];
                let old = distances[index];
                let mut value = old;
                relax(&mut value, &next, ORTHOGONAL);
                block(&mut value, cell);
                track(&mut changed, &old, &value);
                distances[index] = value;
                next = value;
            }
            if changed.iter().any(|&bits| bits != 0) {
                versions[padded] = versions[padded].wrapping_add(1);
                any_changed = true;
            }
        }
        any_changed
    }

    /// Lane-wise half-cost distances at a cell; infinite outside the computed region.
    #[inline]
    pub(crate) fn at(&self, x: u32, y: u32) -> Option<&[i16; L]> {
        let x = i64::from(x) - self.left;
        let y = i64::from(y) - self.top;
        if x < 0 || y < 0 || x >= self.width || y >= self.height {
            return None;
        }
        Some(&self.distances[(y as usize + 1) * self.stride + x as usize + 1])
    }
}

#[inline(always)]
fn relax<const L: usize>(value: &mut [i16; L], from: &[i16; L], cost: i16) {
    for lane in 0..L {
        value[lane] = value[lane].min(from[lane].saturating_add(cost));
    }
}

/// Relaxes through a move that `gate` (`OPEN` or `SHUT`) may forbid.
#[inline(always)]
fn relax_gated<const L: usize>(value: &mut [i16; L], from: &[i16; L], gate: i16, cost: i16) {
    for lane in 0..L {
        value[lane] = value[lane].min(from[lane].max(gate).saturating_add(cost));
    }
}

/// Keeps unwalkable cells unreachable without branching.
#[inline(always)]
fn block<const L: usize>(value: &mut [i16; L], cell: u8) {
    let gate = if cell & WALK != 0 { OPEN } else { SHUT };
    for lane in value.iter_mut() {
        *lane = (*lane).max(gate);
    }
}

#[inline(always)]
fn track<const L: usize>(changed: &mut [i16; L], old: &[i16; L], new: &[i16; L]) {
    for lane in 0..L {
        changed[lane] |= old[lane] ^ new[lane];
    }
}
