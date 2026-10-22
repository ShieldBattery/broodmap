//! On-demand rasterization of the current all-object grid near chosen depots.

use super::{DepotFootprint, PIXELS_PER_WALK_CELL, PixelPosition, TilePosition};
use crate::{PixelRect, TerrainGrid, WalkPosition};

/// The walk cells [`TerrainGrid::with_obstacles`] blocks for `rect`, as a half-open
/// `(left, top, right, bottom)` range clamped to the map.
fn cell_range(grid: &TerrainGrid, rect: PixelRect) -> Option<(u32, u32, u32, u32)> {
    if rect.left >= rect.right || rect.top >= rect.bottom {
        return None;
    }
    let max_x = i64::from(grid.width()) * PIXELS_PER_WALK_CELL;
    let max_y = i64::from(grid.height()) * PIXELS_PER_WALK_CELL;
    let left = i64::from(rect.left).clamp(0, max_x);
    let top = i64::from(rect.top).clamp(0, max_y);
    let right = i64::from(rect.right).clamp(0, max_x);
    let bottom = i64::from(rect.bottom).clamp(0, max_y);
    let cell = |value: i64| u32::try_from(value / PIXELS_PER_WALK_CELL).expect("clamped");
    (left < right && top < bottom).then(|| {
        (
            cell(left),
            cell(top),
            cell(right + PIXELS_PER_WALK_CELL - 1),
            cell(bottom + PIXELS_PER_WALK_CELL - 1),
        )
    })
}

/// The current all-object grid: the permanent-resource grid plus every non-resource obstacle
/// and low mineral, rasterized exactly as [`TerrainGrid::with_obstacles`] would, on demand.
pub(super) struct CurrentOccupancy<'a> {
    grid: &'a TerrainGrid,
    /// Half-open walk-cell ranges `(left, top, right, bottom)`, already clamped to the map.
    rects: Vec<(u32, u32, u32, u32)>,
}

impl<'a> CurrentOccupancy<'a> {
    pub(super) fn new(grid: &'a TerrainGrid, rects: impl IntoIterator<Item = PixelRect>) -> Self {
        let rects = rects
            .into_iter()
            .filter_map(|rect| cell_range(grid, rect))
            .collect();
        Self { grid, rects }
    }

    /// Rasterizes the half-open in-map cell window `[left, right) x [top, bottom)`.
    pub(super) fn window(&self, left: u32, top: u32, right: u32, bottom: u32) -> LocalGrid {
        let width = (right - left) as usize;
        let height = (bottom - top) as usize;
        let stride = width + 1;
        let mut differences = vec![0_i64; stride * (height + 1)];
        for &(x0, y0, x1, y1) in &self.rects {
            let x0 = x0.max(left);
            let y0 = y0.max(top);
            let x1 = x1.min(right);
            let y1 = y1.min(bottom);
            if x0 >= x1 || y0 >= y1 {
                continue;
            }
            let (x0, y0) = ((x0 - left) as usize, (y0 - top) as usize);
            let (x1, y1) = ((x1 - left) as usize, (y1 - top) as usize);
            differences[y0 * stride + x0] += 1;
            differences[y0 * stride + x1] -= 1;
            differences[y1 * stride + x0] -= 1;
            differences[y1 * stride + x1] += 1;
        }
        let grid_width = self.grid.width() as usize;
        let cells = self.grid.cells();
        let mut walkable = vec![false; width * height];
        for y in 0..height {
            let row = &cells[(top as usize + y) * grid_width + left as usize..][..width];
            for x in 0..width {
                let index = y * stride + x;
                let above = if y == 0 {
                    0
                } else {
                    differences[index - stride]
                };
                let before = if x == 0 { 0 } else { differences[index - 1] };
                let above_before = if x == 0 || y == 0 {
                    0
                } else {
                    differences[index - stride - 1]
                };
                differences[index] += above + before - above_before;
                walkable[y * width + x] = row[x].walkable && differences[index] <= 0;
            }
        }
        LocalGrid {
            left,
            top,
            width: width as u32,
            height: height as u32,
            walkable,
        }
    }

    /// Cells reachable from `start` within `maximum` 8-neighbor steps, matching
    /// [`SearchScratch::flood`] on the full current grid. Such a flood cannot leave the
    /// Chebyshev window around `start`, so only that window is rasterized.
    pub(super) fn reach(&self, start: WalkPosition, maximum: u16) -> LocalGrid {
        let radius = u32::from(maximum);
        let left = start.x.saturating_sub(radius);
        let top = start.y.saturating_sub(radius);
        let right = start.x.saturating_add(radius).min(self.grid.width() - 1) + 1;
        let bottom = start.y.saturating_add(radius).min(self.grid.height() - 1) + 1;
        let open = self.window(left, top, right, bottom);
        let width = open.width as usize;
        let height = open.height as usize;
        let mut reached = vec![false; open.walkable.len()];
        let mut distances = vec![0_u16; open.walkable.len()];
        let mut queue = std::collections::VecDeque::new();
        let start_index = (start.y - top) as usize * width + (start.x - left) as usize;
        if open.walkable[start_index] {
            reached[start_index] = true;
            queue.push_back(start_index);
        }
        while let Some(index) = queue.pop_front() {
            let distance = distances[index];
            if distance == maximum {
                continue;
            }
            let x = index % width;
            let y = index / width;
            for (delta_x, delta_y) in NEIGHBOR_DIRECTIONS {
                let (Some(next_x), Some(next_y)) = (
                    x.checked_add_signed(delta_x).filter(|&value| value < width),
                    y.checked_add_signed(delta_y)
                        .filter(|&value| value < height),
                ) else {
                    continue;
                };
                let next = next_y * width + next_x;
                let diagonal = delta_x != 0 && delta_y != 0;
                if reached[next]
                    || !open.walkable[next]
                    || (diagonal
                        && (!open.walkable[y * width + next_x]
                            || !open.walkable[next_y * width + x]))
                {
                    continue;
                }
                reached[next] = true;
                distances[next] = distance + 1;
                queue.push_back(next);
            }
        }
        LocalGrid {
            walkable: reached,
            ..open
        }
    }
}

const NEIGHBOR_DIRECTIONS: [(isize, isize); 8] = [
    (0, -1),
    (-1, 0),
    (1, 0),
    (0, 1),
    (-1, -1),
    (1, -1),
    (-1, 1),
    (1, 1),
];

/// A rectangular window of walk-cell flags; cells outside it read as false.
pub(super) struct LocalGrid {
    left: u32,
    top: u32,
    width: u32,
    height: u32,
    walkable: Vec<bool>,
}

impl LocalGrid {
    pub(super) fn walkable(&self, point: WalkPosition) -> bool {
        let (Some(x), Some(y)) = (
            point.x.checked_sub(self.left),
            point.y.checked_sub(self.top),
        ) else {
            return false;
        };
        x < self.width
            && y < self.height
            && self.walkable[y as usize * self.width as usize + x as usize]
    }
}

pub(super) fn current_footprint_anchor(
    tile: TilePosition,
    depot: DepotFootprint,
    current: &CurrentOccupancy,
) -> Option<WalkPosition> {
    let left = tile.x * 4;
    let top = tile.y * 4;
    let width = depot.width_tiles * 4;
    let height = depot.height_tiles * 4;
    let footprint = current.window(left, top, left + width, top + height);
    let center_x = i64::from(left) * 2 + i64::from(width);
    let center_y = i64::from(top) * 2 + i64::from(height);
    let mut best = None;
    for y in top..top + height {
        for x in left..left + width {
            let point = WalkPosition { x, y };
            if !footprint.walkable(point) {
                continue;
            }
            let dx = i64::from(x) * 2 + 1 - center_x;
            let dy = i64::from(y) * 2 + 1 - center_y;
            let score = (dx * dx + dy * dy, y, x);
            if best.is_none_or(|(prior, _)| score < prior) {
                best = Some((score, point));
            }
        }
    }
    best.map(|(_, point)| point)
}

pub(super) fn nearest_walkable(
    current: &CurrentOccupancy,
    point: PixelPosition,
) -> Option<WalkPosition> {
    let center = WalkPosition {
        x: point.x / 8,
        y: point.y / 8,
    };
    let left = center.x.saturating_sub(2);
    let top = center.y.saturating_sub(2);
    let right = center.x.saturating_add(2).min(current.grid.width() - 1) + 1;
    let bottom = center.y.saturating_add(2).min(current.grid.height() - 1) + 1;
    let nearby = current.window(left, top, right, bottom);
    let mut best = None;
    for y in top..bottom {
        for x in left..right {
            let candidate = WalkPosition { x, y };
            if !nearby.walkable(candidate) {
                continue;
            }
            let dx = i64::from(x) * 8 + 4 - i64::from(point.x);
            let dy = i64::from(y) * 8 + 4 - i64::from(point.y);
            let score = (dx * dx + dy * dy, y, x);
            if best.is_none_or(|(prior, _)| score < prior) {
                best = Some((score, candidate));
            }
        }
    }
    best.map(|(_, point)| point)
}
