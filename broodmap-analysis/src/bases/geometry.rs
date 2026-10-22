//! Walk-cell rectangles, scoring windows, and obstacle stamping.

use super::{DepotFootprint, PIXELS_PER_WALK_CELL, PROXIMITY_RADIUS_CELLS, TilePosition};
use crate::{PixelRect, TerrainGrid, WalkPosition};

/// Inclusive walk-cell rectangle `(left, top, right, bottom)`.
pub(super) type CellRect = (u32, u32, u32, u32);

/// Cells that can hold a resource's perimeter access cells.
pub(super) fn access_ring(bounds: PixelRect, width: u32, height: u32) -> CellRect {
    let left = u32::try_from(bounds.left).unwrap_or(0) / 8;
    let top = u32::try_from(bounds.top).unwrap_or(0) / 8;
    let right = u32::try_from(bounds.right - 1).unwrap_or(0) / 8;
    let bottom = u32::try_from(bounds.bottom - 1).unwrap_or(0) / 8;
    (
        left.saturating_sub(1),
        top.saturating_sub(1),
        right.saturating_add(1).min(width - 1),
        bottom.saturating_add(1).min(height - 1),
    )
}

/// A rectangle of walk cells, with translation into its own local coordinates.
#[derive(Debug, Copy, Clone)]
pub(super) struct CellWindow {
    left: u32,
    top: u32,
    width: u32,
    height: u32,
}

impl CellWindow {
    /// Every cell a scoring flood from these access rings can reach, plus a one-cell border.
    pub(super) fn around(rings: &[CellRect], map_width: u32, map_height: u32) -> Self {
        let margin = u32::from(PROXIMITY_RADIUS_CELLS) + 1;
        let left = rings.iter().map(|ring| ring.0).min().unwrap_or(0);
        let top = rings.iter().map(|ring| ring.1).min().unwrap_or(0);
        let right = rings.iter().map(|ring| ring.2).max().unwrap_or(0);
        let bottom = rings.iter().map(|ring| ring.3).max().unwrap_or(0);
        let left = left.saturating_sub(margin);
        let top = top.saturating_sub(margin);
        let right = right.saturating_add(margin).min(map_width - 1);
        let bottom = bottom.saturating_add(margin).min(map_height - 1);
        Self {
            left,
            top,
            width: right - left + 1,
            height: bottom - top + 1,
        }
    }

    pub(super) fn extract(&self, terrain: &TerrainGrid) -> TerrainGrid {
        let mut cells = Vec::with_capacity(self.width as usize * self.height as usize);
        for y in self.top..self.top + self.height {
            let start = y as usize * terrain.width as usize + self.left as usize;
            cells.extend_from_slice(&terrain.cells[start..start + self.width as usize]);
        }
        TerrainGrid {
            width: self.width,
            height: self.height,
            cells,
        }
    }

    /// Translates pixel bounds; rasterization clips whatever falls outside the window.
    pub(super) fn local_rect(&self, rect: PixelRect) -> PixelRect {
        let x = i32::try_from(i64::from(self.left) * PIXELS_PER_WALK_CELL)
            .expect("validated grid width");
        let y = i32::try_from(i64::from(self.top) * PIXELS_PER_WALK_CELL)
            .expect("validated grid height");
        PixelRect {
            left: rect.left.saturating_sub(x),
            top: rect.top.saturating_sub(y),
            right: rect.right.saturating_sub(x),
            bottom: rect.bottom.saturating_sub(y),
        }
    }

    pub(super) fn local_point(&self, point: WalkPosition) -> Option<WalkPosition> {
        let x = point.x.checked_sub(self.left).filter(|&x| x < self.width)?;
        let y = point.y.checked_sub(self.top).filter(|&y| y < self.height)?;
        Some(WalkPosition { x, y })
    }
}

pub(super) fn footprint_cells(tile: TilePosition, depot: DepotFootprint) -> CellRect {
    let left = tile.x * 4;
    let top = tile.y * 4;
    (
        left,
        top,
        left + depot.width_tiles * 4 - 1,
        top + depot.height_tiles * 4 - 1,
    )
}

/// The bounds of no cells; `union_cells` with it is the identity.
pub(super) const EMPTY_CELL_RECT: CellRect = (u32::MAX, u32::MAX, 0, 0);

pub(super) fn union_cells(a: CellRect, b: CellRect) -> CellRect {
    (a.0.min(b.0), a.1.min(b.1), a.2.max(b.2), a.3.max(b.3))
}

fn axis_gaps(a: CellRect, b: CellRect) -> (u32, u32) {
    let gap = |a_low: u32, a_high: u32, b_low: u32, b_high: u32| {
        b_low
            .saturating_sub(a_high)
            .max(a_low.saturating_sub(b_high))
    };
    (gap(a.0, a.2, b.0, b.2), gap(a.1, a.3, b.1, b.3))
}

/// The cheapest possible octile walk between any cells of two rectangles, ignoring obstacles.
pub(super) fn octile_gap(a: CellRect, b: CellRect) -> u32 {
    let (dx, dy) = axis_gaps(a, b);
    dx.max(dy).saturating_mul(1_000) + dx.min(dy).saturating_mul(414)
}

/// The same grid as `TerrainGrid::with_obstacles`, built by marking each clipped rectangle's
/// cells directly. That costs the rectangles' area rather than a sweep of every cell, so the
/// sweep is only used when the rectangles cover more cells than the grid has.
pub(super) fn stamp_obstacles(mut grid: TerrainGrid, obstacles: &[PixelRect]) -> TerrainGrid {
    let cell_pixels = i32::try_from(PIXELS_PER_WALK_CELL).expect("cell size fits i32");
    let max_x = i32::try_from(grid.width).expect("validated grid width") * cell_pixels;
    let max_y = i32::try_from(grid.height).expect("validated grid height") * cell_pixels;
    let mut spans = Vec::with_capacity(obstacles.len());
    let mut area = 0_usize;
    for rect in obstacles {
        if rect.left >= rect.right || rect.top >= rect.bottom {
            continue;
        }
        // Clamp before converting to cells, exactly as the sweep does.
        let left = rect.left.clamp(0, max_x);
        let top = rect.top.clamp(0, max_y);
        let right = rect.right.clamp(0, max_x);
        let bottom = rect.bottom.clamp(0, max_y);
        if left >= right || top >= bottom {
            continue;
        }
        let x0 = (left / cell_pixels) as usize;
        let y0 = (top / cell_pixels) as usize;
        let x1 = ((right + cell_pixels - 1) / cell_pixels) as usize;
        let y1 = ((bottom + cell_pixels - 1) / cell_pixels) as usize;
        area = area.saturating_add((x1 - x0) * (y1 - y0));
        spans.push((x0, y0, x1, y1));
    }
    if area > grid.cells.len() {
        return grid.with_obstacles(obstacles);
    }
    let width = grid.width as usize;
    for (x0, y0, x1, y1) in spans {
        for y in y0..y1 {
            for cell in &mut grid.cells[y * width + x0..y * width + x1] {
                cell.walkable = false;
            }
        }
    }
    grid
}

pub(super) fn footprint_perimeter(tile: TilePosition, depot: DepotFootprint) -> Vec<WalkPosition> {
    let left = tile.x * 4;
    let top = tile.y * 4;
    let right = left + depot.width_tiles * 4;
    let bottom = top + depot.height_tiles * 4;
    let mut result = Vec::with_capacity((right - left + bottom - top).saturating_mul(2) as usize);
    for x in left..right {
        result.push(WalkPosition { x, y: top });
    }
    for y in top + 1..bottom {
        result.push(WalkPosition { x: right - 1, y });
    }
    if bottom > top + 1 {
        for x in (left..right - 1).rev() {
            result.push(WalkPosition { x, y: bottom - 1 });
        }
    }
    if right > left + 1 {
        for y in (top + 1..bottom - 1).rev() {
            result.push(WalkPosition { x: left, y });
        }
    }
    result
}
