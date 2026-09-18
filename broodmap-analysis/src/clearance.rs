//! Chebyshev clearance over an 8-pixel terrain grid.
//!
//! A radius describes the largest axis-aligned square centered on a walk-cell center that can
//! touch, but not intersect, blocked cell interiors or the map boundary. It is a raster geometry
//! measurement only; it does not model mover footprints, unit collision, or game passability.

use crate::{TerrainGrid, WalkPosition};

const CELL_PIXELS: u16 = 8;
/// Immutable square-clearance radii for a [`TerrainGrid`].
///
/// Values are row-major and measured in logical pixels at 8-pixel walk-cell centers. A blocked
/// cell has radius zero. A walkable cell adjacent to a blocker or map boundary has radius four,
/// because its centered square may touch that boundary.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ClearanceField {
    width: u32,
    height: u32,
    radii_pixels: Vec<u16>,
}

impl ClearanceField {
    /// Width in 8-pixel walk cells.
    pub fn width(&self) -> u32 {
        self.width
    }

    /// Height in 8-pixel walk cells.
    pub fn height(&self) -> u32 {
        self.height
    }

    /// Row-major centered-square radii in logical pixels.
    pub fn radii_pixels(&self) -> &[u16] {
        &self.radii_pixels
    }

    /// Consumes the field and returns its row-major radius buffer without copying it.
    pub fn into_radii_pixels(self) -> Vec<u16> {
        self.radii_pixels
    }

    /// Returns the centered-square radius at an 8-pixel walk-cell position.
    pub fn radius_pixels(&self, position: WalkPosition) -> Option<u16> {
        if position.x >= self.width || position.y >= self.height {
            return None;
        }
        let index = position.y * self.width + position.x;
        self.radii_pixels.get(index as usize).copied()
    }
}

impl TerrainGrid {
    /// Computes an uncached Chebyshev clearance field for this grid's current walkability.
    ///
    /// The field changes naturally when called on a grid returned by [`Self::with_obstacles`].
    /// It measures centered axis-aligned square clearance in the 8-pixel raster, rather than
    /// Euclidean distance, unit fit, corridor width, or engine pathability.
    pub fn clearance(&self) -> ClearanceField {
        let width = self.width as usize;
        let height = self.height as usize;
        let mut distances = Vec::with_capacity(self.cells.len());
        for y in 0..height {
            for x in 0..width {
                let index = y * width + x;
                let edge_distance = (x + 1).min(y + 1).min(width - x).min(height - y);
                distances.push(if self.cells[index].walkable {
                    u16::try_from(edge_distance).expect("validated grid dimensions fit u16")
                } else {
                    0
                });
            }
        }

        // Every 8-neighbor has unit Chebyshev cost. The forward and reverse passes cover every
        // direction once, yielding the exact distance to a blocked or virtual outside cell.
        for y in 0..height {
            for x in 0..width {
                let index = y * width + x;
                if distances[index] == 0 {
                    continue;
                }
                let mut best = distances[index];
                if x > 0 {
                    best = best.min(distances[index - 1].saturating_add(1));
                }
                if y > 0 {
                    best = best.min(distances[index - width].saturating_add(1));
                    if x > 0 {
                        best = best.min(distances[index - width - 1].saturating_add(1));
                    }
                    if x + 1 < width {
                        best = best.min(distances[index - width + 1].saturating_add(1));
                    }
                }
                distances[index] = best;
            }
        }
        for y in (0..height).rev() {
            for x in (0..width).rev() {
                let index = y * width + x;
                if distances[index] == 0 {
                    continue;
                }
                let mut best = distances[index];
                if x + 1 < width {
                    best = best.min(distances[index + 1].saturating_add(1));
                }
                if y + 1 < height {
                    best = best.min(distances[index + width].saturating_add(1));
                    if x > 0 {
                        best = best.min(distances[index + width - 1].saturating_add(1));
                    }
                    if x + 1 < width {
                        best = best.min(distances[index + width + 1].saturating_add(1));
                    }
                }
                distances[index] = best;
            }
        }

        for distance in &mut distances {
            *distance = if *distance == 0 {
                0
            } else {
                *distance * CELL_PIXELS - CELL_PIXELS / 2
            };
        }
        ClearanceField {
            width: self.width,
            height: self.height,
            radii_pixels: distances,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{PixelRect, TerrainCell};

    fn grid(width: u32, height: u32, blocked: impl Fn(u32, u32) -> bool) -> TerrainGrid {
        let mut cells = Vec::with_capacity((width * height) as usize);
        for y in 0..height {
            for x in 0..width {
                cells.push(TerrainCell {
                    walkable: !blocked(x, y),
                    terrain_buildable: true,
                    elevation: 0,
                    ramp: false,
                });
            }
        }
        TerrainGrid::from_cells(width, height, cells).unwrap()
    }

    fn radius(field: &ClearanceField, x: u32, y: u32) -> u16 {
        field.radius_pixels(WalkPosition { x, y }).unwrap()
    }

    // Independent oracle in pixel geometry: distance to blocked rectangles and map sides.
    fn brute_radius(grid: &TerrainGrid, x: u32, y: u32) -> u16 {
        let px = x * 8 + 4;
        let py = y * 8 + 4;
        let mut radius = px
            .min(py)
            .min(grid.width() * 8 - px)
            .min(grid.height() * 8 - py);
        for other_y in 0..grid.height() {
            for other_x in 0..grid.width() {
                if grid
                    .cell(WalkPosition {
                        x: other_x,
                        y: other_y,
                    })
                    .unwrap()
                    .walkable
                {
                    continue;
                }
                let left = other_x * 8;
                let top = other_y * 8;
                let dx = left.saturating_sub(px).max(px.saturating_sub(left + 8));
                let dy = top.saturating_sub(py).max(py.saturating_sub(top + 8));
                radius = radius.min(dx.max(dy));
            }
        }
        radius as u16
    }

    #[test]
    fn sparse_fields_propagate_clearance_across_multiple_rows_and_columns() {
        let mut seed = 0x23af_5731_u32;
        for case in 0..64 {
            let blocked: Vec<_> = (0..17 * 13)
                .map(|_| {
                    seed = seed.wrapping_mul(1_664_525).wrapping_add(1_013_904_223);
                    seed >> 27 == 0
                })
                .collect();
            let terrain = grid(17, 13, |x, y| blocked[(y * 17 + x) as usize]);
            let field = terrain.clearance();
            for y in 0..13 {
                for x in 0..17 {
                    assert_eq!(
                        radius(&field, x, y),
                        brute_radius(&terrain, x, y),
                        "sparse case {case}, ({x}, {y})"
                    );
                }
            }
        }
    }

    #[test]
    fn transform_matches_direct_minimum_on_exhaustive_small_grids() {
        for width in 1..=4 {
            for height in 1..=3 {
                let count = width * height;
                for mask in 0..(1_u32 << count) {
                    let terrain = grid(width, height, |x, y| mask & (1 << (y * width + x)) != 0);
                    let field = terrain.clearance();
                    for y in 0..height {
                        for x in 0..width {
                            assert_eq!(
                                radius(&field, x, y),
                                brute_radius(&terrain, x, y),
                                "{width}x{height}, mask {mask:#x}, ({x}, {y})"
                            );
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn blocked_open_boundary_diagonal_and_corridor_clearance_are_exact() {
        let open = grid(5, 5, |_, _| false).clearance();
        assert_eq!(radius(&open, 0, 0), 4);
        assert_eq!(radius(&open, 2, 2), 20);
        assert_eq!(radius(&open, 4, 2), 4);

        let one_wide = grid(1, 4, |_, _| false).clearance();
        assert_eq!(one_wide.radii_pixels(), &[4, 4, 4, 4]);

        let diagonal = grid(3, 3, |x, y| x == 1 && y == 1).clearance();
        assert_eq!(radius(&diagonal, 1, 1), 0);
        assert_eq!(radius(&diagonal, 0, 0), 4);
        assert_eq!(radius(&diagonal, 2, 0), 4);

        let corridor = grid(7, 5, |_, y| y == 1 || y == 3).clearance();
        assert_eq!(radius(&corridor, 3, 2), 4);
        assert_eq!(radius(&corridor, 3, 0), 4);
    }

    #[test]
    fn obstacles_only_reduce_clearance_and_do_not_mutate_the_source_grid() {
        let terrain = grid(9, 9, |_, _| false);
        let before = terrain.clone();
        let original = terrain.clearance();
        let obstructed = terrain.with_obstacles(&[PixelRect {
            left: 32,
            top: 32,
            right: 40,
            bottom: 40,
        }]);
        let blocked = obstructed.clearance();
        assert_eq!(terrain, before);
        assert!(
            blocked
                .radii_pixels()
                .iter()
                .zip(original.radii_pixels())
                .all(|(blocked, original)| blocked <= original)
        );
        assert_eq!(radius(&blocked, 4, 4), 0);
        assert!(radius(&blocked, 3, 3) < radius(&original, 3, 3));
    }

    #[test]
    fn maximum_grid_center_has_the_expected_radius() {
        let terrain = grid(1_024, 1_024, |_, _| false);
        let field = terrain.clearance();
        assert_eq!(field.width(), 1_024);
        assert_eq!(field.height(), 1_024);
        assert_eq!(field.radii_pixels().len(), 1_024 * 1_024);
        assert_eq!(radius(&field, 512, 512), 4_092);
        assert_eq!(field.radius_pixels(WalkPosition { x: 1_024, y: 0 }), None);
    }
}
