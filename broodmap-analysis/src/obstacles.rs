//! Static obstacle extraction and conservative grid rasterization.
//!
//! [`PixelRect`] is intentionally a low-level API: callers with modded data can supply their own
//! collision rectangles. [`melee_obstacles`] is a convenience for the conventional neutral
//! buildings, resources, and unit sprites in a melee map.

use broodmap::chk::{
    placed_units::{PlacedUnit, UnitState},
    sprites::{Sprite, SpriteFlags},
};
use broodmap_formats::UnitsDat;

use crate::TerrainGrid;

const CELL_PIXELS: i32 = 8;
const BUILDING: u32 = 0x0000_0001;
const FLYER: u32 = 0x0000_0004;
const RESOURCE: u32 = 0x0000_2000;
const START_LOCATION: u16 = 214;

/// Half-open logical pixel bounds: `[left, right) x [top, bottom)`.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct PixelRect {
    pub left: i32,
    pub top: i32,
    pub right: i32,
    pub bottom: i32,
}

/// Returns conservative collision rectangles for conventional melee static blockers.
///
/// This includes neutral or unowned resource nodes and grounded buildings from `UNIT` and unit
/// sprites from `THG2`. Resources survive the UNIT owner filter; other UNIT entries must be
/// neutral (owner 11) or have no recorded owner. THG2 unit sprites must have owner 11.
/// Hallucinated, lifted/in-transit, flying, and pure sprite entries are excluded. Grounded
/// building add-ons remain blockers. Disabled unit sprites remain included: a disabled THG2 unit can
/// be an undrawn but still-blocking door. Dynamic door transitions and other engine collision
/// states are outside this static approximation. The helper uses `units.dat` collision bounds;
/// arbitrary mod data can instead be passed directly to [`TerrainGrid::with_obstacles`]. Missing
/// UNIT ownership is retained conservatively; exact runtime initialization and special door
/// ownership overrides are not simulated. IN_TRANSIT exclusion is an analysis policy, not
/// behavior independently verified against the game. Definitions with any negative collision
/// extent are skipped as unsupported; this adapter is conservative only for valid extents.
/// Callers needing signed/custom shapes should supply explicit pixel rectangles instead.
pub fn melee_obstacles(
    units: &[PlacedUnit],
    sprites: &[Sprite],
    definitions: &UnitsDat,
) -> Vec<PixelRect> {
    let mut obstacles = Vec::new();

    for unit in units {
        let resource = definitions
            .entry(unit.unit_id)
            .is_some_and(|entry| entry.special_ability_flags & RESOURCE != 0);
        if unit.unit_id == START_LOCATION
            || (!resource && unit.owner.is_some_and(|owner| owner != 11))
            || unit
                .state
                .intersects(UnitState::HALLUCINATED | UnitState::IN_TRANSIT)
        {
            continue;
        }
        if let Some(rect) = obstacle_rect(unit.unit_id, unit.x, unit.y, definitions) {
            obstacles.push(rect);
        }
    }

    for sprite in sprites {
        if sprite.owner != 11 || sprite.flags.contains(SpriteFlags::DRAW_AS_SPRITE) {
            continue;
        }
        if let Some(rect) = obstacle_rect(sprite.id, sprite.x, sprite.y, definitions) {
            obstacles.push(rect);
        }
    }

    obstacles
}

fn obstacle_rect(unit_id: u16, x: u16, y: u16, definitions: &UnitsDat) -> Option<PixelRect> {
    if unit_id == START_LOCATION {
        return None;
    }
    let entry = definitions.entry(unit_id)?;
    let flags = entry.special_ability_flags;
    if flags & FLYER != 0 || flags & (BUILDING | RESOURCE) == 0 {
        return None;
    }

    let (left, top, right, bottom) = entry.bounds;
    let left = i32::from(left);
    let top = i32::from(top);
    let right = i32::from(right);
    let bottom = i32::from(bottom);
    if left < 0 || top < 0 || right < 0 || bottom < 0 {
        return None;
    }

    Some(PixelRect {
        left: i32::from(x) - left,
        top: i32::from(y) - top,
        // Raw DAT extents include the last covered pixel. Unlike neobrood's generated
        // IBounds, these fields have not yet received the +1 normalization.
        right: i32::from(x) + right + 1,
        bottom: i32::from(y) + bottom + 1,
    })
}

pub(crate) fn apply_to_grid(grid: &TerrainGrid, obstacles: &[PixelRect]) -> TerrainGrid {
    if obstacles.is_empty() {
        return grid.clone();
    }

    let max_x = i32::try_from(grid.width).expect("validated grid width") * CELL_PIXELS;
    let max_y = i32::try_from(grid.height).expect("validated grid height") * CELL_PIXELS;
    let stride = grid.width as usize + 1;
    let mut differences = vec![0_i64; stride * (grid.height as usize + 1)];
    let mut has_obstacle = false;

    for rect in obstacles {
        if rect.left >= rect.right || rect.top >= rect.bottom {
            continue;
        }

        // Clamp before converting to cells. In particular, never add seven to an untrusted i32.
        let left = rect.left.clamp(0, max_x);
        let top = rect.top.clamp(0, max_y);
        let right = rect.right.clamp(0, max_x);
        let bottom = rect.bottom.clamp(0, max_y);
        if left >= right || top >= bottom {
            continue;
        }

        let x0 = (left / CELL_PIXELS) as usize;
        let y0 = (top / CELL_PIXELS) as usize;
        let x1 = ((right + CELL_PIXELS - 1) / CELL_PIXELS) as usize;
        let y1 = ((bottom + CELL_PIXELS - 1) / CELL_PIXELS) as usize;
        differences[y0 * stride + x0] += 1;
        differences[y0 * stride + x1] -= 1;
        differences[y1 * stride + x0] -= 1;
        differences[y1 * stride + x1] += 1;
        has_obstacle = true;
    }

    if !has_obstacle {
        return grid.clone();
    }

    let mut result = grid.clone();
    for y in 0..grid.height as usize {
        for x in 0..grid.width as usize {
            let index = y * stride + x;
            let above = if y == 0 {
                0
            } else {
                differences[index - stride]
            };
            let left = if x == 0 { 0 } else { differences[index - 1] };
            let above_left = if x == 0 || y == 0 {
                0
            } else {
                differences[index - stride - 1]
            };
            differences[index] += above + left - above_left;
            if differences[index] > 0 {
                result.cells[y * grid.width as usize + x].walkable = false;
            }
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use broodmap::chk::{
        placed_units::{PlacedUnit, UnitInstanceId},
        sprites::{Sprite, SpriteFlags},
    };
    use broodmap_formats::parse_units_dat;

    use super::*;
    use crate::{TerrainCell, WalkPosition};

    const ADDON: u32 = 0x0000_0002;

    type DefinitionFixture = (u16, u32, (i16, i16, i16, i16));

    fn grid(width: u32, height: u32) -> TerrainGrid {
        TerrainGrid::from_cells(
            width,
            height,
            vec![
                TerrainCell {
                    walkable: true,
                    terrain_buildable: true,
                    elevation: 1,
                    ramp: true,
                };
                (width * height) as usize
            ],
        )
        .unwrap()
    }

    fn unit(unit_id: u16, owner: Option<u8>, state: UnitState) -> PlacedUnit {
        PlacedUnit {
            instance_id: UnitInstanceId(0),
            x: 32,
            y: 40,
            unit_id,
            owner,
            hp_percent: None,
            shield_percent: None,
            energy_percent: None,
            resource_amount: None,
            hangar_count: None,
            state,
            linked_id: None,
        }
    }

    fn definitions(entries: &[DefinitionFixture]) -> UnitsDat {
        const FLAGS_OFFSET: usize = 7032;
        const BOUNDS_OFFSET: usize = 12580;
        let mut bytes = vec![0; 19_876];
        for &(id, flags, bounds) in entries {
            let index = id as usize;
            bytes[FLAGS_OFFSET + index * 4..FLAGS_OFFSET + index * 4 + 4]
                .copy_from_slice(&flags.to_le_bytes());
            for (offset, value) in [bounds.0, bounds.1, bounds.2, bounds.3]
                .into_iter()
                .enumerate()
            {
                let start = BOUNDS_OFFSET + index * 8 + offset * 2;
                bytes[start..start + 2].copy_from_slice(&value.to_le_bytes());
            }
        }
        parse_units_dat(&bytes)
    }

    #[test]
    fn obstacles_seal_a_corridor_and_allow_a_detour() {
        let obstacle = PixelRect {
            left: 16,
            top: 0,
            right: 24,
            bottom: 8,
        };
        assert_eq!(
            grid(5, 1)
                .with_obstacles(&[obstacle])
                .route(WalkPosition { x: 0, y: 0 }, WalkPosition { x: 4, y: 0 })
                .unwrap(),
            None
        );

        let detour = grid(5, 3).with_obstacles(&[PixelRect {
            top: 8,
            bottom: 16,
            ..obstacle
        }]);
        let route = detour
            .route(WalkPosition { x: 0, y: 1 }, WalkPosition { x: 4, y: 1 })
            .unwrap()
            .unwrap();
        assert!(route.distance_pixels > 32.0);
    }

    #[test]
    fn rasterization_rounds_clamps_and_ignores_invalid_rectangles() {
        let original = grid(4, 4);
        let changed = original.with_obstacles(&[
            PixelRect {
                left: 7,
                top: 7,
                right: 9,
                bottom: 9,
            },
            PixelRect {
                left: i32::MIN,
                top: i32::MIN,
                right: 1,
                bottom: 1,
            },
            PixelRect {
                left: 10,
                top: 10,
                right: 10,
                bottom: 20,
            },
            PixelRect {
                left: 20,
                top: 20,
                right: 10,
                bottom: 30,
            },
            PixelRect {
                left: i32::MAX - 1,
                top: 0,
                right: i32::MAX,
                bottom: 8,
            },
        ]);
        for y in 0..4 {
            for x in 0..4 {
                let blocked = x <= 1 && y <= 1;
                assert_eq!(
                    changed.cell(WalkPosition { x, y }).unwrap().walkable,
                    !blocked
                );
            }
        }

        let all = original.with_obstacles(&[PixelRect {
            left: i32::MIN,
            top: i32::MIN,
            right: i32::MAX,
            bottom: i32::MAX,
        }]);
        assert!(all.cells().iter().all(|cell| !cell.walkable));
    }

    #[test]
    fn recomputing_from_original_removes_overlap_without_mutating_attributes() {
        let original = grid(4, 1);
        let left = PixelRect {
            left: 0,
            top: 0,
            right: 16,
            bottom: 8,
        };
        let right = PixelRect {
            left: 8,
            top: 0,
            right: 24,
            bottom: 8,
        };
        let both = original.with_obstacles(&[left, right]);
        assert!(both.cells().iter().take(3).all(|cell| !cell.walkable));
        let only_right = original.with_obstacles(&[right]);
        assert!(
            only_right
                .cell(WalkPosition { x: 0, y: 0 })
                .unwrap()
                .walkable
        );
        assert!(
            !only_right
                .cell(WalkPosition { x: 1, y: 0 })
                .unwrap()
                .walkable
        );
        assert!(original.cells().iter().all(|cell| cell.walkable));
        assert!(only_right.cells()[1].terrain_buildable);
        assert_eq!(only_right.cells()[1].elevation, 1);
        assert!(only_right.cells()[1].ramp);
    }

    #[test]
    fn prefix_union_matches_direct_intersection_without_reopening_terrain() {
        let mut cells = grid(5, 4).cells().to_vec();
        cells[3 * 5 + 4].walkable = false;
        cells[3 * 5 + 4].terrain_buildable = false;
        let original = TerrainGrid::from_cells(5, 4, cells).unwrap();
        let obstacles = [
            PixelRect {
                left: -4,
                top: 4,
                right: 9,
                bottom: 12,
            },
            PixelRect {
                left: 16,
                top: 0,
                right: 24,
                bottom: 32,
            },
            PixelRect {
                left: 39,
                top: 24,
                right: 40,
                bottom: 32,
            },
            PixelRect {
                left: 45,
                top: 0,
                right: 32,
                bottom: 8,
            },
            PixelRect {
                left: 400,
                top: -2,
                right: 401,
                bottom: 3,
            },
        ];
        let rasterized = original.with_obstacles(&obstacles);

        for y in 0..original.height() {
            for x in 0..original.width() {
                let cell = original.cell(WalkPosition { x, y }).unwrap();
                let left = i32::try_from(x).unwrap() * CELL_PIXELS;
                let top = i32::try_from(y).unwrap() * CELL_PIXELS;
                let intersects = obstacles.iter().any(|rect| {
                    rect.left < rect.right
                        && rect.top < rect.bottom
                        && rect.left < left + CELL_PIXELS
                        && rect.right > left
                        && rect.top < top + CELL_PIXELS
                        && rect.bottom > top
                });
                let result = rasterized.cell(WalkPosition { x, y }).unwrap();
                assert_eq!(result.walkable, cell.walkable && !intersects);
                assert_eq!(result.terrain_buildable, cell.terrain_buildable);
                assert_eq!(result.elevation, cell.elevation);
                assert_eq!(result.ramp, cell.ramp);
            }
        }
    }

    #[test]
    fn raw_dat_bounds_include_right_bottom_pixels_and_zero_bounds_are_one_pixel() {
        let definitions = definitions(&[(1, BUILDING, (0, 0, 0, 0)), (2, BUILDING, (8, 8, 8, 8))]);
        let obstacles = melee_obstacles(
            &[
                unit(1, None, UnitState::empty()),
                unit(2, None, UnitState::empty()),
            ],
            &[],
            &definitions,
        );
        assert_eq!(
            obstacles[0],
            PixelRect {
                left: 32,
                top: 40,
                right: 33,
                bottom: 41
            }
        );
        assert_eq!(
            obstacles[1],
            PixelRect {
                left: 24,
                top: 32,
                right: 41,
                bottom: 49
            }
        );
        let blocked = grid(8, 8).with_obstacles(&obstacles[1..]);
        // The inclusive pixel at (40,48) occupies the next walk-cell row and column.
        assert!(!blocked.cell(WalkPosition { x: 5, y: 6 }).unwrap().walkable);
        assert!(blocked.cell(WalkPosition { x: 6, y: 6 }).unwrap().walkable);
    }

    #[test]
    fn melee_resources_ignore_owner_and_grounded_addons_remain_blockers() {
        let definitions = definitions(&[
            (1, BUILDING, (8, 8, 7, 7)),
            (2, BUILDING | ADDON, (8, 8, 7, 7)),
            (176, RESOURCE, (8, 8, 7, 7)),
        ]);
        assert_eq!(
            melee_obstacles(
                &[
                    unit(176, Some(0), UnitState::empty()),
                    unit(2, Some(11), UnitState::empty()),
                    unit(1, Some(8), UnitState::empty()),
                    unit(1, Some(7), UnitState::empty()),
                ],
                &[],
                &definitions
            )
            .len(),
            2
        );
        let sprite = Sprite {
            id: 1,
            x: 32,
            y: 40,
            owner: 8,
            flags: SpriteFlags::empty(),
        };
        assert!(melee_obstacles(&[], &[sprite], &definitions).is_empty());
    }

    #[test]
    fn melee_extraction_filters_unit_and_sprite_classes() {
        let definitions = definitions(&[
            (1, BUILDING, (8, 4, 16, 12)),
            (2, BUILDING | FLYER, (8, 4, 16, 12)),
            (3, ADDON | BUILDING, (8, 4, 16, 12)),
            (176, RESOURCE, (8, 4, 16, 12)),
            (4, BUILDING, (-1, 4, 16, 12)),
        ]);
        let sprites = [
            Sprite {
                id: 1,
                x: 64,
                y: 80,
                owner: 11,
                flags: SpriteFlags::empty(),
            },
            Sprite {
                id: 1,
                x: 64,
                y: 80,
                owner: 11,
                flags: SpriteFlags::DRAW_AS_SPRITE,
            },
            Sprite {
                id: 1,
                x: 64,
                y: 80,
                owner: 11,
                flags: SpriteFlags::DISABLED,
            },
            Sprite {
                id: 1,
                x: 64,
                y: 80,
                owner: 7,
                flags: SpriteFlags::empty(),
            },
        ];
        let obstacles = melee_obstacles(
            &[
                unit(1, None, UnitState::empty()),
                unit(1, Some(7), UnitState::empty()),
                unit(1, None, UnitState::HALLUCINATED),
                unit(1, None, UnitState::IN_TRANSIT),
                unit(2, None, UnitState::empty()),
                unit(3, None, UnitState::empty()),
                unit(4, None, UnitState::empty()),
                unit(176, Some(11), UnitState::empty()),
                unit(214, None, UnitState::empty()),
            ],
            &sprites,
            &definitions,
        );
        assert_eq!(
            obstacles,
            vec![
                PixelRect {
                    left: 24,
                    top: 36,
                    right: 49,
                    bottom: 53
                },
                PixelRect {
                    left: 24,
                    top: 36,
                    right: 49,
                    bottom: 53
                },
                PixelRect {
                    left: 24,
                    top: 36,
                    right: 49,
                    bottom: 53
                },
                PixelRect {
                    left: 56,
                    top: 76,
                    right: 81,
                    bottom: 93
                },
                PixelRect {
                    left: 56,
                    top: 76,
                    right: 81,
                    bottom: 93
                },
            ]
        );
    }
}
