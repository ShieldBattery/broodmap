//! Static terrain analysis for StarCraft: Brood War maps.
//!
//! This crate resolves terrain, optional static collision rectangles, square-clearance fields,
//! deterministic clearance-prominent regions, bounded route-local entrance evidence, and
//! experimental partitions formed by boundary spans. It does
//! not simulate moving units, mover clearance, exact game regions, or engine-specific movement
//! rules. In particular,
//! [`TerrainGrid::from_terrain`] uses VF4/CV5 aggregation checked against OpenBW and an
//! MTXM bit-15 creep override verified in the inspected game binary (12310g.exe). This is
//! useful for analysis and UI previews, not a certified engine pathing implementation.

use std::{cmp::Reverse, collections::BinaryHeap};

use broodmap::chk::terrain::TerrainTileIds;
use broodmap_formats::{Cv5, MiniTileFlags, TileGroupFlags, Vf4};
use thiserror::Error;

pub mod areas;
pub mod bases;
pub mod clearance;
pub mod entrances;
pub mod obstacles;
pub mod regions;

pub use areas::{AreaError, BoundaryAssessment, BoundarySpan, PartitionArea, SpanPartition};
pub use bases::{
    BaseCandidate, BaseDiscovery, BaseError, BaseSearchOptions, DepotFootprint, PixelPosition,
    ResourceKind, ResourceNode, TilePosition, discover_bases,
};
pub use clearance::ClearanceField;
pub use entrances::{
    EntranceAnalysis, EntranceCandidate, EntranceError, EntranceOptions, EntranceSurvey,
};
pub use obstacles::{PixelRect, StaticObstacle, melee_obstacle_objects, melee_obstacles};
pub use regions::{Passage, Region, RegionAnalysis, RegionError, RegionOptions};

const MAX_TILES_PER_AXIS: usize = 256;
const WALK_CELLS_PER_TILE: usize = 4;
const MAX_CELLS_PER_AXIS: usize = MAX_TILES_PER_AXIS * WALK_CELLS_PER_TILE;
const MAX_CELL_COUNT: usize = MAX_CELLS_PER_AXIS * MAX_CELLS_PER_AXIS;
const ORTHOGONAL_COST: u64 = 1_000;
const DIAGONAL_COST: u64 = 1_414;

/// An 8-pixel walk-cell coordinate.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct WalkPosition {
    pub x: u32,
    pub y: u32,
}

/// Terrain attributes for one 8x8-pixel walk cell.
///
/// `terrain_buildable` is derived only from CV5's `UNBUILDABLE` bit. It does not establish that
/// a building can actually be placed: occupancy, unit clearance, placement rules, and other
/// engine checks are outside this static terrain model.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct TerrainCell {
    pub walkable: bool,
    pub terrain_buildable: bool,
    pub elevation: u8,
    pub ramp: bool,
}

/// A terrain-only grid indexed in 8x8-pixel walk cells.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TerrainGrid {
    width: u32,
    height: u32,
    cells: Vec<TerrainCell>,
}

/// A point-grid path. `points` includes both requested endpoints.
#[derive(Debug, Clone, PartialEq)]
pub struct Route {
    pub points: Vec<WalkPosition>,
    /// Length in pixels using fixed-point costs: 1000 for an orthogonal 8px step and 1414 for a
    /// diagonal 8px step. The latter approximates `sqrt(2) * 1000`.
    pub distance_pixels: f64,
}

/// An error while constructing or querying a terrain grid.
#[derive(Debug, Error, Copy, Clone, Eq, PartialEq)]
pub enum AnalysisError {
    #[error("terrain dimensions must be between 1x1 and 256x256 tiles, got {width}x{height}")]
    InvalidTerrainDimensions { width: usize, height: usize },
    #[error("terrain has {actual} tile IDs, but {width}x{height} requires {expected}")]
    TerrainTileLength {
        width: usize,
        height: usize,
        expected: usize,
        actual: usize,
    },
    #[error("grid dimensions must be between 1x1 and 1024x1024 walk cells, got {width}x{height}")]
    InvalidGridDimensions { width: u32, height: u32 },
    #[error("grid has {actual} cells, but {width}x{height} requires {expected}")]
    CellLength {
        width: u32,
        height: u32,
        expected: usize,
        actual: usize,
    },
    #[error("cell {index} has unsupported elevation {elevation}; expected 0, 1, or 2")]
    InvalidElevation { index: usize, elevation: u8 },
    #[error("tile {tile_index} references absent CV5 group {group_id}")]
    MissingCv5Group { tile_index: usize, group_id: u16 },
    #[error("tile {tile_index} references absent VF4 megatile {mega_tile_id}")]
    MissingVf4MegaTile {
        tile_index: usize,
        mega_tile_id: u16,
    },
    #[error("route {endpoint} endpoint ({position_x}, {position_y}) is outside the grid")]
    EndpointOutOfBounds {
        endpoint: Endpoint,
        position_x: u32,
        position_y: u32,
    },
    #[error("route {endpoint} endpoint ({position_x}, {position_y}) is blocked")]
    EndpointBlocked {
        endpoint: Endpoint,
        position_x: u32,
        position_y: u32,
    },
}

/// Which endpoint made a route request invalid.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Endpoint {
    Start,
    End,
}

impl std::fmt::Display for Endpoint {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Start => formatter.write_str("start"),
            Self::End => formatter.write_str("end"),
        }
    }
}

impl TerrainGrid {
    /// Resolves CHK MTXM tile IDs through CV5 and VF4 into 8x8-pixel terrain cells.
    ///
    /// A megatile is considered wholly walkable when more than 12 of its 16 VF4 minitiles are
    /// walkable, or when its CV5 group (or its individual MTXM tile ID) carries creep. Otherwise
    /// each cell uses its own VF4 `WALKABLE` bit. The MTXM bit-15 override was verified in
    /// 12310g.exe's tile conversion and walkability predicate; see docs/analysis-design.md.
    /// Missing CV5/VF4 references are errors.
    pub fn from_terrain(
        terrain: &TerrainTileIds,
        cv5: &Cv5,
        vf4: &Vf4,
    ) -> Result<Self, AnalysisError> {
        validate_terrain_shape(terrain)?;
        let width = terrain.width * WALK_CELLS_PER_TILE;
        let height = terrain.height * WALK_CELLS_PER_TILE;
        let cell_count = width * height;

        let mut cells = vec![
            TerrainCell {
                walkable: false,
                terrain_buildable: false,
                elevation: 0,
                ramp: false,
            };
            cell_count
        ];
        for (tile_index, tile_id) in terrain.tiles.iter().enumerate() {
            let group_id = tile_id.group_id();
            let group = cv5.group(group_id).ok_or(AnalysisError::MissingCv5Group {
                tile_index,
                group_id,
            })?;
            let mega_tile_id = group.mega_tiles[tile_id.tile_index() as usize];
            let mini_tiles =
                vf4.mega_tile(mega_tile_id)
                    .ok_or(AnalysisError::MissingVf4MegaTile {
                        tile_index,
                        mega_tile_id,
                    })?;
            let has_creep = tile_id.has_creep() || group.flags.contains(TileGroupFlags::HAS_CREEP);
            let whole_tile_walkable = has_creep
                || mini_tiles
                    .iter()
                    .filter(|flags| flags.contains(MiniTileFlags::WALKABLE))
                    .count()
                    > 12;
            let terrain_buildable = !group.flags.contains(TileGroupFlags::UNBUILDABLE);

            let tile_x = tile_index % terrain.width;
            let tile_y = tile_index / terrain.width;
            for (mini_index, flags) in mini_tiles.iter().enumerate() {
                let mini_x = mini_index % WALK_CELLS_PER_TILE;
                let mini_y = mini_index / WALK_CELLS_PER_TILE;
                let cell_index = (tile_y * WALK_CELLS_PER_TILE + mini_y) * width
                    + tile_x * WALK_CELLS_PER_TILE
                    + mini_x;
                cells[cell_index] = TerrainCell {
                    walkable: whole_tile_walkable || flags.contains(MiniTileFlags::WALKABLE),
                    terrain_buildable,
                    elevation: elevation(*flags),
                    ramp: flags.contains(MiniTileFlags::RAMP),
                };
            }
        }

        Self::from_cells(width as u32, height as u32, cells)
    }

    /// Creates a grid from already-resolved static terrain cells.
    pub fn from_cells(
        width: u32,
        height: u32,
        cells: Vec<TerrainCell>,
    ) -> Result<Self, AnalysisError> {
        let expected = validate_grid_shape(width, height)?;
        if cells.len() != expected {
            return Err(AnalysisError::CellLength {
                width,
                height,
                expected,
                actual: cells.len(),
            });
        }
        if let Some((index, cell)) = cells
            .iter()
            .enumerate()
            .find(|(_, cell)| cell.elevation > 2)
        {
            return Err(AnalysisError::InvalidElevation {
                index,
                elevation: cell.elevation,
            });
        }

        Ok(Self {
            width,
            height,
            cells,
        })
    }

    pub fn width(&self) -> u32 {
        self.width
    }

    pub fn height(&self) -> u32 {
        self.height
    }

    pub fn cells(&self) -> &[TerrainCell] {
        &self.cells
    }

    /// Returns a copy with every walk cell intersecting an obstacle marked unwalkable.
    ///
    /// Obstacles use half-open logical pixel bounds. They are conservatively rasterized onto the
    /// 8x8-pixel grid, so a cell is blocked even when only part of it intersects a rectangle.
    /// The original terrain attributes are retained, including `terrain_buildable`, elevation,
    /// and ramp; only `walkable` changes. Recompute this method from the original grid when an
    /// obstacle is removed.
    pub fn with_obstacles(&self, obstacles: &[PixelRect]) -> Self {
        obstacles::apply_to_grid(self, obstacles)
    }

    pub fn cell(&self, position: WalkPosition) -> Option<&TerrainCell> {
        self.index(position).and_then(|index| self.cells.get(index))
    }

    /// Finds a deterministic 8-neighbor A* route over walkable cells.
    ///
    /// Diagonal movement is only allowed when both orthogonal side cells are walkable, so a route
    /// cannot squeeze through a blocked corner. An invalid or blocked endpoint returns an error;
    /// valid endpoints with no connection return `Ok(None)`.
    pub fn route(
        &self,
        start: WalkPosition,
        end: WalkPosition,
    ) -> Result<Option<Route>, AnalysisError> {
        let start_index = self.endpoint_index(start, Endpoint::Start)?;
        let end_index = self.endpoint_index(end, Endpoint::End)?;

        if start_index == end_index {
            return Ok(Some(Route {
                points: vec![start],
                distance_pixels: 0.0,
            }));
        }

        let mut costs = vec![u64::MAX; self.cells.len()];
        let mut parents = vec![None; self.cells.len()];
        let mut open = BinaryHeap::new();
        costs[start_index] = 0;
        open.push(Reverse((
            heuristic(start, end),
            0_u64,
            start.y,
            start.x,
            start_index,
        )));

        while let Some(Reverse((_, cost, y, x, current))) = open.pop() {
            if costs.get(current).copied() != Some(cost) {
                continue;
            }
            if current == end_index {
                return Ok(Some(self.build_route(
                    start_index,
                    end_index,
                    &parents,
                    cost,
                )));
            }

            let position = WalkPosition { x, y };
            for (next, step_cost) in self.neighbors(position) {
                let Some(next_index) = self.index(next) else {
                    continue;
                };
                let candidate = cost + step_cost;
                if candidate >= costs[next_index] {
                    continue;
                }

                costs[next_index] = candidate;
                parents[next_index] = Some(current);
                let priority = candidate + heuristic(next, end);
                open.push(Reverse((priority, candidate, next.y, next.x, next_index)));
            }
        }

        Ok(None)
    }

    fn endpoint_index(
        &self,
        position: WalkPosition,
        endpoint: Endpoint,
    ) -> Result<usize, AnalysisError> {
        let index = self
            .index(position)
            .ok_or(AnalysisError::EndpointOutOfBounds {
                endpoint,
                position_x: position.x,
                position_y: position.y,
            })?;
        if !self.cells.get(index).is_some_and(|cell| cell.walkable) {
            return Err(AnalysisError::EndpointBlocked {
                endpoint,
                position_x: position.x,
                position_y: position.y,
            });
        }
        Ok(index)
    }

    fn index(&self, position: WalkPosition) -> Option<usize> {
        if position.x >= self.width || position.y >= self.height {
            return None;
        }
        let index = (position.y as usize)
            .checked_mul(self.width as usize)?
            .checked_add(position.x as usize)?;
        (index < self.cells.len()).then_some(index)
    }

    fn position(&self, index: usize) -> WalkPosition {
        let width = self.width as usize;
        WalkPosition {
            x: (index % width) as u32,
            y: (index / width) as u32,
        }
    }

    fn neighbors(&self, position: WalkPosition) -> impl Iterator<Item = (WalkPosition, u64)> + '_ {
        const DIRECTIONS: [(i32, i32); 8] = [
            (0, -1),
            (-1, 0),
            (1, 0),
            (0, 1),
            (-1, -1),
            (1, -1),
            (-1, 1),
            (1, 1),
        ];

        DIRECTIONS
            .into_iter()
            .filter_map(move |(delta_x, delta_y)| {
                let x = position.x.checked_add_signed(delta_x)?;
                let y = position.y.checked_add_signed(delta_y)?;
                let next = WalkPosition { x, y };
                let next_cell = self.cell(next)?;
                if !next_cell.walkable {
                    return None;
                }

                let diagonal = delta_x != 0 && delta_y != 0;
                if diagonal {
                    let side_a = WalkPosition { x, y: position.y };
                    let side_b = WalkPosition { x: position.x, y };
                    if !self.cell(side_a).is_some_and(|cell| cell.walkable)
                        || !self.cell(side_b).is_some_and(|cell| cell.walkable)
                    {
                        return None;
                    }
                }
                Some((
                    next,
                    if diagonal {
                        DIAGONAL_COST
                    } else {
                        ORTHOGONAL_COST
                    },
                ))
            })
    }

    fn build_route(&self, start: usize, end: usize, parents: &[Option<usize>], cost: u64) -> Route {
        let mut indices = Vec::new();
        let mut current = end;
        for _ in 0..=self.cells.len() {
            indices.push(current);
            if current == start {
                break;
            }
            current = parents
                .get(current)
                .and_then(|parent| *parent)
                .unwrap_or(start);
        }
        indices.reverse();

        Route {
            points: indices
                .into_iter()
                .map(|index| self.position(index))
                .collect(),
            distance_pixels: cost as f64 * 8.0 / ORTHOGONAL_COST as f64,
        }
    }
}

fn validate_terrain_shape(terrain: &TerrainTileIds) -> Result<usize, AnalysisError> {
    if terrain.width == 0
        || terrain.height == 0
        || terrain.width > MAX_TILES_PER_AXIS
        || terrain.height > MAX_TILES_PER_AXIS
    {
        return Err(AnalysisError::InvalidTerrainDimensions {
            width: terrain.width,
            height: terrain.height,
        });
    }
    let expected = terrain.width.checked_mul(terrain.height).ok_or(
        AnalysisError::InvalidTerrainDimensions {
            width: terrain.width,
            height: terrain.height,
        },
    )?;
    if terrain.tiles.len() != expected {
        return Err(AnalysisError::TerrainTileLength {
            width: terrain.width,
            height: terrain.height,
            expected,
            actual: terrain.tiles.len(),
        });
    }
    Ok(expected)
}

fn validate_grid_shape(width: u32, height: u32) -> Result<usize, AnalysisError> {
    if width == 0
        || height == 0
        || width as usize > MAX_CELLS_PER_AXIS
        || height as usize > MAX_CELLS_PER_AXIS
    {
        return Err(AnalysisError::InvalidGridDimensions { width, height });
    }
    let expected = (width as usize)
        .checked_mul(height as usize)
        .filter(|count| *count <= MAX_CELL_COUNT)
        .ok_or(AnalysisError::InvalidGridDimensions { width, height })?;
    Ok(expected)
}

fn elevation(flags: MiniTileFlags) -> u8 {
    if flags.contains(MiniTileFlags::LEVEL_HIGH) {
        2
    } else if flags.contains(MiniTileFlags::LEVEL_MID) {
        1
    } else {
        0
    }
}

fn heuristic(from: WalkPosition, to: WalkPosition) -> u64 {
    let delta_x = from.x.abs_diff(to.x) as u64;
    let delta_y = from.y.abs_diff(to.y) as u64;
    let diagonal = delta_x.min(delta_y);
    diagonal * DIAGONAL_COST + (delta_x.max(delta_y) - diagonal) * ORTHOGONAL_COST
}

#[cfg(test)]
mod tests {
    use std::collections::BinaryHeap;

    use super::*;
    use broodmap::chk::terrain::TileId;
    use broodmap_formats::{TileGroup, parse_cv5, parse_vf4};

    fn cell(walkable: bool) -> TerrainCell {
        TerrainCell {
            walkable,
            terrain_buildable: true,
            elevation: 0,
            ramp: false,
        }
    }

    fn grid(width: u32, height: u32, walkable: impl Fn(u32, u32) -> bool) -> TerrainGrid {
        let mut cells = Vec::with_capacity((width * height) as usize);
        for y in 0..height {
            for x in 0..width {
                cells.push(cell(walkable(x, y)));
            }
        }
        TerrainGrid::from_cells(width, height, cells).unwrap()
    }

    fn cv5(groups: Vec<TileGroup>) -> Cv5 {
        Cv5 { groups }
    }

    fn group(flags: TileGroupFlags, mega_tile: u16) -> TileGroup {
        let mut mega_tiles = [0; 16];
        mega_tiles[0] = mega_tile;
        TileGroup {
            group_type: 0,
            flags,
            mega_tiles,
        }
    }

    fn vf4(flags: [MiniTileFlags; 16]) -> Vf4 {
        Vf4 {
            mega_tiles: vec![flags],
        }
    }

    fn terrain(width: usize, height: usize, tiles: Vec<u16>) -> TerrainTileIds {
        TerrainTileIds {
            width,
            height,
            tiles: tiles.into_iter().map(TileId).collect(),
        }
    }

    #[test]
    fn aggregate_walkability_threshold_is_strictly_more_than_twelve() {
        let mut twelve = [MiniTileFlags::empty(); 16];
        for flags in &mut twelve[..12] {
            *flags = MiniTileFlags::WALKABLE;
        }
        let twelve_grid = TerrainGrid::from_terrain(
            &terrain(1, 1, vec![0]),
            &cv5(vec![group(TileGroupFlags::empty(), 0)]),
            &vf4(twelve),
        )
        .unwrap();
        assert!(twelve_grid.cells()[0].walkable);
        assert!(!twelve_grid.cells()[12].walkable);

        let mut thirteen = twelve;
        thirteen[12] = MiniTileFlags::WALKABLE;
        let thirteen_grid = TerrainGrid::from_terrain(
            &terrain(1, 1, vec![0]),
            &cv5(vec![group(TileGroupFlags::empty(), 0)]),
            &vf4(thirteen),
        )
        .unwrap();
        assert!(thirteen_grid.cells().iter().all(|cell| cell.walkable));
    }

    #[test]
    fn group_and_mtxm_creep_override_walkability() {
        let none = vf4([MiniTileFlags::empty(); 16]);
        let group_creep = TerrainGrid::from_terrain(
            &terrain(1, 1, vec![0]),
            &cv5(vec![group(TileGroupFlags::HAS_CREEP, 0)]),
            &none,
        )
        .unwrap();
        assert!(group_creep.cells().iter().all(|cell| cell.walkable));

        let mtxm_creep = TerrainGrid::from_terrain(
            &terrain(1, 1, vec![0x8000]),
            &cv5(vec![group(TileGroupFlags::empty(), 0)]),
            &none,
        )
        .unwrap();
        assert!(mtxm_creep.cells().iter().all(|cell| cell.walkable));
    }

    #[test]
    fn resolves_group_and_tile_index_and_copies_cell_attributes() {
        let mut selected = [MiniTileFlags::empty(); 16];
        selected[0] = MiniTileFlags::WALKABLE | MiniTileFlags::LEVEL_MID;
        selected[1] = MiniTileFlags::LEVEL_HIGH | MiniTileFlags::RAMP;
        let mut unused = [MiniTileFlags::empty(); 16];
        unused[0] = MiniTileFlags::WALKABLE;
        let mut mega_tiles = [1; 16];
        mega_tiles[3] = 0;
        let tile_group = TileGroup {
            group_type: 0,
            flags: TileGroupFlags::UNBUILDABLE,
            mega_tiles,
        };
        let parsed_vf4 = Vf4 {
            mega_tiles: vec![selected, unused],
        };
        let result =
            TerrainGrid::from_terrain(&terrain(1, 1, vec![3]), &cv5(vec![tile_group]), &parsed_vf4)
                .unwrap();
        assert_eq!(result.width(), 4);
        assert_eq!(result.height(), 4);
        assert_eq!(
            result.cells()[0],
            TerrainCell {
                walkable: true,
                terrain_buildable: false,
                elevation: 1,
                ramp: false
            }
        );
        assert_eq!(
            result.cells()[1],
            TerrainCell {
                walkable: false,
                terrain_buildable: false,
                elevation: 2,
                ramp: true
            }
        );
    }

    #[test]
    fn expands_megatiles_into_global_row_major_cells() {
        let walkable = [MiniTileFlags::WALKABLE; 16];
        let blocked = [MiniTileFlags::empty(); 16];
        let terrain = terrain(2, 2, vec![0, 1, 1, 0]);
        let mut mega_tiles = [0; 16];
        mega_tiles[1] = 1;
        let grid = TerrainGrid::from_terrain(
            &terrain,
            &cv5(vec![TileGroup {
                group_type: 0,
                flags: TileGroupFlags::empty(),
                mega_tiles,
            }]),
            &Vf4 {
                mega_tiles: vec![walkable, blocked],
            },
        )
        .unwrap();

        assert!(grid.cell(WalkPosition { x: 0, y: 0 }).unwrap().walkable);
        assert!(!grid.cell(WalkPosition { x: 4, y: 0 }).unwrap().walkable);
        assert!(!grid.cell(WalkPosition { x: 0, y: 4 }).unwrap().walkable);
        assert!(grid.cell(WalkPosition { x: 4, y: 4 }).unwrap().walkable);
    }
    #[test]
    fn missing_table_references_are_errors() {
        let missing_group =
            TerrainGrid::from_terrain(&terrain(1, 1, vec![16]), &Cv5::default(), &Vf4::default());
        assert_eq!(
            missing_group,
            Err(AnalysisError::MissingCv5Group {
                tile_index: 0,
                group_id: 1
            })
        );

        let missing_vf4 = TerrainGrid::from_terrain(
            &terrain(1, 1, vec![0]),
            &cv5(vec![group(TileGroupFlags::empty(), 9)]),
            &Vf4::default(),
        );
        assert_eq!(
            missing_vf4,
            Err(AnalysisError::MissingVf4MegaTile {
                tile_index: 0,
                mega_tile_id: 9
            })
        );
    }

    #[test]
    fn rejects_hostile_shapes_before_using_them() {
        assert_eq!(
            TerrainGrid::from_cells(0, 1, vec![]),
            Err(AnalysisError::InvalidGridDimensions {
                width: 0,
                height: 1
            })
        );
        assert_eq!(
            TerrainGrid::from_cells(1_025, 1, vec![]),
            Err(AnalysisError::InvalidGridDimensions {
                width: 1_025,
                height: 1
            })
        );
        assert_eq!(
            TerrainGrid::from_cells(2, 2, vec![cell(true)]),
            Err(AnalysisError::CellLength {
                width: 2,
                height: 2,
                expected: 4,
                actual: 1
            })
        );
        assert_eq!(
            TerrainGrid::from_terrain(&terrain(0, 1, vec![]), &Cv5::default(), &Vf4::default()),
            Err(AnalysisError::InvalidTerrainDimensions {
                width: 0,
                height: 1
            })
        );
        assert_eq!(
            TerrainGrid::from_terrain(&terrain(1, 1, vec![]), &Cv5::default(), &Vf4::default()),
            Err(AnalysisError::TerrainTileLength {
                width: 1,
                height: 1,
                expected: 1,
                actual: 0
            })
        );
        assert_eq!(
            TerrainGrid::from_cells(
                1,
                1,
                vec![TerrainCell {
                    elevation: 3,
                    ..cell(true)
                }]
            ),
            Err(AnalysisError::InvalidElevation {
                index: 0,
                elevation: 3
            })
        );
    }

    #[test]
    fn diagonal_pinch_is_not_passable() {
        let terrain = grid(2, 2, |x, y| (x, y) == (0, 0) || (x, y) == (1, 1));
        assert_eq!(
            terrain
                .route(WalkPosition { x: 0, y: 0 }, WalkPosition { x: 1, y: 1 })
                .unwrap(),
            None
        );
    }

    #[test]
    fn rejects_out_of_bounds_and_blocked_endpoints() {
        let terrain = grid(2, 2, |x, y| (x, y) != (1, 1));
        assert_eq!(
            terrain.route(WalkPosition { x: 2, y: 0 }, WalkPosition { x: 0, y: 0 }),
            Err(AnalysisError::EndpointOutOfBounds {
                endpoint: Endpoint::Start,
                position_x: 2,
                position_y: 0
            })
        );
        assert_eq!(
            terrain.route(WalkPosition { x: 0, y: 0 }, WalkPosition { x: 1, y: 1 }),
            Err(AnalysisError::EndpointBlocked {
                endpoint: Endpoint::End,
                position_x: 1,
                position_y: 1
            })
        );
    }

    #[test]
    fn same_valid_point_has_zero_distance() {
        let terrain = grid(1, 1, |_, _| true);
        assert_eq!(
            terrain
                .route(WalkPosition { x: 0, y: 0 }, WalkPosition { x: 0, y: 0 })
                .unwrap(),
            Some(Route {
                points: vec![WalkPosition { x: 0, y: 0 }],
                distance_pixels: 0.0
            })
        );
    }

    #[test]
    fn disconnected_cells_return_none() {
        let terrain = grid(3, 1, |x, _| x != 1);
        assert_eq!(
            terrain
                .route(WalkPosition { x: 0, y: 0 }, WalkPosition { x: 2, y: 0 })
                .unwrap(),
            None
        );
    }

    #[test]
    fn routes_around_an_obstacle() {
        let terrain = grid(5, 3, |x, y| !(y == 1 && (1..=3).contains(&x)));
        let route = terrain
            .route(WalkPosition { x: 0, y: 1 }, WalkPosition { x: 4, y: 1 })
            .unwrap()
            .unwrap();
        assert_eq!(route.points.first(), Some(&WalkPosition { x: 0, y: 1 }));
        assert_eq!(route.points.last(), Some(&WalkPosition { x: 4, y: 1 }));
        assert_eq!(route.distance_pixels, 48.0);
    }

    #[test]
    fn ties_choose_a_stable_route() {
        let terrain = grid(3, 3, |x, y| !(x == 1 && y == 1));
        let expected = vec![
            WalkPosition { x: 0, y: 1 },
            WalkPosition { x: 0, y: 0 },
            WalkPosition { x: 1, y: 0 },
            WalkPosition { x: 2, y: 0 },
            WalkPosition { x: 2, y: 1 },
        ];
        for _ in 0..10 {
            assert_eq!(
                terrain
                    .route(WalkPosition { x: 0, y: 1 }, WalkPosition { x: 2, y: 1 })
                    .unwrap()
                    .unwrap()
                    .points,
                expected
            );
        }
    }

    #[test]
    fn astar_matches_independent_dijkstra_on_small_grids() {
        for mask in 0_u16..(1 << 9) {
            let terrain = grid(3, 3, |x, y| mask & (1 << (y * 3 + x)) != 0);
            let start = WalkPosition { x: 0, y: 0 };
            let end = WalkPosition { x: 2, y: 2 };
            if !terrain.cell(start).unwrap().walkable || !terrain.cell(end).unwrap().walkable {
                continue;
            }
            let actual = terrain
                .route(start, end)
                .unwrap()
                .map(|route| route.distance_pixels);
            let expected =
                dijkstra_distance(&terrain, start, end).map(|cost| cost as f64 * 8.0 / 1_000.0);
            assert_eq!(actual, expected, "mask {mask:09b}");
        }
    }

    fn dijkstra_distance(
        grid: &TerrainGrid,
        start: WalkPosition,
        end: WalkPosition,
    ) -> Option<u64> {
        let start = grid.index(start)?;
        let end = grid.index(end)?;
        let mut costs = vec![u64::MAX; grid.cells.len()];
        let mut queue = BinaryHeap::new();
        costs[start] = 0;
        queue.push(Reverse((0_u64, start)));
        while let Some(Reverse((cost, current))) = queue.pop() {
            if costs[current] != cost {
                continue;
            }
            if current == end {
                return Some(cost);
            }
            for (next, step) in grid.neighbors(grid.position(current)) {
                let next = grid.index(next)?;
                let candidate = cost + step;
                if candidate < costs[next] {
                    costs[next] = candidate;
                    queue.push(Reverse((candidate, next)));
                }
            }
        }
        None
    }

    #[test]
    fn accepts_real_format_parsers_as_adapter_inputs() {
        let mut cv5_bytes = vec![0; 52];
        cv5_bytes[20..22].copy_from_slice(&0_u16.to_le_bytes());
        let vf4_bytes = [MiniTileFlags::WALKABLE.bits().to_le_bytes().as_slice(); 16].concat();
        let result = TerrainGrid::from_terrain(
            &terrain(1, 1, vec![0]),
            &parse_cv5(&cv5_bytes),
            &parse_vf4(&vf4_bytes),
        );
        assert!(result.unwrap().cells().iter().all(|cell| cell.walkable));
    }
}
