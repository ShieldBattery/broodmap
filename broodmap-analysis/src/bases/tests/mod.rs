mod clustering;
mod discovery;
mod occupancy;
mod proximity;
mod pruning;

use super::*;
use crate::TerrainCell;

fn grid(width_tiles: u32, height_tiles: u32, walkable: impl Fn(u32, u32) -> bool) -> TerrainGrid {
    let width = width_tiles * 4;
    let height = height_tiles * 4;
    let mut cells = Vec::new();
    for y in 0..height {
        for x in 0..width {
            cells.push(TerrainCell {
                walkable: walkable(x, y),
                terrain_buildable: true,
                elevation: 0,
                ramp: false,
            });
        }
    }
    TerrainGrid::from_cells(width, height, cells).unwrap()
}
fn resource(x: i32, y: i32, kind: ResourceKind, amount: Option<u32>) -> ResourceNode {
    ResourceNode {
        bounds: PixelRect {
            left: x,
            top: y,
            right: x + 16,
            bottom: y + 16,
        },
        kind,
        amount,
    }
}
fn field() -> Vec<ResourceNode> {
    vec![
        resource(176, 112, ResourceKind::Mineral, None),
        resource(208, 112, ResourceKind::Mineral, None),
        resource(176, 144, ResourceKind::Mineral, None),
        resource(208, 144, ResourceKind::Mineral, None),
    ]
}

fn node(bounds: [i32; 4], kind: ResourceKind, amount: u32) -> ResourceNode {
    ResourceNode {
        bounds: PixelRect {
            left: bounds[0],
            top: bounds[1],
            right: bounds[2],
            bottom: bounds[3],
        },
        kind,
        amount: Some(amount),
    }
}
