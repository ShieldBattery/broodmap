use std::ops::Index;
use thiserror::Error;

/// Describes the terrain of the map through MegaTile references. Corresponds to the MTXM chunk in a
/// map file.
///
/// The tiles can be indexed via either the `get` method, or using the `Index` trait if you know the
/// coordinates are valid:
///
/// ```rs
/// let x = 0;
/// let y = 5;
/// let tile_1 = terrain.get(y, x);
/// let tile_2 = terrain[y][x];
/// ```
///
/// See also: http://www.staredit.net/wiki/index.php?title=Terrain_Format
#[derive(Debug, Clone)]
pub struct TerrainMegaTiles {
    /// The width of the map in MegaTiles.
    pub width: usize,
    /// The height of the map in MegaTiles.
    pub height: usize,
    /// The tiles of the map, stored left-to-right, top-to-bottom. Each values is a reference to a
    /// MegaTile. See: http://www.staredit.net/wiki/index.php?title=Terrain_Format
    pub tiles: Vec<u16>,
}

impl TerrainMegaTiles {
    // Retrieves a tile at the given coordinates, checking the bounds to ensure it's a valid
    // position. Returns [None] if the coordinates are out of bounds.
    pub fn get(&self, y: usize, x: usize) -> Option<u16> {
        if x >= self.width || y >= self.height {
            return None;
        }

        Some(self[y][x])
    }
}

/// A row of MegaTile references.
type MegaTileRow = [u16];

impl Index<usize> for TerrainMegaTiles {
    type Output = MegaTileRow;

    /// Returns a row of tiles given a y coordinate. This operation is not checked against the
    /// bounds of the map, and may panic if given values that exceed it.
    fn index(&self, y: usize) -> &Self::Output {
        let row = y * self.width;
        &self.tiles[row..row + self.width]
    }
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum TerrainMegaTilesError {
    #[error("Chunk missing")]
    ChunkMissing,
    #[error("Bad map dimensions: {0}x{1} exceeds 256x256")]
    BadDimensions(usize, usize),
}

pub fn read_terrain_mega_tiles(
    data: &[u8],
    width: usize,
    height: usize,
) -> Result<TerrainMegaTiles, TerrainMegaTilesError> {
    if width > 256 || height > 256 {
        return Err(TerrainMegaTilesError::BadDimensions(width, height));
    }

    let tile_count = (width * height) as usize;
    let max_data_len = tile_count * 2;
    let data = if data.len() > max_data_len {
        &data[..max_data_len]
    } else {
        data
    };

    let mut tiles = data
        .chunks_exact(2)
        .map(|chunk| u16::from_le_bytes(chunk.try_into().unwrap()))
        .collect::<Vec<_>>();
    if tiles.len() != tile_count {
        tiles.resize(tile_count, 0);
    }

    Ok(TerrainMegaTiles {
        width,
        height,
        tiles,
    })
}
