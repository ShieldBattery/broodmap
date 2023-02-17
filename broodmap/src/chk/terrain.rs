use std::ops::Index;
use thiserror::Error;

/// A tile ID, which is a reference to a particular tile within a tileset. The top 0x7FF bits of the
/// ID refer to the tile group, and the bottom 0xF bits refer to the tile index within that group.
/// This can be mapped to tile group flags and a mega-tile ID through a CV5 file.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct TileId(pub u16);

impl TileId {
    /// Returns whether or not this tile is marked as having creep on it.
    pub fn has_creep(&self) -> bool {
        self.0 & 0x8000 != 0
    }

    /// Returns the group ID of this tile, which can be used to index the entries of a CV5 file.
    pub fn group_id(&self) -> u16 {
        (self.0 >> 4) & 0x7FF
    }

    /// Returns the index of this tile within its group. This can be used to index the mega-tiles of
    /// a particular entry in a CV5 file.
    pub fn tile_index(&self) -> u16 {
        self.0 & 0xF
    }

    /// Returns the unified tile ID (with the creep flag removed).
    pub fn id(&self) -> u16 {
        self.0 & 0x7FFF
    }
}

impl From<u16> for TileId {
    fn from(value: u16) -> Self {
        Self(value)
    }
}

/// Describes the terrain of the map through tile ID references. Corresponds to the MTXM chunk in a
/// map file.
///
/// See also: http://www.staredit.net/wiki/index.php?title=Terrain_Format
#[derive(Debug, Clone)]
pub struct TerrainTileIds {
    /// The width of the map in MegaTiles.
    pub width: usize,
    /// The height of the map in MegaTiles.
    pub height: usize,
    /// The tiles of the map, stored left-to-right, top-to-bottom. Each values is a tile ID, which
    /// can be used to index a CV5 file for the tileset of the map.
    /// See: http://www.staredit.net/wiki/index.php?title=Terrain_Format
    pub tiles: Vec<TileId>,
}

impl TerrainTileIds {
    // Retrieves a tile at the given coordinates, checking the bounds to ensure it's a valid
    // position. Returns [None] if the coordinates are out of bounds.
    pub fn get(&self, y: usize, x: usize) -> Option<TileId> {
        if x >= self.width || y >= self.height {
            return None;
        }

        Some(self[y][x])
    }
}

type TileIdRow = [TileId];

impl Index<usize> for TerrainTileIds {
    type Output = TileIdRow;

    /// Returns a row of tiles given a y coordinate. This operation is not checked against the
    /// bounds of the map, and may panic if given values that exceed it.
    fn index(&self, y: usize) -> &Self::Output {
        let row = y * self.width;
        &self.tiles[row..row + self.width]
    }
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum TerrainError {
    #[error("Chunk missing")]
    ChunkMissing,
    #[error("Bad map dimensions: {0}x{1} exceeds 256x256")]
    BadDimensions(usize, usize),
}

pub fn read_terrain(
    data: &[u8],
    width: usize,
    height: usize,
) -> Result<TerrainTileIds, TerrainError> {
    if width > 256 || height > 256 {
        return Err(TerrainError::BadDimensions(width, height));
    }

    let tile_count = width * height;
    let max_data_len = tile_count * 2;
    let data = if data.len() > max_data_len {
        &data[..max_data_len]
    } else {
        data
    };

    let mut tiles = data
        .chunks_exact(2)
        .map(|chunk| u16::from_le_bytes(chunk.try_into().unwrap()).into())
        .collect::<Vec<_>>();
    if tiles.len() != tile_count {
        tiles.resize(tile_count, TileId::default());
    }

    Ok(TerrainTileIds {
        width,
        height,
        tiles,
    })
}
