//! The terrain compositor: CHK tile IDs -> CV5 megatile IDs -> `.dds.vr4` frames -> a single
//! RGBA image. See `docs/render-design.md`, "Terrain" and phase 1 of "Phasing".

use std::collections::HashMap;

use thiserror::Error;

use broodmap::chk::terrain::TerrainTileIds;
use broodmap::chk::tileset::Tileset;
use broodmap_formats::{DdsFormat, DdsVr4, DdsVr4Error, Frame, parse_cv5, parse_dds};

use crate::bc::{decode_bc1, decode_bc3};
use crate::image::{RgbaImage, scale_rgba};
use crate::options::{RenderOptions, resolve_tier};
use crate::source::{AssetRequest, SourceError, TilesetDataSource};

/// Upper bound on the width/height (in pixels) we'll attempt to decode a single megatile frame
/// at, regardless of what a (possibly malformed) DDS header claims. Real SC:R megatile frames
/// are 32/64/128px; this just keeps a corrupt/hostile asset from driving an unbounded
/// allocation per frame.
const MAX_DECODE_DIM: u32 = 1024;

/// Errors rendering terrain. Missing individual megatile *frames* are not errors (they render
/// as an opaque black tile); a missing/unreadable CV5 or `.dds.vr4` *file* is.
#[derive(Error, Debug)]
pub enum RenderError {
    #[error("failed to read tileset asset: {0}")]
    Source(#[from] SourceError),
    #[error("failed to parse tileset megatile texture container: {0}")]
    DdsVr4(#[from] DdsVr4Error),
}

/// Renders a map's terrain to a single RGBA image.
///
/// Output dimensions are `(terrain.width * ppt) x (terrain.height * ppt)`, where `ppt` (pixels
/// per tile) is resolved from `options` and the map's size (see
/// [`crate::options::RenderOptions`]). Each unique megatile is decoded and downscaled to `ppt`
/// once, then blitted everywhere it appears, so cost is proportional to the number of distinct
/// megatiles used, not the map's tile count.
pub fn render_terrain(
    terrain: &TerrainTileIds,
    tileset: Tileset,
    source: &dyn TilesetDataSource,
    options: &RenderOptions,
) -> Result<RgbaImage, RenderError> {
    if terrain.width == 0 || terrain.height == 0 {
        return Ok(RgbaImage {
            width: 0,
            height: 0,
            data: Vec::new(),
        });
    }

    // Map dimensions are capped at 256x256 (see `broodmap::chk::terrain::read_terrain`) and ppt
    // is capped at a tier's native size (<=128), so this product comfortably fits in u32.
    let map_w = terrain.width as u32;
    let map_h = terrain.height as u32;
    let (tier, pack, ppt) = resolve_tier(options, map_w, map_h);

    let cv5_bytes = source.read(&AssetRequest::Cv5(tileset))?;
    let cv5 = parse_cv5(cv5_bytes.as_ref());

    let dds_vr4_bytes = source.read(&AssetRequest::TilesetDds(tileset, tier, pack))?;
    let dds_vr4 = DdsVr4::parse(dds_vr4_bytes.as_ref())?;

    let out_w = map_w * ppt;
    let out_h = map_h * ppt;
    let mut out = vec![0u8; out_w as usize * out_h as usize * 4];

    // Cache decoded (and downscaled-to-ppt) tiles by megatile ID: a map's distinct megatile
    // count is typically tiny relative to its tile count.
    let mut tile_cache: HashMap<u16, Vec<u8>> = HashMap::new();

    for y in 0..terrain.height {
        for x in 0..terrain.width {
            // Flat, bounds-checked access rather than `terrain[y][x]`: `TerrainTileIds`'s
            // fields are public, so a caller-constructed value may have fewer tiles than
            // `width * height` claims. Missing tiles render as tile 0 instead of panicking.
            let tile_id = terrain
                .tiles
                .get(y * terrain.width + x)
                .copied()
                .unwrap_or_default();
            let megatile_id = cv5
                .group(tile_id.group_id())
                .map(|group| group.mega_tiles[tile_id.tile_index() as usize])
                .unwrap_or(0);

            let tile_rgba = tile_cache
                .entry(megatile_id)
                .or_insert_with(|| render_megatile(&dds_vr4, megatile_id, ppt));

            blit_tile(
                &mut out,
                out_w,
                tile_rgba,
                ppt,
                x as u32 * ppt,
                y as u32 * ppt,
            );
        }
    }

    Ok(RgbaImage {
        width: out_w,
        height: out_h,
        data: out,
    })
}

/// Decodes a single megatile to a `ppt`x`ppt` RGBA8 tile. Falls back to opaque black for a
/// missing frame, an unparseable/undecodable payload, a zero-sized declared image, or (paletted
/// frames) a missing embedded palette.
fn render_megatile(dds_vr4: &DdsVr4, megatile_id: u16, ppt: u32) -> Vec<u8> {
    let Some(frame) = dds_vr4.frame(megatile_id) else {
        return opaque_black(ppt);
    };

    let (rgba, decode_w, decode_h) = match frame {
        Frame::Dds(dds_bytes) => {
            let Ok(dds) = parse_dds(dds_bytes) else {
                return opaque_black(ppt);
            };

            let decode_w = dds.width.min(MAX_DECODE_DIM);
            let decode_h = dds.height.min(MAX_DECODE_DIM);
            if decode_w == 0 || decode_h == 0 {
                return opaque_black(ppt);
            }

            let rgba = match dds.format {
                DdsFormat::Bc1 => decode_bc1(dds.payload, decode_w, decode_h),
                DdsFormat::Bc3 => decode_bc3(dds.payload, decode_w, decode_h),
                DdsFormat::Rgba8 => {
                    let needed = decode_w as usize * decode_h as usize * 4;
                    let mut buf = vec![0u8; needed];
                    let copy_len = needed.min(dds.payload.len());
                    buf[..copy_len].copy_from_slice(&dds.payload[..copy_len]);
                    buf
                }
                _ => return opaque_black(ppt),
            };
            (rgba, decode_w, decode_h)
        }
        Frame::Paletted {
            indices,
            width,
            height,
        } => {
            let Some(palette) = dds_vr4.palette() else {
                return opaque_black(ppt);
            };

            let decode_w = width.min(MAX_DECODE_DIM);
            let decode_h = height.min(MAX_DECODE_DIM);
            if decode_w == 0 || decode_h == 0 {
                return opaque_black(ppt);
            }

            let needed = decode_w as usize * decode_h as usize;
            if indices.len() < needed {
                return opaque_black(ppt);
            }

            let mut rgba = vec![0u8; needed * 4];
            for (texel, &index) in rgba.chunks_exact_mut(4).zip(&indices[..needed]) {
                let [r, g, b] = palette.rgb(index);
                texel[0] = r;
                texel[1] = g;
                texel[2] = b;
                texel[3] = 255;
            }
            (rgba, decode_w, decode_h)
        }
    };

    if decode_w == ppt && decode_h == ppt {
        rgba
    } else {
        scale_rgba(&rgba, decode_w, decode_h, ppt, ppt)
    }
}

/// An opaque black `ppt`x`ppt` RGBA8 tile, used as a fallback for missing/unusable megatile art.
fn opaque_black(ppt: u32) -> Vec<u8> {
    let mut tile = vec![0u8; ppt as usize * ppt as usize * 4];
    for texel in tile.chunks_exact_mut(4) {
        texel[3] = 255;
    }
    tile
}

/// Blits a `ppt`x`ppt` RGBA8 tile into `out` (an `out_w`-wide RGBA8 image) at `(dst_x, dst_y)`.
fn blit_tile(out: &mut [u8], out_w: u32, tile: &[u8], ppt: u32, dst_x: u32, dst_y: u32) {
    let row_bytes = ppt as usize * 4;
    for ty in 0..ppt {
        let src_start = ty as usize * row_bytes;
        let Some(src_row) = tile.get(src_start..src_start + row_bytes) else {
            continue;
        };
        let dst_start = (((dst_y + ty) as usize) * out_w as usize + dst_x as usize) * 4;
        if let Some(dst_row) = out.get_mut(dst_start..dst_start + row_bytes) {
            dst_row.copy_from_slice(src_row);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::MemorySource;
    use crate::tier::{ArtPack, ArtStyle, AssetTier};
    use broodmap::chk::terrain::TileId;

    /// Builds a minimal 124-byte `DDS_HEADER` (plus magic) for a BC1 (DXT1) image, following the
    /// layout in `broodmap-formats/src/dds.rs`'s test helper.
    fn dds_header(height: u32, width: u32) -> Vec<u8> {
        let mut data = Vec::with_capacity(4 + 124);
        data.extend_from_slice(b"DDS ");
        data.extend_from_slice(&124u32.to_le_bytes()); // dwSize
        data.extend_from_slice(&0u32.to_le_bytes()); // dwFlags
        data.extend_from_slice(&height.to_le_bytes());
        data.extend_from_slice(&width.to_le_bytes());
        data.extend_from_slice(&0u32.to_le_bytes()); // dwPitchOrLinearSize
        data.extend_from_slice(&0u32.to_le_bytes()); // dwDepth
        data.extend_from_slice(&1u32.to_le_bytes()); // dwMipMapCount
        data.extend_from_slice(&[0u8; 44]); // dwReserved1[11]
        data.extend_from_slice(&32u32.to_le_bytes()); // pixel format dwSize
        data.extend_from_slice(&0x4u32.to_le_bytes()); // pf_flags (DDPF_FOURCC)
        data.extend_from_slice(b"DXT1");
        data.extend_from_slice(&0u32.to_le_bytes()); // rgb_bit_count
        data.extend_from_slice(&[0u8; 16]); // bit masks
        data.extend_from_slice(&[0u8; 20]); // dwCaps..dwReserved2
        data
    }

    /// A solid-color 4x4 BC1 block: `color0 == color1`, all indices 0, so every texel decodes
    /// to that color at full alpha.
    fn solid_bc1_dds(color565: u16) -> Vec<u8> {
        let mut data = dds_header(4, 4);
        data.extend_from_slice(&color565.to_le_bytes()); // color0
        data.extend_from_slice(&color565.to_le_bytes()); // color1
        data.extend_from_slice(&0u32.to_le_bytes()); // indices
        data
    }

    /// Builds a DDS-record-layout `.dds.vr4` file (HD tier): format code `0x1004` (bit `0x10`
    /// clear), one record per frame.
    fn dds_vr4_bytes(frames: &[&[u8]]) -> Vec<u8> {
        let mut data = Vec::new();
        data.extend_from_slice(&0u32.to_le_bytes()); // file size (informational, unvalidated)
        data.extend_from_slice(&(frames.len() as u16).to_le_bytes());
        data.extend_from_slice(&0x1004u16.to_le_bytes()); // format code: HD, DDS-record layout
        for frame in frames {
            data.extend_from_slice(&0u32.to_le_bytes()); // zero
            data.extend_from_slice(&0u16.to_le_bytes()); // width (unused by this layout)
            data.extend_from_slice(&0u16.to_le_bytes()); // height (unused by this layout)
            data.extend_from_slice(&(frame.len() as u32).to_le_bytes());
            data.extend_from_slice(frame);
        }
        data
    }

    /// Builds a paletted-layout (SD) `.dds.vr4` file: format code `0x1011` (scale 1 = 32px, bit
    /// `0x10` set), given a shared tile width/height, a full 1024-byte palette, and raw
    /// palette-index tile bytes.
    fn paletted_dds_vr4_bytes(
        width: u16,
        height: u16,
        palette: &[u8; 1024],
        tiles: &[&[u8]],
    ) -> Vec<u8> {
        let mut data = Vec::new();
        data.extend_from_slice(&0u32.to_le_bytes()); // file size (informational, unvalidated)
        data.extend_from_slice(&(tiles.len() as u16).to_le_bytes());
        data.extend_from_slice(&0x1011u16.to_le_bytes()); // format code: SD, paletted layout
        data.extend_from_slice(&width.to_le_bytes());
        data.extend_from_slice(&height.to_le_bytes());
        data.extend_from_slice(palette);
        for tile in tiles {
            data.extend_from_slice(tile);
        }
        data
    }

    fn cv5_entry(mega_tile_0: u16) -> Vec<u8> {
        let mut entry = Vec::with_capacity(52);
        entry.extend_from_slice(&0u16.to_le_bytes()); // group_type
        entry.extend_from_slice(&0u16.to_le_bytes()); // flags
        entry.extend_from_slice(&[0u8; 16]); // unused rects
        entry.extend_from_slice(&mega_tile_0.to_le_bytes()); // mega_tiles[0]
        entry.extend_from_slice(&[0u8; 30]); // mega_tiles[1..16]
        assert_eq!(entry.len(), 52);
        entry
    }

    #[test]
    fn renders_checkerboard_terrain_with_exact_quadrant_colors() {
        // Two tile groups, each pointing at one of two solid-color BC1 megatile frames.
        let red565 = 0xF800u16; // group 0 -> megatile 0
        let blue565 = 0x001Fu16; // group 1 -> megatile 1

        let mut cv5 = Vec::new();
        cv5.extend(cv5_entry(0)); // group 0 -> megatile 0 (red)
        cv5.extend(cv5_entry(1)); // group 1 -> megatile 1 (blue)

        let red_frame = solid_bc1_dds(red565);
        let blue_frame = solid_bc1_dds(blue565);
        let dds_vr4 = dds_vr4_bytes(&[&red_frame, &blue_frame]);

        let mut source = MemorySource::new();
        source.insert(AssetRequest::Cv5(Tileset::Jungle), cv5);
        source.insert(
            AssetRequest::TilesetDds(Tileset::Jungle, AssetTier::Sd, ArtPack::Standard),
            dds_vr4,
        );

        // 2x2 checkerboard: (0,0) and (1,1) use group 0 (red), (1,0) and (0,1) use group 1
        // (blue). group_id is bits 4..15 of the tile ID, tile_index the bottom 4 bits.
        let group0_tile = TileId(0); // group_id 0, tile_index 0
        let group1_tile = TileId(16); // group_id 1, tile_index 0
        let terrain = TerrainTileIds {
            width: 2,
            height: 2,
            tiles: vec![group0_tile, group1_tile, group1_tile, group0_tile],
        };

        // ppt=4: Original style pins Sd, max_dimension=8 over a 2-tile-wide map => desired_ppt=4.
        let options = RenderOptions {
            art_style: ArtStyle::Original,
            max_dimension: Some(8),
            ..Default::default()
        };

        let image = render_terrain(&terrain, Tileset::Jungle, &source, &options).unwrap();
        assert_eq!(image.width, 8);
        assert_eq!(image.height, 8);
        assert_eq!(image.data.len(), 8 * 8 * 4);

        let red = [255, 0, 0, 255];
        let blue = [0, 0, 255, 255];
        let pixel = |img: &RgbaImage, x: usize, y: usize| -> [u8; 4] {
            let offset = (y * img.width as usize + x) * 4;
            img.data[offset..offset + 4].try_into().unwrap()
        };

        // Top-left quadrant (tile 0,0): red.
        assert_eq!(pixel(&image, 0, 0), red);
        assert_eq!(pixel(&image, 3, 3), red);
        // Top-right quadrant (tile 1,0): blue.
        assert_eq!(pixel(&image, 4, 0), blue);
        assert_eq!(pixel(&image, 7, 3), blue);
        // Bottom-left quadrant (tile 0,1): blue.
        assert_eq!(pixel(&image, 0, 4), blue);
        assert_eq!(pixel(&image, 3, 7), blue);
        // Bottom-right quadrant (tile 1,1): red.
        assert_eq!(pixel(&image, 4, 4), red);
        assert_eq!(pixel(&image, 7, 7), red);
    }

    #[test]
    fn renders_paletted_checkerboard_terrain_with_exact_quadrant_colors() {
        // Same checkerboard setup as the DDS-layout test above, but backed by a paletted (SD)
        // `.dds.vr4`: two solid-color 4x4 tiles, each a single palette index repeated.
        let mut palette = [0u8; 1024];
        palette[4..8].copy_from_slice(&[0xFF, 0x00, 0x00, 0xEE]); // index 1: red, pad ignored
        palette[8..12].copy_from_slice(&[0x00, 0x00, 0xFF, 0x00]); // index 2: blue

        let mut cv5 = Vec::new();
        cv5.extend(cv5_entry(0)); // group 0 -> megatile 0 (red)
        cv5.extend(cv5_entry(1)); // group 1 -> megatile 1 (blue)

        let red_tile = [1u8; 16]; // 4x4, all index 1
        let blue_tile = [2u8; 16]; // 4x4, all index 2
        let dds_vr4 = paletted_dds_vr4_bytes(4, 4, &palette, &[&red_tile, &blue_tile]);

        let mut source = MemorySource::new();
        source.insert(AssetRequest::Cv5(Tileset::Jungle), cv5);
        source.insert(
            AssetRequest::TilesetDds(Tileset::Jungle, AssetTier::Sd, ArtPack::Standard),
            dds_vr4,
        );

        let group0_tile = TileId(0); // group_id 0, tile_index 0
        let group1_tile = TileId(16); // group_id 1, tile_index 0
        let terrain = TerrainTileIds {
            width: 2,
            height: 2,
            tiles: vec![group0_tile, group1_tile, group1_tile, group0_tile],
        };

        // ppt=4: Original style pins Sd, max_dimension=8 over a 2-tile-wide map => desired_ppt=4,
        // matching the tile's native 4x4 so no scaling is exercised.
        let options = RenderOptions {
            art_style: ArtStyle::Original,
            max_dimension: Some(8),
            ..Default::default()
        };

        let image = render_terrain(&terrain, Tileset::Jungle, &source, &options).unwrap();
        assert_eq!(image.width, 8);
        assert_eq!(image.height, 8);
        assert_eq!(image.data.len(), 8 * 8 * 4);

        let red = [255, 0, 0, 255];
        let blue = [0, 0, 255, 255];
        let pixel = |img: &RgbaImage, x: usize, y: usize| -> [u8; 4] {
            let offset = (y * img.width as usize + x) * 4;
            img.data[offset..offset + 4].try_into().unwrap()
        };

        // Top-left quadrant (tile 0,0): red.
        assert_eq!(pixel(&image, 0, 0), red);
        assert_eq!(pixel(&image, 3, 3), red);
        // Top-right quadrant (tile 1,0): blue.
        assert_eq!(pixel(&image, 4, 0), blue);
        assert_eq!(pixel(&image, 7, 3), blue);
        // Bottom-left quadrant (tile 0,1): blue.
        assert_eq!(pixel(&image, 0, 4), blue);
        assert_eq!(pixel(&image, 3, 7), blue);
        // Bottom-right quadrant (tile 1,1): red.
        assert_eq!(pixel(&image, 4, 4), red);
        assert_eq!(pixel(&image, 7, 7), red);
    }

    #[test]
    fn malformed_terrain_with_missing_tiles_does_not_panic() {
        // TerrainTileIds' fields are public, so a caller can claim 2x2 while providing fewer
        // (or zero) tiles. Missing tiles render as tile 0 rather than panicking.
        let cv5 = cv5_entry(0);
        let dds_vr4 = dds_vr4_bytes(&[&solid_bc1_dds(0xFFFF)]);

        let mut source = MemorySource::new();
        source.insert(AssetRequest::Cv5(Tileset::Jungle), cv5);
        source.insert(
            AssetRequest::TilesetDds(Tileset::Jungle, AssetTier::Sd, ArtPack::Standard),
            dds_vr4,
        );

        let terrain = TerrainTileIds {
            width: 2,
            height: 2,
            tiles: vec![TileId(0)], // 3 tiles short of the claimed 2x2
        };
        let options = RenderOptions {
            art_style: ArtStyle::Original,
            max_dimension: Some(8),
            ..Default::default()
        };

        let image = render_terrain(&terrain, Tileset::Jungle, &source, &options).unwrap();
        assert_eq!((image.width, image.height), (8, 8));
        // All four quadrants resolve to megatile 0 (white), whether present or defaulted.
        for texel in image.data.chunks_exact(4) {
            assert_eq!(texel, [255, 255, 255, 255]);
        }
    }

    #[test]
    fn empty_terrain_returns_empty_image() {
        let terrain = TerrainTileIds {
            width: 0,
            height: 0,
            tiles: Vec::new(),
        };
        let source = MemorySource::new();
        let options = RenderOptions::default();
        let image = render_terrain(&terrain, Tileset::Jungle, &source, &options).unwrap();
        assert_eq!(image.width, 0);
        assert_eq!(image.height, 0);
        assert!(image.data.is_empty());
    }

    #[test]
    fn missing_cv5_is_an_error() {
        let terrain = TerrainTileIds {
            width: 1,
            height: 1,
            tiles: vec![TileId(0)],
        };
        let source = MemorySource::new();
        let options = RenderOptions {
            art_style: ArtStyle::Original,
            ..Default::default()
        };
        let err = render_terrain(&terrain, Tileset::Jungle, &source, &options).unwrap_err();
        assert!(matches!(err, RenderError::Source(SourceError::NotFound)));
    }

    #[test]
    fn missing_megatile_frame_renders_opaque_black_not_an_error() {
        // CV5 references megatile 5, but the dds.vr4 only has 1 frame (id 0).
        let cv5 = cv5_entry(5);
        let dds_vr4 = dds_vr4_bytes(&[&solid_bc1_dds(0xFFFF)]);

        let mut source = MemorySource::new();
        source.insert(AssetRequest::Cv5(Tileset::Jungle), cv5);
        source.insert(
            AssetRequest::TilesetDds(Tileset::Jungle, AssetTier::Sd, ArtPack::Standard),
            dds_vr4,
        );

        let terrain = TerrainTileIds {
            width: 1,
            height: 1,
            tiles: vec![TileId(0)],
        };
        let options = RenderOptions {
            art_style: ArtStyle::Original,
            max_dimension: Some(4),
            ..Default::default()
        };

        let image = render_terrain(&terrain, Tileset::Jungle, &source, &options).unwrap();
        for texel in image.data.chunks_exact(4) {
            assert_eq!(texel, [0, 0, 0, 255]);
        }
    }
}
