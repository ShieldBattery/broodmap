//! The terrain compositor: CHK tile IDs -> CV5 megatile IDs -> `.dds.vr4` frames -> a single
//! RGBA image. See `docs/render-design.md`, "Terrain" and phase 1 of "Phasing".
//!
//! Megatiles are composited at their *native* resolution into a rolling window of horizontal
//! strips, one tile row at a time, and the native image is resampled down to the requested
//! output size by [`crate::resample`] (linear-light Catmull-Rom). Resampling the composited image
//! rather than each tile in isolation is what gets the filter windows across tile boundaries and
//! stops a box filter's aliasing from breaking up shorelines and cliff edges; the strip window is
//! what makes that affordable, since the full native image is never materialized — a 256x256 map
//! at HD's 128 px/tile would be 32Ki x 32Ki, 4 GiB of RGBA. See `crate::resample`'s module docs
//! for the measurements.

use std::collections::HashMap;

use broodmap::chk::terrain::TerrainTileIds;
use broodmap::chk::tileset::Tileset;
use broodmap_formats::{DdsFormat, DdsVr4, Frame, parse_cv5, parse_dds};

use crate::bc::{decode_bc1, decode_bc3};
use crate::error::RenderError;
use crate::image::{RgbaImage, scale_rgba};
use crate::options::{RenderOptions, resolve_tier};
use crate::resample::{NativeRows, resample_linear};
use crate::source::{AssetRequest, TilesetDataSource};

/// Upper bound on the width/height (in pixels) we'll attempt to decode a single megatile frame
/// at, regardless of what a (possibly malformed) DDS header claims. Real SC:R megatile frames
/// are 32/64/128px; this just keeps a corrupt/hostile asset from driving an unbounded
/// allocation per frame.
const MAX_DECODE_DIM: u32 = 1024;

/// Upper bound on the tile size we'll composite at. The largest real tier is HD's 128px; capping
/// here bounds the strip buffer (`map_w * NATIVE * NATIVE * 4` bytes, 16 MiB at the worst legal
/// map width) no matter what a hostile `.dds.vr4` claims its frames are.
const MAX_NATIVE_TILE_PX: u32 = 128;

/// Budget for the decoded-megatile cache. A tileset has a few thousand megatiles and a map
/// typically uses a few hundred, so this is never reached in practice; it exists so a
/// pathological map (65,536 distinct megatiles) can't turn a bounded render into a multi-GiB
/// one. Past the budget, tiles are decoded on demand instead of cached.
const MAX_TILE_CACHE_BYTES: usize = 64 * 1024 * 1024;

/// The CHK parser's own documented invariant on map dimensions (see
/// `broodmap::chk::terrain::read_terrain`). `TerrainTileIds`'s `width`/`height` fields are
/// public, though, so a caller-constructed value can claim anything — this clamps every
/// downstream computation to that invariant regardless of what the struct says, rather than
/// trusting it to drive allocation sizes or loop bounds. A map beyond this renders as its
/// 256x256-clamped corner: permissive, and documented, like every other malformed-input case in
/// this crate.
const MAX_TERRAIN_DIM: usize = 256;

/// Renders a map's terrain to a single RGBA image.
///
/// Output dimensions are `(terrain.width * ppt) x (terrain.height * ppt)`, where `ppt` (pixels
/// per tile) is resolved from `options` and the map's size (see
/// [`crate::options::RenderOptions`]). Each unique megatile is decoded once, at the art's native
/// tile size, and cached; the map is then composited strip by strip and resampled to `ppt`, so
/// peak memory is the output buffer plus one tile-row strip plus the filter window — never a
/// full-resolution intermediate.
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

    // `TerrainTileIds::width`/`height` are public and untrusted (a caller-constructed value
    // needn't respect the CHK parser's own 256x256 invariant), so they're clamped here *before*
    // any product involving them — everything from here on (`map_w * map_h` capacity, `out_w *
    // out_h` output sizing) is computed from the clamped `u32`s and comfortably fits (ppt is
    // separately capped at a tier's native size, <=128).
    let map_w = terrain.width.min(MAX_TERRAIN_DIM) as u32;
    let map_h = terrain.height.min(MAX_TERRAIN_DIM) as u32;
    let (tier, pack, ppt) = resolve_tier(options, map_w, map_h);

    let cv5_bytes = source.read(&AssetRequest::Cv5(tileset))?;
    let cv5 = parse_cv5(cv5_bytes.as_ref());

    let dds_vr4_bytes = source.read(&AssetRequest::TilesetDds(tileset, tier, pack))?;
    let dds_vr4 = DdsVr4::parse(dds_vr4_bytes.as_ref())?;

    // Resolve the CHK's tile IDs to megatile IDs up front (at most 256*256 `u16`s, post-clamp).
    // This is the only thing the CV5 is needed for, and having it as a flat grid keeps the strip
    // compositor free of tile-lookup concerns.
    let mut megatiles = Vec::with_capacity(map_w as usize * map_h as usize);
    for y in 0..map_h as usize {
        for x in 0..map_w as usize {
            // Flat, bounds-checked access rather than `terrain[y][x]`: `TerrainTileIds`'s fields
            // are public, so a caller-constructed value may have fewer tiles than `width *
            // height` claims (or a `width` so large that `y * terrain.width` overshoots `tiles`
            // entirely). Missing tiles render as tile 0 instead of panicking either way. Row
            // stride uses the *original*, unclamped `terrain.width` — that's the layout the flat
            // `tiles` vec actually uses — while `x`/`y` themselves range only over the clamped
            // dimensions.
            let tile_id = terrain
                .tiles
                .get(y * terrain.width + x)
                .copied()
                .unwrap_or_default();
            megatiles.push(
                cv5.group(tile_id.group_id())
                    .map(|group| group.mega_tiles[tile_id.tile_index() as usize])
                    .unwrap_or(0),
            );
        }
    }

    let native_px = native_tile_px(&dds_vr4, tier.tile_px(), ppt);
    let out_w = map_w * ppt;
    let out_h = map_h * ppt;

    let mut compositor = StripCompositor::new(&megatiles, map_w, &dds_vr4, native_px);

    let data = if native_px == ppt {
        // Native-resolution render: the composite *is* the output, byte for byte what a straight
        // per-tile blit produces. No filtering to do.
        let mut data = vec![0u8; out_w as usize * out_h as usize * 4];
        let row_bytes = out_w as usize * 4;
        for y in 0..out_h {
            let start = y as usize * row_bytes;
            data[start..start + row_bytes].copy_from_slice(compositor.row(y));
        }
        data
    } else {
        resample_linear(
            &mut compositor,
            map_w * native_px,
            map_h * native_px,
            out_w,
            out_h,
        )
    };

    Ok(RgbaImage {
        width: out_w,
        height: out_h,
        data,
    })
}

/// The tile size to composite at: the art's own frame size, clamped to something sane.
///
/// This is normally just `tier_px` (real `.dds.vr4` frames are exactly their tier's native size),
/// but probing the asset means synthetic/hostile files whose frames are a different size are
/// composited at *their* resolution rather than being upscaled to the tier's and immediately
/// downscaled again. The floor at `ppt` keeps this a downscale-only path.
fn native_tile_px(dds_vr4: &DdsVr4, tier_px: u32, ppt: u32) -> u32 {
    let probed = (0..dds_vr4.frame_count().min(8) as u16).find_map(|id| {
        let dim = match dds_vr4.frame(id)? {
            Frame::Paletted { width, height, .. } => width.max(height),
            Frame::Dds(bytes) => {
                let dds = parse_dds(bytes).ok()?;
                dds.width.max(dds.height)
            }
        };
        (dim > 0).then_some(dim)
    });

    probed
        .unwrap_or(tier_px)
        .clamp(1, MAX_NATIVE_TILE_PX)
        .max(ppt)
}

/// Composites the map one tile row at a time, handing out native-resolution rows in order.
///
/// Only the strip currently being read is held: [`crate::resample::resample_linear`] pulls rows
/// monotonically, so once a tile row has been fully consumed it can be overwritten by the next.
struct StripCompositor<'a> {
    megatiles: &'a [u16],
    map_w: u32,
    dds_vr4: &'a DdsVr4<'a>,
    tile_px: u32,
    /// `map_w * tile_px` wide, `tile_px` tall, RGBA8.
    strip: Vec<u8>,
    /// The tile row currently composited into `strip`.
    strip_row: Option<u32>,
    /// Decoded megatiles at native size, keyed by megatile ID.
    cache: HashMap<u16, Vec<u8>>,
    cache_bytes: usize,
}

impl<'a> StripCompositor<'a> {
    fn new(
        megatiles: &'a [u16],
        map_w: u32,
        dds_vr4: &'a DdsVr4<'a>,
        tile_px: u32,
    ) -> StripCompositor<'a> {
        let native_w = map_w as usize * tile_px as usize;
        StripCompositor {
            megatiles,
            map_w,
            dds_vr4,
            tile_px,
            strip: vec![0u8; native_w * tile_px as usize * 4],
            strip_row: None,
            cache: HashMap::new(),
            cache_bytes: 0,
        }
    }

    fn composite_tile_row(&mut self, tile_row: u32) {
        let native_w = self.map_w * self.tile_px;
        for tx in 0..self.map_w {
            let megatile_id = self
                .megatiles
                .get((tile_row * self.map_w + tx) as usize)
                .copied()
                .unwrap_or(0);

            if !self.cache.contains_key(&megatile_id) && self.cache_bytes < MAX_TILE_CACHE_BYTES {
                let tile = render_megatile(self.dds_vr4, megatile_id, self.tile_px);
                self.cache_bytes += tile.len();
                self.cache.insert(megatile_id, tile);
            }

            let dst_x = tx * self.tile_px;
            match self.cache.get(&megatile_id) {
                Some(tile) => blit_tile(&mut self.strip, native_w, tile, self.tile_px, dst_x, 0),
                // Cache budget exhausted: decode on demand rather than growing without bound.
                None => {
                    let tile = render_megatile(self.dds_vr4, megatile_id, self.tile_px);
                    blit_tile(&mut self.strip, native_w, &tile, self.tile_px, dst_x, 0);
                }
            }
        }
        self.strip_row = Some(tile_row);
    }
}

impl NativeRows for StripCompositor<'_> {
    fn row(&mut self, y: u32) -> &[u8] {
        let tile_row = y / self.tile_px;
        if self.strip_row != Some(tile_row) {
            self.composite_tile_row(tile_row);
        }
        let row_bytes = self.map_w as usize * self.tile_px as usize * 4;
        let start = (y % self.tile_px) as usize * row_bytes;
        &self.strip[start..start + row_bytes]
    }
}

/// Decodes a single megatile to a `ppt`x`ppt` RGBA8 tile (`ppt` here being the *composite* tile
/// size, i.e. the art's native size — the downscale to the output's px/tile happens later, over
/// the whole composited image). Falls back to opaque black for a missing frame, an
/// unparseable/undecodable payload, a zero-sized declared image, or (paletted frames) a missing
/// embedded palette.
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
    use crate::source::{MemorySource, SourceError};
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
        solid_bc1_dds_sized(color565, 4)
    }

    /// A solid-color `dim`x`dim` BC1 image (`dim` a multiple of 4), for tests that need the
    /// composite resolution to be larger than the output's px/tile.
    fn solid_bc1_dds_sized(color565: u16, dim: u32) -> Vec<u8> {
        let mut data = dds_header(dim, dim);
        for _ in 0..(dim / 4) * (dim / 4) {
            data.extend_from_slice(&color565.to_le_bytes()); // color0
            data.extend_from_slice(&color565.to_le_bytes()); // color1
            data.extend_from_slice(&0u32.to_le_bytes()); // indices
        }
        data
    }

    /// A two-tile-wide, one-tile-tall map: tile 0 uses megatile 0, tile 1 uses megatile 1.
    fn two_tile_source(left: Vec<u8>, right: Vec<u8>) -> (MemorySource, TerrainTileIds) {
        let mut cv5 = Vec::new();
        cv5.extend(cv5_entry(0));
        cv5.extend(cv5_entry(1));

        let mut source = MemorySource::new();
        source.insert(AssetRequest::Cv5(Tileset::Jungle), cv5);
        source.insert(
            AssetRequest::TilesetDds(Tileset::Jungle, AssetTier::Sd, ArtPack::Standard),
            dds_vr4_bytes(&[&left, &right]),
        );

        let terrain = TerrainTileIds {
            width: 2,
            height: 1,
            tiles: vec![TileId(0), TileId(16)],
        };
        (source, terrain)
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

    /// `TerrainTileIds::width`/`height` are public and untrusted, so a caller can claim
    /// dimensions the CHK parser itself would never produce (its own invariant caps both at
    /// 256). Trusting a claim like 100,000x100,000 directly would try to allocate (and iterate)
    /// on the order of 10 billion tiles; this must instead clamp to the documented 256x256 and
    /// render something small and bounded, not panic or attempt a huge allocation.
    #[test]
    fn lying_huge_terrain_dimensions_are_clamped_not_trusted() {
        let cv5 = cv5_entry(0);
        let dds_vr4 = dds_vr4_bytes(&[&solid_bc1_dds(0xFFFF)]);

        let mut source = MemorySource::new();
        source.insert(AssetRequest::Cv5(Tileset::Jungle), cv5);
        source.insert(
            AssetRequest::TilesetDds(Tileset::Jungle, AssetTier::Sd, ArtPack::Standard),
            dds_vr4,
        );

        let terrain = TerrainTileIds {
            width: 100_000,
            height: 100_000,
            tiles: vec![TileId(0)], // nowhere near 100,000 x 100,000 tiles
        };
        let options = RenderOptions {
            art_style: ArtStyle::Original,
            max_dimension: Some(256), // 256 (clamped) tiles wide => 1 px/tile
            ..Default::default()
        };

        let image = render_terrain(&terrain, Tileset::Jungle, &source, &options).unwrap();
        assert!(
            image.width <= 256 && image.height <= 256,
            "expected a 256-clamped output, got {}x{}",
            image.width,
            image.height
        );
        assert!(!image.data.is_empty());
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

    /// The core fix: at a downscale, the filter window straddles the boundary between two tiles,
    /// so the seam resolves to intermediate values. Under the old per-tile box downscale each
    /// tile was filtered in isolation and the two thumbnails butted together unchanged — which is
    /// exactly why identical megatiles produced byte-identical output and the grid showed.
    #[test]
    fn downscaling_blends_across_the_tile_boundary() {
        // 16px megatiles composited natively, 4 px/tile out: a 4:1 downscale.
        let (source, terrain) = two_tile_source(
            solid_bc1_dds_sized(0x0000, 16), // black
            solid_bc1_dds_sized(0xFFFF, 16), // white
        );
        let options = RenderOptions {
            art_style: ArtStyle::Original,
            max_dimension: Some(8),
            ..Default::default()
        };

        let image = render_terrain(&terrain, Tileset::Jungle, &source, &options).unwrap();
        assert_eq!((image.width, image.height), (8, 4));

        let luma: Vec<u8> = image.data[..8 * 4].chunks_exact(4).map(|t| t[0]).collect();
        assert_eq!(luma[0], 0, "far from the seam the tiles keep their color");
        assert_eq!(luma[7], 255);
        assert!(
            luma[3] > 0 && luma[4] < 255,
            "the tile boundary must blend, got {luma:?}"
        );
    }

    /// Terrain is filtered in linear light, so a 50/50 black-and-white tile pair averages to the
    /// linear mid-gray (~188) rather than sRGB averaging's much darker 127.
    #[test]
    fn downscaling_filters_in_linear_light() {
        let (source, terrain) = two_tile_source(
            solid_bc1_dds_sized(0x0000, 32),
            solid_bc1_dds_sized(0xFFFF, 32),
        );
        // 2 tiles, 2px cap => 1 px/tile: each output pixel spans a whole 32px tile, and the
        // filter's tails reach well into its neighbour.
        let options = RenderOptions {
            art_style: ArtStyle::Original,
            max_dimension: Some(2),
            ..Default::default()
        };

        let image = render_terrain(&terrain, Tileset::Jungle, &source, &options).unwrap();
        assert_eq!((image.width, image.height), (2, 1));
        // Neither pixel is pure black/white (the seam bleeds), and the bright one sits far above
        // the sRGB-space midpoint the old box filter would have produced.
        assert!(
            image.data[4] > 200,
            "expected linear-light weighting, got {}",
            image.data[4]
        );
    }

    /// Renders are reproducible: the resampler's weights and accumulation order are fixed, so
    /// the same inputs always produce the same bytes.
    #[test]
    fn renders_are_byte_identical_across_runs() {
        let (source, terrain) = two_tile_source(
            solid_bc1_dds_sized(0x7BEF, 32),
            solid_bc1_dds_sized(0x4A69, 32),
        );
        let options = RenderOptions {
            art_style: ArtStyle::Original,
            max_dimension: Some(16),
            ..Default::default()
        };

        let first = render_terrain(&terrain, Tileset::Jungle, &source, &options).unwrap();
        let second = render_terrain(&terrain, Tileset::Jungle, &source, &options).unwrap();
        assert_eq!(first, second);
    }

    /// At the art's native resolution the fast path skips filtering entirely and emits the
    /// composited art untouched — the same pixels a straight per-tile blit produced.
    #[test]
    fn native_resolution_output_is_the_unfiltered_composite() {
        let (source, terrain) = two_tile_source(
            solid_bc1_dds_sized(0x7BEF, 4),
            solid_bc1_dds_sized(0x4A69, 4),
        );
        let options = RenderOptions {
            art_style: ArtStyle::Original,
            max_dimension: Some(8), // 4 px/tile, exactly the 4px art's native size
            ..Default::default()
        };

        let image = render_terrain(&terrain, Tileset::Jungle, &source, &options).unwrap();
        assert_eq!(image.width, 8);
        for texel in image.data[..4 * 4].chunks_exact(4) {
            assert_eq!(texel, [123, 125, 123, 255]);
        }
        for texel in image.data[4 * 4..8 * 4].chunks_exact(4) {
            assert_eq!(texel, [74, 77, 74, 255]);
        }
    }

    /// A big map at a small output must stay bounded by the *output* plus one strip, never the
    /// native intermediate. Here that intermediate would be 128 tiles x 32px = 4096px square
    /// (64 MiB of RGBA); the render holds a 4096x32 strip (512 KiB), a filter window of a few
    /// resampled rows, and the 128x128 output instead. (The same argument is what makes a 256x256
    /// map at HD's 128px tiles — a 4 GiB intermediate — renderable at all.)
    #[test]
    fn large_map_at_small_output_stays_bounded() {
        let mut cv5 = Vec::new();
        cv5.extend(cv5_entry(0));
        let mut source = MemorySource::new();
        source.insert(AssetRequest::Cv5(Tileset::Jungle), cv5);
        source.insert(
            AssetRequest::TilesetDds(Tileset::Jungle, AssetTier::Sd, ArtPack::Standard),
            dds_vr4_bytes(&[&solid_bc1_dds_sized(0x2965, 32)]),
        );

        let terrain = TerrainTileIds {
            width: 128,
            height: 128,
            tiles: vec![TileId(0); 128 * 128],
        };
        let options = RenderOptions {
            art_style: ArtStyle::Original,
            max_dimension: Some(128), // 1 px/tile: a 32:1 downscale
            ..Default::default()
        };

        let image = render_terrain(&terrain, Tileset::Jungle, &source, &options).unwrap();
        assert_eq!((image.width, image.height), (128, 128));
        assert_eq!(image.data.len(), 128 * 128 * 4);
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
