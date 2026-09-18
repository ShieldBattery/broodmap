//! Hand-rolled BC1 (DXT1) and BC3 (DXT5) block decoders, producing RGBA8.
//!
//! No external dependency: SC:R's `.dds.vr4` megatile frames are BC1/BC3-compressed DDS payloads
//! (see `broodmap-formats`), and decoding them to plain RGBA for CPU compositing is this crate's
//! job (`docs/render-design.md`, "Parse != decode").
//!
//! Permissive per the workspace's no-panic philosophy (`AGENTS.md`): these decoders never panic
//! on malformed/truncated input. If `data` doesn't contain a full block for some position in the
//! image, that block's texels are filled with transparent black rather than erroring.

/// Width/height (in texels) of a single BC block.
const BLOCK_DIM: u32 = 4;

/// Decodes a BC1 (DXT1)-compressed image to RGBA8, row-major.
///
/// `data` is read as a row-major grid of 8-byte blocks (`ceil(width/4) * ceil(height/4)` of
/// them). Blocks past the end of `data` are rendered as transparent black; texels of edge blocks
/// that fall outside `width`x`height` are dropped.
pub(crate) fn decode_bc1(data: &[u8], width: u32, height: u32) -> Vec<u8> {
    decode_blocks(data, width, height, 8, |block| {
        decode_color_block(block, false)
    })
}

/// Decodes a BC3 (DXT5)-compressed image to RGBA8, row-major.
///
/// Block layout mirrors [`decode_bc1`] but with 16-byte blocks: an 8-byte alpha sub-block
/// followed by a BC1-style 8-byte color sub-block (always decoded in 4-color mode, per the
/// BC3/DXT5 spec — see `decode_color_block`).
pub(crate) fn decode_bc3(data: &[u8], width: u32, height: u32) -> Vec<u8> {
    decode_blocks(data, width, height, 16, |block| {
        let alpha = decode_alpha_block(&block[0..8]);
        let mut texels = decode_color_block(&block[8..16], true);
        for (texel, a) in texels.iter_mut().zip(alpha.iter()) {
            texel[3] = *a;
        }
        texels
    })
}

/// Walks the block grid for an image of `width`x`height`, decoding each present block with
/// `decode_block` and blitting it into an RGBA8 output buffer. `block_size` is 8 for BC1, 16 for
/// BC3. Blocks beyond the end of `data` (rather than merely truncated) are filled with
/// transparent black without calling `decode_block` at all.
fn decode_blocks(
    data: &[u8],
    width: u32,
    height: u32,
    block_size: usize,
    mut decode_block: impl FnMut(&[u8]) -> [[u8; 4]; 16],
) -> Vec<u8> {
    if width == 0 || height == 0 {
        return Vec::new();
    }
    let Some(buf_len) = (width as usize)
        .checked_mul(height as usize)
        .and_then(|n| n.checked_mul(4))
    else {
        return Vec::new();
    };
    let mut out = vec![0u8; buf_len];

    let blocks_wide = width.div_ceil(BLOCK_DIM);
    let blocks_high = height.div_ceil(BLOCK_DIM);
    let available_blocks = data.len() / block_size;

    for by in 0..blocks_high {
        for bx in 0..blocks_wide {
            let block_index = (by as usize) * (blocks_wide as usize) + (bx as usize);
            let texels = if block_index < available_blocks {
                let start = block_index * block_size;
                decode_block(&data[start..start + block_size])
            } else {
                [[0u8; 4]; 16]
            };

            for ty in 0..BLOCK_DIM {
                let y = by * BLOCK_DIM + ty;
                if y >= height {
                    continue;
                }
                for tx in 0..BLOCK_DIM {
                    let x = bx * BLOCK_DIM + tx;
                    if x >= width {
                        continue;
                    }
                    let texel = texels[(ty * BLOCK_DIM + tx) as usize];
                    let out_offset = ((y as usize) * (width as usize) + x as usize) * 4;
                    out[out_offset..out_offset + 4].copy_from_slice(&texel);
                }
            }
        }
    }

    out
}

/// Expands an RGB565 value to 8-bit-per-channel RGB.
fn expand_565(value: u16) -> [u8; 3] {
    let r5 = ((value >> 11) & 0x1F) as u8;
    let g6 = ((value >> 5) & 0x3F) as u8;
    let b5 = (value & 0x1F) as u8;
    [
        (r5 << 3) | (r5 >> 2),
        (g6 << 2) | (g6 >> 4),
        (b5 << 3) | (b5 >> 2),
    ]
}

/// Decodes a BC1-style 8-byte color block (`u16 color0, u16 color1, u32 indices`, all LE) into
/// 16 RGBA texels in block-row-major order (texel `n` is at `(n % 4, n / 4)`).
///
/// `force_four_color`: BC3's embedded color block always uses 4-color mode (its own alpha is
/// irrelevant, since alpha comes from the separate alpha sub-block and overwrites it), so the
/// normal `color0 > color1` mode selection is skipped when this is `true`.
///
/// Rounding: the interpolated palette entries use truncating integer division per channel
/// (`(2*c0 + c1) / 3`, `(c0 + 2*c1) / 3` in 4-color mode; `(c0 + c1) / 2` for the third entry in
/// 3-color mode) with no `+1` rounding bias — this matches the literal weighted-average formulas
/// rather than round-to-nearest.
fn decode_color_block(block: &[u8], force_four_color: bool) -> [[u8; 4]; 16] {
    let color0 = u16::from_le_bytes(block[0..2].try_into().unwrap());
    let color1 = u16::from_le_bytes(block[2..4].try_into().unwrap());
    let indices = u32::from_le_bytes(block[4..8].try_into().unwrap());

    let c0 = expand_565(color0);
    let c1 = expand_565(color1);

    let four_color = force_four_color || color0 > color1;

    let mut palette = [[0u8; 4]; 4];
    palette[0] = [c0[0], c0[1], c0[2], 255];
    palette[1] = [c1[0], c1[1], c1[2], 255];
    if four_color {
        palette[2] = [
            ((2 * c0[0] as u32 + c1[0] as u32) / 3) as u8,
            ((2 * c0[1] as u32 + c1[1] as u32) / 3) as u8,
            ((2 * c0[2] as u32 + c1[2] as u32) / 3) as u8,
            255,
        ];
        palette[3] = [
            ((c0[0] as u32 + 2 * c1[0] as u32) / 3) as u8,
            ((c0[1] as u32 + 2 * c1[1] as u32) / 3) as u8,
            ((c0[2] as u32 + 2 * c1[2] as u32) / 3) as u8,
            255,
        ];
    } else {
        palette[2] = [
            ((c0[0] as u32 + c1[0] as u32) / 2) as u8,
            ((c0[1] as u32 + c1[1] as u32) / 2) as u8,
            ((c0[2] as u32 + c1[2] as u32) / 2) as u8,
            255,
        ];
        palette[3] = [0, 0, 0, 0];
    }

    let mut texels = [[0u8; 4]; 16];
    for (n, texel) in texels.iter_mut().enumerate() {
        let idx = (indices >> (n * 2)) & 0x3;
        *texel = palette[idx as usize];
    }
    texels
}

/// Decodes a BC3 8-byte alpha sub-block (`u8 alpha0, u8 alpha1`, then a 48-bit little-endian
/// stream of 3-bit indices, texel `n`'s index at bit `3*n`) into 16 alpha values, in block-row-
/// major order.
fn decode_alpha_block(block: &[u8]) -> [u8; 16] {
    let a0 = block[0];
    let a1 = block[1];

    let mut palette = [0u8; 8];
    palette[0] = a0;
    palette[1] = a1;
    if a0 > a1 {
        palette[2] = ((6 * a0 as u32 + a1 as u32) / 7) as u8;
        palette[3] = ((5 * a0 as u32 + 2 * a1 as u32) / 7) as u8;
        palette[4] = ((4 * a0 as u32 + 3 * a1 as u32) / 7) as u8;
        palette[5] = ((3 * a0 as u32 + 4 * a1 as u32) / 7) as u8;
        palette[6] = ((2 * a0 as u32 + 5 * a1 as u32) / 7) as u8;
        palette[7] = ((a0 as u32 + 6 * a1 as u32) / 7) as u8;
    } else {
        palette[2] = ((4 * a0 as u32 + a1 as u32) / 5) as u8;
        palette[3] = ((3 * a0 as u32 + 2 * a1 as u32) / 5) as u8;
        palette[4] = ((2 * a0 as u32 + 3 * a1 as u32) / 5) as u8;
        palette[5] = ((a0 as u32 + 4 * a1 as u32) / 5) as u8;
        palette[6] = 0;
        palette[7] = 255;
    }

    // 6 bytes = 48-bit little-endian index stream, 3 bits per texel, texel 0 in the LSBs.
    let mut bits: u64 = 0;
    for i in 0..6 {
        bits |= (block[2 + i] as u64) << (8 * i);
    }

    let mut alphas = [0u8; 16];
    for (n, alpha) in alphas.iter_mut().enumerate() {
        let idx = (bits >> (n * 3)) & 0x7;
        *alpha = palette[idx as usize];
    }
    alphas
}

#[cfg(test)]
mod tests {
    use super::*;

    fn bc1_block(color0: u16, color1: u16, indices: u32) -> Vec<u8> {
        let mut data = Vec::with_capacity(8);
        data.extend_from_slice(&color0.to_le_bytes());
        data.extend_from_slice(&color1.to_le_bytes());
        data.extend_from_slice(&indices.to_le_bytes());
        data
    }

    /// Packs 16 2-bit index values (texel n at bit 2n) the way a real BC1 block would.
    fn pack_indices_2bit(indices: [u32; 16]) -> u32 {
        let mut packed = 0u32;
        for (n, idx) in indices.iter().enumerate() {
            packed |= (idx & 0x3) << (n * 2);
        }
        packed
    }

    /// Packs 16 3-bit index values (texel n at bit 3n) into a BC3 alpha block's 6 index bytes.
    fn pack_indices_3bit(indices: [u32; 16]) -> [u8; 6] {
        let mut bits: u64 = 0;
        for (n, idx) in indices.iter().enumerate() {
            bits |= ((*idx & 0x7) as u64) << (n * 3);
        }
        let mut bytes = [0u8; 6];
        for (i, byte) in bytes.iter_mut().enumerate() {
            *byte = ((bits >> (8 * i)) & 0xFF) as u8;
        }
        bytes
    }

    #[test]
    fn bc1_solid_color_block() {
        // color0 == color1 == white; every index selects palette[0].
        let data = bc1_block(0xFFFF, 0xFFFF, 0);
        let rgba = decode_bc1(&data, 4, 4);
        assert_eq!(rgba.len(), 4 * 4 * 4);
        for texel in rgba.as_chunks::<4>().0 {
            assert_eq!(*texel, [255, 255, 255, 255]);
        }
    }

    #[test]
    fn bc1_four_color_block_maps_each_index() {
        // color0 (red, 0xF800) > color1 (blue, 0x001F) as raw u16 => 4-color opaque mode.
        let color0 = 0xF800u16;
        let color1 = 0x001Fu16;
        assert!(color0 > color1);

        let c0 = expand_565(color0);
        let c1 = expand_565(color1);
        assert_eq!(c0, [255, 0, 0]);
        assert_eq!(c1, [0, 0, 255]);

        let expected = [
            [c0[0], c0[1], c0[2], 255],
            [c1[0], c1[1], c1[2], 255],
            [
                ((2 * c0[0] as u32 + c1[0] as u32) / 3) as u8,
                ((2 * c0[1] as u32 + c1[1] as u32) / 3) as u8,
                ((2 * c0[2] as u32 + c1[2] as u32) / 3) as u8,
                255,
            ],
            [
                ((c0[0] as u32 + 2 * c1[0] as u32) / 3) as u8,
                ((c0[1] as u32 + 2 * c1[1] as u32) / 3) as u8,
                ((c0[2] as u32 + 2 * c1[2] as u32) / 3) as u8,
                255,
            ],
        ];

        let mut indices = [0u32; 16];
        for (n, idx) in indices.iter_mut().enumerate() {
            *idx = (n % 4) as u32;
        }
        let data = bc1_block(color0, color1, pack_indices_2bit(indices));
        let rgba = decode_bc1(&data, 4, 4);

        for (n, texel) in rgba.as_chunks::<4>().0.iter().enumerate() {
            assert_eq!(*texel, expected[n % 4], "texel {n}");
        }
    }

    #[test]
    fn bc1_transparent_mode_block() {
        // color0 (blue, 0x001F) < color1 (red, 0xF800) as raw u16 => 3-color + transparent mode.
        let color0 = 0x001Fu16;
        let color1 = 0xF800u16;
        assert!(color0 < color1);

        let c0 = expand_565(color0);
        let c1 = expand_565(color1);

        let expected = [
            [c0[0], c0[1], c0[2], 255],
            [c1[0], c1[1], c1[2], 255],
            [
                ((c0[0] as u32 + c1[0] as u32) / 2) as u8,
                ((c0[1] as u32 + c1[1] as u32) / 2) as u8,
                ((c0[2] as u32 + c1[2] as u32) / 2) as u8,
                255,
            ],
            [0, 0, 0, 0],
        ];

        let mut indices = [0u32; 16];
        for (n, idx) in indices.iter_mut().enumerate() {
            *idx = (n % 4) as u32;
        }
        let data = bc1_block(color0, color1, pack_indices_2bit(indices));
        let rgba = decode_bc1(&data, 4, 4);

        for (n, texel) in rgba.as_chunks::<4>().0.iter().enumerate() {
            assert_eq!(*texel, expected[n % 4], "texel {n}");
        }
    }

    #[test]
    fn bc3_alpha_interpolated_mode() {
        // alpha0 (255) > alpha1 (0) => 8-value interpolated palette.
        let a0 = 255u8;
        let a1 = 0u8;
        let expected_alpha = [
            a0,
            a1,
            ((6 * a0 as u32 + a1 as u32) / 7) as u8,
            ((5 * a0 as u32 + 2 * a1 as u32) / 7) as u8,
            ((4 * a0 as u32 + 3 * a1 as u32) / 7) as u8,
            ((3 * a0 as u32 + 4 * a1 as u32) / 7) as u8,
            ((2 * a0 as u32 + 5 * a1 as u32) / 7) as u8,
            ((a0 as u32 + 6 * a1 as u32) / 7) as u8,
        ];

        let mut indices = [0u32; 16];
        for (n, idx) in indices.iter_mut().enumerate() {
            *idx = (n % 8) as u32;
        }

        let mut data = Vec::with_capacity(16);
        data.push(a0);
        data.push(a1);
        data.extend_from_slice(&pack_indices_3bit(indices));
        // Solid white color block so only alpha varies.
        data.extend_from_slice(&bc1_block(0xFFFF, 0xFFFF, 0));
        assert_eq!(data.len(), 16);

        let rgba = decode_bc3(&data, 4, 4);
        for (n, texel) in rgba.as_chunks::<4>().0.iter().enumerate() {
            assert_eq!(texel[0..3], [255, 255, 255]);
            assert_eq!(texel[3], expected_alpha[n % 8], "texel {n}");
        }
    }

    #[test]
    fn bc3_alpha_linear_mode() {
        // alpha0 (0) <= alpha1 (255) => linear 4-value palette plus explicit 0/255 endpoints.
        let a0 = 0u8;
        let a1 = 255u8;
        let expected_alpha = [
            a0,
            a1,
            ((4 * a0 as u32 + a1 as u32) / 5) as u8,
            ((3 * a0 as u32 + 2 * a1 as u32) / 5) as u8,
            ((2 * a0 as u32 + 3 * a1 as u32) / 5) as u8,
            ((a0 as u32 + 4 * a1 as u32) / 5) as u8,
            0,
            255,
        ];

        let mut indices = [0u32; 16];
        for (n, idx) in indices.iter_mut().enumerate() {
            *idx = (n % 8) as u32;
        }

        let mut data = Vec::with_capacity(16);
        data.push(a0);
        data.push(a1);
        data.extend_from_slice(&pack_indices_3bit(indices));
        data.extend_from_slice(&bc1_block(0xFFFF, 0xFFFF, 0));
        assert_eq!(data.len(), 16);

        let rgba = decode_bc3(&data, 4, 4);
        for (n, texel) in rgba.as_chunks::<4>().0.iter().enumerate() {
            assert_eq!(texel[3], expected_alpha[n % 8], "texel {n}");
        }
    }

    #[test]
    fn short_data_does_not_panic_and_fills_transparent() {
        assert_eq!(decode_bc1(&[], 4, 4), vec![0u8; 4 * 4 * 4]);
        assert_eq!(decode_bc3(&[], 4, 4), vec![0u8; 4 * 4 * 4]);

        // Partial trailing block (fewer bytes than one full block) on a multi-block image: the
        // first block decodes normally, the incomplete second is transparent.
        let mut data = bc1_block(0xFFFF, 0xFFFF, 0);
        data.extend_from_slice(&[0xAB; 3]); // not a full second block
        let rgba = decode_bc1(&data, 8, 4);
        // First 4x4 block (left half of each row) is opaque white.
        for y in 0..4 {
            for x in 0..4 {
                let offset = (y * 8 + x) * 4;
                assert_eq!(&rgba[offset..offset + 4], [255, 255, 255, 255]);
            }
        }
        // Second 4x4 block (right half) is transparent black.
        for y in 0..4 {
            for x in 4..8 {
                let offset = (y * 8 + x) * 4;
                assert_eq!(&rgba[offset..offset + 4], [0, 0, 0, 0]);
            }
        }
    }

    #[test]
    fn zero_dimensions_do_not_panic() {
        assert!(decode_bc1(&[0u8; 8], 0, 0).is_empty());
        assert!(decode_bc3(&[0u8; 16], 0, 4).is_empty());
    }

    #[test]
    fn edge_block_drops_out_of_bounds_texels() {
        // 5x5 image needs a 2x2 block grid (blocks cover 8x8); only a 5x5 region should be
        // written out, with the extra texels of edge blocks dropped rather than indexed OOB.
        let data = bc1_block(0xFFFF, 0xFFFF, 0);
        let mut full_data = Vec::new();
        for _ in 0..4 {
            full_data.extend_from_slice(&data);
        }
        let rgba = decode_bc1(&full_data, 5, 5);
        assert_eq!(rgba.len(), 5 * 5 * 4);
    }
}
