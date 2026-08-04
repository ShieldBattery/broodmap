//! Linear-light, whole-image resampling of the composited terrain layer, driven a strip at a
//! time so the full-resolution intermediate never exists.
//!
//! # Why this exists
//!
//! The terrain layer used to downscale each megatile independently (decode a 64px tile, box
//! filter it to `ppt`, blit it everywhere that megatile appears). That is fast and bounded, but
//! it is a poor downscale in two ways that show plainly on real maps:
//!
//! 1. **A box filter aliases.** Its stopband leakage is severe, so detail above the output's
//!    Nyquist limit folds back in as speckle. On Lost Temple this reads as a broken, stippled
//!    shoreline and a crawling moire across open water — worst at non-integer ratios (a 64px
//!    tile into 7 output pixels means some output pixels average 9 source rows and some 10,
//!    which beats against the art's own texture), but visible at aligned ratios too.
//! 2. **Per-tile windows never cross a tile boundary**, so the filter can't see the neighbouring
//!    tile's pixels that legitimately belong in an output pixel straddling the seam. Combined
//!    with (1), edges that run diagonally across tiles come out broken at every tile edge.
//!
//! Averaging in sRGB space compounds both: sRGB is a perceptual encoding, so its arithmetic mean
//! is not a physically meaningful average of light and darkens every mixed-brightness pixel.
//!
//! This module replaces all of it. Tiles are composited at their *native* resolution into a
//! rolling window of horizontal strips, and the native image is resampled to the output size with
//! a separable Catmull-Rom kernel (see [`catmull_rom`] for the measurements behind that choice)
//! evaluated in linear light. Against a linear-light Lanczos-3 downscale of the native render,
//! this lands ~2.5x closer than the old per-tile box path at every size tested.
//!
//! Note what this deliberately does *not* claim to fix: Brood War terrain draws from a small set
//! of megatile variants, and the SC:R HD art's variants differ from one another less than the
//! original art's did, so open ground still looks repetitive at preview scale. That repetition
//! is in the art, not the filter — see `docs/render-design.md`, "Downscaling quality".
//!
//! # Memory
//!
//! Peak memory is O(window + output), never O(native image): a 256x256 map at HD's native 128
//! px/tile would be a 32Ki x 32Ki intermediate (4 GiB) and must never materialize. Concretely
//! this holds one composited tile-row strip (`map_w * tile_px` wide by `tile_px` tall, at most
//! 16 MiB) plus a ring of horizontally-resampled `f32` output rows. The ring is `~4 * ratio`
//! rows of `out_w` pixels, and `out_w = map_w * ppt` while `ratio = tile_px / ppt`, so the `ppt`
//! cancels: the ring is bounded by `~64 * tile_px * map_w` bytes (about 2 MiB at the worst legal
//! map size), regardless of the requested output resolution.

use std::sync::OnceLock;

/// A source of full-width native-resolution RGBA8 rows.
///
/// [`resample_linear`] pulls rows in strictly increasing order and never revisits one, which is
/// what lets the implementation (see `crate::terrain::StripCompositor`) keep only the tile-row
/// strip currently being read.
pub(crate) trait NativeRows {
    /// Returns native row `y` as exactly `native_w * 4` RGBA8 bytes. A short row is tolerated
    /// (the missing texels simply aren't sampled) but never expected.
    fn row(&mut self, y: u32) -> &[u8];
}

/// Size of the linear -> sRGB reverse lookup table. Fine enough that every 8-bit code round-trips
/// exactly: the tightest 8-bit spacing in linear light is at black, `(1/12.92)/255 ≈ 3.03e-4`,
/// which is ~2.5 table steps wide.
const ENCODE_TABLE_LEN: usize = 8192;

/// sRGB <-> linear-light conversion tables. Built once, shared, and read-only afterwards.
struct ColorTables {
    /// 8-bit sRGB code -> linear light in `[0, 1]`.
    decode: [f32; 256],
    /// Quantized linear light -> 8-bit sRGB code.
    encode: [u8; ENCODE_TABLE_LEN],
}

fn tables() -> &'static ColorTables {
    static TABLES: OnceLock<Box<ColorTables>> = OnceLock::new();
    TABLES.get_or_init(|| {
        let mut decode = [0f32; 256];
        for (code, slot) in decode.iter_mut().enumerate() {
            *slot = srgb_to_linear(code as f32 / 255.0);
        }

        let mut encode = [0u8; ENCODE_TABLE_LEN];
        for (i, slot) in encode.iter_mut().enumerate() {
            let linear = i as f32 / (ENCODE_TABLE_LEN - 1) as f32;
            *slot = (linear_to_srgb(linear) * 255.0).round().clamp(0.0, 255.0) as u8;
        }

        Box::new(ColorTables { decode, encode })
    })
}

/// The sRGB electro-optical transfer function (IEC 61966-2-1): encoded value -> linear light.
fn srgb_to_linear(c: f32) -> f32 {
    if c <= 0.040_45 {
        c / 12.92
    } else {
        ((c + 0.055) / 1.055).powf(2.4)
    }
}

/// The inverse of [`srgb_to_linear`]: linear light -> encoded value.
fn linear_to_srgb(c: f32) -> f32 {
    if c <= 0.003_130_8 {
        c * 12.92
    } else {
        1.055 * c.powf(1.0 / 2.4) - 0.055
    }
}

/// Encodes a linear-light value as an 8-bit sRGB code, clamping the kernel's negative-lobe
/// overshoot back into range.
#[inline]
fn encode_linear(tables: &ColorTables, value: f32) -> u8 {
    let quantized = value.clamp(0.0, 1.0) * (ENCODE_TABLE_LEN - 1) as f32 + 0.5;
    tables.encode[quantized as usize]
}

/// The Catmull-Rom member of the Mitchell-Netravali cubic family (B = 0, C = 1/2), evaluated in
/// units of the destination grid. Support is `|x| < 2`; the negative lobes in `1 <= |x| < 2` are
/// what let it hold an edge instead of smearing it.
///
/// The kernel choice was measured, not assumed. Against a linear-light Lanczos-3 downscale of the
/// native 8192px render of Lost Temple (the standard reference for "what this image should look
/// like"), RMSE at `--size 900`/`1024`/`2048` came out:
///
/// | kernel                   | 900     | 1024    | 2048    |
/// |--------------------------|---------|---------|---------|
/// | per-tile box (the old path) | 0.00963 | 0.00916 | 0.00776 |
/// | Mitchell (B = C = 1/3)   | 0.00665 | 0.00679 | 0.00691 |
/// | Catmull-Rom (B = 0, C = 1/2) | 0.00363 | 0.00370 | 0.00361 |
///
/// Mitchell is the usual default for downscaling, but its wide main lobe visibly softens terrain
/// at these ratios; Catmull-Rom is roughly 45% closer to the reference and, on this art, shows no
/// objectionable ringing (the highest-contrast content is cliff-against-water, and the overshoot
/// there stays under the clamp).
pub(crate) fn catmull_rom(x: f32) -> f32 {
    const B: f32 = 0.0;
    const C: f32 = 0.5;

    let x = x.abs();
    let x2 = x * x;
    let x3 = x2 * x;

    if x < 1.0 {
        ((12.0 - 9.0 * B - 6.0 * C) * x3 + (-18.0 + 12.0 * B + 6.0 * C) * x2 + (6.0 - 2.0 * B))
            / 6.0
    } else if x < 2.0 {
        ((-B - 6.0 * C) * x3
            + (6.0 * B + 30.0 * C) * x2
            + (-12.0 * B - 48.0 * C) * x
            + (8.0 * B + 24.0 * C))
            / 6.0
    } else {
        0.0
    }
}

/// Precomputed filter taps for one axis: for each destination sample, the first source index it
/// reads and its (normalized) weights.
struct Axis {
    /// First source index read by destination sample `i`.
    starts: Vec<u32>,
    /// `weights[offsets[i]..offsets[i + 1]]` are destination sample `i`'s weights.
    offsets: Vec<u32>,
    weights: Vec<f32>,
    /// The widest tap count over all destination samples (the ring buffer depth needed).
    max_taps: usize,
}

impl Axis {
    /// Builds the tap table mapping `src_len` source samples onto `dst_len` destination samples.
    ///
    /// Destination sample centers map to source coordinates by the usual half-pixel convention
    /// (`(i + 0.5) * scale - 0.5`), and the kernel is stretched by `scale` when downscaling so it
    /// acts as a low-pass filter at the *destination* rate. Taps that fall outside the source are
    /// dropped and the surviving weights renormalized, which keeps the filter a partition of
    /// unity everywhere including the edges (no dark or bright rim).
    fn build(src_len: u32, dst_len: u32) -> Axis {
        let mut starts = Vec::with_capacity(dst_len as usize);
        let mut offsets = Vec::with_capacity(dst_len as usize + 1);
        let mut weights = Vec::new();
        let mut max_taps = 0usize;

        offsets.push(0);
        if src_len == 0 || dst_len == 0 {
            return Axis {
                starts,
                offsets,
                weights,
                max_taps,
            };
        }

        let scale = src_len as f32 / dst_len as f32;
        // Upscaling would shrink the kernel below its natural width, which just aliases; clamp so
        // the filter is never narrower than the source grid. (The renderer only ever downscales.)
        let support = scale.max(1.0);
        let radius = 2.0 * support;

        for i in 0..dst_len {
            let center = (i as f32 + 0.5) * scale - 0.5;
            let lo = ((center - radius).ceil() as i64).max(0);
            let hi = ((center + radius).floor() as i64).min(src_len as i64 - 1);
            // A degenerate window can't happen for the ratios the renderer uses, but clamp it to
            // a single nearest tap rather than emitting an empty (divide-by-zero) window.
            let (lo, hi) = if lo > hi {
                let nearest = (center.round() as i64).clamp(0, src_len as i64 - 1);
                (nearest, nearest)
            } else {
                (lo, hi)
            };

            starts.push(lo as u32);
            let first = weights.len();
            let mut total = 0.0f32;
            for s in lo..=hi {
                let w = catmull_rom((s as f32 - center) / support);
                weights.push(w);
                total += w;
            }
            if total != 0.0 {
                let inv = 1.0 / total;
                for w in &mut weights[first..] {
                    *w *= inv;
                }
            }

            max_taps = max_taps.max(weights.len() - first);
            offsets.push(weights.len() as u32);
        }

        Axis {
            starts,
            offsets,
            weights,
            max_taps,
        }
    }

    #[inline]
    fn taps(&self, i: usize) -> (u32, &[f32]) {
        let from = self.offsets[i] as usize;
        let to = self.offsets[i + 1] as usize;
        (self.starts[i], &self.weights[from..to])
    }
}

/// Resamples a native-resolution RGBA8 image (pulled one row at a time from `rows`) down to
/// `out_w` x `out_h`, filtering in linear light with a separable Catmull-Rom kernel.
///
/// Alpha is filtered as-is (linear, unencoded). Terrain is fully opaque, so there is no
/// premultiplication question to answer here; the sprite overlay keeps its own premultiplied
/// per-frame path.
pub(crate) fn resample_linear(
    rows: &mut dyn NativeRows,
    native_w: u32,
    native_h: u32,
    out_w: u32,
    out_h: u32,
) -> Vec<u8> {
    let stride = out_w as usize * 4;
    let mut out = vec![0u8; stride * out_h as usize];
    if out.is_empty() || native_w == 0 || native_h == 0 {
        return out;
    }

    let tables = tables();
    let h_axis = Axis::build(native_w, out_w);
    let v_axis = Axis::build(native_h, out_h);

    // Ring of horizontally-resampled rows, in linear light. Depth is exactly the widest vertical
    // window, which is what bounds this to a window rather than the whole native image.
    let window = v_axis.max_taps.max(1);
    let mut ring = vec![0f32; window * stride];
    let mut accum = vec![0f32; stride];
    let mut produced: u32 = 0;

    for oy in 0..out_h {
        let (v_start, v_weights) = v_axis.taps(oy as usize);
        let end = v_start + v_weights.len() as u32;

        while produced < end {
            let src = rows.row(produced);
            let slot = (produced % window as u32) as usize * stride;
            resample_row_horizontal(src, &h_axis, out_w, tables, &mut ring[slot..slot + stride]);
            produced += 1;
        }

        accum.fill(0.0);
        for (k, &weight) in v_weights.iter().enumerate() {
            let slot = ((v_start + k as u32) % window as u32) as usize * stride;
            for (dst, &src) in accum.iter_mut().zip(&ring[slot..slot + stride]) {
                *dst += src * weight;
            }
        }

        let out_row = &mut out[oy as usize * stride..(oy as usize + 1) * stride];
        for (texel, linear) in out_row.chunks_exact_mut(4).zip(accum.chunks_exact(4)) {
            texel[0] = encode_linear(tables, linear[0]);
            texel[1] = encode_linear(tables, linear[1]);
            texel[2] = encode_linear(tables, linear[2]);
            texel[3] = (linear[3].clamp(0.0, 1.0) * 255.0 + 0.5) as u8;
        }
    }

    out
}

/// Horizontally resamples one native RGBA8 row into `dst` as linear-light `f32` RGBA.
fn resample_row_horizontal(
    src: &[u8],
    axis: &Axis,
    out_w: u32,
    tables: &ColorTables,
    dst: &mut [f32],
) {
    for ox in 0..out_w as usize {
        let (start, weights) = axis.taps(ox);
        let mut acc = [0.0f32; 4];
        for (k, &weight) in weights.iter().enumerate() {
            let offset = (start as usize + k) * 4;
            // Defensive: the compositor always hands over a full row, but a short one must skip
            // texels rather than panic (this crate's inputs are untrusted asset bytes).
            let Some(texel) = src.get(offset..offset + 4) else {
                continue;
            };
            acc[0] += tables.decode[texel[0] as usize] * weight;
            acc[1] += tables.decode[texel[1] as usize] * weight;
            acc[2] += tables.decode[texel[2] as usize] * weight;
            acc[3] += (texel[3] as f32 / 255.0) * weight;
        }
        dst[ox * 4..ox * 4 + 4].copy_from_slice(&acc);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A [`NativeRows`] over a plain in-memory image, for tests.
    struct Rows {
        data: Vec<u8>,
        width: u32,
    }

    impl NativeRows for Rows {
        fn row(&mut self, y: u32) -> &[u8] {
            let stride = self.width as usize * 4;
            &self.data[y as usize * stride..(y as usize + 1) * stride]
        }
    }

    #[test]
    fn kernel_is_symmetric_and_compactly_supported() {
        for i in 0..200 {
            let x = i as f32 / 50.0;
            assert_eq!(
                catmull_rom(x),
                catmull_rom(-x),
                "kernel must be even at {x}"
            );
        }
        assert_eq!(catmull_rom(2.0), 0.0);
        assert_eq!(catmull_rom(2.5), 0.0);
        assert_eq!(catmull_rom(-3.0), 0.0);
        // The negative lobes are the sharpness a box filter lacks.
        assert!(
            catmull_rom(1.5) < 0.0,
            "expected a negative lobe at |x| = 1.5"
        );
    }

    /// The kernel reconstructs a constant exactly: sampled on any unit-spaced lattice, its taps
    /// sum to 1 for every sub-pixel phase.
    #[test]
    fn kernel_is_a_partition_of_unity() {
        for phase_step in 0..64 {
            let phase = phase_step as f32 / 64.0;
            let sum: f32 = (-3..=3).map(|i| catmull_rom(i as f32 - phase)).sum();
            assert!(
                (sum - 1.0).abs() < 1e-5,
                "taps at phase {phase} summed to {sum}"
            );
        }
    }

    /// Every built tap window is normalized, including the clipped ones at the edges — otherwise
    /// downscales would rim the image with a dark or bright border.
    #[test]
    fn axis_weights_sum_to_one_everywhere() {
        for (src, dst) in [(64u32, 8u32), (128, 3), (100, 7), (8, 8), (5, 4), (1, 1)] {
            let axis = Axis::build(src, dst);
            for i in 0..dst as usize {
                let (_, weights) = axis.taps(i);
                let sum: f32 = weights.iter().sum();
                assert!(
                    (sum - 1.0).abs() < 1e-4,
                    "{src}->{dst} sample {i} weights summed to {sum}"
                );
                assert!(!weights.is_empty());
            }
        }
    }

    #[test]
    fn axis_windows_never_exceed_the_ring_depth() {
        for (src, dst) in [(64u32, 8u32), (128, 5), (37, 9)] {
            let axis = Axis::build(src, dst);
            for i in 0..dst as usize {
                let (_, weights) = axis.taps(i);
                assert!(weights.len() <= axis.max_taps);
            }
        }
    }

    /// Every 8-bit code must survive sRGB -> linear -> sRGB through the tables, or the identity
    /// case would drift.
    #[test]
    fn srgb_tables_round_trip_within_one_lsb() {
        let tables = tables();
        for code in 0..=255u8 {
            let linear = tables.decode[code as usize];
            let back = encode_linear(tables, linear);
            assert!(
                back.abs_diff(code) <= 1,
                "code {code} round-tripped to {back}"
            );
        }
        // ...and in practice, exactly.
        for code in 0..=255u8 {
            assert_eq!(encode_linear(tables, tables.decode[code as usize]), code);
        }
    }

    #[test]
    fn resampling_a_flat_image_is_exact() {
        let width = 64;
        let height = 64;
        let mut rows = Rows {
            data: vec![0u8; width * height * 4],
            width: width as u32,
        };
        for texel in rows.data.chunks_exact_mut(4) {
            texel.copy_from_slice(&[137, 42, 200, 255]);
        }

        let out = resample_linear(&mut rows, width as u32, height as u32, 8, 8);
        for texel in out.chunks_exact(4) {
            assert_eq!(texel, [137, 42, 200, 255], "a flat field must stay flat");
        }
    }

    /// The point of the whole exercise: a filter window at a tile boundary must straddle it, so
    /// the seam between two differently-colored tiles resolves to intermediate values instead of
    /// two byte-identical thumbnails butted together.
    #[test]
    fn tile_boundary_blends_instead_of_butting() {
        // Two 32px "tiles" side by side: black then white, downscaled 4:1 to 8x1 output pixels.
        let width = 64usize;
        let mut data = vec![255u8; width * 4];
        for (x, texel) in data.chunks_exact_mut(4).enumerate() {
            let value = if x < 32 { 0 } else { 255 };
            texel.copy_from_slice(&[value, value, value, 255]);
        }
        let mut rows = Rows {
            data,
            width: width as u32,
        };

        let out = resample_linear(&mut rows, width as u32, 1, 8, 1);
        let luma: Vec<u8> = out.chunks_exact(4).map(|t| t[0]).collect();

        assert_eq!(luma[0], 0, "far from the seam the tiles keep their color");
        assert_eq!(luma[7], 255);
        // The two output pixels either side of the seam must have moved toward each other.
        assert!(
            luma[3] > 0 && luma[4] < 255,
            "expected a blended seam, got {luma:?}"
        );
    }

    #[test]
    fn resampling_is_deterministic() {
        let width = 48usize;
        let height = 48usize;
        let mut data = vec![0u8; width * height * 4];
        for (i, texel) in data.chunks_exact_mut(4).enumerate() {
            texel.copy_from_slice(&[(i % 251) as u8, (i % 199) as u8, (i % 173) as u8, 255]);
        }

        let mut a = Rows {
            data: data.clone(),
            width: width as u32,
        };
        let mut b = Rows {
            data,
            width: width as u32,
        };
        assert_eq!(
            resample_linear(&mut a, width as u32, height as u32, 7, 7),
            resample_linear(&mut b, width as u32, height as u32, 7, 7)
        );
    }

    /// Linear-light filtering is the whole reason a 50/50 black-and-white mix comes out at ~188
    /// rather than sRGB averaging's 127: half the *light* is much brighter than half the *code*.
    #[test]
    fn filtering_happens_in_linear_light() {
        let mut rows = Rows {
            data: vec![0, 0, 0, 255, 255, 255, 255, 255],
            width: 2,
        };
        let out = resample_linear(&mut rows, 2, 1, 1, 1);
        assert!(
            (185..=190).contains(&out[0]),
            "expected the linear-light mid-gray (~188), got {}",
            out[0]
        );
    }

    #[test]
    fn alpha_passes_through_unfiltered_by_the_color_transfer() {
        // Alpha is linear coverage, not a color: it must not go through the sRGB tables.
        let mut rows = Rows {
            data: vec![255, 255, 255, 0, 255, 255, 255, 255],
            width: 2,
        };
        let out = resample_linear(&mut rows, 2, 1, 1, 1);
        assert!((126..=129).contains(&out[3]), "got alpha {}", out[3]);
    }

    #[test]
    fn degenerate_sizes_do_not_panic() {
        let mut rows = Rows {
            data: vec![1, 2, 3, 4],
            width: 1,
        };
        assert!(resample_linear(&mut rows, 1, 1, 0, 4).is_empty());
        assert!(
            resample_linear(&mut rows, 0, 0, 4, 4)
                .iter()
                .all(|&b| b == 0)
        );
    }
}
