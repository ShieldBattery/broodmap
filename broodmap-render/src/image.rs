//! RGBA image buffer, box-downscaling, and (optional) PNG encoding.
//!
//! Downscaling at draw time (rather than decoding a full-resolution intermediate and scaling the
//! whole composited image) is what keeps memory proportional to the *output* buffer instead of
//! the source assets — see `docs/render-design.md`, "Asset formats and tiers". This is the sprite
//! overlay's path: `.anim` frames are decoded and scaled one at a time, so a box filter over a
//! frame is the whole story. The terrain layer needs a filter whose windows cross tile boundaries
//! and so goes through `crate::resample` instead; see that module for why.

#[cfg(feature = "png")]
use thiserror::Error;

/// An RGBA8, row-major image buffer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RgbaImage {
    pub width: u32,
    pub height: u32,
    pub data: Vec<u8>,
}

/// Errors from [`RgbaImage::encode_png`].
#[cfg(feature = "png")]
#[derive(Error, Debug)]
pub enum PngEncodeError {
    #[error("PNG encoding failed: {0}")]
    Encode(String),
}

#[cfg(feature = "png")]
impl RgbaImage {
    /// Encodes this image as a PNG byte stream.
    pub fn encode_png(&self) -> Result<Vec<u8>, PngEncodeError> {
        let mut buf = Vec::new();
        {
            let mut encoder = png::Encoder::new(&mut buf, self.width, self.height);
            encoder.set_color(png::ColorType::Rgba);
            encoder.set_depth(png::BitDepth::Eight);
            let mut writer = encoder
                .write_header()
                .map_err(|e| PngEncodeError::Encode(e.to_string()))?;
            writer
                .write_image_data(&self.data)
                .map_err(|e| PngEncodeError::Encode(e.to_string()))?;
        }
        Ok(buf)
    }
}

/// Box-downscales an RGBA8 image from `src_w`x`src_h` to `dst_w`x`dst_h`.
///
/// Only downscaling (`dst <= src` in each dimension) is a supported/exact operation; that's the
/// only direction the renderer ever needs (draw-time scaling from a decoded asset down to the
/// requested tile size). Returns a plain copy when `dst == src`. Each destination pixel is the
/// integer average (per channel, `u32` accumulators) of the source texels in
/// `[x*src_w/dst_w, (x+1)*src_w/dst_w) x [y*src_h/dst_h, (y+1)*src_h/dst_h)`; these ranges are
/// never empty when `dst <= src`. If `dst > src` in some dimension (upscaling — unsupported),
/// the range is defensively widened by one texel so it can't be empty, rather than panicking.
///
/// Channels are averaged independently (straight alpha), which is exact for the fully opaque
/// terrain megatiles this scales. Translucent art (`.anim` sprite frames) must use
/// [`scale_rgba_premultiplied`] instead, or transparent texels' RGB drags the average toward
/// black and leaves dark fringes on every sprite edge.
///
/// # Accumulator bound
///
/// `sums` is a per-channel `u32` accumulator over every source texel in one destination pixel's
/// window; the worst case is a single destination pixel (whole image collapsed to 1x1) at max
/// channel value 255. This module's only caller (`crate::terrain`) decodes megatile frames at
/// `MAX_DECODE_DIM` (1024) at most, so the largest possible window is 1024*1024 texels * 255 =
/// ~267M, comfortably inside `u32::MAX` (~4.29B) — a 16x margin. If a future caller feeds this a
/// larger source, that margin shrinks; widen to `u64` (as `scale_rgba_premultiplied` does, whose
/// per-texel product is 255x larger and did overflow at realistic sizes) rather than assuming it
/// still holds.
pub(crate) fn scale_rgba(src: &[u8], src_w: u32, src_h: u32, dst_w: u32, dst_h: u32) -> Vec<u8> {
    if dst_w == src_w && dst_h == src_h {
        return src.to_vec();
    }
    if dst_w == 0 || dst_h == 0 || src_w == 0 || src_h == 0 {
        return Vec::new();
    }

    let mut out = vec![0u8; dst_w as usize * dst_h as usize * 4];

    for dy in 0..dst_h {
        let y0 = (dy * src_h) / dst_h;
        let y1 = (((dy + 1) * src_h) / dst_h).max(y0 + 1).min(src_h);
        for dx in 0..dst_w {
            let x0 = (dx * src_w) / dst_w;
            let x1 = (((dx + 1) * src_w) / dst_w).max(x0 + 1).min(src_w);

            let mut sums = [0u32; 4];
            let mut count = 0u32;
            for sy in y0..y1 {
                for sx in x0..x1 {
                    let offset = (sy as usize * src_w as usize + sx as usize) * 4;
                    if let Some(texel) = src.get(offset..offset + 4) {
                        for (sum, &channel) in sums.iter_mut().zip(texel) {
                            *sum += channel as u32;
                        }
                        count += 1;
                    }
                }
            }

            let out_offset = (dy as usize * dst_w as usize + dx as usize) * 4;
            for c in 0..4 {
                if let Some(avg) = sums[c].checked_div(count) {
                    out[out_offset + c] = avg as u8;
                }
            }
        }
    }

    out
}

/// Box-downscales an RGBA8 image the way translucent art has to be filtered: each source texel's
/// RGB is weighted by its own alpha (premultiplied), the premultiplied values are averaged, and
/// the result is divided back out by the averaged alpha.
///
/// This is the difference between an opaque red pixel next to a transparent one downscaling to
/// "red at half alpha" (correct) and "dark red at half alpha" (what straight-alpha averaging
/// gives — the transparent texel's RGB, usually black, is meaningless but still counted).
/// `.anim` frames are full of transparent black padding, so this matters everywhere.
///
/// Same sampling rules and downscale-only contract as [`scale_rgba`].
///
/// # Accumulator bound
///
/// `rgb`'s per-texel contribution is `channel * alpha`, up to `255 * 255 == 65025` — 255x a
/// straight channel sum — so a `u32` accumulator overflows at a box of only ~66K texels (a
/// ~257x257 crop, or anything bigger, downscaled to a single destination pixel); a 1024x1024
/// `.anim` frame collapsed to one output texel (a legitimate worst case at extreme zoom-out) is
/// over 1M texels and silently wrapped (or panicked, in debug builds) under that accumulator.
/// `rgb`/`alpha`/`count` are all `u64` here so no source size this crate ever decodes can
/// overflow them.
pub(crate) fn scale_rgba_premultiplied(
    src: &[u8],
    src_w: u32,
    src_h: u32,
    dst_w: u32,
    dst_h: u32,
) -> Vec<u8> {
    if dst_w == src_w && dst_h == src_h {
        return src.to_vec();
    }
    if dst_w == 0 || dst_h == 0 || src_w == 0 || src_h == 0 {
        return Vec::new();
    }

    let mut out = vec![0u8; dst_w as usize * dst_h as usize * 4];

    for dy in 0..dst_h {
        let y0 = (dy * src_h) / dst_h;
        let y1 = (((dy + 1) * src_h) / dst_h).max(y0 + 1).min(src_h);
        for dx in 0..dst_w {
            let x0 = (dx * src_w) / dst_w;
            let x1 = (((dx + 1) * src_w) / dst_w).max(x0 + 1).min(src_w);

            let mut rgb = [0u64; 3];
            let mut alpha = 0u64;
            let mut count = 0u64;
            for sy in y0..y1 {
                for sx in x0..x1 {
                    let offset = (sy as usize * src_w as usize + sx as usize) * 4;
                    if let Some(texel) = src.get(offset..offset + 4) {
                        let a = texel[3] as u64;
                        for (sum, &channel) in rgb.iter_mut().zip(&texel[..3]) {
                            *sum += channel as u64 * a;
                        }
                        alpha += a;
                        count += 1;
                    }
                }
            }

            let out_offset = (dy as usize * dst_w as usize + dx as usize) * 4;
            if count == 0 {
                continue;
            }
            // Unpremultiply against the *summed* alpha rather than the averaged one: it's the
            // same ratio without a second rounding step. A fully transparent box leaves the
            // destination texel at transparent black.
            for c in 0..3 {
                if let Some(value) = (rgb[c] + alpha / 2).checked_div(alpha) {
                    out[out_offset + c] = value.min(255) as u8;
                }
            }
            out[out_offset + 3] = (alpha / count).min(255) as u8;
        }
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pixel(r: u8, g: u8, b: u8, a: u8) -> [u8; 4] {
        [r, g, b, a]
    }

    fn image_from_pixels(pixels: &[[u8; 4]]) -> Vec<u8> {
        pixels.iter().flatten().copied().collect()
    }

    #[test]
    // The `0 +`/`+ 0` terms below are deliberate: each line spells out all four source pixels'
    // contribution to that channel, even when one happens to be zero.
    #[allow(clippy::identity_op)]
    fn two_by_two_to_one_by_one_averages() {
        let pixels = [
            pixel(0, 0, 0, 255),
            pixel(255, 255, 255, 255),
            pixel(100, 150, 200, 255),
            pixel(50, 50, 50, 0),
        ];
        let src = image_from_pixels(&pixels);
        let out = scale_rgba(&src, 2, 2, 1, 1);
        assert_eq!(
            out,
            vec![
                ((0u32 + 255 + 100 + 50) / 4) as u8,
                ((0u32 + 255 + 150 + 50) / 4) as u8,
                ((0u32 + 255 + 200 + 50) / 4) as u8,
                ((255u32 + 255 + 255 + 0) / 4) as u8,
            ]
        );
    }

    #[test]
    fn four_by_four_checkerboard_to_two_by_two() {
        // Each 2x2 quadrant is a uniform color; downscaling should reproduce it exactly.
        let quadrants = [
            pixel(255, 0, 0, 255),
            pixel(0, 255, 0, 255),
            pixel(0, 0, 255, 255),
            pixel(255, 255, 0, 255),
        ];
        let mut src = vec![[0u8; 4]; 16];
        for y in 0..4u32 {
            for x in 0..4u32 {
                let quadrant = (y / 2) * 2 + (x / 2);
                src[(y * 4 + x) as usize] = quadrants[quadrant as usize];
            }
        }
        let src_bytes = image_from_pixels(&src);
        let out = scale_rgba(&src_bytes, 4, 4, 2, 2);
        assert_eq!(out, image_from_pixels(&quadrants));
    }

    #[test]
    fn identity_returns_copy() {
        let pixels = [pixel(1, 2, 3, 4), pixel(5, 6, 7, 8)];
        let src = image_from_pixels(&pixels);
        assert_eq!(scale_rgba(&src, 2, 1, 2, 1), src);
        assert_eq!(scale_rgba_premultiplied(&src, 2, 1, 2, 1), src);
    }

    /// The whole point of the premultiplied path: opaque red beside transparent black must
    /// downscale to *full* red at half alpha, not to half-brightness red.
    #[test]
    fn premultiplied_downscale_keeps_color_beside_transparency() {
        let src = image_from_pixels(&[pixel(255, 0, 0, 255), pixel(0, 0, 0, 0)]);

        let premultiplied = scale_rgba_premultiplied(&src, 2, 1, 1, 1);
        assert_eq!(premultiplied, vec![255, 0, 0, 127]);

        // ...whereas the straight-alpha filter darkens it, which is exactly the fringing this
        // avoids.
        let straight = scale_rgba(&src, 2, 1, 1, 1);
        assert_eq!(straight, vec![127, 0, 0, 127]);
    }

    #[test]
    fn premultiplied_downscale_matches_straight_alpha_when_fully_opaque() {
        let pixels = [
            pixel(0, 10, 20, 255),
            pixel(40, 50, 60, 255),
            pixel(80, 90, 100, 255),
            pixel(120, 130, 140, 255),
        ];
        let src = image_from_pixels(&pixels);
        // (Both average to (60, 70, 80); the premultiplied path rounds to nearest, the straight
        // one truncates, and these values divide evenly so the two agree exactly.)
        assert_eq!(
            scale_rgba_premultiplied(&src, 2, 2, 1, 1),
            scale_rgba(&src, 2, 2, 1, 1)
        );
    }

    #[test]
    fn premultiplied_downscale_of_fully_transparent_box_is_transparent_black() {
        let src = image_from_pixels(&[pixel(200, 200, 200, 0), pixel(100, 100, 100, 0)]);
        assert_eq!(
            scale_rgba_premultiplied(&src, 2, 1, 1, 1),
            vec![0, 0, 0, 0],
            "no visible color may leak out of fully transparent texels"
        );
    }

    /// Regression test for the `u32` accumulator overflow: a box with more than ~66K texels
    /// (255 * 255 per texel, `u32::MAX / 65025 ~= 66_052`) blows past `u32::MAX` under the old
    /// accumulator type — this exercises a box well past that (300x300 = 90,000 texels) and
    /// pins the exact expected output, which the old code could never reach (it panicked in
    /// debug builds, or silently wrapped to a wrong, non-255 value in release).
    #[test]
    fn large_opaque_white_box_downscale_does_not_overflow() {
        const DIM: u32 = 300;
        let src = vec![255u8; DIM as usize * DIM as usize * 4];
        let out = scale_rgba_premultiplied(&src, DIM, DIM, 1, 1);
        assert_eq!(out, vec![255, 255, 255, 255]);
    }

    #[test]
    fn premultiplied_downscale_handles_degenerate_sizes() {
        let src = image_from_pixels(&[pixel(1, 2, 3, 4)]);
        assert!(scale_rgba_premultiplied(&src, 1, 1, 0, 1).is_empty());
        assert!(scale_rgba_premultiplied(&src, 0, 1, 1, 1).is_empty());
        // Short input: missing texels are simply not sampled, never a panic.
        assert_eq!(
            scale_rgba_premultiplied(&[0u8; 4], 2, 2, 1, 1).len(),
            4,
            "truncated source must still produce a full destination"
        );
    }
}
