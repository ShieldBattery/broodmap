//! RGBA image buffer, box-downscaling, and (optional) PNG encoding.
//!
//! Downscaling at draw time (rather than decoding a full-resolution intermediate and scaling
//! the whole composited image) is what keeps memory proportional to the *output* buffer instead
//! of the source assets — see `docs/render-design.md`, "Asset formats and tiers".

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
/// TODO(phase 2): channels are averaged independently (straight alpha). That's exact for the
/// opaque terrain this currently scales, but downscaling translucent sprite art this way
/// produces dark fringes (transparent texels' RGB pulls the average down) — switch to
/// premultiplied-alpha filtering before reusing this for `.anim` frames.
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
        let out = scale_rgba(&src, 2, 1, 2, 1);
        assert_eq!(out, src);
    }
}
