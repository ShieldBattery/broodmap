//! The "player token" drawn for start-location color blocks: an anti-aliased rounded rectangle
//! with a soft drop shadow, a subtle vertical gradient fill, and a two-tone bezel stroke (a dark
//! outer ring, a bright inner ring), all derived from a signed-distance-field (SDF) of a rounded
//! rect. Every dimension (corner radius, stroke widths, shadow offset) is a fraction of the
//! block's own output-pixel size, so the token looks proportionally consistent whether it's 8px
//! or 200px across.
//!
//! At very small sizes the full bezel treatment would eat itself — the corner radius and both
//! stroke rings can't fit inside a handful of pixels without consuming the entire fill — so below
//! [`TINY_THRESHOLD_PX`] the token drops the shadow and inner stroke and falls back to a plain
//! fill with a single-pixel dark outline.

use crate::image::RgbaImage;
use crate::overlay::blend_over;

/// Below this output-pixel size (the block's shorter side), drop the shadow and inner stroke and
/// fall back to a flat fill with a single-pixel outline — there isn't room for a bezel without it
/// eating the fill entirely.
const TINY_THRESHOLD_PX: f32 = 8.0;

/// Corner radius as a fraction of the block's shorter side.
const RADIUS_FRACTION: f32 = 1.0 / 5.0;

/// Outer (dark) stroke width as a fraction of the block's shorter side.
const OUTER_STROKE_FRACTION: f32 = 0.06;

/// Drop shadow's downward offset, as a fraction of the block's height.
const SHADOW_OFFSET_FRACTION: f32 = 0.06;

/// Drop shadow peak alpha (at its own centre, ignoring the soft-edge falloff).
const SHADOW_ALPHA: f32 = 0.35;

/// How many output pixels the shadow's edge is blurred over.
const SHADOW_SOFTNESS_PX: f32 = 2.0;

const OUTER_STROKE_ALPHA: f32 = 0.75;
const INNER_STROKE_ALPHA: f32 = 0.70;

/// Vertical gradient strength: the top row scales the fill color by `1 + GRADIENT_STRENGTH`, the
/// bottom row by `1 - GRADIENT_STRENGTH`, lerped in between.
const GRADIENT_STRENGTH: f32 = 0.12;

/// Softness (in output pixels) used to antialias crisp edges: the shape's own boundary and the
/// stroke-band boundaries.
const EDGE_AA_PX: f32 = 0.75;

/// Signed distance from `(px, py)` — already relative to the rect's centre — to the boundary of
/// an axis-aligned rounded rectangle with half-extents `(half_w, half_h)` and corner radius `r`.
/// Negative inside, positive outside, zero on the boundary.
///
/// Standard rounded-rect SDF (Inigo Quilez): shrink the box by `r` in each axis, measure the
/// distance to *that* box's boundary (clamping negative components to zero, since inside the
/// shrunk box's span on an axis there's no distance contribution from it), then subtract `r`
/// back off to round the corners out.
fn sdf_rounded_rect(px: f32, py: f32, half_w: f32, half_h: f32, r: f32) -> f32 {
    let r = r.min(half_w).min(half_h).max(0.0);
    let qx = (px.abs() - (half_w - r)).max(0.0);
    let qy = (py.abs() - (half_h - r)).max(0.0);
    (qx * qx + qy * qy).sqrt() - r
}

/// Hermite smoothstep, `0` at/before `edge0`, `1` at/after `edge1`, cubic in between. `edge0` may
/// be greater than `edge1` (used to build falling ramps).
fn smoothstep(edge0: f32, edge1: f32, x: f32) -> f32 {
    if edge0 == edge1 {
        return if x < edge0 { 0.0 } else { 1.0 };
    }
    let t = ((x - edge0) / (edge1 - edge0)).clamp(0.0, 1.0);
    t * t * (3.0 - 2.0 * t)
}

/// SDF value `d` (negative inside, positive outside) to a `[0, 1]` coverage fraction: `1` when
/// fully inside (`d <= -softness`), `0` when fully outside (`d >= softness`), smoothed between.
fn coverage(d: f32, softness: f32) -> f32 {
    1.0 - smoothstep(-softness, softness, d)
}

/// Straight-alpha lerp of a color, `t` in `[0, 1]` (out of range is clamped): `t == 0` keeps
/// `base`, `t == 1` gives `other`.
fn lerp_rgb(base: [u8; 3], other: [u8; 3], t: f32) -> [u8; 3] {
    let t = t.clamp(0.0, 1.0);
    let mut out = [0u8; 3];
    for c in 0..3 {
        let v = base[c] as f32 * (1.0 - t) + other[c] as f32 * t;
        out[c] = v.round().clamp(0.0, 255.0) as u8;
    }
    out
}

/// Straight-alpha src-over of a single texel, both `[r, g, b, a]`.
fn over(dst: [u8; 4], src: [u8; 4]) -> [u8; 4] {
    let sa = src[3] as f32 / 255.0;
    if sa <= 0.0 {
        return dst;
    }
    let da = dst[3] as f32 / 255.0;
    let out_a = sa + da * (1.0 - sa);
    if out_a <= 0.0 {
        return [0, 0, 0, 0];
    }
    let mut out = [0u8; 4];
    for c in 0..3 {
        let v = (src[c] as f32 * sa + dst[c] as f32 * da * (1.0 - sa)) / out_a;
        out[c] = v.round().clamp(0.0, 255.0) as u8;
    }
    out[3] = (out_a * 255.0).round().clamp(0.0, 255.0) as u8;
    out
}

/// Draws one player token: a rounded, gradient-filled, bezel-stroked block in `color`, bounded by
/// `[left, right) x [top, bottom)` (output pixels), composited src-over onto `image`.
///
/// `left`/`top`/`right`/`bottom` may extend past the image's edges (or be entirely off it) — the
/// composite is clipped the same way [`blend_over`] clips ordinary sprite blits.
pub(crate) fn draw_player_token(
    image: &mut RgbaImage,
    left: i32,
    top: i32,
    right: i32,
    bottom: i32,
    color: [u8; 3],
) {
    let box_w = (right - left).max(1) as f32;
    let box_h = (bottom - top).max(1) as f32;
    let min_dim = box_w.min(box_h);
    let tiny = min_dim < TINY_THRESHOLD_PX;

    let half_w = box_w / 2.0;
    let half_h = box_h / 2.0;
    let cx = left as f32 + half_w;
    let cy = top as f32 + half_h;

    let radius = (min_dim * RADIUS_FRACTION).min(half_w).min(half_h).max(0.0);

    // Stroke widths are capped relative to the block's own size so a small-but-not-tiny block
    // can never have its fill entirely eaten by the bezel. They're also capped to leave at least
    // `EDGE_AA_PX` of headroom under the corner radius: the rings are read off as distance bands
    // of a single SDF (see below), and a band at depth `t` from the boundary is only a true
    // constant-width offset curve — concentric with the boundary at the corners as well as the
    // flat edges — while `t <= radius`. Past that depth the SDF's simplified formula saturates to
    // a flat `-radius` interior, so without headroom the innermost band's "fully inside" coverage
    // never reaches 1 and the whole interior reads as a partial, muddy blend instead of a clean
    // fill; the margin keeps a pixel that's `radius` deep unambiguously past every band.
    let max_ring_depth = (radius - EDGE_AA_PX).max(0.0);
    let outer_w = if tiny {
        1.0
    } else {
        (min_dim * OUTER_STROKE_FRACTION)
            .max(1.0)
            .min(min_dim / 4.0)
    }
    .min(max_ring_depth);
    let inner_w = if tiny {
        0.0
    } else {
        (outer_w / 2.0).max(1.0).min(min_dim / 6.0)
    }
    .min((max_ring_depth - outer_w).max(0.0));
    let shadow_offset = if tiny {
        0.0
    } else {
        box_h * SHADOW_OFFSET_FRACTION
    };

    // Padded bounding box: the shadow can extend past the block's own edge, and every soft edge
    // needs a little slack so it isn't clipped mid-falloff.
    let pad = if tiny {
        2
    } else {
        (shadow_offset.ceil() as i32 + SHADOW_SOFTNESS_PX.ceil() as i32 + 2).max(3)
    };
    let full_x0 = left - pad;
    let full_y0 = top - pad;
    let full_x1 = full_x0 + (box_w.ceil() as i32 + pad * 2).max(1);
    let full_y1 = full_y0 + (box_h.ceil() as i32 + pad * 2).max(1);

    // Clip the padded box to the image's own bounds *before* allocating anything: `left..bottom`
    // is untrusted (it comes from a start location's `units.dat` placebox, which can be as large
    // as the raw `i16` allows), and pixels outside the image are discarded by `blend_over` below
    // anyway — there's nothing lost by never computing them. Without this, a huge-but-mostly-
    // offscreen token could still demand a huge-but-mostly-wasted buffer.
    let clip_x0 = full_x0.max(0);
    let clip_y0 = full_y0.max(0);
    let clip_x1 = full_x1.min(image.width as i32);
    let clip_y1 = full_y1.min(image.height as i32);
    if clip_x1 <= clip_x0 || clip_y1 <= clip_y0 {
        return;
    }

    let out_w = (clip_x1 - clip_x0) as u32;
    let out_h = (clip_y1 - clip_y0) as u32;
    let mut buf = vec![0u8; out_w as usize * out_h as usize * 4];

    for oy in 0..out_h {
        let py_abs = clip_y0 as f32 + oy as f32 + 0.5;
        let py = py_abs - cy;

        // Vertical gradient: +GRADIENT_STRENGTH at the block's top row, -GRADIENT_STRENGTH at
        // its bottom row, lerped (and clamped, since padding rows fall outside [0, 1]).
        let t = ((py_abs - top as f32) / box_h).clamp(0.0, 1.0);
        let gradient = 1.0 + GRADIENT_STRENGTH - 2.0 * GRADIENT_STRENGTH * t;
        let fill_color = [
            (color[0] as f32 * gradient).round().clamp(0.0, 255.0) as u8,
            (color[1] as f32 * gradient).round().clamp(0.0, 255.0) as u8,
            (color[2] as f32 * gradient).round().clamp(0.0, 255.0) as u8,
        ];

        for ox in 0..out_w {
            let px_abs = clip_x0 as f32 + ox as f32 + 0.5;
            let px = px_abs - cx;

            let mut out = [0u8, 0, 0, 0];

            if !tiny {
                let d_shadow = sdf_rounded_rect(px, py - shadow_offset, half_w, half_h, radius);
                let shadow_cov = coverage(d_shadow, SHADOW_SOFTNESS_PX);
                if shadow_cov > 0.0 {
                    let a = (shadow_cov * SHADOW_ALPHA * 255.0)
                        .round()
                        .clamp(0.0, 255.0) as u8;
                    out = over(out, [0, 0, 0, a]);
                }
            }

            // One SDF evaluation for the shape's own (outer) boundary, then the stroke rings are
            // read off as *distance bands* of that single field rather than as separate nested
            // shapes: `coverage(d0 + depth, aa)` asks "is this pixel at least `depth` past the
            // boundary?". For `depth <= radius`, the level set `d0 == -depth` is an exact
            // constant-width offset of the boundary curve — around the corner arcs as well as the
            // flat edges (shifting a point `t` inside a corner arc along its radius keeps
            // `d == -t`; shifting a flat-edge point inward by `t` does too) — so banding a single
            // SDF this way is concentric by construction, unlike evaluating the SDF again at
            // shrunk half-extents (which moves the corner arcs' centres and bulges the gap).
            let d0 = sdf_rounded_rect(px, py, half_w, half_h, radius);
            let cov_edge = coverage(d0, EDGE_AA_PX);
            if cov_edge > 0.0 {
                let mut rgb = fill_color;

                let cov_past_outer = coverage(d0 + outer_w, EDGE_AA_PX);
                let outer_ring = (cov_edge - cov_past_outer).clamp(0.0, 1.0);
                if outer_ring > 0.0 {
                    rgb = lerp_rgb(rgb, [0, 0, 0], outer_ring * OUTER_STROKE_ALPHA);
                }

                if inner_w > 0.0 {
                    let cov_past_inner = coverage(d0 + outer_w + inner_w, EDGE_AA_PX);
                    let inner_ring = (cov_past_outer - cov_past_inner).clamp(0.0, 1.0);
                    if inner_ring > 0.0 {
                        rgb = lerp_rgb(rgb, [255, 255, 255], inner_ring * INNER_STROKE_ALPHA);
                    }
                }

                let a = (cov_edge * 255.0).round().clamp(0.0, 255.0) as u8;
                out = over(out, [rgb[0], rgb[1], rgb[2], a]);
            }

            let o = (oy as usize * out_w as usize + ox as usize) * 4;
            buf[o..o + 4].copy_from_slice(&out);
        }
    }

    blend_over(image, &buf, out_w, out_h, clip_x0, clip_y0);
}

#[cfg(test)]
mod tests {
    use super::*;

    // -----------------------------------------------------------------------------------------
    // SDF math sanity
    // -----------------------------------------------------------------------------------------

    #[test]
    fn sdf_is_negative_inside_and_positive_outside() {
        // A 40x20 box (half-extents 20x10), modest 4px corner radius.
        let (hw, hh, r) = (20.0, 10.0, 4.0);
        assert!(
            sdf_rounded_rect(0.0, 0.0, hw, hh, r) < 0.0,
            "centre is inside"
        );
        assert!(
            sdf_rounded_rect(5.0, 3.0, hw, hh, r) < 0.0,
            "off-centre but well within bounds"
        );
        assert!(
            sdf_rounded_rect(100.0, 0.0, hw, hh, r) > 0.0,
            "far to the right is outside"
        );
        assert!(
            sdf_rounded_rect(0.0, 100.0, hw, hh, r) > 0.0,
            "far below is outside"
        );
    }

    #[test]
    fn sdf_is_zero_on_the_flat_edge_and_exact_distance_beyond_it() {
        let (hw, hh, r) = (20.0, 10.0, 4.0);
        // The right edge's flat (non-rounded) span is exactly at x == half_w when y == 0.
        assert!((sdf_rounded_rect(hw, 0.0, hw, hh, r)).abs() < 1e-4);
        // 5 units further out, the distance to that flat edge is exactly 5.
        assert!((sdf_rounded_rect(hw + 5.0, 0.0, hw, hh, r) - 5.0).abs() < 1e-4);
    }

    #[test]
    fn corner_rounding_pulls_the_boundary_in_from_the_sharp_corner() {
        let (hw, hh, r) = (20.0, 10.0, 4.0);
        // The unrounded rect's sharp corner sits just outside the rounded shape...
        let at_sharp_corner = sdf_rounded_rect(hw, hh, hw, hh, r);
        assert!(
            at_sharp_corner > 0.0,
            "sharp corner point should read as outside a rounded rect: {at_sharp_corner}"
        );
        // ...but a point pulled back toward the centre by the radius, along the diagonal, lands
        // back on (or very near) the rounded boundary.
        let inset = r - r / std::f32::consts::SQRT_2;
        let near_boundary = sdf_rounded_rect(hw - inset, hh - inset, hw, hh, r);
        assert!(
            near_boundary.abs() < 0.5,
            "corner-radius-inset point should sit near the boundary: {near_boundary}"
        );
        // A radius of zero degenerates to a plain (sharp-cornered) rect: the sharp corner is then
        // exactly on the boundary.
        assert!((sdf_rounded_rect(hw, hh, hw, hh, 0.0)).abs() < 1e-4);
    }

    /// The bug this guards against: banding the SDF by re-evaluating it at shrunk half-extents
    /// (keeping the same radius) moves the rounded corners' arc centres, so the gap between two
    /// "rings" built that way is wider at the corners than along the flat edges. Banding a
    /// *single* SDF by distance instead must give the exact same depth reading in both places.
    #[test]
    fn distance_bands_are_constant_width_around_corners_not_just_flat_edges() {
        // A 100x100 box (half-extents 50x50) with a generous 20px corner radius, so there's
        // plenty of room to sample well inside the "arc" region without hitting the degenerate
        // flat interior (see `sdf_is_zero_on_the_flat_edge...`).
        let (hw, hh, r) = (50.0, 50.0, 20.0);
        let arc_centre = (hw - r, hh - r);

        for depth in [0.0, 5.0, 10.0, 15.0, 20.0] {
            // A point offset inward by exactly `depth` from the boundary, along the corner's
            // 45-degree diagonal (i.e. on the arc of radius `r - depth` centred on `arc_centre`).
            let radial = r - depth;
            let angle = std::f32::consts::FRAC_PI_4;
            let corner_px = arc_centre.0 + radial * angle.cos();
            let corner_py = arc_centre.1 + radial * angle.sin();
            let d_corner = sdf_rounded_rect(corner_px, corner_py, hw, hh, r);

            // A point the same `depth` inward from the boundary, but along the flat edge (away
            // from any corner).
            let d_flat = sdf_rounded_rect(hw - depth, 0.0, hw, hh, r);

            assert!(
                (d_corner + depth).abs() < 1e-3,
                "corner point at depth {depth} should read d == -{depth}, got {d_corner}"
            );
            assert!(
                (d_flat + depth).abs() < 1e-3,
                "flat-edge point at depth {depth} should read d == -{depth}, got {d_flat}"
            );
            assert!(
                (d_corner - d_flat).abs() < 1e-3,
                "same depth must read the same distance at a corner as on a flat edge: \
                 corner {d_corner}, flat {d_flat}"
            );
        }
    }

    #[test]
    fn smoothstep_and_coverage_saturate_at_their_edges() {
        assert_eq!(smoothstep(0.0, 1.0, -1.0), 0.0);
        assert_eq!(smoothstep(0.0, 1.0, 2.0), 1.0);
        assert_eq!(smoothstep(0.0, 1.0, 0.5), 0.5);

        assert_eq!(coverage(-10.0, 1.0), 1.0, "deep inside is full coverage");
        assert_eq!(coverage(10.0, 1.0), 0.0, "far outside is no coverage");
        assert!((coverage(0.0, 1.0) - 0.5).abs() < 1e-6, "boundary is half");
    }

    // -----------------------------------------------------------------------------------------
    // Rendering
    // -----------------------------------------------------------------------------------------

    fn blank_image(w: u32, h: u32) -> RgbaImage {
        RgbaImage {
            width: w,
            height: h,
            data: [0, 0, 0, 0].repeat((w * h) as usize),
        }
    }

    fn pixel(image: &RgbaImage, x: u32, y: u32) -> [u8; 4] {
        let o = (y as usize * image.width as usize + x as usize) * 4;
        image.data[o..o + 4].try_into().unwrap()
    }

    #[test]
    fn full_size_token_centre_is_the_flat_player_color() {
        // At the vertical midpoint the gradient factor is exactly 1.0, and the centre is far
        // from every stroke band, so the centre pixel should be the *exact* input color.
        let mut image = blank_image(80, 80);
        let color = [244, 4, 4];
        draw_player_token(&mut image, 10, 10, 70, 70, color);
        let center = pixel(&image, 40, 40);
        assert_eq!(center, [color[0], color[1], color[2], 255]);
    }

    #[test]
    fn full_size_token_has_a_dark_outer_ring_and_bright_inner_ring() {
        let mut image = blank_image(80, 80);
        draw_player_token(&mut image, 10, 10, 70, 70, [244, 4, 4]);
        // Box is 60x60, so outer stroke width is 0.06*60 = 3.6px and inner is 1.8px (both
        // floored up to >= 1px by construction). A couple of pixels in from the left edge (x =
        // 10) should land in the outer (dark) band; a few more in, the inner (bright) band.
        let outer = pixel(&image, 11, 40);
        assert!(
            outer[0] < 150 && outer[1] < 150 && outer[2] < 150,
            "expected a dark outer-stroke pixel, got {outer:?}"
        );
        let inner = pixel(&image, 14, 40);
        assert!(
            inner[0] > 150 && inner[1] > 150 && inner[2] > 150,
            "expected a bright inner-stroke pixel, got {inner:?}"
        );
    }

    #[test]
    fn tiny_token_degrades_gracefully_without_white_bleed_or_panics() {
        // A 6x5 block: below the tiny threshold, so shadow and inner (white) stroke must be
        // fully dropped.
        let mut image = blank_image(20, 20);
        draw_player_token(&mut image, 5, 5, 11, 10, [244, 4, 4]);

        let mut saw_fill = false;
        for y in 0..20 {
            for x in 0..20 {
                let p = pixel(&image, x, y);
                assert!(
                    !(p[0] > 200 && p[1] > 200 && p[2] > 200),
                    "no white inner-stroke bleed expected at tiny sizes: {p:?} at ({x},{y})"
                );
                if p == [244, 4, 4, 255] {
                    saw_fill = true;
                }
            }
        }
        assert!(saw_fill, "the block's own color should still show through");
    }

    /// The fix this guards against: `left..bottom` comes from an untrusted `units.dat` placebox
    /// (up to raw `i16` range, before any zoom scaling), so a hostile/corrupted table plus a
    /// high-zoom render could ask for a padded working buffer many GiB in size. Clipping to the
    /// image's own bounds before allocating means the buffer is never bigger than what's
    /// actually visible, regardless of how large the requested box is.
    #[test]
    fn huge_box_clips_to_the_visible_image_without_a_huge_allocation() {
        let mut image = blank_image(40, 40);
        // A ~100,000 x 100,000 logical-px box, centred on the image -- far larger than any real
        // start-location placebox could be even at 4x zoom.
        draw_player_token(&mut image, -49980, -49980, 50020, 50020, [244, 4, 4]);
        let center = pixel(&image, 20, 20);
        assert_eq!(
            center,
            [244, 4, 4, 255],
            "deep interior of a huge token still reads as flat fill"
        );
    }

    #[test]
    fn pixels_well_outside_the_token_are_left_untouched() {
        let mut image = blank_image(80, 80);
        image.data.chunks_exact_mut(4).for_each(|p| {
            p.copy_from_slice(&[0, 0, 255, 255]); // solid "terrain" blue
        });
        draw_player_token(&mut image, 10, 10, 70, 70, [244, 4, 4]);
        // Comfortably past the shadow/stroke/AA padding on every side.
        assert_eq!(pixel(&image, 0, 40), [0, 0, 255, 255]);
        assert_eq!(pixel(&image, 40, 0), [0, 0, 255, 255]);
        assert_eq!(pixel(&image, 79, 40), [0, 0, 255, 255]);
        assert_eq!(pixel(&image, 40, 79), [0, 0, 255, 255]);
    }
}
