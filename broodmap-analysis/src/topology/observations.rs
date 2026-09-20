use super::context::{distance, point};
use super::{TopologyBase, TopologyBoundary, TopologyObservation};
use crate::PixelPosition;
use std::collections::BTreeSet;

fn compare_observations(a: &TopologyObservation, b: &TopologyObservation) -> std::cmp::Ordering {
    a.endpoints
        .cmp(&b.endpoints)
        .then(a.width_pixels.total_cmp(&b.width_pixels))
        .then(a.sources.cmp(&b.sources))
}
fn parallel(a: [PixelPosition; 2], b: [PixelPosition; 2]) -> bool {
    let dx = f64::from(a[1].x) - f64::from(a[0].x);
    let dy = f64::from(a[1].y) - f64::from(a[0].y);
    let bx = f64::from(b[1].x) - f64::from(b[0].x);
    let by = f64::from(b[1].y) - f64::from(b[0].y);
    let lengths = dx.hypot(dy) * bx.hypot(by);
    lengths > 0.0 && (dx * bx + dy * by).abs() / lengths >= (std::f64::consts::PI / 12.0).cos()
}
fn anchor_between(a: [PixelPosition; 2], b: [PixelPosition; 2], bases: &[TopologyBase]) -> bool {
    let [a0, a1] = a.map(point);
    let [mut b0, mut b1] = b.map(point);
    let ad = [a1[0] - a0[0], a1[1] - a0[1]];
    let mut bd = [b1[0] - b0[0], b1[1] - b0[1]];
    if ad[0] * bd[0] + ad[1] * bd[1] < 0.0 {
        std::mem::swap(&mut b0, &mut b1);
        bd = [-bd[0], -bd[1]];
    }
    let cross = |d: [f64; 2], origin: [f64; 2], p: [f64; 2]| {
        d[0] * (p[1] - origin[1]) - d[1] * (p[0] - origin[0])
    };
    let within = |start: [f64; 2], d: [f64; 2], p: [f64; 2]| {
        let len = d[0].hypot(d[1]);
        let projection = ((p[0] - start[0]) * d[0] + (p[1] - start[1]) * d[1]) / len;
        projection >= 0.0 && projection <= len
    };
    bases.iter().filter_map(|b| b.route_anchor).any(|p| {
        let p = [f64::from(p.x) * 8.0 + 4.0, f64::from(p.y) * 8.0 + 4.0];
        cross(ad, a0, p) * cross(bd, b0, p) < 0.0 && within(a0, ad, p) && within(b0, bd, p)
    })
}
fn can_merge(a: &TopologyObservation, b: &TopologyObservation, bases: &[TopologyBase]) -> bool {
    parallel(a.endpoints, b.endpoints)
        && a.endpoints
            .iter()
            .zip(b.endpoints)
            .all(|(&a, b)| distance(point(a), point(b)) <= 32.0)
        && !anchor_between(a.endpoints, b.endpoints, bases)
        && !anchor_between(b.endpoints, a.endpoints, bases)
}
pub(super) fn consolidate(
    mut observations: Vec<TopologyObservation>,
    bases: &[TopologyBase],
) -> Vec<TopologyBoundary> {
    observations.sort_by(compare_observations);
    let mut groups: Vec<Vec<TopologyObservation>> = Vec::new();
    for observation in observations {
        if let Some(group) = groups
            .iter_mut()
            .find(|g| g.iter().all(|m| can_merge(&observation, m, bases)))
        {
            group.push(observation);
        } else {
            groups.push(vec![observation]);
        }
    }
    let mut result: Vec<_> = groups
        .into_iter()
        .map(|members| {
            let representative = members
                .iter()
                .min_by(|a, b| {
                    a.width_pixels
                        .total_cmp(&b.width_pixels)
                        .then(a.endpoints.cmp(&b.endpoints))
                })
                .unwrap();
            TopologyBoundary {
                endpoints: representative.endpoints,
                width_pixels: representative.width_pixels,
                sources: members
                    .iter()
                    .flat_map(|m| m.sources.iter().copied())
                    .collect::<BTreeSet<_>>()
                    .into_iter()
                    .collect(),
                observations: members,
                ramp: None,
            }
        })
        .collect();
    result.sort_by(|a, b| {
        a.endpoints
            .cmp(&b.endpoints)
            .then(a.width_pixels.total_cmp(&b.width_pixels))
            .then(a.sources.cmp(&b.sources))
    });
    result
}
fn orient(a: PixelPosition, b: PixelPosition, p: PixelPosition) -> i64 {
    (i64::from(b.x) - i64::from(a.x)) * (i64::from(p.y) - i64::from(a.y))
        - (i64::from(b.y) - i64::from(a.y)) * (i64::from(p.x) - i64::from(a.x))
}
fn crosses(a: &TopologyBoundary, b: &TopologyBoundary) -> bool {
    let [a0, a1] = a.endpoints;
    let [b0, b1] = b.endpoints;
    let c = orient(a0, a1, b0);
    let d = orient(a0, a1, b1);
    let e = orient(b0, b1, a0);
    let f = orient(b0, b1, a1);
    ((c < 0 && d > 0) || (c > 0 && d < 0)) && ((e < 0 && f > 0) || (e > 0 && f < 0))
}
pub(super) fn non_crossing(mut boundaries: Vec<TopologyBoundary>) -> Vec<TopologyBoundary> {
    boundaries.sort_by(|a, b| {
        b.ramp
            .is_some()
            .cmp(&a.ramp.is_some())
            .then(a.width_pixels.total_cmp(&b.width_pixels))
            .then(a.endpoints.cmp(&b.endpoints))
    });
    let mut result: Vec<TopologyBoundary> = Vec::new();
    for b in boundaries {
        if !result
            .iter()
            .any(|a| a.endpoints == b.endpoints || crosses(a, &b))
        {
            result.push(b);
        }
    }
    result.sort_by_key(|b| b.endpoints);
    result
}

#[cfg(test)]
mod tests {
    use super::super::context::canonical;
    use super::super::{RampBoundary, RampSide};
    use super::*;
    use crate::WalkPosition;

    fn observation(
        points: [[u32; 2]; 2],
        width: f64,
        sources: Vec<[u32; 2]>,
    ) -> TopologyObservation {
        TopologyObservation {
            endpoints: canonical(points.map(|[x, y]| PixelPosition { x, y })),
            width_pixels: width,
            sources,
        }
    }
    fn boundary(points: [[u32; 2]; 2], width: f64) -> TopologyBoundary {
        let raw = observation(points, width, vec![[1, 2]]);
        TopologyBoundary {
            endpoints: raw.endpoints,
            width_pixels: width,
            sources: raw.sources.clone(),
            observations: vec![raw],
            ramp: None,
        }
    }
    #[test]
    fn complete_link_proximity_does_not_chain_and_preserves_raw_evidence() {
        let observations = vec![
            observation([[32, 0], [32, 96]], 96.0, vec![[3, 1]]),
            observation([[56, 0], [56, 96]], 88.0, vec![[3, 1], [1, 3]]),
            observation([[80, 0], [80, 96]], 80.0, vec![[2, 3]]),
        ];
        let output = consolidate(observations.clone(), &[]);
        assert_eq!(output.len(), 2);
        assert_eq!(output[0].endpoints, observations[1].endpoints);
        assert_eq!(output[0].sources, vec![[1, 3], [3, 1]]);
        assert_eq!(output[0].observations, observations[..2]);
        assert_eq!(output[1].observations, observations[2..]);
        assert_eq!(
            consolidate(observations.into_iter().rev().collect(), &[]),
            output
        );
    }
    #[test]
    fn nearby_grouping_respects_anchor_projection_parallelism_and_distance() {
        let a = observation([[32, 0], [32, 96]], 96.0, vec![]);
        let b = observation([[56, 0], [56, 96]], 96.0, vec![]);
        let anchor = |x, y| {
            [TopologyBase {
                id: 1,
                route_anchor: Some(WalkPosition { x, y }),
            }]
        };
        assert_eq!(
            consolidate(vec![a.clone(), b.clone()], &anchor(5, 5)).len(),
            2
        );
        assert_eq!(consolidate(vec![a.clone(), b], &anchor(5, 20)).len(), 1);
        let tilted = observation([[16, 0], [48, 96]], 100.0, vec![]);
        assert_eq!(consolidate(vec![a.clone(), tilted], &[]).len(), 2);
        let distant = observation([[65, 0], [65, 96]], 96.0, vec![]);
        assert_eq!(consolidate(vec![a, distant], &[]).len(), 2);
    }
    #[test]
    fn crossing_filter_keeps_narrow_sections_without_merging_discarded_evidence() {
        let broad = boundary([[0, 48], [96, 48]], 96.0);
        let narrow = boundary([[32, 0], [32, 96]], 40.0);
        let second = boundary([[64, 0], [64, 96]], 48.0);
        let output = non_crossing(vec![broad.clone(), narrow.clone(), second.clone()]);
        assert_eq!(output, vec![narrow.clone(), second.clone()]);
        assert_eq!(non_crossing(vec![second, narrow, broad]), output);
    }
    #[test]
    fn touching_or_collinear_sections_survive_but_exact_ramp_duplicates_do_not() {
        let a = boundary([[0, 32], [64, 32]], 64.0);
        let b = boundary([[64, 32], [64, 96]], 64.0);
        let c = boundary([[32, 32], [96, 32]], 64.0);
        assert_eq!(non_crossing(vec![a.clone(), b.clone(), c.clone()]).len(), 3);
        let mut ramp = a.clone();
        ramp.ramp = Some(RampBoundary {
            id: 9,
            end: RampSide::Lower,
            lower_elevation: 0,
            upper_elevation: 1,
        });
        ramp.width_pixels = 80.0;
        let result = non_crossing(vec![a.clone(), ramp.clone()]);
        assert_eq!(result, vec![ramp.clone()]);
        assert_eq!(non_crossing(vec![ramp, a]), result);
    }
    #[test]
    fn equal_width_crossings_use_canonical_geometry_not_insertion_order() {
        let a = boundary([[0, 0], [96, 96]], 96.0);
        let b = boundary([[0, 96], [96, 0]], 96.0);
        assert_eq!(non_crossing(vec![a.clone(), b.clone()]), vec![a.clone()]);
        assert_eq!(non_crossing(vec![b, a.clone()]), vec![a]);
    }
}
