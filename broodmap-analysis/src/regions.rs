//! Deterministic terrain regions from clearance-persistent markers.
//!
//! This is a raster analysis over the current [`TerrainGrid`] walkability. Markers are not exact
//! h-maxima: descending union-find records a local peak when it loses to a higher peak by both
//! requested absolute and relative clearance prominence, then a deterministic priority flood partitions
//! walkable cells among those markers. Passages are representative adjacent label crossings, rather than
//! complete chokepoint or corridor-width measurements.

use std::{
    cmp::Reverse,
    collections::{BTreeMap, BinaryHeap},
};

use thiserror::Error;

use crate::{TerrainGrid, WalkPosition};

const MIN_PROMINENCE_PIXELS: u16 = 8;
const MAX_PROMINENCE_PIXELS: u16 = 4_096;

/// Controls persistence filtering for clearance-derived region markers.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct RegionOptions {
    /// A losing local peak is retained only when its clearance exceeds its merge level by at
    /// least this many logical pixels.
    pub min_prominence_pixels: u16,
    /// A losing local peak must also exceed its merge level by this percentage of its own
    /// clearance. Zero disables relative filtering.
    pub min_relative_prominence_percent: u8,
}

impl Default for RegionOptions {
    fn default() -> Self {
        Self {
            min_prominence_pixels: 32,
            min_relative_prominence_percent: 40,
        }
    }
}

/// Error returned while deriving terrain regions.
#[derive(Debug, Error, Copy, Clone, Eq, PartialEq)]
pub enum RegionError {
    #[error(
        "region prominence must be between {MIN_PROMINENCE_PIXELS} and {MAX_PROMINENCE_PIXELS} pixels, got {min_prominence_pixels}"
    )]
    InvalidProminence { min_prominence_pixels: u16 },
    #[error(
        "relative region prominence must be at most 100 percent, got {min_relative_prominence_percent}"
    )]
    InvalidRelativeProminence { min_relative_prominence_percent: u8 },
}

/// One persistent clearance marker and its final priority-flood partition size.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Region {
    /// Contiguous, one-based deterministic region ID.
    pub id: u32,
    /// The marker cell, with row-major tie breaking among equal clearances.
    pub peak: WalkPosition,
    /// Centered-square clearance at [`Self::peak`], in logical pixels.
    pub peak_clearance_pixels: u16,
    /// Number of walkable cells assigned to this region.
    pub cell_count: u32,
}

/// One deterministic representative adjacency between two final region labels.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Passage {
    /// Sorted one-based region IDs.
    pub regions: [u32; 2],
    /// Adjacent walk cells, ordered to correspond to [`Self::regions`].
    pub endpoints: [WalkPosition; 2],
    /// The lower clearance radius of the two endpoint cells, in logical pixels.
    pub clearance_radius_pixels: u16,
}

/// Immutable labels, persistent markers, and representative inter-region passages.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RegionAnalysis {
    width: u32,
    height: u32,
    labels: Vec<u32>,
    regions: Vec<Region>,
    passages: Vec<Passage>,
}

impl RegionAnalysis {
    /// Width in 8-pixel walk cells.
    pub fn width(&self) -> u32 {
        self.width
    }

    /// Height in 8-pixel walk cells.
    pub fn height(&self) -> u32 {
        self.height
    }

    /// Row-major one-based region labels. Blocked cells are zero.
    pub fn labels(&self) -> &[u32] {
        &self.labels
    }

    /// Persistent markers in contiguous ID order.
    pub fn regions(&self) -> &[Region] {
        &self.regions
    }

    /// One representative valid adjacency for each pair of touching regions.
    pub fn passages(&self) -> &[Passage] {
        &self.passages
    }

    /// Returns the one-based region label at a walk-cell position.
    pub fn region_at(&self, position: WalkPosition) -> Option<u32> {
        if position.x >= self.width || position.y >= self.height {
            return None;
        }
        let index = (position.y as usize)
            .checked_mul(self.width as usize)?
            .checked_add(position.x as usize)?;
        self.labels.get(index).copied().filter(|&label| label != 0)
    }
}

impl TerrainGrid {
    /// Derives deterministic clearance-prominent terrain regions for this grid's current walkability.
    ///
    /// The marker stage uses descending clearance activation and peak persistence rather than an
    /// exact h-maxima implementation. The partition stage is a deterministic ownership heuristic:
    /// it gives each walkable cell to a marker by preferring a higher path bottleneck clearance,
    /// then a shorter local plateau flood distance, then lower region ID and row-major cell order.
    /// The local distance resets after a clearance descent. It does not claim a global
    /// bottleneck/shortest-path optimum, because ownership finalizes cells and prunes later
    /// candidates. Diagonal steps follow [`Self::neighbors`], so they never
    /// pass through blocked corners. The analysis is uncached and naturally reflects a grid
    /// returned by [`Self::with_obstacles`].
    pub fn regions(&self, options: &RegionOptions) -> Result<RegionAnalysis, RegionError> {
        if !(MIN_PROMINENCE_PIXELS..=MAX_PROMINENCE_PIXELS).contains(&options.min_prominence_pixels)
        {
            return Err(RegionError::InvalidProminence {
                min_prominence_pixels: options.min_prominence_pixels,
            });
        }
        if options.min_relative_prominence_percent > 100 {
            return Err(RegionError::InvalidRelativeProminence {
                min_relative_prominence_percent: options.min_relative_prominence_percent,
            });
        }

        let clearances = self.clearance().into_radii_pixels();
        let mut activation_order: Vec<_> = self
            .cells
            .iter()
            .enumerate()
            .filter_map(|(index, cell)| cell.walkable.then_some(index))
            .collect();
        activation_order.sort_unstable_by(|left, right| {
            clearances[*right]
                .cmp(&clearances[*left])
                .then_with(|| left.cmp(right))
        });

        let mut active = vec![false; self.cells.len()];
        let mut markers = vec![false; self.cells.len()];
        let mut components = DisjointSet::new(self.cells.len());
        for index in activation_order {
            active[index] = true;
            components.activate(index);
            let level = clearances[index];
            for (neighbor, _) in self.neighbors(self.position(index)) {
                let neighbor_index = self
                    .index(neighbor)
                    .expect("TerrainGrid neighbor must remain in bounds");
                if active[neighbor_index] {
                    components.union(
                        index,
                        neighbor_index,
                        level,
                        options,
                        &clearances,
                        &mut markers,
                    );
                }
            }
        }

        for (index, is_active) in active.iter().copied().enumerate() {
            if is_active {
                let root = components.find(index);
                markers[components.peak(root)] = true;
            }
        }
        let marker_indices: Vec<_> = markers
            .iter()
            .enumerate()
            .filter_map(|(index, marker)| marker.then_some(index))
            .collect();
        // The flood needs only marker indices and clearances. Release the marker-stage buffers
        // before it grows its heap, which keeps peak working memory bounded on 1024x1024 grids.
        drop(active);
        drop(components);
        drop(markers);

        let (labels, cell_counts) = partition(self, &clearances, &marker_indices);
        let regions = marker_indices
            .into_iter()
            .enumerate()
            .map(|(offset, peak_index)| Region {
                id: u32::try_from(offset + 1)
                    .expect("validated grid has at most one million cells"),
                peak: self.position(peak_index),
                peak_clearance_pixels: clearances[peak_index],
                cell_count: cell_counts[offset],
            })
            .collect();
        let passages = passages(self, &clearances, &labels);

        Ok(RegionAnalysis {
            width: self.width,
            height: self.height,
            labels,
            regions,
            passages,
        })
    }
}

struct DisjointSet {
    parent: Vec<usize>,
    rank: Vec<u8>,
    peak: Vec<usize>,
}

impl DisjointSet {
    fn new(count: usize) -> Self {
        Self {
            parent: (0..count).collect(),
            rank: vec![0; count],
            peak: vec![usize::MAX; count],
        }
    }

    fn activate(&mut self, index: usize) {
        self.parent[index] = index;
        self.rank[index] = 0;
        self.peak[index] = index;
    }

    fn find(&mut self, index: usize) -> usize {
        let mut root = index;
        while self.parent[root] != root {
            root = self.parent[root];
        }
        let mut current = index;
        while self.parent[current] != current {
            let parent = self.parent[current];
            self.parent[current] = root;
            current = parent;
        }
        root
    }

    fn peak(&self, root: usize) -> usize {
        self.peak[root]
    }

    fn union(
        &mut self,
        left: usize,
        right: usize,
        level: u16,
        options: &RegionOptions,
        clearances: &[u16],
        markers: &mut [bool],
    ) {
        let left_root = self.find(left);
        let right_root = self.find(right);
        if left_root == right_root {
            return;
        }
        let left_peak = self.peak[left_root];
        let right_peak = self.peak[right_root];
        let (winning_peak, losing_peak) = if dominates(left_peak, right_peak, clearances) {
            (left_peak, right_peak)
        } else {
            (right_peak, left_peak)
        };
        if retains_peak(clearances[losing_peak], level, options) {
            markers[losing_peak] = true;
        }

        let (parent, child) = match self.rank[left_root].cmp(&self.rank[right_root]) {
            std::cmp::Ordering::Less => (right_root, left_root),
            std::cmp::Ordering::Greater => (left_root, right_root),
            std::cmp::Ordering::Equal => {
                self.rank[left_root] = self.rank[left_root].saturating_add(1);
                (left_root, right_root)
            }
        };
        self.parent[child] = parent;
        self.peak[parent] = winning_peak;
    }
}

fn retains_peak(peak_height: u16, merge_level: u16, options: &RegionOptions) -> bool {
    let prominence = peak_height.saturating_sub(merge_level);
    prominence >= options.min_prominence_pixels
        && u32::from(prominence) * 100
            >= u32::from(peak_height) * u32::from(options.min_relative_prominence_percent)
}

fn dominates(left: usize, right: usize, clearances: &[u16]) -> bool {
    clearances[left] > clearances[right] || (clearances[left] == clearances[right] && left < right)
}

fn partition(terrain: &TerrainGrid, clearances: &[u16], markers: &[usize]) -> (Vec<u32>, Vec<u32>) {
    let count = clearances.len();
    let mut labels = vec![0; count];
    let mut bottlenecks = vec![0; count];
    let mut plateau_costs = vec![u32::MAX; count];
    let mut owners = vec![u32::MAX; count];
    let mut cell_counts = vec![0; markers.len()];
    let mut open = BinaryHeap::new();

    for (offset, &peak) in markers.iter().enumerate() {
        let id = u32::try_from(offset + 1).expect("validated grid has at most one million cells");
        bottlenecks[peak] = clearances[peak];
        plateau_costs[peak] = 0;
        owners[peak] = id;
        open.push((clearances[peak], Reverse(0_u32), Reverse(id), Reverse(peak)));
    }

    while let Some((bottleneck, Reverse(plateau_cost), Reverse(owner), Reverse(index))) = open.pop()
    {
        if labels[index] != 0
            || bottlenecks[index] != bottleneck
            || plateau_costs[index] != plateau_cost
            || owners[index] != owner
        {
            continue;
        }
        labels[index] = owner;
        let owner_offset = usize::try_from(owner - 1).expect("one-based region ID fits usize");
        cell_counts[owner_offset] += 1;

        for (neighbor, step_cost) in terrain.neighbors(terrain.position(index)) {
            let neighbor_index = terrain
                .index(neighbor)
                .expect("TerrainGrid neighbor must remain in bounds");
            if labels[neighbor_index] != 0 {
                continue;
            }
            let candidate_bottleneck = bottleneck.min(clearances[neighbor_index]);
            let candidate_plateau_cost = if candidate_bottleneck < bottleneck {
                0
            } else {
                plateau_cost.saturating_add(
                    u32::try_from(step_cost).expect("TerrainGrid step costs fit u32"),
                )
            };
            if !better_flood_candidate(
                candidate_bottleneck,
                candidate_plateau_cost,
                owner,
                bottlenecks[neighbor_index],
                plateau_costs[neighbor_index],
                owners[neighbor_index],
            ) {
                continue;
            }
            bottlenecks[neighbor_index] = candidate_bottleneck;
            plateau_costs[neighbor_index] = candidate_plateau_cost;
            owners[neighbor_index] = owner;
            open.push((
                candidate_bottleneck,
                Reverse(candidate_plateau_cost),
                Reverse(owner),
                Reverse(neighbor_index),
            ));
        }
    }

    (labels, cell_counts)
}

fn better_flood_candidate(
    bottleneck: u16,
    plateau_cost: u32,
    owner: u32,
    prior_bottleneck: u16,
    prior_plateau_cost: u32,
    prior_owner: u32,
) -> bool {
    bottleneck > prior_bottleneck
        || (bottleneck == prior_bottleneck
            && (plateau_cost < prior_plateau_cost
                || (plateau_cost == prior_plateau_cost && owner < prior_owner)))
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct PassageChoice {
    endpoints: [usize; 2],
    clearance_radius_pixels: u16,
}

fn passages(terrain: &TerrainGrid, clearances: &[u16], labels: &[u32]) -> Vec<Passage> {
    let mut choices = BTreeMap::new();
    for (index, &label) in labels.iter().enumerate() {
        if label == 0 {
            continue;
        }
        for (neighbor, _) in terrain.neighbors(terrain.position(index)) {
            let neighbor_index = terrain
                .index(neighbor)
                .expect("TerrainGrid neighbor must remain in bounds");
            if index >= neighbor_index {
                continue;
            }
            let neighbor_label = labels[neighbor_index];
            if neighbor_label == 0 || label == neighbor_label {
                continue;
            }
            let (regions, endpoints) = if label < neighbor_label {
                ([label, neighbor_label], [index, neighbor_index])
            } else {
                ([neighbor_label, label], [neighbor_index, index])
            };
            let candidate = PassageChoice {
                endpoints,
                clearance_radius_pixels: clearances[index].min(clearances[neighbor_index]),
            };
            choices
                .entry(regions)
                .and_modify(|existing: &mut PassageChoice| {
                    if candidate.clearance_radius_pixels > existing.clearance_radius_pixels
                        || (candidate.clearance_radius_pixels == existing.clearance_radius_pixels
                            && candidate.endpoints < existing.endpoints)
                    {
                        *existing = candidate;
                    }
                })
                .or_insert(candidate);
        }
    }
    choices
        .into_iter()
        .map(|(regions, choice)| Passage {
            regions,
            endpoints: choice.endpoints.map(|index| terrain.position(index)),
            clearance_radius_pixels: choice.clearance_radius_pixels,
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, VecDeque};

    use super::*;
    use crate::TerrainCell;

    fn grid(width: u32, height: u32, blocked: impl Fn(u32, u32) -> bool) -> TerrainGrid {
        let mut cells = Vec::with_capacity((width * height) as usize);
        for y in 0..height {
            for x in 0..width {
                cells.push(TerrainCell {
                    walkable: !blocked(x, y),
                    terrain_buildable: true,
                    elevation: 0,
                    ramp: false,
                });
            }
        }
        TerrainGrid::from_cells(width, height, cells).unwrap()
    }

    fn label_at(analysis: &RegionAnalysis, width: u32, x: u32, y: u32) -> u32 {
        analysis.labels()[(y * width + x) as usize]
    }

    fn assert_region_connected(terrain: &TerrainGrid, analysis: &RegionAnalysis, region: Region) {
        let width = terrain.width();
        let mut seen = vec![false; analysis.labels().len()];
        let peak_index = (region.peak.y * width + region.peak.x) as usize;
        let mut queue = VecDeque::from([peak_index]);
        seen[peak_index] = true;
        let mut visited = 0_u32;
        while let Some(index) = queue.pop_front() {
            visited += 1;
            for (neighbor, _) in terrain.neighbors(terrain.position(index)) {
                let neighbor_index = terrain.index(neighbor).unwrap();
                if !seen[neighbor_index] && analysis.labels()[neighbor_index] == region.id {
                    seen[neighbor_index] = true;
                    queue.push_back(neighbor_index);
                }
            }
        }
        assert_eq!(
            visited, region.cell_count,
            "region {} must be connected",
            region.id
        );
    }

    #[test]
    fn all_blocked_and_option_limits_are_explicit() {
        let terrain = grid(3, 2, |_, _| true);
        let analysis = terrain.regions(&RegionOptions::default()).unwrap();
        assert_eq!(analysis.width(), 3);
        assert_eq!(analysis.height(), 2);
        assert_eq!(analysis.labels(), &[0; 6]);
        assert!(analysis.regions().is_empty());
        assert!(analysis.passages().is_empty());
        assert_eq!(analysis.region_at(WalkPosition { x: 0, y: 0 }), None);
        assert_eq!(analysis.region_at(WalkPosition { x: 3, y: 0 }), None);
        assert_eq!(
            terrain.regions(&RegionOptions {
                min_prominence_pixels: 7,
                ..RegionOptions::default()
            }),
            Err(RegionError::InvalidProminence {
                min_prominence_pixels: 7,
            })
        );
        assert!(
            terrain
                .regions(&RegionOptions {
                    min_prominence_pixels: MIN_PROMINENCE_PIXELS,
                    ..RegionOptions::default()
                })
                .is_ok()
        );
        assert!(
            terrain
                .regions(&RegionOptions {
                    min_prominence_pixels: MAX_PROMINENCE_PIXELS,
                    ..RegionOptions::default()
                })
                .is_ok()
        );
        assert_eq!(
            terrain.regions(&RegionOptions {
                min_prominence_pixels: MAX_PROMINENCE_PIXELS + 1,
                ..RegionOptions::default()
            }),
            Err(RegionError::InvalidProminence {
                min_prominence_pixels: MAX_PROMINENCE_PIXELS + 1,
            })
        );
        assert_eq!(
            terrain.regions(&RegionOptions {
                min_relative_prominence_percent: 101,
                ..RegionOptions::default()
            }),
            Err(RegionError::InvalidRelativeProminence {
                min_relative_prominence_percent: 101,
            })
        );
    }

    #[test]
    fn open_flat_is_one_region_and_every_walkable_cell_has_a_label() {
        let terrain = grid(9, 7, |_, _| false);
        let analysis = terrain.regions(&RegionOptions::default()).unwrap();
        assert_eq!(analysis.regions().len(), 1);
        assert_eq!(analysis.regions()[0].id, 1);
        assert_eq!(analysis.regions()[0].cell_count, 63);
        assert_eq!(analysis.region_at(analysis.regions()[0].peak), Some(1));
        assert!(analysis.labels().iter().all(|&label| label == 1));
        assert!(analysis.passages().is_empty());
        assert_region_connected(&terrain, &analysis, analysis.regions()[0]);
    }

    #[test]
    fn diagonal_pinch_is_not_a_connection() {
        let terrain = grid(3, 3, |x, y| !matches!((x, y), (0, 0) | (1, 1)));
        let analysis = terrain.regions(&RegionOptions::default()).unwrap();
        assert_eq!(analysis.regions().len(), 2);
        assert_ne!(label_at(&analysis, 3, 0, 0), label_at(&analysis, 3, 1, 1));
        assert_eq!(label_at(&analysis, 3, 2, 2), 0);
        assert!(analysis.passages().is_empty());
        for &region in analysis.regions() {
            assert_region_connected(&terrain, &analysis, region);
        }
    }

    fn rooms_and_corridor() -> TerrainGrid {
        grid(15, 9, |x, y| {
            let left_room = (1..=5).contains(&x) && (1..=7).contains(&y);
            let right_room = (9..=13).contains(&x) && (1..=7).contains(&y);
            let corridor = (6..=8).contains(&x) && y == 4;
            !(left_room || right_room || corridor)
        })
    }

    #[test]
    fn equal_height_plateaus_and_peaks_use_row_major_dominance() {
        let plateau = grid(6, 6, |_, _| false);
        let plateau_analysis = plateau.regions(&RegionOptions::default()).unwrap();
        assert_eq!(plateau_analysis.regions().len(), 1);
        assert_eq!(
            plateau_analysis.regions()[0].peak,
            WalkPosition { x: 2, y: 2 },
            "same-height plateau births must collapse to the first row-major cell"
        );

        let terrain = rooms_and_corridor();
        let fine = terrain
            .regions(&RegionOptions {
                min_prominence_pixels: 8,
                min_relative_prominence_percent: 0,
            })
            .unwrap();
        let coarse = terrain
            .regions(&RegionOptions {
                min_prominence_pixels: 24,
                min_relative_prominence_percent: 0,
            })
            .unwrap();
        assert_eq!(fine.regions().len(), 2);
        assert_eq!(
            fine.regions()[0].peak_clearance_pixels,
            fine.regions()[1].peak_clearance_pixels,
            "the room peaks are deliberately equal-height"
        );
        assert_eq!(coarse.regions().len(), 1);
        assert_eq!(
            coarse.regions()[0].peak,
            fine.regions()[0].peak,
            "the first row-major equal-height peak survives coarse prominence"
        );
    }

    #[test]
    fn prominence_separates_broad_rooms_across_a_narrow_corridor() {
        let terrain = rooms_and_corridor();
        let low = terrain
            .regions(&RegionOptions {
                min_prominence_pixels: 8,
                min_relative_prominence_percent: 0,
            })
            .unwrap();
        let high = terrain
            .regions(&RegionOptions {
                min_prominence_pixels: 24,
                min_relative_prominence_percent: 0,
            })
            .unwrap();
        assert_eq!(low.regions().len(), 2);
        assert_eq!(high.regions().len(), 1);
        assert!(
            low.labels()
                .iter()
                .zip(terrain.cells())
                .all(|(&label, cell)| { (label == 0) == !cell.walkable })
        );
        for &region in low.regions() {
            assert_region_connected(&terrain, &low, region);
        }
    }

    #[test]
    fn passages_are_valid_representatives_for_every_touching_pair() {
        let terrain = rooms_and_corridor();
        let analysis = terrain
            .regions(&RegionOptions {
                min_prominence_pixels: 8,
                min_relative_prominence_percent: 0,
            })
            .unwrap();
        assert_eq!(analysis.passages().len(), 1);
        let passage = analysis.passages()[0];
        assert_eq!(passage.regions, [1, 2]);
        assert_eq!(
            analysis.region_at(passage.endpoints[0]),
            Some(passage.regions[0])
        );
        assert_eq!(
            analysis.region_at(passage.endpoints[1]),
            Some(passage.regions[1])
        );
        assert!(
            terrain
                .neighbors(passage.endpoints[0])
                .any(|(neighbor, _)| neighbor == passage.endpoints[1])
        );
        let expected = terrain
            .clearance()
            .radius_pixels(passage.endpoints[0])
            .unwrap()
            .min(
                terrain
                    .clearance()
                    .radius_pixels(passage.endpoints[1])
                    .unwrap(),
            );
        assert_eq!(passage.clearance_radius_pixels, expected);

        let mut pairs = BTreeMap::new();
        for (index, &label) in analysis.labels().iter().enumerate() {
            if label == 0 {
                continue;
            }
            for (neighbor, _) in terrain.neighbors(terrain.position(index)) {
                let neighbor_index = terrain.index(neighbor).unwrap();
                if index < neighbor_index {
                    let other = analysis.labels()[neighbor_index];
                    if other != 0 && other != label {
                        pairs.insert([label.min(other), label.max(other)], ());
                    }
                }
            }
        }
        assert_eq!(
            analysis
                .passages()
                .iter()
                .map(|passage| passage.regions)
                .collect::<Vec<_>>(),
            pairs.into_keys().collect::<Vec<_>>()
        );
    }

    fn rooms_with_multiple_crossings() -> TerrainGrid {
        grid(19, 11, |x, y| {
            let left_room = (1..=6).contains(&x) && (1..=9).contains(&y);
            let right_room = (12..=17).contains(&x) && (1..=9).contains(&y);
            let corridor = (7..=11).contains(&x) && (4..=6).contains(&y);
            !(left_room || right_room || corridor)
        })
    }

    #[test]
    fn passage_prefers_best_clearance_then_lexicographic_endpoint_indices() {
        let terrain = rooms_with_multiple_crossings();
        let analysis = terrain
            .regions(&RegionOptions {
                min_prominence_pixels: 8,
                min_relative_prominence_percent: 0,
            })
            .unwrap();
        let clearances = terrain.clearance();
        let mut crossings: BTreeMap<[u32; 2], Vec<(u16, [usize; 2])>> = BTreeMap::new();
        for (index, &label) in analysis.labels().iter().enumerate() {
            if label == 0 {
                continue;
            }
            for (neighbor, _) in terrain.neighbors(terrain.position(index)) {
                let neighbor_index = terrain.index(neighbor).unwrap();
                if index >= neighbor_index {
                    continue;
                }
                let other = analysis.labels()[neighbor_index];
                if other == 0 || other == label {
                    continue;
                }
                let (regions, endpoints) = if label < other {
                    ([label, other], [index, neighbor_index])
                } else {
                    ([other, label], [neighbor_index, index])
                };
                let radius = clearances
                    .radius_pixels(terrain.position(index))
                    .unwrap()
                    .min(
                        clearances
                            .radius_pixels(terrain.position(neighbor_index))
                            .unwrap(),
                    );
                crossings
                    .entry(regions)
                    .or_default()
                    .push((radius, endpoints));
            }
        }
        let (regions, candidates) = crossings
            .iter()
            .find(|(_, candidates)| candidates.len() > 1)
            .expect("wide corridor must create multiple valid crossings");
        let mut expected = candidates[0];
        for &candidate in &candidates[1..] {
            if candidate.0 > expected.0 || (candidate.0 == expected.0 && candidate.1 < expected.1) {
                expected = candidate;
            }
        }
        let passage = analysis
            .passages()
            .iter()
            .find(|passage| passage.regions == *regions)
            .expect("every crossing label pair has one representative passage");
        assert_eq!(passage.clearance_radius_pixels, expected.0);
        assert_eq!(
            passage
                .endpoints
                .map(|position| terrain.index(position).unwrap()),
            expected.1
        );
    }

    fn wide_low_contrast_lobes() -> TerrainGrid {
        grid(80, 40, |x, y| {
            let left = (1..=34).contains(&x) && (1..=38).contains(&y);
            let right = (45..=78).contains(&x) && (1..=38).contains(&y);
            let bridge = (35..=44).contains(&x) && (7..=32).contains(&y);
            !(left || right || bridge)
        })
    }

    #[test]
    fn relative_prominence_merges_wide_low_contrast_lobes() {
        let terrain = wide_low_contrast_lobes();
        let absolute_only = terrain
            .regions(&RegionOptions {
                min_prominence_pixels: 32,
                min_relative_prominence_percent: 0,
            })
            .unwrap();
        let relative = terrain
            .regions(&RegionOptions {
                min_prominence_pixels: 32,
                min_relative_prominence_percent: 40,
            })
            .unwrap();
        assert_eq!(absolute_only.regions().len(), 2);
        assert_eq!(relative.regions().len(), 1);
        assert!(
            absolute_only
                .regions()
                .iter()
                .all(|region| region.peak_clearance_pixels == 132)
        );
    }

    #[test]
    fn relative_prominence_keeps_small_rooms_separated_by_a_narrow_door() {
        let terrain = rooms_and_corridor();
        let analysis = terrain
            .regions(&RegionOptions {
                min_prominence_pixels: 8,
                min_relative_prominence_percent: 40,
            })
            .unwrap();
        assert_eq!(analysis.regions().len(), 2);
        assert_eq!(analysis.passages().len(), 1);
    }

    fn asymmetric_doorway_rooms() -> TerrainGrid {
        grid(50, 25, |x, y| {
            let large = (1..=27).contains(&x) && (1..=23).contains(&y);
            let small = (31..=45).contains(&x) && (7..=17).contains(&y);
            let door = (28..=30).contains(&x) && (11..=13).contains(&y);
            !(large || small || door)
        })
    }

    #[test]
    fn local_plateau_distance_cuts_the_asymmetric_doorway() {
        let terrain = asymmetric_doorway_rooms();
        let analysis = terrain
            .regions(&RegionOptions {
                min_prominence_pixels: 32,
                min_relative_prominence_percent: 40,
            })
            .unwrap();
        assert_eq!(analysis.regions().len(), 2);
        let passage = analysis.passages().first().copied().unwrap();
        assert_eq!(passage.regions, [1, 2]);
        assert!(
            passage
                .endpoints
                .iter()
                .all(|point| (28..=30).contains(&point.x) && (11..=13).contains(&point.y))
        );
        // The former cumulative seed-distance tie-break let the small room claim 38 cells
        // along the large room's far wall, including (26, 12) beside the entrance.
        for y in 1..=23 {
            for x in 1..=27 {
                assert_eq!(
                    analysis.region_at(WalkPosition { x, y }),
                    Some(1),
                    "large-room cell {x},{y} must remain before the doorway cut"
                );
            }
        }
        for y in 7..=17 {
            for x in 31..=45 {
                assert_eq!(
                    analysis.region_at(WalkPosition { x, y }),
                    Some(2),
                    "small-room cell {x},{y} must remain beyond the doorway cut"
                );
            }
        }
    }

    #[test]
    fn repeated_analysis_is_deterministic_and_ids_are_contiguous() {
        let terrain = rooms_and_corridor();
        let options = RegionOptions {
            min_prominence_pixels: 8,
            min_relative_prominence_percent: 0,
        };
        let first = terrain.regions(&options).unwrap();
        let second = terrain.regions(&options).unwrap();
        assert_eq!(first, second);
        for (offset, region) in first.regions().iter().enumerate() {
            assert_eq!(region.id, u32::try_from(offset + 1).unwrap());
            assert_eq!(first.region_at(region.peak), Some(region.id));
            assert_region_connected(&terrain, &first, *region);
        }
    }
}
