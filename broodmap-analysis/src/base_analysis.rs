//! One-call resource-base analysis for stock melee map data.
//!
//! [`discover_bases`] is game-agnostic and expects normalized inputs. This module supplies the
//! conventional melee adapter: it resolves CHK terrain, classifies stock mineral fields
//! (176-178) and vespene geysers (188) from UNIT/THG2 using caller-supplied `units.dat`, keeps
//! every other static melee object as a non-resource obstacle, and collects start markers (214).
//! Mods with different resource IDs should normalize their own inputs and call
//! [`discover_bases`] directly.

use broodmap::{
    Chk,
    chk::{
        placed_units::{PlacedUnit, PlacedUnitsError, UnitState},
        sprites::{Sprite, SpriteError, SpriteFlags},
        terrain::TerrainError,
    },
};
use broodmap_formats::{Cv5, UnitsDat, Vf4};
use thiserror::Error;

use crate::{
    AnalysisError, BaseDiscovery, BaseError, BaseSearchOptions, PixelPosition, PixelRect,
    ResourceKind, ResourceNode, StaticObstacle, TerrainGrid, discover_bases,
    melee_obstacle_objects,
};

const START_LOCATION: u16 = 214;
const RESOURCE: u32 = 0x0000_2000;
const FLYER: u32 = 0x0000_0004;
const NEUTRAL_OWNER: u8 = 11;

/// Game tables needed to analyze one map. `cv5` and `vf4` must belong to the map's tileset;
/// mismatched tables cannot be detected from their bytes alone.
#[derive(Debug, Copy, Clone)]
pub struct AnalysisAssets<'a> {
    pub cv5: &'a Cv5,
    pub vf4: &'a Vf4,
    pub units: &'a UnitsDat,
}

/// A start-location marker from the UNIT chunk.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct StartLocation {
    pub position: PixelPosition,
    /// The marker's raw UNIT owner (zero-based player slot), if recorded.
    pub owner: Option<u8>,
}

/// Normalized melee inputs for [`discover_bases`], in UNIT then THG2 order.
#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct MeleeBaseInputs {
    /// Mineral fields and raw geysers. UNIT amounts are retained; THG2 amounts are unknown.
    pub resources: Vec<ResourceNode>,
    /// Static melee objects other than classified resources.
    pub obstacles: Vec<StaticObstacle>,
    pub starts: Vec<StartLocation>,
    /// Stock resource placements whose `units.dat` entry is not a grounded resource container
    /// with non-negative collision extents. They are not resources; any static-object policy
    /// still applies to them as ordinary obstacles.
    pub ignored_resources: usize,
}

impl MeleeBaseInputs {
    /// Start positions in input order, as [`discover_bases`] expects them.
    pub fn start_positions(&self) -> Vec<PixelPosition> {
        self.starts.iter().map(|start| start.position).collect()
    }

    pub fn mineral_count(&self) -> usize {
        self.resource_count(ResourceKind::Mineral)
    }

    pub fn gas_count(&self) -> usize {
        self.resource_count(ResourceKind::Gas)
    }

    fn resource_count(&self, kind: ResourceKind) -> usize {
        self.resources
            .iter()
            .filter(|resource| resource.kind == kind)
            .count()
    }
}

/// Classifies stock melee resources, non-resource static obstacles, and start markers.
///
/// UNIT resources follow the static obstacle adapter's grounded-resource policy regardless of
/// owner; hallucinated and in-transit placements are skipped. THG2 resources need owner 11 and
/// must be unit sprites. Non-resource objects come from [`melee_obstacle_objects`], so they obey
/// its owner, flyer, and collision-bound rules.
pub fn melee_base_inputs(
    units: &[PlacedUnit],
    sprites: &[Sprite],
    definitions: &UnitsDat,
) -> MeleeBaseInputs {
    // The solver adds resource occupancy itself so it can omit only explicitly clearable
    // mineral nodes. Keep every non-resource object, including overlapping rectangles.
    let other_units: Vec<_> = units
        .iter()
        .filter(|unit| resource_extents(unit.unit_id, definitions).is_none())
        .copied()
        .collect();
    let other_sprites: Vec<_> = sprites
        .iter()
        .filter(|sprite| resource_extents(sprite.id, definitions).is_none())
        .copied()
        .collect();
    let mut inputs = MeleeBaseInputs {
        obstacles: melee_obstacle_objects(&other_units, &other_sprites, definitions),
        ..MeleeBaseInputs::default()
    };
    let mut add_resource = |id: u16, x: u16, y: u16, amount: Option<u32>| {
        if !is_stock_resource_id(id) {
            return;
        }
        match resource_node(id, x, y, amount, definitions) {
            Some(node) => inputs.resources.push(node),
            None => inputs.ignored_resources += 1,
        }
    };
    let mut starts = Vec::new();
    for unit in units {
        if unit.unit_id == START_LOCATION {
            starts.push(StartLocation {
                position: PixelPosition {
                    x: u32::from(unit.x),
                    y: u32::from(unit.y),
                },
                owner: unit.owner,
            });
        } else if !unit
            .state
            .intersects(UnitState::HALLUCINATED | UnitState::IN_TRANSIT)
        {
            add_resource(unit.unit_id, unit.x, unit.y, unit.resource_amount);
        }
    }
    for sprite in sprites {
        if sprite.owner == NEUTRAL_OWNER && !sprite.flags.contains(SpriteFlags::DRAW_AS_SPRITE) {
            add_resource(sprite.id, sprite.x, sprite.y, None);
        }
    }
    inputs.starts = starts;
    inputs
}

fn is_stock_resource_id(id: u16) -> bool {
    matches!(id, 176..=178 | 188)
}

/// The kind and raw inclusive collision extents of a stock grounded resource container.
fn resource_extents(id: u16, definitions: &UnitsDat) -> Option<(ResourceKind, [i16; 4])> {
    let kind = match id {
        176..=178 => ResourceKind::Mineral,
        188 => ResourceKind::Gas,
        _ => return None,
    };
    let entry = definitions.entry(id)?;
    // Grounded resource containers only, matching the static obstacle adapter.
    if entry.special_ability_flags & (RESOURCE | FLYER) != RESOURCE {
        return None;
    }
    let (left, top, right, bottom) = entry.bounds;
    let extents = [left, top, right, bottom];
    extents
        .iter()
        .all(|&extent| extent >= 0)
        .then_some((kind, extents))
}

fn resource_node(
    id: u16,
    x: u16,
    y: u16,
    amount: Option<u32>,
    definitions: &UnitsDat,
) -> Option<ResourceNode> {
    let (kind, [left, top, right, bottom]) = resource_extents(id, definitions)?;
    // Raw units.dat extents are inclusive; add one for half-open bounds. Amount marks potential
    // clearing prerequisites; current routing still includes the node.
    Some(ResourceNode {
        bounds: PixelRect {
            left: i32::from(x) - i32::from(left),
            top: i32::from(y) - i32::from(top),
            right: i32::from(x) + i32::from(right) + 1,
            bottom: i32::from(y) + i32::from(bottom) + 1,
        },
        kind,
        amount,
    })
}

/// Why base discovery did not run. These are work limits, not malformed input: the map's
/// terrain and normalized inputs are still reported.
#[derive(Debug, Error, Copy, Clone, Eq, PartialEq)]
pub enum DiscoverySkipReason {
    #[error(
        "dense resource field: {actual} resources exceed the {maximum}-resource discovery limit"
    )]
    DenseResourceField { maximum: usize, actual: usize },
    #[error("{actual} start locations exceed the {maximum}-start discovery limit")]
    TooManyStarts { maximum: usize, actual: usize },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BaseDiscoveryOutcome {
    Complete(BaseDiscovery),
    Skipped(DiscoverySkipReason),
}

impl BaseDiscoveryOutcome {
    /// The discovered bases, or `None` when discovery was skipped.
    pub fn discovery(&self) -> Option<&BaseDiscovery> {
        match self {
            Self::Complete(discovery) => Some(discovery),
            Self::Skipped(_) => None,
        }
    }
}

/// Resolved terrain, normalized inputs, and base discovery for one map.
///
/// Indices in the discovery refer to `inputs.resources`, `inputs.obstacles`, and
/// `inputs.starts`. `terrain` is the unobstructed grid; apply the object rectangles with
/// [`TerrainGrid::with_obstacles`] for routes that respect them.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BaseAnalysis {
    pub terrain: TerrainGrid,
    pub inputs: MeleeBaseInputs,
    pub discovery: BaseDiscoveryOutcome,
}

impl BaseAnalysis {
    /// Runs discovery on already-normalized inputs. Exceeding the solver's resource or start
    /// limits produces [`BaseDiscoveryOutcome::Skipped`]; other discovery errors (an unaligned
    /// grid, or a depot footprint that is empty or larger than the map) are returned.
    pub fn from_inputs(
        terrain: TerrainGrid,
        inputs: MeleeBaseInputs,
        options: &BaseSearchOptions,
    ) -> Result<Self, BaseError> {
        let discovery = match discover_bases(
            &terrain,
            &inputs.resources,
            &inputs.obstacles,
            &inputs.start_positions(),
            options,
        ) {
            Ok(discovery) => BaseDiscoveryOutcome::Complete(discovery),
            Err(BaseError::TooManyResources { maximum, actual }) => {
                BaseDiscoveryOutcome::Skipped(DiscoverySkipReason::DenseResourceField {
                    maximum,
                    actual,
                })
            }
            Err(BaseError::TooManyStarts { maximum, actual }) => {
                BaseDiscoveryOutcome::Skipped(DiscoverySkipReason::TooManyStarts {
                    maximum,
                    actual,
                })
            }
            Err(
                error @ (BaseError::GridNotTileAligned { .. } | BaseError::InvalidFootprint { .. }),
            ) => return Err(error),
        };
        Ok(Self {
            terrain,
            inputs,
            discovery,
        })
    }
}

/// An error that prevents analyzing a map's bases at all.
#[derive(Debug, Error, Copy, Clone, Eq, PartialEq)]
pub enum BaseAnalysisError {
    #[error("map terrain unavailable: {0}")]
    Terrain(TerrainError),
    #[error("terrain resolution failed: {0}")]
    Grid(#[from] AnalysisError),
    #[error("base discovery failed: {0}")]
    Discovery(#[from] BaseError),
}

/// Resolves a map's terrain, normalizes its melee objects, and discovers resource bases.
///
/// Missing UNIT or THG2 chunks contribute no objects. A map whose resource or start count exceeds
/// discovery's limits is not an error: the result reports why discovery was skipped alongside
/// its terrain, starts, and resources. Missing terrain, CV5/VF4 entries absent for a map tile,
/// and invalid `options` are errors.
pub fn analyze_bases(
    chk: &Chk,
    assets: AnalysisAssets<'_>,
    options: &BaseSearchOptions,
) -> Result<BaseAnalysis, BaseAnalysisError> {
    let terrain = chk
        .terrain()
        .map_err(|&error| BaseAnalysisError::Terrain(error))?;
    let terrain = TerrainGrid::from_terrain(terrain, assets.cv5, assets.vf4)?;
    // Match explicitly so future parser error variants cannot silently become object-free maps.
    let units = match chk.placed_units() {
        Ok(units) => units.as_slice(),
        Err(PlacedUnitsError::ChunkMissing) => &[],
    };
    let sprites = match chk.sprites() {
        Ok(sprites) => sprites.as_slice(),
        Err(SpriteError::ChunkMissing) => &[],
    };
    let inputs = melee_base_inputs(units, sprites, assets.units);
    Ok(BaseAnalysis::from_inputs(terrain, inputs, options)?)
}
