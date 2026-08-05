#![no_main]

use broodmap::chk::placed_units::{PlacedUnit, UnitInstanceId, UnitState};
use broodmap::chk::player_colors::{PlayerColor, PlayerColors};
use broodmap::chk::sprites::{Sprite, SpriteFlags};
use broodmap::chk::terrain::{TerrainTileIds, TileId};
use broodmap::chk::tileset::Tileset;
use broodmap_render::{MinimapOptions, build_minimap_table, render_minimap};
use libfuzzer_sys::fuzz_target;

/// Map width/height are each capped to this range (in tiles), matching `render_terrain`'s own
/// bound: a render never does more than `MAX_MAP_DIM * MAX_MAP_DIM` tile blits regardless of
/// input.
const MAX_MAP_DIM: u8 = 8;

/// How many synthetic units/sprites are carved out of the input, each from a small fixed-size
/// record -- bounds the loop regardless of `-max_len`.
const MAX_UNITS: usize = 4;
const MAX_SPRITES: usize = 4;

fuzz_target!(|data: &[u8]| {
    // Part 1: `build_minimap_table` is the dev-time generator's whole reason for being fuzzable
    // at all -- it's the one function in `crate::minimap` that parses untrusted bytes (the
    // classic CV5/VX4EX/VR4/WPE tileset files). Deterministically split the input into four
    // roughly-equal quarters so the same fuzzer input always builds the same four byte slices;
    // every parser involved is permissive (garbage in, garbage/error out, never a panic), so
    // short/truncated/nonsensical quarters are exactly the interesting case.
    let quarter = data.len() / 4;
    let (cv5, rest) = data.split_at(quarter.min(data.len()));
    let (vx4ex, rest) = rest.split_at(quarter.min(rest.len()));
    let (vr4, rest) = rest.split_at(quarter.min(rest.len()));
    let wpe = rest;
    let _ = build_minimap_table(cv5, vx4ex, vr4, wpe);

    // Part 2: the zero-asset render path itself, over fuzzed CHK terrain plus a handful of
    // fuzzed units/sprites, through the real committed Jungle table (the public API has no seam
    // to inject a synthetic one -- `render_minimap_with_table` is crate-private; the committed
    // table exercises the exact same terrain/creep/dot code paths a synthetic one would).
    let Some((&w_byte, rest)) = data.split_first() else {
        return;
    };
    let Some((&h_byte, rest)) = rest.split_first() else {
        return;
    };
    let width = (w_byte % MAX_MAP_DIM) as usize + 1;
    let height = (h_byte % MAX_MAP_DIM) as usize + 1;
    let tile_count = width * height;

    let mut cursor = rest;
    let mut tiles = Vec::with_capacity(tile_count);
    for _ in 0..tile_count {
        let (lo, r) = take_byte(cursor);
        let (hi, r) = take_byte(r);
        cursor = r;
        tiles.push(TileId(u16::from_le_bytes([lo, hi])));
    }
    let terrain = TerrainTileIds {
        width,
        height,
        tiles,
    };

    let mut units = Vec::with_capacity(MAX_UNITS);
    for i in 0..MAX_UNITS {
        let (unit_id_lo, r) = take_byte(cursor);
        let (unit_id_hi, r) = take_byte(r);
        let (x_lo, r) = take_byte(r);
        let (x_hi, r) = take_byte(r);
        let (y_lo, r) = take_byte(r);
        let (y_hi, r) = take_byte(r);
        let (owner_byte, r) = take_byte(r);
        let (state_byte, r) = take_byte(r);
        cursor = r;
        units.push(PlacedUnit {
            instance_id: UnitInstanceId(i as u32),
            x: u16::from_le_bytes([x_lo, x_hi]),
            y: u16::from_le_bytes([y_lo, y_hi]),
            unit_id: u16::from_le_bytes([unit_id_lo, unit_id_hi]),
            owner: if owner_byte % 2 == 0 {
                None
            } else {
                Some(owner_byte)
            },
            hp_percent: None,
            shield_percent: None,
            energy_percent: None,
            resource_amount: None,
            hangar_count: None,
            state: UnitState::from_bits_truncate(state_byte as u16),
            linked_id: None,
        });
    }

    let mut sprites = Vec::with_capacity(MAX_SPRITES);
    for _ in 0..MAX_SPRITES {
        let (id_lo, r) = take_byte(cursor);
        let (id_hi, r) = take_byte(r);
        let (x_lo, r) = take_byte(r);
        let (x_hi, r) = take_byte(r);
        let (y_lo, r) = take_byte(r);
        let (y_hi, r) = take_byte(r);
        let (owner_byte, r) = take_byte(r);
        let (flags_byte, r) = take_byte(r);
        cursor = r;
        sprites.push(Sprite {
            id: u16::from_le_bytes([id_lo, id_hi]),
            x: u16::from_le_bytes([x_lo, x_hi]),
            y: u16::from_le_bytes([y_lo, y_hi]),
            owner: owner_byte,
            flags: SpriteFlags::from_bits_truncate(flags_byte),
        });
    }

    let (scale_byte, _) = take_byte(cursor);

    let mut colors = [PlayerColor::default(); 8];
    for (i, color) in colors.iter_mut().enumerate() {
        *color = PlayerColor::Indexed(i as u8);
    }
    let player_colors = PlayerColors { colors };

    for unit_filter in [
        broodmap_render::UnitFilter::Melee,
        broodmap_render::UnitFilter::AsPlaced,
    ] {
        let options = MinimapOptions {
            scale: scale_byte as u32,
            unit_filter,
            ..Default::default()
        };
        let _ = render_minimap(
            &terrain,
            Tileset::Jungle,
            &units,
            &sprites,
            &player_colors,
            None,
            &options,
        );
    }
});

/// Takes the next byte off `data`, or `0` if it's exhausted -- keeps every field-extraction call
/// site above infallible instead of threading `Option` through the whole record-parsing chain.
fn take_byte(data: &[u8]) -> (u8, &[u8]) {
    match data.split_first() {
        Some((&b, rest)) => (b, rest),
        None => (0, data),
    }
}
