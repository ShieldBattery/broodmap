#![no_main]

use broodmap::chk::terrain::{TerrainTileIds, TileId};
use broodmap::chk::tileset::Tileset;
use broodmap_render::{
    ArtPack, ArtStyle, AssetRequest, AssetTier, MemorySource, RenderOptions, render_terrain,
};
use libfuzzer_sys::fuzz_target;

/// Map width/height are each capped to this range (in tiles), so a render never does more than
/// `MAX_MAP_DIM * MAX_MAP_DIM` tile blits regardless of input.
const MAX_MAP_DIM: u8 = 8;

/// `max_dimension` passed to `RenderOptions`, keeping per-run decode/scale work small even at the
/// largest capped map size and tier's native tile size.
const MAX_OUTPUT_DIM: u32 = 64;

fuzz_target!(|data: &[u8]| {
    // Deterministically carve the input into (width, height, tile IDs, Cv5 bytes, TilesetDds
    // bytes) so the same fuzzer input always builds the same render inputs.
    let Some((&w_byte, rest)) = data.split_first() else {
        return;
    };
    let Some((&h_byte, rest)) = rest.split_first() else {
        return;
    };

    let width = (w_byte % MAX_MAP_DIM) as usize + 1;
    let height = (h_byte % MAX_MAP_DIM) as usize + 1;
    let tile_count = width * height;
    let tile_bytes_len = tile_count * 2;

    let (tile_bytes, remainder) = if rest.len() >= tile_bytes_len {
        rest.split_at(tile_bytes_len)
    } else {
        (rest, &[][..])
    };

    let mut tiles = Vec::with_capacity(tile_count);
    for i in 0..tile_count {
        let lo = tile_bytes.get(i * 2).copied().unwrap_or(0);
        let hi = tile_bytes.get(i * 2 + 1).copied().unwrap_or(0);
        tiles.push(TileId(u16::from_le_bytes([lo, hi])));
    }

    let terrain = TerrainTileIds {
        width,
        height,
        tiles,
    };

    // Split the remaining bytes in half: first half is the Cv5 asset, second half the
    // TilesetDds asset. Both parsers/decoders are permissive, so arbitrary bytes here (short,
    // truncated, or nonsensical) are exactly the interesting case.
    let half = remainder.len() / 2;
    let (cv5_bytes, dds_bytes) = remainder.split_at(half);

    let mut source = MemorySource::new();
    source.insert(AssetRequest::Cv5(Tileset::Jungle), cv5_bytes.to_vec());
    for tier in [AssetTier::Sd, AssetTier::Hd2, AssetTier::Hd] {
        source.insert(
            AssetRequest::TilesetDds(Tileset::Jungle, tier, ArtPack::Standard),
            dds_bytes.to_vec(),
        );
    }

    // Pass 1: Original style (exercises the Sd/paletted decode path directly).
    let sd_options = RenderOptions {
        art_style: ArtStyle::Original,
        max_dimension: Some(MAX_OUTPUT_DIM),
        ..Default::default()
    };
    let _ = render_terrain(&terrain, Tileset::Jungle, &source, &sd_options);

    // Pass 2: default style (Remastered), still bounded by max_dimension so this stays cheap
    // even though it resolves to the Hd2 tier's DDS decode path instead.
    let default_options = RenderOptions {
        max_dimension: Some(MAX_OUTPUT_DIM),
        ..Default::default()
    };
    let _ = render_terrain(&terrain, Tileset::Jungle, &source, &default_options);
});
