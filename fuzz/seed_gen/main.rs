//! Generates the committed seed corpus under `fuzz/seeds/<target>/`.
//!
//! Seeds are copied/derived from the real test map assets in `../broodmap/assets/` (these are
//! already committed test fixtures used by broodmap's own test suite, so reusing them here
//! doesn't introduce any new licensing concerns). Anything over `MAX_SEED_SIZE` is skipped to
//! keep the corpus modest.
//!
//! Run with `cargo run --bin seed_gen` from `fuzz/`.

use std::fs;
use std::path::{Path, PathBuf};

use broodmap::Chk;
use broodmap::chk::terrain::{TerrainTileIds, TileId};
use broodmap::chk::tileset::Tileset;
use broodmap::extract_chk_from_map;
use broodmap_formats::{DdsVr4, Frame, parse_cv5, parse_dds};
use broodmap_render::{
    ArtPack, ArtStyle, AssetRequest, AssetTier, MemorySource, RenderOptions, render_terrain,
};

/// Seeds (and CHKs extracted from map seeds) larger than this are skipped.
const MAX_SEED_SIZE: u64 = 256 * 1024;

/// Map files (.scm/.scx) copied as-is for the `mpq_parse` and `full_pipeline` targets, chosen for
/// structural diversity: smallest known-valid map, a normal full-featured map, a map protector
/// output, an imploded (compressed) file, negative chunk table offsets, and a corrupted/malformed
/// header.
const MPQ_SEED_MAPS: &[&str] = &[
    "smallest.scm",
    "lt.scm",
    "protected-2.scx",
    "imploded.scm",
    "negativeoffsets.scx",
    "corrupted-0.scx",
];

/// Map files that CHK bytes get extracted from (via `extract_chk_from_map`) for the `chk_parse`
/// target. Chosen to cover the smallest map, a normal full-featured map, and a couple of
/// protector outputs -- other maps in `../broodmap/assets` either fail extraction (the corrupted
/// ones) or produce a CHK over `MAX_SEED_SIZE`.
const CHK_SEED_MAPS: &[&str] = &[
    "smallest.scm",
    "lt.scm",
    "protected-5.scm",
    "protected-3.scx",
];

fn main() {
    let assets_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("../broodmap/assets");
    let seeds_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("seeds");

    let mpq_dir = seeds_root.join("mpq_parse");
    let pipeline_dir = seeds_root.join("full_pipeline");
    let chk_dir = seeds_root.join("chk_parse");

    fs::create_dir_all(&mpq_dir).expect("create mpq_parse seed dir");
    fs::create_dir_all(&pipeline_dir).expect("create full_pipeline seed dir");
    fs::create_dir_all(&chk_dir).expect("create chk_parse seed dir");

    for name in MPQ_SEED_MAPS {
        let src = assets_dir.join(name);
        let bytes = fs::read(&src).unwrap_or_else(|e| panic!("read {}: {e}", src.display()));
        if bytes.len() as u64 > MAX_SEED_SIZE {
            println!(
                "skipping {name} for mpq_parse/full_pipeline: {} bytes exceeds {} byte cap",
                bytes.len(),
                MAX_SEED_SIZE
            );
            continue;
        }

        // Sanity check against the real parser so we don't commit a seed that can't even be
        // read back by the library (mirrors what the fuzz target itself will do).
        if let Err(e) = broodmap::Mpq::from_bytes(&bytes) {
            println!("note: {name} does not parse as a valid MPQ ({e}), seeding anyway");
        }

        write_seed(&mpq_dir, name, &bytes);
        write_seed(&pipeline_dir, name, &bytes);
    }

    for name in CHK_SEED_MAPS {
        let src = assets_dir.join(name);
        let bytes = fs::read(&src).unwrap_or_else(|e| panic!("read {}: {e}", src.display()));

        match extract_chk_from_map(&bytes, None, None) {
            Ok((chk, _mpq)) => {
                let chk_bytes = chk.data();
                if chk_bytes.len() as u64 > MAX_SEED_SIZE {
                    println!(
                        "skipping {name} for chk_parse: extracted CHK is {} bytes, exceeds {} byte cap",
                        chk_bytes.len(),
                        MAX_SEED_SIZE
                    );
                    continue;
                }

                // Sanity check: the bytes we're about to write should parse cleanly with the
                // real CHK parser.
                if let Err(e) = Chk::from_bytes(chk_bytes.to_vec(), None) {
                    panic!("extracted CHK for {name} failed to re-parse: {e}");
                }

                let out_name = format!("{name}.chk");
                write_seed(&chk_dir, &out_name, chk_bytes);
            }
            Err(e) => {
                println!("skipping {name} for chk_parse: extraction failed ({e})");
            }
        }
    }

    write_formats_parse_seeds(&seeds_root);
    write_render_terrain_seed(&seeds_root);

    println!("done");
}

/// Writes synthetic (not Blizzard-derived) seeds for the `formats_parse` target: a valid CV5
/// tile-group entry, a DDS-record-layout `.dds.vr4` (HD-tier shape) whose one frame is itself a
/// valid DXT1 DDS, and a paletted-layout (SD) `.dds.vr4` with a couple of tiles.
fn write_formats_parse_seeds(seeds_root: &Path) {
    let dir = seeds_root.join("formats_parse");
    fs::create_dir_all(&dir).expect("create formats_parse seed dir");

    // A single valid CV5 tile-group entry (52 bytes).
    let mut mega_tiles = [0u16; 16];
    for (i, tile) in mega_tiles.iter_mut().enumerate() {
        *tile = i as u16;
    }
    let cv5_bytes = make_cv5_entry(1, 0x0001, mega_tiles);
    let cv5 = parse_cv5(&cv5_bytes);
    assert_eq!(
        cv5.groups.len(),
        1,
        "seed CV5 entry should parse to one group"
    );
    write_seed(&dir, "cv5_valid.bin", &cv5_bytes);

    // A DDS-record-layout `.dds.vr4` (HD-tier shape, format code 0x1004) with one frame, whose
    // payload is itself a valid DXT1 DDS file -- exercises `DdsVr4::parse` -> `Frame::Dds` ->
    // `parse_dds` end to end.
    let dxt1_dds = make_dxt1_dds(4, 4, 0xF800); // solid red 4x4 BC1 block
    let dds_record_vr4 = make_dds_record_vr4(&[&dxt1_dds]);
    {
        let vr4 = DdsVr4::parse(&dds_record_vr4).expect("seed DDS-record vr4 should parse");
        assert_eq!(vr4.frame_count(), 1);
        match vr4.frame(0) {
            Some(Frame::Dds(payload)) => {
                parse_dds(payload).expect("embedded DDS frame should parse");
            }
            other => panic!("expected a DDS frame, got {other:?}"),
        }
    }
    write_seed(&dir, "dds_record_vr4.bin", &dds_record_vr4);

    // A paletted-layout (SD) `.dds.vr4` (format code 0x1011) with a couple of 2x2 tiles and a
    // distinctive palette.
    let mut palette = [0u8; 1024];
    palette[4..8].copy_from_slice(&[0xFF, 0x00, 0x00, 0x00]); // index 1: red
    palette[8..12].copy_from_slice(&[0x00, 0x00, 0xFF, 0x00]); // index 2: blue
    let tile_a = [1u8; 4]; // 2x2, all index 1
    let tile_b = [2u8; 4]; // 2x2, all index 2
    let paletted_vr4 = make_paletted_vr4(2, 2, &palette, &[&tile_a, &tile_b]);
    {
        let vr4 = DdsVr4::parse(&paletted_vr4).expect("seed paletted vr4 should parse");
        assert_eq!(vr4.frame_count(), 2);
        assert!(vr4.palette().is_some());
    }
    write_seed(&dir, "paletted_vr4.bin", &paletted_vr4);
}

/// Writes a synthetic (not Blizzard-derived) seed for the `render_terrain` target: a
/// well-formed composite input matching that target's input-splitting scheme (2 selector bytes
/// for width/height, tile IDs, then the remaining bytes split evenly into Cv5/TilesetDds asset
/// bytes), built as a 2x2 checkerboard mirroring `broodmap-render`'s own
/// `renders_checkerboard_terrain_...` unit test.
fn write_render_terrain_seed(seeds_root: &Path) {
    let dir = seeds_root.join("render_terrain");
    fs::create_dir_all(&dir).expect("create render_terrain seed dir");

    let width_byte = 1u8; // (1 % 8) + 1 == 2
    let height_byte = 1u8; // (1 % 8) + 1 == 2

    // 2x2 checkerboard: group 0 (megatile 0) / group 1 (megatile 1). group_id is bits 4..15 of
    // the tile ID, tile_index the bottom 4 bits.
    let tile_ids: [u16; 4] = [0, 16, 16, 0];
    let mut tile_bytes = Vec::with_capacity(8);
    for id in tile_ids {
        tile_bytes.extend_from_slice(&id.to_le_bytes());
    }

    let mut mega_tiles_group0 = [0u16; 16];
    mega_tiles_group0[0] = 0; // group 0 -> megatile 0
    let mut mega_tiles_group1 = [0u16; 16];
    mega_tiles_group1[0] = 1; // group 1 -> megatile 1
    let mut cv5_bytes = Vec::new();
    cv5_bytes.extend(make_cv5_entry(0, 0, mega_tiles_group0));
    cv5_bytes.extend(make_cv5_entry(0, 0, mega_tiles_group1));

    let red_frame = make_dxt1_dds(4, 4, 0xF800);
    let blue_frame = make_dxt1_dds(4, 4, 0x001F);
    let mut dds_bytes = make_dds_record_vr4(&[&red_frame, &blue_frame]);

    // Pad the shorter of the two asset byte strings with trailing zeros so they're equal length:
    // both parsers ignore trailing/unused bytes, and this guarantees the fuzz target's
    // `remainder.split_at(remainder.len() / 2)` lands exactly on the boundary between them.
    let target_len = cv5_bytes.len().max(dds_bytes.len());
    cv5_bytes.resize(target_len, 0);
    dds_bytes.resize(target_len, 0);

    let mut seed = Vec::new();
    seed.push(width_byte);
    seed.push(height_byte);
    seed.extend(&tile_bytes);
    seed.extend(&cv5_bytes);
    seed.extend(&dds_bytes);

    // Sanity check: replay this seed's construction through the real render pipeline (mirroring
    // what the `render_terrain` fuzz target does) to make sure it renders successfully rather
    // than erroring, before committing it.
    let terrain = TerrainTileIds {
        width: 2,
        height: 2,
        tiles: tile_ids.iter().map(|&id| TileId(id)).collect(),
    };
    let mut source = MemorySource::new();
    source.insert(AssetRequest::Cv5(Tileset::Jungle), cv5_bytes);
    source.insert(
        AssetRequest::TilesetDds(Tileset::Jungle, AssetTier::Sd, ArtPack::Standard),
        dds_bytes,
    );
    let options = RenderOptions {
        art_style: ArtStyle::Original,
        max_dimension: Some(64),
        ..Default::default()
    };
    render_terrain(&terrain, Tileset::Jungle, &source, &options)
        .expect("seed render_terrain input should render successfully");

    write_seed(&dir, "checkerboard.bin", &seed);
}

/// Builds a single 52-byte CV5 tile-group entry, matching `broodmap-formats/src/cv5.rs`'s layout.
fn make_cv5_entry(group_type: u16, flags: u16, mega_tiles: [u16; 16]) -> Vec<u8> {
    let mut entry = Vec::with_capacity(52);
    entry.extend_from_slice(&group_type.to_le_bytes());
    entry.extend_from_slice(&flags.to_le_bytes());
    entry.extend_from_slice(&[0u8; 16]); // unused rects
    for tile in mega_tiles {
        entry.extend_from_slice(&tile.to_le_bytes());
    }
    entry
}

/// Builds a minimal valid DDS file (magic + 124-byte `DDS_HEADER`) containing a single solid-
/// color BC1 (DXT1) block, matching `broodmap-formats/src/dds.rs`'s test header layout.
fn make_dxt1_dds(height: u32, width: u32, color565: u16) -> Vec<u8> {
    let mut data = Vec::with_capacity(4 + 124 + 8);
    data.extend_from_slice(b"DDS ");
    data.extend_from_slice(&124u32.to_le_bytes()); // dwSize
    data.extend_from_slice(&0u32.to_le_bytes()); // dwFlags
    data.extend_from_slice(&height.to_le_bytes());
    data.extend_from_slice(&width.to_le_bytes());
    data.extend_from_slice(&0u32.to_le_bytes()); // dwPitchOrLinearSize
    data.extend_from_slice(&0u32.to_le_bytes()); // dwDepth
    data.extend_from_slice(&1u32.to_le_bytes()); // dwMipMapCount
    data.extend_from_slice(&[0u8; 44]); // dwReserved1[11]
    data.extend_from_slice(&32u32.to_le_bytes()); // pixel format dwSize
    data.extend_from_slice(&0x4u32.to_le_bytes()); // pf_flags (DDPF_FOURCC)
    data.extend_from_slice(b"DXT1");
    data.extend_from_slice(&0u32.to_le_bytes()); // rgb_bit_count
    data.extend_from_slice(&[0u8; 16]); // bit masks
    data.extend_from_slice(&[0u8; 20]); // dwCaps..dwReserved2
    debug_assert_eq!(data.len(), 4 + 124);

    // A solid-color 4x4 BC1 block: color0 == color1, all indices 0.
    data.extend_from_slice(&color565.to_le_bytes()); // color0
    data.extend_from_slice(&color565.to_le_bytes()); // color1
    data.extend_from_slice(&0u32.to_le_bytes()); // indices
    data
}

/// Builds a DDS-record-layout `.dds.vr4` file (HD tier: format code `0x1004`, bit `0x10` clear),
/// one record per frame, matching `broodmap-formats/src/dds_vr4.rs`'s layout.
fn make_dds_record_vr4(frames: &[&[u8]]) -> Vec<u8> {
    let mut data = Vec::new();
    data.extend_from_slice(&0u32.to_le_bytes()); // file size (informational, unvalidated)
    data.extend_from_slice(&(frames.len() as u16).to_le_bytes());
    data.extend_from_slice(&0x1004u16.to_le_bytes()); // format code: HD, DDS-record layout
    for frame in frames {
        data.extend_from_slice(&0u32.to_le_bytes()); // zero
        data.extend_from_slice(&0u16.to_le_bytes()); // width (unused by this layout)
        data.extend_from_slice(&0u16.to_le_bytes()); // height (unused by this layout)
        data.extend_from_slice(&(frame.len() as u32).to_le_bytes());
        data.extend_from_slice(frame);
    }
    data
}

/// Builds a paletted-layout (SD) `.dds.vr4` file (format code `0x1011`: scale 1 = 32px, bit
/// `0x10` set), given a shared tile width/height, a full 1024-byte palette, and raw
/// palette-index tile bytes, matching `broodmap-formats/src/dds_vr4.rs`'s layout.
fn make_paletted_vr4(width: u16, height: u16, palette: &[u8; 1024], tiles: &[&[u8]]) -> Vec<u8> {
    let mut data = Vec::new();
    data.extend_from_slice(&0u32.to_le_bytes()); // file size (informational, unvalidated)
    data.extend_from_slice(&(tiles.len() as u16).to_le_bytes());
    data.extend_from_slice(&0x1011u16.to_le_bytes()); // format code: SD, paletted layout
    data.extend_from_slice(&width.to_le_bytes());
    data.extend_from_slice(&height.to_le_bytes());
    data.extend_from_slice(palette);
    for tile in tiles {
        data.extend_from_slice(tile);
    }
    data
}

fn write_seed(dir: &Path, name: &str, bytes: &[u8]) {
    let path = dir.join(name);
    fs::write(&path, bytes).unwrap_or_else(|e| panic!("write {}: {e}", path.display()));
    println!(
        "wrote {} ({} bytes)",
        relative(&path).display(),
        bytes.len()
    );
}

fn relative(path: &Path) -> PathBuf {
    path.strip_prefix(env!("CARGO_MANIFEST_DIR"))
        .unwrap_or(path)
        .to_path_buf()
}
