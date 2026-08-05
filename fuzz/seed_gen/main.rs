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
use broodmap::chk::placed_units::{PlacedUnit, UnitInstanceId, UnitState};
use broodmap::chk::player_colors::{PlayerColor, PlayerColors};
use broodmap::chk::sprites::{Sprite, SpriteFlags};
use broodmap::chk::terrain::{TerrainTileIds, TileId};
use broodmap::chk::tileset::Tileset;
use broodmap::extract_chk_from_map;
use broodmap_formats::{
    Anim, DdsVr4, Frame, MainSdAnim, parse_cv5, parse_dds, parse_flingy_dat, parse_images_dat,
    parse_images_rel, parse_sprites_dat, parse_tbl, parse_teamcolor_mask, parse_units_dat,
};
use broodmap_render::{
    ArtPack, ArtStyle, AssetRequest, AssetTier, MemorySource, MinimapOptions, PlannedBlock,
    PlannedSprite, RenderOptions, RenderPlan, SdCanvasSource, TerrainPlan, build_minimap_table,
    execute_plan, render_minimap, render_terrain,
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
    write_anim_parse_seed(&seeds_root);
    write_mainsd_parse_seed(&seeds_root);
    write_minimap_parse_seed(&seeds_root);
    write_plan_execute_seed(&seeds_root);

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

    // A `units.dat`-shaped buffer at its real size (19876 bytes, mostly zero) with a few nonzero
    // values planted in each column this crate exposes, at unit ID 5 -- mirrors
    // `broodmap-formats/src/dat.rs`'s own
    // `units_dat_extracts_exposed_columns_incl_subrange_affected_offsets` test, including a
    // subrange-affected column (bounds sits after the buildings-only `addon_size` column) so the
    // seed exercises non-trivial offset computation, not just column 0.
    let units_dat = make_units_dat_seed();
    {
        let dat = parse_units_dat(&units_dat);
        let entry = dat.entry(5).expect("seed units.dat entry 5 should parse");
        assert_eq!(entry.flingy, 42, "seed units.dat should round-trip");
    }
    write_seed(&dir, "units_dat_valid.bin", &units_dat);

    // Small (not full-size) `flingy.dat`/`sprites.dat`/`images.dat` buffers: each exposed column
    // starts near the front of its file (see dat.rs's column layout), so a short buffer covering
    // just past the interesting entry -- with the remaining, unwritten entries reading back as
    // zero via the parser's own permissive truncation handling -- exercises the same real column
    // math as a full-size file while staying small, matching this corpus's preference for compact
    // seeds. (`images.dat`'s `special_overlay` column sits much further into the file, so that one
    // entry stays zero/`None` here rather than requiring a near-full-size buffer.)
    let flingy_dat = make_flingy_dat_seed();
    {
        let dat = parse_flingy_dat(&flingy_dat);
        assert_eq!(
            dat.sprite_id(3),
            Some(555),
            "seed flingy.dat should round-trip"
        );
    }
    write_seed(&dir, "flingy_dat_valid.bin", &flingy_dat);

    let sprites_dat = make_sprites_dat_seed();
    {
        let dat = parse_sprites_dat(&sprites_dat);
        assert_eq!(
            dat.image_id(20),
            Some(42),
            "seed sprites.dat should round-trip"
        );
    }
    write_seed(&dir, "sprites_dat_valid.bin", &sprites_dat);

    let images_dat = make_images_dat_seed();
    {
        let dat = parse_images_dat(&images_dat);
        let entry = dat.entry(0).expect("seed images.dat entry 0 should parse");
        assert_eq!(entry.grp, 12, "seed images.dat should round-trip");
        assert!(entry.has_directional_frames);
        assert_eq!(entry.render_style, 9);
        assert_eq!(entry.color_shift, 2);
    }
    write_seed(&dir, "images_dat_valid.bin", &images_dat);

    // A valid `images.rel`: a couple of records, one with the redirect flag+ref set, matching
    // `broodmap-formats/src/rel.rs`'s own test layout.
    let rel_bytes = make_rel_seed();
    {
        let rel = parse_images_rel(&rel_bytes);
        assert_eq!(
            rel.resolve(0),
            0,
            "seed images.rel entry 0 should not redirect"
        );
        assert_eq!(
            rel.resolve(1),
            42,
            "seed images.rel entry 1 should redirect to 42"
        );
    }
    write_seed(&dir, "rel_valid.bin", &rel_bytes);

    // A valid `.tbl` string table with a few entries.
    let tbl_bytes = make_tbl_seed();
    {
        let tbl = parse_tbl(&tbl_bytes);
        assert_eq!(
            tbl.get(0).as_deref(),
            Some("Zerg"),
            "seed .tbl should round-trip"
        );
        assert_eq!(tbl.get(2).as_deref(), Some("Protoss"));
    }
    write_seed(&dir, "tbl_valid.bin", &tbl_bytes);
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

/// Writes a synthetic (not Blizzard-derived) seed for the `plan_execute` target: a byte string
/// matching that target's carve order (flags, small-mode dims, ppt, tier/pack, megatiles, one
/// sprite on image 0, the second canvas-source image id, one block, the informational
/// width/height, then four equal quarters of asset bytes: TilesetDds, mainSD.anim, per-image
/// anim, GRP). Built so the executed plan actually renders — terrain from a real DDS-record
/// `.dds.vr4`, SD sprite art from a valid `mainSD.anim`, a parseable GRP header — rather than
/// exercising only the everything-fails paths.
fn write_plan_execute_seed(seeds_root: &Path) {
    let dir = seeds_root.join("plan_execute");
    fs::create_dir_all(&dir).expect("create plan_execute seed dir");

    let mut seed = Vec::new();
    seed.push(0u8); // flags: bit 0 clear -> small-dims mode
    seed.extend_from_slice(&1u16.to_le_bytes()); // map_w: (1 % 8) + 1 == 2
    seed.extend_from_slice(&1u16.to_le_bytes()); // map_h: 2
    seed.extend_from_slice(&32u32.to_le_bytes()); // px_per_tile
    seed.push(0u8); // tier: Sd
    seed.push(0u8); // pack: Standard
    seed.push(4u8); // megatile count
    for megatile in [0u16, 1, 1, 0] {
        seed.extend_from_slice(&megatile.to_le_bytes());
    }
    seed.push(1u8); // sprite count
    // Sprite 0 (image id is forced to 0 by the target, not read from bytes):
    seed.extend_from_slice(&0u32.to_le_bytes()); // frame
    seed.push(0u8); // flip: false
    seed.extend_from_slice(&32i32.to_le_bytes()); // x
    seed.extend_from_slice(&32i32.to_le_bytes()); // y
    seed.push(1u8); // tint: Some
    seed.extend_from_slice(&[255, 0, 0]); // tint rgb
    seed.push(0u8); // is_shadow: false
    seed.extend_from_slice(&5u16.to_le_bytes()); // second sd_canvases image id
    seed.push(1u8); // block count
    seed.extend_from_slice(&32i32.to_le_bytes()); // block x
    seed.extend_from_slice(&32i32.to_le_bytes()); // block y
    seed.extend_from_slice(&64u32.to_le_bytes()); // block width
    seed.extend_from_slice(&64u32.to_le_bytes()); // block height
    seed.extend_from_slice(&[0, 255, 0]); // block color
    seed.extend_from_slice(&64u32.to_le_bytes()); // plan.width (informational)
    seed.extend_from_slice(&64u32.to_le_bytes()); // plan.height (informational)

    // The four asset quarters, zero-padded to equal length so the target's split_at quarters
    // land exactly on the payload boundaries (all these parsers ignore trailing bytes).
    let red_frame = make_dxt1_dds(4, 4, 0xF800);
    let blue_frame = make_dxt1_dds(4, 4, 0x001F);
    let mut dds_bytes = make_dds_record_vr4(&[&red_frame, &blue_frame]);
    let mut mainsd_bytes = make_mainsd_seed();
    let mut anim_bytes = make_anim_seed();
    // A classic GRP header: 1 frame, 64x64 canvas (the frame table/pixels are never read).
    let mut grp_bytes = Vec::new();
    grp_bytes.extend_from_slice(&1u16.to_le_bytes());
    grp_bytes.extend_from_slice(&64u16.to_le_bytes());
    grp_bytes.extend_from_slice(&64u16.to_le_bytes());
    let quarter = dds_bytes
        .len()
        .max(mainsd_bytes.len())
        .max(anim_bytes.len())
        .max(grp_bytes.len());
    for bytes in [
        &mut dds_bytes,
        &mut mainsd_bytes,
        &mut anim_bytes,
        &mut grp_bytes,
    ] {
        bytes.resize(quarter, 0);
    }

    // Sanity check: replay the exact plan/source the fuzz target will carve from this seed and
    // make sure it executes successfully rather than erroring, before committing it.
    let plan = RenderPlan {
        width: 64,
        height: 64,
        px_per_tile: 32,
        terrain: TerrainPlan {
            tileset: Tileset::Jungle,
            tier: AssetTier::Sd,
            pack: ArtPack::Standard,
            width: 2,
            height: 2,
            megatiles: vec![0, 1, 1, 0],
        },
        unit_tier: AssetTier::Sd,
        unit_pack: ArtPack::Standard,
        sprites: vec![PlannedSprite {
            image_id: 0,
            frame: 0,
            flip: false,
            x: 32,
            y: 32,
            tint: Some([255, 0, 0]),
            is_shadow: false,
        }],
        sd_canvases: vec![
            SdCanvasSource {
                image_id: 0,
                grp_path: "a".to_string(),
            },
            SdCanvasSource {
                image_id: 5,
                grp_path: "missing".to_string(),
            },
        ],
        blocks: vec![PlannedBlock {
            x: 32,
            y: 32,
            width: 64,
            height: 64,
            color: [0, 255, 0],
        }],
        manifest: Vec::new(),
    };
    let mut source = MemorySource::new();
    source.insert(
        AssetRequest::TilesetDds(Tileset::Jungle, AssetTier::Sd, ArtPack::Standard),
        dds_bytes.clone(),
    );
    source.insert(AssetRequest::MainSdAnim, mainsd_bytes.clone());
    source.insert(
        AssetRequest::Grp {
            path: "a".to_string(),
        },
        grp_bytes.clone(),
    );
    let image = execute_plan(&plan, &source).expect("seed plan_execute input should execute");
    assert_eq!((image.width, image.height), (64, 64));

    seed.extend(&dds_bytes);
    seed.extend(&mainsd_bytes);
    seed.extend(&anim_bytes);
    seed.extend(&grp_bytes);
    write_seed(&dir, "sd_plan.bin", &seed);
}

/// Writes a synthetic (not Blizzard-derived) seed for the `minimap_parse` target: four
/// equal-length, well-formed quarters (a valid CV5 entry, VX4EX entry, VR4 bitmap, WPE palette --
/// exactly `build_minimap_table`'s four inputs), followed by trailing bytes for a small terrain
/// grid plus a few unit/sprite records, matching that target's own byte-splitting scheme.
fn write_minimap_parse_seed(seeds_root: &Path) {
    let dir = seeds_root.join("minimap_parse");
    fs::create_dir_all(&dir).expect("create minimap_parse seed dir");

    // A single CV5 group (group 0) whose megatile 0 points at VX4EX entry 0's minitile[0],
    // which points at VR4 bitmap 0, whose byte 55 (palette index 5) is colored by the WPE
    // palette -- exercises the full sampling chain, not just the degrade-to-0 fallback path.
    let mut mega_tiles = [0u16; 16];
    mega_tiles[0] = 0;
    let mut cv5 = make_cv5_entry(0, 0, mega_tiles);

    let mut vx4ex = Vec::new();
    let raw = 0u32 << 1; // vr4 index 0, no flip
    vx4ex.extend_from_slice(&raw.to_le_bytes());
    for _ in 1..16 {
        vx4ex.extend_from_slice(&0u32.to_le_bytes());
    }

    let mut vr4 = [0u8; 64];
    vr4[55] = 5;

    let mut wpe = vec![0u8; 1024];
    wpe[5 * 4..5 * 4 + 3].copy_from_slice(&[10, 20, 30]);

    {
        let table = build_minimap_table(&cv5, &vx4ex, &vr4, &wpe)
            .expect("seed minimap table inputs should build successfully");
        assert_eq!(
            table.len(),
            2 + 16 * 4 + 768,
            "one CV5 group -> 16 unified tile ids, 4 quadrant bytes each"
        );
    }

    // Pad the four quarters to equal length (matching `write_render_terrain_seed`'s pattern) so
    // the fuzz target's `data.len() / 4` split lands exactly on each boundary -- every parser
    // involved ignores trailing bytes, so this doesn't change what any of them parse.
    let target_len = [cv5.len(), vx4ex.len(), vr4.len(), wpe.len()]
        .into_iter()
        .max()
        .unwrap();
    let mut vr4 = vr4.to_vec();
    cv5.resize(target_len, 0);
    vx4ex.resize(target_len, 0);
    vr4.resize(target_len, 0);
    wpe.resize(target_len, 0);

    let mut seed = Vec::new();
    seed.extend(&cv5);
    seed.extend(&vx4ex);
    seed.extend(&vr4);
    seed.extend(&wpe);

    // Trailing bytes for the render half: a 2x2 terrain grid (matching the checkerboard used
    // elsewhere in this file), 4 unit records and 4 sprite records (8 bytes each, matching the
    // fuzz target's per-record layout), and a final `scale` byte.
    seed.push(1); // width byte: (1 % 8) + 1 == 2
    seed.push(1); // height byte: (1 % 8) + 1 == 2
    for tile_id in [0u16, 16, 16, 0] {
        seed.extend_from_slice(&tile_id.to_le_bytes());
    }
    for i in 0..4u16 {
        seed.extend_from_slice(&i.to_le_bytes()); // unit_id
        seed.extend_from_slice(&(i * 32).to_le_bytes()); // x
        seed.extend_from_slice(&(i * 32).to_le_bytes()); // y
        seed.push(i as u8); // owner byte
        seed.push(0); // state byte
    }
    for i in 0..4u16 {
        seed.extend_from_slice(&i.to_le_bytes()); // sprite id
        seed.extend_from_slice(&(i * 32).to_le_bytes()); // x
        seed.extend_from_slice(&(i * 32).to_le_bytes()); // y
        seed.push(i as u8); // owner byte
        seed.push(0); // flags byte
    }
    seed.push(4); // scale byte

    // Sanity check: replay the render half through the real public API (mirroring the fuzz
    // target's simple 2x2 terrain, no units/sprites, and a small scale) to make sure a
    // structurally similar input renders without issue before committing the seed.
    let terrain = TerrainTileIds {
        width: 2,
        height: 2,
        tiles: vec![TileId(0), TileId(16), TileId(16), TileId(0)],
    };
    let mut colors = [PlayerColor::default(); 8];
    for (i, color) in colors.iter_mut().enumerate() {
        *color = PlayerColor::Indexed(i as u8);
    }
    let player_colors = PlayerColors { colors };
    let units = [PlacedUnit {
        instance_id: UnitInstanceId(0),
        x: 32,
        y: 32,
        unit_id: 0,
        owner: Some(0),
        hp_percent: None,
        shield_percent: None,
        energy_percent: None,
        resource_amount: None,
        hangar_count: None,
        state: UnitState::empty(),
        linked_id: None,
    }];
    let sprites = [Sprite {
        id: 0,
        x: 32,
        y: 32,
        owner: 0,
        flags: SpriteFlags::empty(),
    }];
    let options = MinimapOptions {
        scale: 4,
        ..Default::default()
    };
    let image = render_minimap(
        &terrain,
        Tileset::Jungle,
        &units,
        &sprites,
        &player_colors,
        None,
        &options,
    );
    // 2x2 map -> M = 2 <= 64 -> native "Quad" mode, base 4x4; scale 4 -> 16x16 output.
    assert_eq!((image.width, image.height), (16, 16));

    write_seed(&dir, "quartered.bin", &seed);
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

// ---------------------------------------------------------------------------------------------
// units.dat / flingy.dat / sprites.dat / images.dat synthetic seed builders
// ---------------------------------------------------------------------------------------------
//
// These mirror the private column layouts in `broodmap-formats/src/dat.rs` (kept in sync there by
// that crate's own `total_size(...) == *_DAT_SIZE` compile-time assertions); duplicated here since
// those layout constants aren't part of the crate's public API.

const UNITS_COUNT: usize = 228;
const BUILDINGS_COUNT: usize = 96;
const UNITS_ONLY_COUNT: usize = 106;
const UNITS_DAT_SIZE: usize = 19876;

const UNITS_COLUMN_SIZES: &[usize] = &[
    UNITS_COUNT,
    UNITS_COUNT * 2,
    UNITS_COUNT * 2,
    BUILDINGS_COUNT * 2,
    UNITS_COUNT * 4,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT * 2,
    UNITS_COUNT * 4,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT * 4,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_ONLY_COUNT * 2,
    UNITS_COUNT * 2,
    UNITS_COUNT * 2,
    UNITS_ONLY_COUNT * 2,
    UNITS_ONLY_COUNT * 2,
    UNITS_ONLY_COUNT * 2,
    UNITS_ONLY_COUNT * 2,
    UNITS_COUNT * 4,
    BUILDINGS_COUNT * 4,
    UNITS_COUNT * 8,
    UNITS_COUNT * 2,
    UNITS_COUNT * 2,
    UNITS_COUNT * 2,
    UNITS_COUNT * 2,
    UNITS_COUNT * 2,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT,
    UNITS_COUNT * 2,
    UNITS_COUNT * 2,
    UNITS_COUNT * 2,
    UNITS_COUNT,
    UNITS_COUNT * 2,
];
const UNITS_COL_FLINGY: usize = 0;
const UNITS_COL_SUB_UNIT_1: usize = 1;
const UNITS_COL_UNIT_DIRECTION: usize = 5;
const UNITS_COL_SPECIAL_ABILITY_FLAGS: usize = 22;
const UNITS_COL_PLACEBOX_SIZE: usize = 36;
const UNITS_COL_BOUNDS: usize = 38;
const UNITS_COL_STAR_EDIT_GROUP_FLAGS: usize = 44;

const FLINGY_COLUMN_SIZES: &[usize] = &[
    209 * 2, // sprite: u16
    209 * 4, // speed: u32
    209 * 2, // acceleration: u16
    209 * 4, // halt_distance: u32
    209,     // turn_radius: u8
    209,     // unused: u8
    209,     // movement_control: u8
];
const FLINGY_COL_SPRITE: usize = 0;

const SPRITES_COLUMN_SIZES: &[usize] = &[
    517 * 2, // image: u16
    387,     // health_bar: u8 (selectable-only)
    517,     // unknown: u8
    517,     // visible: u8
    387,     // selection_circle: u8 (selectable-only)
    387,     // selection_circle_offset: u8 (selectable-only)
];
const SPRITES_COL_IMAGE: usize = 0;

const IMAGES_COLUMN_SIZES: &[usize] = &[
    999 * 4, // grp: u32
    999,     // has_directional_frames: u8
    999,     // clickable: u8
    999,     // use_full_iscript: u8
    999,     // always_visible: u8
    999,     // render_style: u8
    999,     // color_shift: u8
    999 * 4, // iscript: u32
    999 * 4, // shield_overlay: u32
    999 * 4, // attack_overlay: u32
    999 * 4, // damage_overlay: u32
    999 * 4, // special_overlay: u32
    999 * 4, // landing_dust_overlay: u32
    999 * 4, // lift_off_dust_overlay: u32
];
const IMAGES_COL_GRP: usize = 0;
const IMAGES_COL_HAS_DIRECTIONAL_FRAMES: usize = 1;
const IMAGES_COL_RENDER_STYLE: usize = 5;
const IMAGES_COL_COLOR_SHIFT: usize = 6;

/// Returns the byte offset where column `index` (into `sizes`) begins, matching
/// `dat.rs::column_offset`.
fn dat_column_offset(sizes: &[usize], index: usize) -> usize {
    sizes[..index].iter().sum()
}

fn dat_total_size(sizes: &[usize]) -> usize {
    sizes.iter().sum()
}

/// Builds a full-size (19876-byte) `units.dat` buffer, mostly zero, with unit ID 5's exposed
/// columns set to distinctive nonzero values -- mirrors dat.rs's own
/// `units_dat_extracts_exposed_columns_incl_subrange_affected_offsets` test, including a
/// subrange-affected column (`bounds` sits after the buildings-only `addon_size` column), so the
/// seed exercises non-trivial offset computation, not just column 0.
fn make_units_dat_seed() -> Vec<u8> {
    assert_eq!(
        dat_total_size(UNITS_COLUMN_SIZES),
        UNITS_DAT_SIZE,
        "seed_gen's copy of the units.dat column layout has drifted from dat.rs"
    );
    let mut data = vec![0u8; UNITS_DAT_SIZE];
    let unit_id = 5usize;

    data[dat_column_offset(UNITS_COLUMN_SIZES, UNITS_COL_FLINGY) + unit_id] = 42;

    let sub_unit_1_off = dat_column_offset(UNITS_COLUMN_SIZES, UNITS_COL_SUB_UNIT_1) + unit_id * 2;
    data[sub_unit_1_off..sub_unit_1_off + 2].copy_from_slice(&777u16.to_le_bytes());

    data[dat_column_offset(UNITS_COLUMN_SIZES, UNITS_COL_UNIT_DIRECTION) + unit_id] = 3;

    let flags_off =
        dat_column_offset(UNITS_COLUMN_SIZES, UNITS_COL_SPECIAL_ABILITY_FLAGS) + unit_id * 4;
    data[flags_off..flags_off + 4].copy_from_slice(&0x0000_0005u32.to_le_bytes());

    let placebox_off = dat_column_offset(UNITS_COLUMN_SIZES, UNITS_COL_PLACEBOX_SIZE) + unit_id * 4;
    data[placebox_off..placebox_off + 2].copy_from_slice(&10i16.to_le_bytes());
    data[placebox_off + 2..placebox_off + 4].copy_from_slice(&20i16.to_le_bytes());

    let bounds_off = dat_column_offset(UNITS_COLUMN_SIZES, UNITS_COL_BOUNDS) + unit_id * 8;
    data[bounds_off..bounds_off + 2].copy_from_slice(&(-1i16).to_le_bytes());
    data[bounds_off + 2..bounds_off + 4].copy_from_slice(&(-2i16).to_le_bytes());
    data[bounds_off + 4..bounds_off + 6].copy_from_slice(&30i16.to_le_bytes());
    data[bounds_off + 6..bounds_off + 8].copy_from_slice(&40i16.to_le_bytes());

    data[dat_column_offset(UNITS_COLUMN_SIZES, UNITS_COL_STAR_EDIT_GROUP_FLAGS) + unit_id] = 0x80;

    data
}

/// Builds a small (not full-size) `flingy.dat`-shaped buffer covering just past `sprite` column
/// entry 3 (the only column this crate exposes, at column 0), with the rest of the real file
/// permissively read back as zero by the parser's own truncation handling.
fn make_flingy_dat_seed() -> Vec<u8> {
    let sprite_off = dat_column_offset(FLINGY_COLUMN_SIZES, FLINGY_COL_SPRITE) + 3 * 2;
    let mut data = vec![0u8; sprite_off + 2];
    data[sprite_off..sprite_off + 2].copy_from_slice(&555u16.to_le_bytes());
    data
}

/// Builds a small `sprites.dat`-shaped buffer covering just past `image` column entry 20.
fn make_sprites_dat_seed() -> Vec<u8> {
    let image_off = dat_column_offset(SPRITES_COLUMN_SIZES, SPRITES_COL_IMAGE) + 20 * 2;
    let mut data = vec![0u8; image_off + 2];
    data[image_off..image_off + 2].copy_from_slice(&42u16.to_le_bytes());
    data
}

/// Builds a small `images.dat`-shaped buffer covering entry 0's `grp`/`has_directional_frames`/
/// `render_style`/`color_shift` columns (all near the front of the file at entry 0). The overlay
/// columns (including `special_overlay`) sit much further into the file and are left permissively
/// zero (`None`) here to keep the seed small.
fn make_images_dat_seed() -> Vec<u8> {
    let image_id = 0usize;
    let color_shift_end =
        dat_column_offset(IMAGES_COLUMN_SIZES, IMAGES_COL_COLOR_SHIFT) + image_id + 1;
    let mut data = vec![0u8; color_shift_end];

    let grp_off = dat_column_offset(IMAGES_COLUMN_SIZES, IMAGES_COL_GRP) + image_id * 4;
    data[grp_off..grp_off + 4].copy_from_slice(&12u32.to_le_bytes());
    data[dat_column_offset(IMAGES_COLUMN_SIZES, IMAGES_COL_HAS_DIRECTIONAL_FRAMES) + image_id] = 1;
    data[dat_column_offset(IMAGES_COLUMN_SIZES, IMAGES_COL_RENDER_STYLE) + image_id] = 9;
    data[dat_column_offset(IMAGES_COLUMN_SIZES, IMAGES_COL_COLOR_SHIFT) + image_id] = 2;

    data
}

// ---------------------------------------------------------------------------------------------
// images.rel / .tbl synthetic seed builders
// ---------------------------------------------------------------------------------------------

/// Builds a valid `images.rel` buffer: a couple of 8-byte records, one with the redirect flag and
/// a real ref image set, matching `broodmap-formats/src/rel.rs`'s own test layout.
fn make_rel_seed() -> Vec<u8> {
    const REDIRECT_FLAG: u32 = 0x200;
    let mut data = Vec::new();
    data.extend_from_slice(&0u32.to_le_bytes()); // image 0: rel_type = 0, no redirect
    data.extend_from_slice(&0u32.to_le_bytes());
    data.extend_from_slice(&REDIRECT_FLAG.to_le_bytes()); // image 1: redirect to 42
    data.extend_from_slice(&42u32.to_le_bytes());
    data
}

/// Builds a valid `.tbl` string table with three entries, matching `broodmap-formats/src/tbl.rs`'s
/// own test layout.
fn make_tbl_seed() -> Vec<u8> {
    let entries = ["Zerg", "Terran", "Protoss"];
    let header_size = 2 + entries.len() * 2;
    let mut offsets = Vec::with_capacity(entries.len());
    let mut strings_blob = Vec::new();
    for s in entries {
        offsets.push((header_size + strings_blob.len()) as u16);
        strings_blob.extend_from_slice(s.as_bytes());
        strings_blob.push(0);
    }

    let mut data = Vec::with_capacity(header_size + strings_blob.len());
    data.extend_from_slice(&(entries.len() as u16).to_le_bytes());
    for offset in offsets {
        data.extend_from_slice(&offset.to_le_bytes());
    }
    data.extend_from_slice(&strings_blob);
    data
}

// ---------------------------------------------------------------------------------------------
// .anim synthetic seed builder
// ---------------------------------------------------------------------------------------------
//
// Mirrors the byte layout from `broodmap-formats/src/anim.rs`'s private `AnimBuilder` test
// fixture, which isn't reachable from here (it's a `#[cfg(test)]` helper in another crate).

const ANIM_LAYER_NAME_REGION_START: usize = 0x0C;
const ANIM_LAYER_NAME_SLOT_SIZE: usize = 32;
const ANIM_FRAME_TABLE_HEADER_OFFSET: usize = 0x14C;
const ANIM_LAYER_RECORDS_OFFSET: usize = 0x158;
const ANIM_LAYER_RECORD_SIZE: usize = 12;
const ANIM_FRAME_RECORD_SIZE: usize = 16;
const ANIM_TYPE_HD: u8 = 2;
const ANIM_NO_REF_ID: u16 = 0xFFFF;

/// A single synthetic anim frame, in raw 4K-unit coordinates (see anim.rs's module docs).
struct AnimSeedFrame {
    texture_x: u16,
    texture_y: u16,
    offset_x: i16,
    offset_y: i16,
    width: u16,
    height: u16,
}

/// Builds a valid 2-layer (`"diffuse"`, `"teamcolor"`), 2-frame HD `.anim` buffer, each layer's
/// payload a real (tiny) DXT1 DDS file so downstream `parse_dds` calls succeed too.
fn make_anim_seed() -> Vec<u8> {
    let layer_names = ["diffuse", "teamcolor"];
    let diffuse_payload = make_dxt1_dds(4, 4, 0xF800); // solid red
    let teamcolor_payload = make_dxt1_dds(4, 4, 0x001F); // solid blue
    let layer_payloads: [(&[u8], u16, u16); 2] =
        [(&diffuse_payload, 4, 4), (&teamcolor_payload, 4, 4)];
    let frames = [
        AnimSeedFrame {
            texture_x: 0,
            texture_y: 0,
            offset_x: 0,
            offset_y: 0,
            width: 4,
            height: 4,
        },
        AnimSeedFrame {
            texture_x: 4,
            texture_y: 0,
            offset_x: 1,
            offset_y: -1,
            width: 4,
            height: 4,
        },
    ];

    let num_layers = layer_names.len();
    let mut data = vec![0u8; ANIM_LAYER_RECORDS_OFFSET + num_layers * ANIM_LAYER_RECORD_SIZE];
    data[0..4].copy_from_slice(b"ANIM");
    data[4] = 4; // scale: HD
    data[5] = ANIM_TYPE_HD;
    data[6..8].copy_from_slice(&0u16.to_le_bytes()); // unknown
    data[8..10].copy_from_slice(&(num_layers as u16).to_le_bytes());
    data[10..12].copy_from_slice(&1u16.to_le_bytes()); // num_entries

    for (i, name) in layer_names.iter().enumerate() {
        let start = ANIM_LAYER_NAME_REGION_START + i * ANIM_LAYER_NAME_SLOT_SIZE;
        data[start..start + name.len()].copy_from_slice(name.as_bytes());
    }

    let canvas_width = 8u16;
    let canvas_height = 4u16;
    let frame_arr_offset = data.len(); // frames appended after the (fixed-size) layer records
    data[ANIM_FRAME_TABLE_HEADER_OFFSET..ANIM_FRAME_TABLE_HEADER_OFFSET + 2]
        .copy_from_slice(&(frames.len() as u16).to_le_bytes());
    data[ANIM_FRAME_TABLE_HEADER_OFFSET + 2..ANIM_FRAME_TABLE_HEADER_OFFSET + 4]
        .copy_from_slice(&ANIM_NO_REF_ID.to_le_bytes());
    data[ANIM_FRAME_TABLE_HEADER_OFFSET + 4..ANIM_FRAME_TABLE_HEADER_OFFSET + 6]
        .copy_from_slice(&canvas_width.to_le_bytes());
    data[ANIM_FRAME_TABLE_HEADER_OFFSET + 6..ANIM_FRAME_TABLE_HEADER_OFFSET + 8]
        .copy_from_slice(&canvas_height.to_le_bytes());
    data[ANIM_FRAME_TABLE_HEADER_OFFSET + 8..ANIM_FRAME_TABLE_HEADER_OFFSET + 12]
        .copy_from_slice(&(frame_arr_offset as u32).to_le_bytes());

    // Layer texture records, payloads appended right after the (fixed-size) frame table, in
    // layer order.
    let mut payload_cursor = frame_arr_offset + frames.len() * ANIM_FRAME_RECORD_SIZE;
    let mut payloads: Vec<&[u8]> = Vec::new();
    for (i, (payload, w, h)) in layer_payloads.iter().enumerate() {
        let rec_start = ANIM_LAYER_RECORDS_OFFSET + i * ANIM_LAYER_RECORD_SIZE;
        data[rec_start..rec_start + 4].copy_from_slice(&(payload_cursor as u32).to_le_bytes());
        data[rec_start + 4..rec_start + 8].copy_from_slice(&(payload.len() as u32).to_le_bytes());
        data[rec_start + 8..rec_start + 10].copy_from_slice(&w.to_le_bytes());
        data[rec_start + 10..rec_start + 12].copy_from_slice(&h.to_le_bytes());
        payloads.push(payload);
        payload_cursor += payload.len();
    }

    data.resize(payload_cursor, 0);

    let mut frame_offset = frame_arr_offset;
    for frame in &frames {
        data[frame_offset..frame_offset + 2].copy_from_slice(&frame.texture_x.to_le_bytes());
        data[frame_offset + 2..frame_offset + 4].copy_from_slice(&frame.texture_y.to_le_bytes());
        data[frame_offset + 4..frame_offset + 6].copy_from_slice(&frame.offset_x.to_le_bytes());
        data[frame_offset + 6..frame_offset + 8].copy_from_slice(&frame.offset_y.to_le_bytes());
        data[frame_offset + 8..frame_offset + 10].copy_from_slice(&frame.width.to_le_bytes());
        data[frame_offset + 10..frame_offset + 12].copy_from_slice(&frame.height.to_le_bytes());
        // unknown u32 left zeroed
        frame_offset += ANIM_FRAME_RECORD_SIZE;
    }

    let mut payload_offset = frame_arr_offset + frames.len() * ANIM_FRAME_RECORD_SIZE;
    for payload in &payloads {
        data[payload_offset..payload_offset + payload.len()].copy_from_slice(payload);
        payload_offset += payload.len();
    }

    data
}

/// Writes a synthetic (not Blizzard-derived) seed for the `anim_parse` target: a valid 2-layer
/// (`"diffuse"`, `"teamcolor"`), 2-frame HD `.anim` file.
fn write_anim_parse_seed(seeds_root: &Path) {
    let dir = seeds_root.join("anim_parse");
    fs::create_dir_all(&dir).expect("create anim_parse seed dir");

    let anim_bytes = make_anim_seed();
    {
        let anim = Anim::parse(&anim_bytes).expect("seed anim should parse");
        assert_eq!(anim.frame_count(), 2, "seed anim should round-trip");
        assert_eq!(anim.layers().len(), 2);
        assert!(anim.layer("diffuse").is_some());
        assert!(anim.layer("teamcolor").is_some());
        for i in 0..anim.frame_count() {
            assert!(anim.frame_texel_rect(i).is_some());
        }
    }
    write_seed(&dir, "hd_anim_valid.bin", &anim_bytes);
}

// ---------------------------------------------------------------------------------------------
// mainSD.anim synthetic seed builder
// ---------------------------------------------------------------------------------------------
//
// Mirrors the byte layout from `broodmap-formats/src/mainsd.rs`'s private `MainSdBuilder` test
// fixture, which isn't reachable from here (it's a `#[cfg(test)]` helper in another crate). See
// that module's doc comments for the full layout: 12 B header, 10x32 B name region at 0x0C,
// entry offset table (num_entries x u32 absolute) at 0x14C, then entries.

const MAINSD_LAYER_NAME_REGION_START: usize = 0x0C;
const MAINSD_LAYER_NAME_SLOT_SIZE: usize = 32;
const MAINSD_LAYER_NAME_SLOTS: usize = 10;
const MAINSD_ENTRY_OFFSET_TABLE_START: usize =
    MAINSD_LAYER_NAME_REGION_START + MAINSD_LAYER_NAME_SLOTS * MAINSD_LAYER_NAME_SLOT_SIZE; // 0x14C
const MAINSD_ENTRY_OFFSET_SIZE: usize = 4;
const MAINSD_ENTRY_HEADER_SIZE: usize = 12;
const MAINSD_LAYER_RECORD_SIZE: usize = 12;
const MAINSD_FRAME_RECORD_SIZE: usize = 16;
const MAINSD_TYPE_SD: u8 = 1;
const MAINSD_NO_REF_ID: u16 = 0xFFFF;
const MAINSD_TEAMCOLOR_MAGIC: &[u8; 4] = b"BMP ";

/// Builds a valid `mainSD.anim` buffer with two entries: entry 0 is a real entry with 2 layers
/// (`"diffuse"`, `"teamcolor"`) and 2 frames, entry 1 is a 12-byte inline reference pointing at
/// entry 0.
fn make_mainsd_seed() -> Vec<u8> {
    let layer_names = ["diffuse", "teamcolor"];
    let num_layers = layer_names.len();
    let num_entries = 2usize;

    // diffuse: reuse the same synthetic DXT1 DDS bytes the HD anim seed uses.
    let diffuse_payload = make_dxt1_dds(4, 4, 0xF800); // solid red, 4x4
    let (diffuse_w, diffuse_h) = (4u16, 4u16);

    // teamcolor: "BMP " magic + width*height bytes of 0/255 (per mainsd.rs's raw stencil format),
    // width/height matching the diffuse layer's dimensions.
    let mut teamcolor_payload = MAINSD_TEAMCOLOR_MAGIC.to_vec();
    for i in 0..(diffuse_w as usize * diffuse_h as usize) {
        teamcolor_payload.push(if i % 2 == 0 { 0 } else { 255 });
    }

    let frames = [
        AnimSeedFrame {
            texture_x: 0,
            texture_y: 0,
            offset_x: 0,
            offset_y: 0,
            width: 4,
            height: 4,
        },
        AnimSeedFrame {
            texture_x: 4,
            texture_y: 0,
            offset_x: 1,
            offset_y: -1,
            width: 4,
            height: 4,
        },
    ];

    let offset_table_size = num_entries * MAINSD_ENTRY_OFFSET_SIZE;
    let mut data = vec![0u8; MAINSD_ENTRY_OFFSET_TABLE_START + offset_table_size];

    data[0..4].copy_from_slice(b"ANIM");
    data[4] = 1; // scale: SD is definitionally 1 texel/logical px
    data[5] = MAINSD_TYPE_SD;
    data[6..8].copy_from_slice(&0u16.to_le_bytes()); // unknown
    data[8..10].copy_from_slice(&(num_layers as u16).to_le_bytes());
    data[10..12].copy_from_slice(&(num_entries as u16).to_le_bytes());

    for (i, name) in layer_names.iter().enumerate() {
        let start = MAINSD_LAYER_NAME_REGION_START + i * MAINSD_LAYER_NAME_SLOT_SIZE;
        data[start..start + name.len()].copy_from_slice(name.as_bytes());
    }

    let mut entry_offsets = Vec::with_capacity(num_entries);

    // Entry 0: the real entry.
    entry_offsets.push(data.len() as u32);
    {
        let header_pos = data.len();
        data.extend(std::iter::repeat_n(0u8, MAINSD_ENTRY_HEADER_SIZE));

        let layer_records_pos = data.len();
        data.extend(std::iter::repeat_n(
            0u8,
            num_layers * MAINSD_LAYER_RECORD_SIZE,
        ));

        let frame_arr_offset = data.len();
        data.extend(std::iter::repeat_n(
            0u8,
            frames.len() * MAINSD_FRAME_RECORD_SIZE,
        ));
        for (fi, frame) in frames.iter().enumerate() {
            let fo = frame_arr_offset + fi * MAINSD_FRAME_RECORD_SIZE;
            data[fo..fo + 2].copy_from_slice(&frame.texture_x.to_le_bytes());
            data[fo + 2..fo + 4].copy_from_slice(&frame.texture_y.to_le_bytes());
            data[fo + 4..fo + 6].copy_from_slice(&frame.offset_x.to_le_bytes());
            data[fo + 6..fo + 8].copy_from_slice(&frame.offset_y.to_le_bytes());
            data[fo + 8..fo + 10].copy_from_slice(&frame.width.to_le_bytes());
            data[fo + 10..fo + 12].copy_from_slice(&frame.height.to_le_bytes());
            // unknown u32 left zeroed
        }

        let layer_payloads: [(&[u8], u16, u16); 2] = [
            (&diffuse_payload, diffuse_w, diffuse_h),
            (&teamcolor_payload, diffuse_w, diffuse_h),
        ];
        for (li, (payload, w, h)) in layer_payloads.iter().enumerate() {
            let rec_pos = layer_records_pos + li * MAINSD_LAYER_RECORD_SIZE;
            let payload_off = data.len();
            data[rec_pos..rec_pos + 4].copy_from_slice(&(payload_off as u32).to_le_bytes());
            data[rec_pos + 4..rec_pos + 8].copy_from_slice(&(payload.len() as u32).to_le_bytes());
            data[rec_pos + 8..rec_pos + 10].copy_from_slice(&w.to_le_bytes());
            data[rec_pos + 10..rec_pos + 12].copy_from_slice(&h.to_le_bytes());
            data.extend_from_slice(payload);
        }

        data[header_pos..header_pos + 2].copy_from_slice(&(frames.len() as u16).to_le_bytes());
        data[header_pos + 2..header_pos + 4].copy_from_slice(&MAINSD_NO_REF_ID.to_le_bytes());
        data[header_pos + 4..header_pos + 6].copy_from_slice(&0u16.to_le_bytes()); // canvas_width
        data[header_pos + 6..header_pos + 8].copy_from_slice(&0u16.to_le_bytes()); // canvas_height
        data[header_pos + 8..header_pos + 12]
            .copy_from_slice(&(frame_arr_offset as u32).to_le_bytes());
    }

    // Entry 1: a 12-byte inline reference pointing at entry 0.
    entry_offsets.push(data.len() as u32);
    {
        let mut hdr = [0u8; MAINSD_ENTRY_HEADER_SIZE];
        hdr[2..4].copy_from_slice(&0u16.to_le_bytes()); // ref_id = 0 (target entry)
        data.extend_from_slice(&hdr);
    }

    for (i, off) in entry_offsets.iter().enumerate() {
        let pos = MAINSD_ENTRY_OFFSET_TABLE_START + i * MAINSD_ENTRY_OFFSET_SIZE;
        data[pos..pos + MAINSD_ENTRY_OFFSET_SIZE].copy_from_slice(&off.to_le_bytes());
    }

    data
}

/// Writes a synthetic (not Blizzard-derived) seed for the `mainsd_parse` target: a valid
/// 2-layer, 2-entry `mainSD.anim` bundle (one real entry with both a `diffuse` and `teamcolor`
/// layer, plus a reference entry pointing at it).
fn write_mainsd_parse_seed(seeds_root: &Path) {
    let dir = seeds_root.join("mainsd_parse");
    fs::create_dir_all(&dir).expect("create mainsd_parse seed dir");

    let sd_bytes = make_mainsd_seed();
    {
        let sd_anim = MainSdAnim::parse(&sd_bytes).expect("seed mainSD.anim should parse");
        assert_eq!(
            sd_anim.num_entries(),
            2,
            "seed mainSD.anim should round-trip"
        );

        let real = sd_anim.entry(0).expect("real entry should resolve");
        assert_eq!(real.frame_count(), 2);
        let diffuse = real
            .layer("diffuse")
            .expect("diffuse layer should be present");
        parse_dds(diffuse.data).expect("seed diffuse layer should parse as DDS");
        let teamcolor = real
            .layer("teamcolor")
            .expect("teamcolor layer should be present");
        parse_teamcolor_mask(teamcolor.data, teamcolor.width, teamcolor.height)
            .expect("seed teamcolor layer should parse as a mask");

        let referenced = sd_anim.entry(1).expect("ref entry should resolve");
        assert_eq!(
            referenced.frame_count(),
            2,
            "ref entry should resolve to the real entry's frames"
        );
    }
    write_seed(&dir, "sd_bundle_valid.bin", &sd_bytes);
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
