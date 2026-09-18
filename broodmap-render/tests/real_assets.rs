//! Integration tests against a real StarCraft: Remastered install.
//!
//! Gated on the `BROODMAP_TEST_SCR_DIR` environment variable (the install directory containing
//! `.build.info`); each test self-skips when it's unset so CI and asset-less machines stay
//! green. Run locally with e.g.:
//!
//! ```text
//! BROODMAP_TEST_SCR_DIR='C:\Program Files (x86)\StarCraft' cargo test -p broodmap-render --features casc --test real_assets
//! ```
#![cfg(feature = "casc")]

use broodmap::extract_chk_from_map;
use broodmap_formats::{MainSdAnim, parse_grp_header};
use broodmap_render::{
    ArtStyle, AssetRequest, CascSource, GameData, RenderOptions, StartLocations, TilesetDataSource,
    build_minimap_table, compress_minimap_table, render_chk_preview, render_terrain,
    required_preview_assets, required_preview_assets_for_chk, required_preview_graphics,
    required_preview_graphics_for_chk, required_terrain_assets,
};

fn scr_source() -> Option<CascSource> {
    let dir = std::env::var_os("BROODMAP_TEST_SCR_DIR")?;
    Some(CascSource::open(dir).expect("BROODMAP_TEST_SCR_DIR should be a valid SC:R install"))
}

fn lost_temple_bytes() -> Vec<u8> {
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../broodmap/assets/lt.scm");
    std::fs::read(path).expect("test asset lt.scm should exist in broodmap/assets")
}

/// Renders Lost Temple in every art style and sanity-checks the output: exact dimensions,
/// fully opaque, and clearly non-uniform (a broken decode path tends to produce a flat black
/// or single-color image).
#[test]
fn renders_lost_temple_in_all_styles() {
    let Some(source) = scr_source() else {
        eprintln!("skipping: BROODMAP_TEST_SCR_DIR not set");
        return;
    };

    let map_bytes = lost_temple_bytes();
    let (chk, _mpq) = extract_chk_from_map(&map_bytes, None, None).expect("lt.scm should parse");
    let terrain = chk.terrain().expect("lt.scm should have terrain");
    let tileset = chk.tileset();

    for style in [
        ArtStyle::Original,
        ArtStyle::Remastered,
        ArtStyle::Cartooned,
    ] {
        let options = RenderOptions {
            art_style: style,
            max_dimension: Some(256),
            ..Default::default()
        };
        let image = render_terrain(terrain, tileset, &source, &options)
            .unwrap_or_else(|e| panic!("{style:?} render failed: {e}"));

        assert_eq!((image.width, image.height), (256, 256), "{style:?}");
        assert_eq!(image.data.len(), 256 * 256 * 4, "{style:?}");

        let mut distinct = std::collections::HashSet::new();
        for texel in image.data.as_chunks::<4>().0 {
            assert_eq!(texel[3], 255, "{style:?}: terrain must be fully opaque");
            distinct.insert([texel[0], texel[1], texel[2]]);
        }
        assert!(
            distinct.len() > 64,
            "{style:?}: expected varied terrain, got {} distinct colors",
            distinct.len()
        );
    }
}

/// The prefetch API must name exactly the assets the renderer will request, and they must all
/// actually exist in a real install (catches path-scheme regressions: tier prefixes, the
/// Carbot infix, filename stems).
#[test]
fn required_assets_exist_in_real_install() {
    let Some(source) = scr_source() else {
        eprintln!("skipping: BROODMAP_TEST_SCR_DIR not set");
        return;
    };

    let map_bytes = lost_temple_bytes();
    let (chk, _mpq) = extract_chk_from_map(&map_bytes, None, None).expect("lt.scm should parse");
    let terrain = chk.terrain().expect("lt.scm should have terrain");

    for style in [
        ArtStyle::Original,
        ArtStyle::Remastered,
        ArtStyle::Cartooned,
    ] {
        // Both a small render (HD2-family tiers) and a large one (HD tier for the
        // Remastered-family styles).
        for max_dimension in [Some(256), Some(16384)] {
            let options = RenderOptions {
                art_style: style,
                max_dimension,
                max_output_pixels: u32::MAX,
                ..Default::default()
            };
            let assets = required_terrain_assets(
                chk.tileset(),
                terrain.width as u32,
                terrain.height as u32,
                &options,
            );
            for req in &assets {
                let bytes = source.read(req).unwrap_or_else(|e| {
                    panic!(
                        "{style:?}/{max_dimension:?}: {} unreadable: {e}",
                        req.casc_path()
                    )
                });
                assert!(!bytes.is_empty(), "{}", req.casc_path());
            }
        }
    }
}

/// The preview render must actually draw something on top of the terrain (Lost Temple has
/// mineral fields, geysers, doodads and four start locations), and must do it without changing
/// the output's dimensions.
#[test]
fn lost_temple_preview_overlays_the_terrain() {
    let Some(source) = scr_source() else {
        eprintln!("skipping: BROODMAP_TEST_SCR_DIR not set");
        return;
    };

    let map_bytes = lost_temple_bytes();
    let (chk, _mpq) = extract_chk_from_map(&map_bytes, None, None).expect("lt.scm should parse");
    let terrain = chk.terrain().expect("lt.scm should have terrain");

    let options = RenderOptions {
        art_style: ArtStyle::Remastered,
        max_dimension: Some(1024),
        ..Default::default()
    };

    let terrain_only =
        render_terrain(terrain, chk.tileset(), &source, &options).expect("terrain render");
    let preview = render_chk_preview(&chk, &source, &options).expect("preview render");

    assert_eq!((preview.image.width, preview.image.height), (1024, 1024));
    assert_eq!(
        (terrain_only.width, terrain_only.height),
        (preview.image.width, preview.image.height),
        "the overlay must not change the output size"
    );
    assert!(
        preview.warnings.is_empty(),
        "a Remastered preview should have nothing to skip: {:?}",
        preview.warnings
    );

    let changed = terrain_only
        .data
        .as_chunks::<4>()
        .0
        .iter()
        .zip(preview.image.data.as_chunks::<4>().0.iter())
        .filter(|(a, b)| a != b)
        .count();
    assert!(
        changed > 1000,
        "expected the unit/sprite overlay to cover a meaningful area, only {changed} px changed"
    );

    // Start-location tokens are drawn in each owner's color; Lost Temple's four players are
    // red/blue/teal/purple by default. The token rendering applies a vertical gradient
    // (+/-12%), strokes, and antialiasing, so exact byte-equality with the palette color is
    // not guaranteed anywhere — instead require a pixel *near* each expected color (within the
    // gradient's worst-case shift plus rounding slack per channel).
    fn close(a: [u8; 3], b: [u8; 3]) -> bool {
        a.iter().zip(b).all(|(&x, y)| {
            let tol = 2 + (y as u32 * 12).div_ceil(100) as i32; // gradient +/-12% + rounding
            (x as i32 - y as i32).abs() <= tol
        })
    }
    for (slot, expected) in [(0, [244, 4, 4]), (1, [12, 72, 204]), (2, [44, 180, 148])] {
        let found = preview
            .image
            .data
            .as_chunks::<4>()
            .0
            .iter()
            .any(|t| close([t[0], t[1], t[2]], expected));
        assert!(
            found,
            "no pixel near player {slot}'s start-location token color {expected:?}"
        );
    }
}

/// Every asset the two-round prefetch API names must exist in a real install — this is what
/// catches an `.anim`/`.dat` path-scheme regression (tier prefixes, the `anim/Carbot/` infix,
/// the `arr/` directory, the start-location Carbot fallback).
#[test]
fn required_preview_assets_exist_in_real_install() {
    let Some(source) = scr_source() else {
        eprintln!("skipping: BROODMAP_TEST_SCR_DIR not set");
        return;
    };

    let map_bytes = lost_temple_bytes();
    let (chk, _mpq) = extract_chk_from_map(&map_bytes, None, None).expect("lt.scm should parse");
    let terrain = chk.terrain().expect("lt.scm should have terrain");
    let (map_w, map_h) = (terrain.width as u32, terrain.height as u32);

    for style in [ArtStyle::Remastered, ArtStyle::Cartooned] {
        // `Sprite` start locations pull in image 588, whose Carbot art doesn't exist — the
        // request must fall back to the standard pack or this read fails.
        let options = RenderOptions {
            art_style: style,
            max_dimension: Some(1024),
            start_locations: StartLocations::Sprite,
            ..Default::default()
        };

        for req in required_preview_assets(chk.tileset(), map_w, map_h, &options) {
            let bytes = source
                .read(&req)
                .unwrap_or_else(|e| panic!("{style:?}: {} unreadable: {e}", req.casc_path()));
            assert!(!bytes.is_empty(), "{}", req.casc_path());
        }

        let data = GameData::load(&source).expect("the .dat tables should load");
        let units = chk.placed_units().expect("lt.scm should have units");
        let sprites = chk.sprites().expect("lt.scm should have sprites");
        let graphics = required_preview_graphics(units, sprites, &data, map_w, map_h, &options);
        assert!(
            !graphics.is_empty(),
            "{style:?}: Lost Temple's units must need some art"
        );

        // Shadow anims are a best-effort layer (see `RenderOptions::show_shadows`'s docs and
        // `docs/render-design.md`'s "Shadows" note): a real install's Cartooned pack doesn't ship
        // every shadow, and the renderer's contract for that is a silent, per-drawable skip, not
        // an error. So the *baseline* (shadow-free) requests are a hard requirement here — a
        // missing one would mean a real `.anim` path-scheme regression — but any extra request
        // that only exists because of shadows is allowed to 404.
        let baseline_options = RenderOptions {
            show_shadows: false,
            ..options.clone()
        };
        let baseline =
            required_preview_graphics(units, sprites, &data, map_w, map_h, &baseline_options);
        let baseline_set: std::collections::HashSet<_> = baseline.iter().collect();

        for req in &graphics {
            match source.read(req) {
                Ok(bytes) => assert!(!bytes.is_empty(), "{}", req.casc_path()),
                Err(e) if !baseline_set.contains(req) => {
                    eprintln!(
                        "{style:?}: shadow-only asset {} unreadable ({e}) -- tolerated, the \
                         renderer must skip the shadow for it, not fail",
                        req.casc_path()
                    );
                }
                Err(e) => panic!("{style:?}: {} unreadable: {e}", req.casc_path()),
            }
        }
    }
}

/// The `Chk`-based prefetch wrappers must name the same assets as the manual two-round calls,
/// end to end against a real install (catching a mismatch that only the wrappers, or only the
/// manual calls, would have).
#[test]
fn chk_prefetch_wrappers_match_the_manual_calls_and_exist_in_real_install() {
    let Some(source) = scr_source() else {
        eprintln!("skipping: BROODMAP_TEST_SCR_DIR not set");
        return;
    };

    let map_bytes = lost_temple_bytes();
    let (chk, _mpq) = extract_chk_from_map(&map_bytes, None, None).expect("lt.scm should parse");
    let terrain = chk.terrain().expect("lt.scm should have terrain");
    let (map_w, map_h) = (terrain.width as u32, terrain.height as u32);

    let options = RenderOptions {
        art_style: ArtStyle::Remastered,
        max_dimension: Some(1024),
        ..Default::default()
    };

    let round1 = required_preview_assets_for_chk(&chk, &options);
    assert_eq!(
        round1,
        required_preview_assets(chk.tileset(), map_w, map_h, &options)
    );
    for req in &round1 {
        source
            .read(req)
            .unwrap_or_else(|e| panic!("{} unreadable: {e}", req.casc_path()));
    }

    let data = GameData::load(&source).expect("the .dat tables should load");
    let round2 = required_preview_graphics_for_chk(&chk, &data, &options);
    assert_eq!(
        round2,
        required_preview_graphics(
            chk.placed_units().expect("lt.scm should have units"),
            chk.sprites().expect("lt.scm should have sprites"),
            &data,
            map_w,
            map_h,
            &options,
        )
    );
    assert!(!round2.is_empty(), "Lost Temple's units must need some art");
    for req in &round2 {
        source
            .read(req)
            .unwrap_or_else(|e| panic!("{} unreadable: {e}", req.casc_path()));
    }
}

/// Mixing styles across layers must actually change the pixels, and `Original` units now render
/// for real (drawn from `mainSD.anim`) rather than degrading to a warning.
#[test]
fn unit_style_can_differ_from_the_terrain_style() {
    let Some(source) = scr_source() else {
        eprintln!("skipping: BROODMAP_TEST_SCR_DIR not set");
        return;
    };

    let map_bytes = lost_temple_bytes();
    let (chk, _mpq) = extract_chk_from_map(&map_bytes, None, None).expect("lt.scm should parse");

    let cartooned = RenderOptions {
        art_style: ArtStyle::Cartooned,
        max_dimension: Some(512),
        ..Default::default()
    };
    let mixed = RenderOptions {
        unit_style: Some(ArtStyle::Remastered),
        ..cartooned.clone()
    };

    let pure = render_chk_preview(&chk, &source, &cartooned).expect("cartooned preview");
    let mixed = render_chk_preview(&chk, &source, &mixed).expect("mixed preview");
    assert_ne!(
        pure.image.data, mixed.image.data,
        "Remastered units over Cartooned terrain must look different from all-Cartooned"
    );

    // `Original` units are fully supported now: they render (from `mainSD.anim`), no warning.
    let original_units = RenderOptions {
        unit_style: Some(ArtStyle::Original),
        ..cartooned
    };
    let original = render_chk_preview(&chk, &source, &original_units).expect("SD-unit preview");
    assert!(original.warnings.is_empty(), "{:?}", original.warnings);
    assert_ne!(
        pure.image.data, original.image.data,
        "SD units over Cartooned terrain must actually draw different art"
    );
}

/// A full `ArtStyle::Original` render must actually composite SD unit/sprite art (drawn from
/// `mainSD.anim`) over the terrain, not just render terrain -- and it must do so without
/// warnings, now that the `.dat`/`.rel` tables are a hard (and, for a real install, always
/// satisfiable) dependency for every style.
///
/// Also probes `mainSD.anim` directly through the public API as a cheap, high-value regression
/// anchor: image 344 (the vespene geyser's main art) should have one frame per tileset (8), and
/// image 345 (a same-GRP `render_style`-0 variant of it, not a shadow -- its real shadow is 346,
/// via `images.rel`'s type-8 table; see `GameData::shadow_image_pre_redirect`'s docs) should
/// resolve via the container's own inline reference entry, mirroring `images.rel`'s ordinary
/// redirect for the same pair (verified 131/131 agreement against real data -- see
/// `broodmap_formats::mainsd`'s module docs).
#[test]
fn original_style_renders_sd_units() {
    let Some(source) = scr_source() else {
        eprintln!("skipping: BROODMAP_TEST_SCR_DIR not set");
        return;
    };

    let map_bytes = lost_temple_bytes();
    let (chk, _mpq) = extract_chk_from_map(&map_bytes, None, None).expect("lt.scm should parse");
    let terrain = chk.terrain().expect("lt.scm should have terrain");

    let options = RenderOptions {
        art_style: ArtStyle::Original,
        max_dimension: Some(512),
        ..Default::default()
    };

    let preview = render_chk_preview(&chk, &source, &options).expect("SD preview render");
    assert!(preview.warnings.is_empty(), "{:?}", preview.warnings);

    let terrain_only =
        render_terrain(terrain, chk.tileset(), &source, &options).expect("SD terrain render");
    assert_eq!(
        (terrain_only.width, terrain_only.height),
        (preview.image.width, preview.image.height)
    );
    assert_ne!(
        terrain_only.data, preview.image.data,
        "SD unit/sprite art must actually be composited over the terrain"
    );

    let bundle_bytes = source
        .read(&AssetRequest::MainSdAnim)
        .expect("mainSD.anim should be readable from a real install");
    let bundle = MainSdAnim::parse(&bundle_bytes).expect("mainSD.anim should parse");
    let geyser_main = bundle.entry(344).expect("image 344 should resolve");
    assert_eq!(
        geyser_main.frame_count(),
        8,
        "the geyser has one art frame per tileset"
    );
    assert!(
        bundle.entry(345).is_ok(),
        "image 345 (the geyser's +1 variant) should resolve via the container's own reference"
    );

    // Frame-table-equality regression anchor (see `broodmap_formats::grp`'s module docs): image
    // 239 (marine's main art) must keep the same frame count as the classic GRP whose header
    // supplies its SD canvas override -- if this ever drifts, `mainSD.anim`'s frame tables have
    // stopped being the classic GRP frame tables verbatim, which the SD canvas fix depends on.
    let marine_main = bundle.entry(239).expect("image 239 should resolve");
    assert_eq!(marine_main.frame_count(), 229);

    let marine_grp_bytes = source
        .read(&AssetRequest::Grp {
            path: "terran\\marine.grp".to_string(),
        })
        .expect("terran\\marine.grp should be readable from a real install");
    let marine_grp = parse_grp_header(&marine_grp_bytes).expect("marine.grp header should parse");
    assert_eq!(marine_grp.frame_count, 229);
    assert_eq!((marine_grp.width, marine_grp.height), (64, 64));
}

/// Drift guard for the committed minimap tables (`broodmap-render/src/minimap/tables/*.bin`):
/// re-derives Jungle's table from the real install's classic `.cv5`/`.vx4ex`/`.vr4`/`.wpe` files
/// via `build_minimap_table` and byte-compares it against the committed `jungle.bin`. A mismatch
/// means the committed tables are stale -- regenerate them with `cargo run -p broodmap-cli --
/// gen-minimap-tables` and commit the result.
///
/// Reads the classic tileset files directly through `broodcasc::Storage` rather than
/// `broodmap_render::CascSource`/`AssetRequest`: those files are dev-time-only inputs to the
/// minimap table generator (never read at render time), so `AssetRequest` has no variant for
/// them -- see `broodmap_render::build_minimap_table`'s docs.
#[test]
fn committed_jungle_minimap_table_matches_a_fresh_generation() {
    let Some(dir) = std::env::var_os("BROODMAP_TEST_SCR_DIR") else {
        eprintln!("skipping: BROODMAP_TEST_SCR_DIR not set");
        return;
    };
    let storage = broodcasc::Storage::open(dir)
        .expect("BROODMAP_TEST_SCR_DIR should be a valid SC:R install");

    let cv5 = storage
        .read_file("TileSet/jungle.cv5")
        .expect("TileSet/jungle.cv5 should be readable");
    let vx4ex = storage
        .read_file("TileSet/jungle.vx4ex")
        .expect("TileSet/jungle.vx4ex should be readable");
    let vr4 = storage
        .read_file("TileSet/jungle.vr4")
        .expect("TileSet/jungle.vr4 should be readable");
    let wpe = storage
        .read_file("TileSet/jungle.wpe")
        .expect("TileSet/jungle.wpe should be readable");

    let fresh = build_minimap_table(&cv5, &vx4ex, &vr4, &wpe)
        .expect("a real install's assets should build a minimap table");
    let fresh_blob = compress_minimap_table(&fresh);

    let committed = include_bytes!("../src/minimap/tables/jungle.bin");
    assert_eq!(
        fresh_blob.as_slice(),
        committed.as_slice(),
        "committed jungle.bin has drifted from a fresh `gen-minimap-tables` run -- regenerate \
         and commit the 8 tables"
    );
}
