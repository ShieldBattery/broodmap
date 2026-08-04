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
use broodmap_render::{
    ArtStyle, CascSource, GameData, RenderOptions, StartLocations, TilesetDataSource,
    render_chk_preview, render_terrain, required_preview_assets, required_preview_assets_for_chk,
    required_preview_graphics, required_preview_graphics_for_chk, required_terrain_assets,
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
        for texel in image.data.chunks_exact(4) {
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
        .chunks_exact(4)
        .zip(preview.image.data.chunks_exact(4))
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
            .chunks_exact(4)
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
        let graphics = required_preview_graphics(
            chk.placed_units().expect("lt.scm should have units"),
            chk.sprites().expect("lt.scm should have sprites"),
            &data,
            map_w,
            map_h,
            &options,
        );
        assert!(
            !graphics.is_empty(),
            "{style:?}: Lost Temple's units must need some art"
        );
        for req in &graphics {
            let bytes = source
                .read(req)
                .unwrap_or_else(|e| panic!("{style:?}: {} unreadable: {e}", req.casc_path()));
            assert!(!bytes.is_empty(), "{}", req.casc_path());
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

/// Mixing styles across layers must actually change the pixels, and the `Original` unit layer
/// must degrade to a warning rather than an error.
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

    // `Original` units aren't implemented yet: warn and skip the art, don't fail.
    let original_units = RenderOptions {
        unit_style: Some(ArtStyle::Original),
        ..cartooned
    };
    let preview =
        render_chk_preview(&chk, &source, &original_units).expect("original-unit preview");
    assert_eq!(preview.warnings.len(), 1, "{:?}", preview.warnings);
    assert!(preview.warnings[0].contains("mainSD.anim"));
}
