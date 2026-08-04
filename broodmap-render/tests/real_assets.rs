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
    ArtStyle, CascSource, RenderOptions, TilesetDataSource, render_terrain, required_terrain_assets,
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
