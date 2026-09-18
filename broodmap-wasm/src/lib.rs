//! Browser bindings for broodmap's preview renderer and terrain analysis: parse a map, learn which assets it needs
//! (the two-round prefetch API), feed fetched bytes back in, render.
//!
//! The render core is sync while browser fetches are async, so the flow is
//! prefetch-then-render over a [`MemorySource`] (see `docs/render-design.md`, "Data sources"):
//!
//! ```js
//! const map = new MapRenderer(mapBytes);
//! const opts = JSON.stringify({ style: "original", size: 1024 });
//! for (const path of map.requiredAssets(opts)) {
//!     map.addAsset(path, await fetchBytes(`${assetBase}/${path}`));
//! }
//! for (const path of map.requiredGraphics(opts)) {
//!     map.addAsset(path, await fetchBytes(`${assetBase}/${path}`));
//! }
//! const image = map.renderRgba(opts); // or renderPng(opts) / minimapPng(opts) / planJson(opts)
//! ```
//!
//! Analysis is independent of artwork: `requiredMapAnalysisAssets()` names CV5, VF4, and
//! units.dat for `analyzeMap(cv5, vf4, units)`, returning an owned [`TerrainAnalysis`] snapshot.
//! Use `cellFlags()` for overlays and `routeJson()` for point-grid routes; `setObstaclesEnabled()`
//! switches between static map obstacles and terrain alone. `findBasesJson("{}")` discovers stock
//! resource-base candidates; `baseRouteJson(a, b)` compares their ground/air distances.
//! `requiredAnalysisAssets()` and
//! `analyzeTerrain(cv5, vf4)` provide the terrain-only subset. These methods accept bytes directly,
//! without `addAsset`, and do not establish exact engine pathing or building placement legality.
//!
//! Asset keys are the CASC catalog paths (`AssetRequest::casc_path`), which double as URL
//! suffixes: a directory produced by `broodmap-cli fetch-assets` served statically is a
//! complete asset origin. Assets that fail to fetch can simply be skipped — a missing anim/GRP
//! degrades that one drawable, exactly like the native renderer.
//!
//! Options are a JSON string mirroring the CLI's flags (all fields optional):
//! `{ "style": "original"|"remastered"|"cartooned", "unitStyle": ..., "size": 1024,
//! "startLocations": "block"|"sprite"|"hidden", "asPlaced": false, "showCritters": true,
//! "showResources": true, "showDoodads": true, "showNeutralBuildings": true,
//! "showShadows": true, "terrainOnly": false, "scale": 4 }` (`scale` is minimap-only).

mod analysis;

pub use analysis::TerrainAnalysis;

use std::collections::HashMap;

use wasm_bindgen::prelude::*;

use broodmap::chk::Chk;
use broodmap::extract_chk_from_map;
use broodmap_render::{
    ArtStyle, AssetRequest, GameData, MemorySource, MinimapOptions, RenderOptions, StartLocations,
    UnitFilter, plan_chk_preview, render_chk_minimap, render_chk_preview, render_terrain,
    required_preview_assets_for_chk, required_preview_graphics_for_chk,
};

/// The JSON options object (see the module docs). Every field is optional; defaults match the
/// library's own ([`RenderOptions::default`] plus `scale: 1`).
#[derive(Debug, Default, serde::Deserialize)]
#[serde(rename_all = "camelCase", default, deny_unknown_fields)]
struct JsOptions {
    style: Option<String>,
    unit_style: Option<String>,
    size: Option<u32>,
    start_locations: Option<String>,
    as_placed: bool,
    show_critters: Option<bool>,
    show_resources: Option<bool>,
    show_doodads: Option<bool>,
    show_neutral_buildings: Option<bool>,
    show_shadows: Option<bool>,
    terrain_only: bool,
    /// Minimap only: the integer upscale of the native (<= 128 px) minimap image.
    scale: Option<u32>,
}

impl JsOptions {
    fn parse(json: &str) -> Result<JsOptions, String> {
        if json.trim().is_empty() {
            return Ok(JsOptions::default());
        }
        serde_json::from_str(json).map_err(|e| format!("invalid options: {e}"))
    }

    fn render_options(&self) -> Result<RenderOptions, String> {
        let defaults = RenderOptions::default();
        Ok(RenderOptions {
            art_style: parse_style(self.style.as_deref())?.unwrap_or_default(),
            unit_style: parse_style(self.unit_style.as_deref())?,
            max_dimension: self.size.or(Some(1024)),
            start_locations: self.start_locations()?,
            unit_filter: self.unit_filter(),
            show_critters: self.show_critters.unwrap_or(defaults.show_critters),
            show_resources: self.show_resources.unwrap_or(defaults.show_resources),
            show_doodad_sprites: self.show_doodads.unwrap_or(defaults.show_doodad_sprites),
            show_neutral_buildings: self
                .show_neutral_buildings
                .unwrap_or(defaults.show_neutral_buildings),
            show_shadows: self.show_shadows.unwrap_or(defaults.show_shadows),
            ..defaults
        })
    }

    fn minimap_options(&self) -> Result<MinimapOptions, String> {
        Ok(MinimapOptions {
            scale: self.scale.unwrap_or(1),
            start_locations: self.start_locations()?,
            unit_filter: self.unit_filter(),
            ..Default::default()
        })
    }

    fn start_locations(&self) -> Result<StartLocations, String> {
        match self.start_locations.as_deref() {
            None => Ok(StartLocations::default()),
            Some("block") => Ok(StartLocations::ColorBlock),
            Some("sprite") => Ok(StartLocations::Sprite),
            Some("hidden") => Ok(StartLocations::Hidden),
            Some(other) => Err(format!(
                "unknown startLocations \"{other}\" (expected block, sprite or hidden)"
            )),
        }
    }

    fn unit_filter(&self) -> UnitFilter {
        if self.as_placed {
            UnitFilter::AsPlaced
        } else {
            UnitFilter::Melee
        }
    }
}

fn parse_style(style: Option<&str>) -> Result<Option<ArtStyle>, String> {
    match style {
        None => Ok(None),
        Some("original") => Ok(Some(ArtStyle::Original)),
        Some("remastered") => Ok(Some(ArtStyle::Remastered)),
        Some("cartooned") | Some("carbot") => Ok(Some(ArtStyle::Cartooned)),
        Some(other) => Err(format!(
            "unknown style \"{other}\" (expected original, remastered or cartooned)"
        )),
    }
}

/// A rendered RGBA image, ready for `new ImageData(new Uint8ClampedArray(data), width, height)`.
#[wasm_bindgen]
pub struct RenderedImage {
    width: u32,
    height: u32,
    data: Vec<u8>,
    warnings: Vec<String>,
}

#[wasm_bindgen]
impl RenderedImage {
    #[wasm_bindgen(getter)]
    pub fn width(&self) -> u32 {
        self.width
    }

    #[wasm_bindgen(getter)]
    pub fn height(&self) -> u32 {
        self.height
    }

    /// The RGBA8 pixels, row-major (copies out of WASM memory).
    #[wasm_bindgen(getter)]
    pub fn data(&self) -> Vec<u8> {
        self.data.clone()
    }

    /// Non-fatal notes about things the render skipped.
    #[wasm_bindgen(getter)]
    pub fn warnings(&self) -> Vec<String> {
        self.warnings.clone()
    }
}

/// A parsed map plus the assets fed to it so far. See the module docs for the prefetch/render
/// flow.
#[wasm_bindgen]
pub struct MapRenderer {
    chk: Chk,
    source: MemorySource,
    /// Reverse lookup for [`MapRenderer::add_asset`]: every request this map has ever named
    /// (via `requiredAssets`/`requiredGraphics`), keyed by its CASC path.
    known: HashMap<String, AssetRequest>,
}

#[wasm_bindgen]
impl MapRenderer {
    /// Parses a `.scm`/`.scx` map file (MPQ + CHK).
    #[wasm_bindgen(constructor)]
    pub fn new(map_bytes: &[u8]) -> Result<MapRenderer, String> {
        let (chk, _mpq) = extract_chk_from_map(map_bytes, None, None)
            .map_err(|e| format!("failed to parse map: {e}"))?;
        Ok(MapRenderer {
            chk,
            source: MemorySource::new(),
            known: HashMap::new(),
        })
    }

    /// Map width in tiles (0 if the map has no readable terrain).
    #[wasm_bindgen(getter, js_name = widthTiles)]
    pub fn width_tiles(&self) -> u32 {
        self.chk.terrain().map(|t| t.width as u32).unwrap_or(0)
    }

    /// Map height in tiles (0 if the map has no readable terrain).
    #[wasm_bindgen(getter, js_name = heightTiles)]
    pub fn height_tiles(&self) -> u32 {
        self.chk.terrain().map(|t| t.height as u32).unwrap_or(0)
    }

    /// Round 1 of the prefetch: the CASC paths of the base assets (tileset tables + `.dat`
    /// tables) this map needs under `options`. Fetch and [`add_asset`](Self::add_asset) all of
    /// them before calling [`required_graphics`](Self::required_graphics).
    #[wasm_bindgen(js_name = requiredAssets)]
    pub fn required_assets(&mut self, options: &str) -> Result<Vec<String>, String> {
        let opts = JsOptions::parse(options)?.render_options()?;
        Ok(self.register(required_preview_assets_for_chk(&self.chk, &opts)))
    }

    /// Round 2 of the prefetch: the CASC paths of the art assets (anims, SD canvas GRPs) the
    /// map's units and sprites need under `options`. Requires round 1's tables to already be
    /// added. A path that fails to fetch can simply be skipped — that one drawable degrades,
    /// the render still succeeds.
    #[wasm_bindgen(js_name = requiredGraphics)]
    pub fn required_graphics(&mut self, options: &str) -> Result<Vec<String>, String> {
        let opts = JsOptions::parse(options)?.render_options()?;
        let data = GameData::load(&self.source)
            .map_err(|e| format!("game data tables not loaded (add requiredAssets first): {e}"))?;
        Ok(self.register(required_preview_graphics_for_chk(&self.chk, &data, &opts)))
    }

    /// Feeds one fetched asset back in, keyed by the CASC path `requiredAssets`/
    /// `requiredGraphics` returned. Unknown paths are an error (they'd silently never be read).
    #[wasm_bindgen(js_name = addAsset)]
    pub fn add_asset(&mut self, casc_path: &str, bytes: Vec<u8>) -> Result<(), String> {
        let request = self.known.get(casc_path).ok_or_else(|| {
            format!(
                "unknown asset path \"{casc_path}\" (not named by requiredAssets/requiredGraphics)"
            )
        })?;
        self.source.insert(request.clone(), bytes);
        Ok(())
    }

    /// Renders the map preview to RGBA pixels.
    #[wasm_bindgen(js_name = renderRgba)]
    pub fn render_rgba(&self, options: &str) -> Result<RenderedImage, String> {
        let js_opts = JsOptions::parse(options)?;
        let opts = js_opts.render_options()?;
        if js_opts.terrain_only {
            let terrain = self
                .chk
                .terrain()
                .map_err(|e| format!("map has no readable terrain: {e}"))?;
            let image = render_terrain(terrain, self.chk.tileset(), &self.source, &opts)
                .map_err(|e| format!("render failed: {e}"))?;
            return Ok(RenderedImage {
                width: image.width,
                height: image.height,
                data: image.data,
                warnings: Vec::new(),
            });
        }
        let preview = render_chk_preview(&self.chk, &self.source, &opts)
            .map_err(|e| format!("render failed: {e}"))?;
        Ok(RenderedImage {
            width: preview.image.width,
            height: preview.image.height,
            data: preview.image.data,
            warnings: preview.warnings,
        })
    }

    /// Renders the map preview and encodes it as a PNG.
    #[wasm_bindgen(js_name = renderPng)]
    pub fn render_png(&self, options: &str) -> Result<Vec<u8>, String> {
        let image = self.render_rgba(options)?;
        broodmap_render::RgbaImage {
            width: image.width,
            height: image.height,
            data: image.data,
        }
        .encode_png()
        .map_err(|e| format!("PNG encoding failed: {e}"))
    }

    /// Renders the zero-asset minimap to RGBA pixels. Needs no assets at all; if round 1's
    /// `.dat` tables happen to be loaded they refine dot sizing and melee filtering.
    #[wasm_bindgen(js_name = minimapRgba)]
    pub fn minimap_rgba(&self, options: &str) -> Result<RenderedImage, String> {
        let opts = JsOptions::parse(options)?.minimap_options()?;
        let data = GameData::load(&self.source).ok();
        let preview = render_chk_minimap(&self.chk, data.as_ref(), &opts);
        Ok(RenderedImage {
            width: preview.image.width,
            height: preview.image.height,
            data: preview.image.data,
            warnings: preview.warnings,
        })
    }

    /// Renders the zero-asset minimap and encodes it as a PNG.
    #[wasm_bindgen(js_name = minimapPng)]
    pub fn minimap_png(&self, options: &str) -> Result<Vec<u8>, String> {
        let image = self.minimap_rgba(options)?;
        broodmap_render::RgbaImage {
            width: image.width,
            height: image.height,
            data: image.data,
        }
        .encode_png()
        .map_err(|e| format!("PNG encoding failed: {e}"))
    }

    /// Builds the render plan (the decide/draw split — see `broodmap-render`'s `plan` module)
    /// as JSON, for external executors like a GPU renderer. Requires round 1's assets.
    #[wasm_bindgen(js_name = planJson)]
    pub fn plan_json(&self, options: &str) -> Result<String, String> {
        let opts = JsOptions::parse(options)?.render_options()?;
        let plan = plan_chk_preview(&self.chk, &self.source, &opts)
            .map_err(|e| format!("planning failed: {e}"))?;
        serde_json::to_string(&plan).map_err(|e| format!("plan serialization: {e}"))
    }
}

impl MapRenderer {
    /// Registers requests in the path -> request lookup and returns their CASC paths.
    fn register(&mut self, requests: Vec<AssetRequest>) -> Vec<String> {
        requests
            .into_iter()
            .map(|request| {
                let path = request.casc_path();
                self.known.insert(path.clone(), request);
                path
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn options_parse_defaults_and_rejects_unknown_fields() {
        let opts = JsOptions::parse("").unwrap();
        let render = opts.render_options().unwrap();
        assert_eq!(render.art_style, ArtStyle::Original);
        assert_eq!(render.max_dimension, Some(1024));
        assert_eq!(render.unit_filter, UnitFilter::Melee);

        let opts =
            JsOptions::parse(r#"{"style":"cartooned","size":512,"asPlaced":true,"scale":4}"#)
                .unwrap();
        assert_eq!(
            opts.render_options().unwrap().art_style,
            ArtStyle::Cartooned
        );
        assert_eq!(opts.render_options().unwrap().max_dimension, Some(512));
        assert_eq!(
            opts.render_options().unwrap().unit_filter,
            UnitFilter::AsPlaced
        );
        assert_eq!(opts.minimap_options().unwrap().scale, 4);

        assert!(JsOptions::parse(r#"{"styel":"original"}"#).is_err());
        assert!(
            JsOptions::parse(r#"{"style":"hd"}"#)
                .unwrap()
                .render_options()
                .is_err()
        );
    }

    #[test]
    fn map_renderer_round_trips_a_real_map_bundleless() {
        // The zero-asset minimap needs nothing fetched at all, so it exercises parse +
        // rendering end to end without any asset fixtures.
        let map_bytes = include_bytes!("../../broodmap/assets/lt.scm");
        let map = MapRenderer::new(map_bytes).unwrap();
        assert_eq!((map.width_tiles(), map.height_tiles()), (128, 128));

        let minimap = map.minimap_rgba(r#"{"scale":2}"#).unwrap();
        assert_eq!((minimap.width, minimap.height), (256, 256));
        assert_eq!(minimap.data.len(), 256 * 256 * 4);
    }

    #[test]
    fn add_asset_rejects_paths_the_map_never_asked_for() {
        let map_bytes = include_bytes!("../../broodmap/assets/lt.scm");
        let mut map = MapRenderer::new(map_bytes).unwrap();
        assert!(map.add_asset("TileSet/jungle.cv5", vec![0]).is_err());

        let paths = map.required_assets("").unwrap();
        assert!(paths.contains(&"TileSet/jungle.cv5".to_string()));
        assert!(map.add_asset("TileSet/jungle.cv5", vec![0]).is_ok());
    }
}
