# Map rendering design

Status: draft, pre-implementation. This documents the design for rendering map images (and
minimap-style images) from Brood War maps using StarCraft: Remastered assets.

## Goals

- Render map previews from a parsed `Chk` plus SC:R game assets: images a player can use to
  navigate/evaluate the map. A preview is more than terrain — it always includes resources,
  preplaced buildings (e.g. destructible temples), and preplaced units (eggs, sunken colonies,
  and similar are common map elements). "Terrain-only" is an implementation milestone, not a
  product mode.
- Render minimap-style images: preset color per terrain tile (ideally matching the game's own
  minimap algorithm) plus unit/resource dots. This mode requires no graphical assets at runtime,
  making it a zero-dependency and low-resource fallback as well as a fast path.
- Assets come from a local SC:R install or Blizzard's CDN via `broodcasc` (sibling repo), or from
  any custom byte source.
- Output is configurable: start location display, unit class toggles (critters, neutral
  buildings, resources, doodad sprites), melee-mode filtering, output scale, encoding.
- WASM compilation must remain possible for every core crate. Consumers span: native CLI, a
  TS/Node server (WASM), Electron, browser with GPU rendering, and neobrood (Bevy) sharing the
  format parsers.
- Remastered assets are the primary target, deliberately: no known library today renders map
  previews with Remastered assets, so it is this project's defining feature. `ArtStyle::Original`
  (SD) support follows, not leads (see phasing).

## Non-goals

- The classic 1.16.1 pipeline (GRP + WPE palettes + VX4/VR4 minitile assembly, tunit.pcx
  remapping). We render exclusively from Remastered asset formats across all quality tiers. The
  data-source seam leaves the door open if someone wants this later.
- Animation: palette cycling, iscript playback, water/lava animation. Static first-frame renders
  only.
- Game-accurate creep edge transitions (`.tmsk`) in v1. Creep rendering starts simple.
- Writing/authoring any of these formats.

## Crate structure

```
broodmap           CHK/MPQ parsing (unchanged)
broodmap-formats   SC:R asset format parsers: CV5, VF4, VX4/VX4EX, VR4, WPE, .dds.vr4,
                   .anim (HD + SD), .dat (units/sprites/flingy/images), .rel, .tbl, .lo,
                   DDS container
broodmap-render    data-source trait, BC decode -> RGBA, render plan, CPU rasterizer,
                   RenderOptions, minimap, encoders
broodmap-cli       grows a real `render` subcommand (dogfood + manual testing)
```

`broodmap-formats` exists as its own crate (rather than a module of `broodmap-render`) because
neobrood is a second consumer: its CV5/VF4 and HD `.anim` parsers are pure parsing interleaved
with Bevy `AssetLoader` plumbing, and its .dat parsing lives in `gen_rules` codegen. The plan is
to extract those parsers here (preserving their tested behavior), let neobrood keep thin Bevy
shims, and add what neobrood lacks (SD `mainSD.anim`) where both projects benefit.

All crates follow broodmap conventions: nom parsers, sync, no I/O in core code paths, thiserror,
fuzz targets per parser, WASM-clean.

### Parse != decode

Format parsers return structure plus raw payload slices borrowed from the input — e.g. the anim
parser yields the frame table and each layer's DDS bytes; the `.dds.vr4` parser yields
megatile-indexed DDS blobs. They never decode pixels. This is required because consumers split:

- neobrood and the browser GPU path upload BC-compressed payloads directly to the GPU
  (WebGL2 S3TC / WebGPU `texture-compression-bc`).
- The CPU rasterizer and mobile-browser fallback decode BC1/BC3 to RGBA.

BC decoding to RGBA lives in `broodmap-render` (pure Rust — `texpresso` or hand-rolled BC1/BC3),
not in the parsers.

## Asset formats and tiers

Quality tiers: `AssetTier { Sd, Hd2, Hd }`. Tile sizes 32/64/128 px. CASC path prefixes are
roughly `sd/`, `hd2/`, and none (HD at the root) — exact paths to be verified against
`Storage::file_names()` in phase 1, along with the exact HD anim filename scheme
(`anim/main_###.anim` or similar) and Carbot pack layout (out of scope but shouldn't be
precluded by the path mapping).

Terrain: CHK tile ID -> (CV5 group, index) -> megatile ID -> frame in
`{tier}/TileSet/{name}.dds.vr4`, where frame index == megatile ID. VF4 is parsed for
completeness (neobrood needs it for pathing) but rendering doesn't use it.

The `.dds.vr4` container was reverse-engineered from real files and verified bit-exactly
against the classic `vx4ex -> vr4 -> wpe` chain (all 49,369 megatiles across the 8 tilesets;
full spec in `broodmap-formats/src/dds_vr4.rs`): common header of u32 file size, u16 frame
count, u16 format code (low nibble = scale 1/2/4, bit 0x10 = paletted). HD (0x1004) and HD2
(0x1002) frames are per-record DDS blobs (BC1/BC3); SD (0x1011) is self-contained — an
embedded 256-entry RGB palette (identical to the tileset's `.wpe`) followed by raw 8-bit
indexed 32x32 tiles. So `ArtStyle::Original` terrain needs no palette/VX4/VR4 files at
render time either, and lands in phase 1 alongside HD/HD2 rather than phase 3 (only the
unit/sprite side of Original, `mainSD.anim`, remains phase 3).

Units/sprites: placed unit / THG2 sprite ID -> units.dat -> flingy.dat -> sprites.dat ->
images.dat -> image ID -> `.rel` redirection -> anim. HD/HD2: one `.anim` per image ID. SD: a
single bundled `mainSD.anim` with per-entry offsets and shared atlas textures — a different
container, same building blocks; this is new work (neobrood explicitly doesn't support it) and
its own parser module. Player color uses the anim `teamcolor` mask layer composited with a
player color, replacing classic palette remapping.

Art style is the only user-facing quality knob: `ArtStyle { Original, Remastered, Cartooned }`.
The SD art is genuinely different art from the HD art (HD2 is the same art as HD at half
resolution), and Cartooned (the "StarCraft: Cartooned" / Carbot pack, CASC paths with a
`Carbot/` infix like `HD2/Carbot/TileSet/...`) ships the same HD/HD2 sizes as Remastered. The
concrete tier is always derived, never chosen: `Original` is SD (the only resolution that art
exists at); the Remastered-family styles use HD only when the effective px/tile exceeds HD2's
native 64 — including treating "no size cap" as HD2-native, since full HD is enormous
(128px/tile) and only worth fetching when the output actually shows it. Invalid style/tier
combinations (original at HD, SD Cartooned) are unrepresentable rather than runtime errors.
Requested output size is honored by scaling at draw time —
each megatile/sprite frame is decoded and downscaled to the effective tile size before
blitting, so memory stays proportional to the output buffer. This matters: compositing a
256x256 map at full HD (128 px/tile) would be a 32k x 32k image (~4 GB RGBA); "1024x1024
rendered from HD assets" must never materialize that intermediate.

## broodmap-render API

### Data sources

```rust
pub enum AssetRequest {
  Cv5(Tileset),
  TilesetDds(Tileset, AssetTier, ArtPack),
  Dat(DatKind),                 // units, sprites, flingy, images
  ImagesTbl,
  ImagesRel,
  Anim(u16, AssetTier),         // image ID; SD maps to the single mainSD.anim
  // ...
}

pub trait TilesetDataSource {
  fn read(&self, req: &AssetRequest) -> Result<Bytes, SourceError>;
}
```

Keyed by what is needed, not by path — all game-data knowledge stays in the render crate;
sources are dumb byte fetchers. `AssetRequest` is `Eq + Hash` with a stable string form for
keying caches/URLs. Provided implementations:

- `CascSource` (feature `casc`): wraps `broodcasc::Storage` or `CdnStorage` (both expose the
  same read surface). Local install and CDN come along together. Non-default feature; publishing
  is gated on broodcasc reaching crates.io.
- `DirSource` (feature `fs`): plain directory of extracted files (CascView trees, test
  fixtures).
- `MemorySource`: prefilled map of `AssetRequest -> bytes`. The primary WASM pattern — the trait
  stays sync; browser fetch is async; prefetch-then-render bridges the two.
- `FnSource`: closure adapter for anything else synchronous (OPFS sync handles in a worker,
  custom bundles, tests).

### Prefetch support (two rounds)

Knowing which anims a map needs requires the .dat chain, which is itself an asset:

```rust
pub fn required_base_assets(chk: &Chk, opts: &RenderOptions) -> Vec<AssetRequest>;
// after loading those into GameData:
pub fn required_graphics(chk: &Chk, data: &GameData, opts: &RenderOptions) -> Vec<AssetRequest>;
```

WASM flow: parse CHK -> fetch round 1 -> resolve -> fetch round 2 -> render, all sync between
fetches. Native sources just answer `read()` on demand and never see the rounds. Options
participate in round 2: filtered-out units' anims are never requested.

### Render plan (decide vs. draw)

The compositor's intermediate representation is public: a tile layer (megatile ID grid), an
ordered sprite list (`{ image_ref, frame, x, y, flip, tint }`), and a texture manifest keyed by
`AssetRequest`. Plain structs, `serde` behind a feature for the JS boundary.

- The built-in CPU rasterizer is one executor of the plan (BC decode -> RGBA blit).
- A browser GPU consumer is another: WASM emits plan + raw BC payloads, JS uploads compressed
  textures and draws quads. Mobile browsers (no BC support) fall back to CPU decode + RGBA
  upload, executing the same plan.
- A TS server uses the CPU path end-to-end to PNG. Electron picks per context.

### RenderOptions

Plain struct/builder, applied at plan construction (so filtering also prunes asset fetching).
Defaults are preview-oriented: everything a navigating player would want visible is on —
resources, preplaced buildings and units, doodad sprites, critters (toggleable, but most map
previews keep them).

- `start_locations: Hidden | Marker { style } | Sprite`
- toggles: critters, neutral buildings, resources, THG2 doodad sprites
- `unit_filter: AsPlaced | Melee` — melee drops preplaced player-owned units (the game replaces
  them with starting workers) and keeps neutral units/resources; decided from CHK ownership,
  forces, and unit properties
- creep on/off
- `art_style: Original | Remastered | Cartooned`; target output size (the HD/HD2 tier is
  derived from these two — there is deliberately no explicit tier knob)
- `max_output_pixels` (default 64Mi px = 256 MiB RGBA): a hard output-buffer budget. The
  resolution is clamped down (before tier selection, so budget-shrunk renders also fetch the
  cheaper tier) rather than errored, so rendering always succeeds within bounded memory.

`required_terrain_assets(tileset, map_w, map_h, options)` exposes exactly which
`AssetRequest`s a render will make, so prefetching callers (WASM) don't duplicate
tier-selection logic. The phase-2 two-round `required_*` API extends this.

### Minimap

Not a downsampled preview: a preset color per terrain tile, the way the game's minimap works.
We match BW's actual algorithm, confirmed from OpenBW (`ui/ui.h`, `draw_minimap`), which
renders exactly 1 px per map tile:

- Per tile, take its megatile; look up minitile `[0]` (top-left of the 4x4 grid) in VX4;
  ignore the horizontal-flip bit (the game's minimap path deliberately doesn't apply it);
  sample byte 55 (row 6, col 7) of that minitile's 8x8 VR4 bitmap; the resulting palette
  index, through WPE, is the tile's color. No averaging.
- Creep-flagged tiles substitute a pseudo-random megatile from the creep tile group
  (`cv5[1]`), weighted ~4% toward variant tiles 6-12, else uniform 0-5, chosen once per map
  cell. We replicate with a deterministic per-cell hash so output is stable.

The baked tables therefore map megatile ID -> RGB (the byte-55 sample resolved through the
palette), plus the 13 creep-group colors per tileset.

Runtime rendering requires no graphical assets: per-megatile (or per-group) colors are baked
into the crate as small static tables per tileset, generated by a dev-time tool (a bin in this
repo, run against real assets via `CascSource`/`DirSource`, output committed — a color table
derived from the art, not the art itself). At 8 tilesets x thousands of megatiles x 3 bytes,
the tables are tens of KB total. This makes minimap rendering a zero-asset fast path: `Chk` in,
image out — usable on a server or in WASM with nothing fetched at all.

On top of the terrain colors: unit/building dots from the standard player color table (sized
from unit bounds), resources optionally highlighted, start locations per the same options as
the preview renderer. Bounded-size output regardless of map size.

### Encoding

Core output is raw RGBA + dimensions; consumers can always bring their own encoder (sharp on
Node, `canvas.toBlob` in browsers). Convenience features, all pure Rust and WASM-clean:

- `png` (`png` crate)
- `jpeg` (`jpeg-encoder`)
- `webp-lossless` (`image-webp`)

Lossy WebP would require libwebp C bindings and is intentionally out — consumer-side concern.

Streaming output (future): rendering is naturally row-ordered, so a banded entry point (e.g.
`render_terrain_to(writer)`) can emit one tile-row of pixels at a time and stream them into an
encoder, dropping peak memory from the full frame to ~one band + tile cache + input bytes (and
letting the pixel budget relax, since memory is bounded by construction). Format caveats: PNG
streams scanline-by-scanline with no loss; baseline JPEG works in MCU-row bands (progressive
does not); WebP encoders need the whole frame, so streaming and WebP don't compose. Phase 2
sprites need y-extent bucketing so each sprite is drawn into every band it intersects (the
render plan computes up front; the executor walks it bandwise).

## Hardening

- Fuzz targets for every parser in `broodmap-formats`, matching existing project practice
  (fuzz/ workspace, seed corpus, CI smoke runs).
- Resource limits in the spirit of broodmap's `ResourceLimits`: cap decoded image dimensions,
  frame counts, and total allocation from untrusted asset bytes.
- Tests against real game data are env-var-gated (like broodcasc's) since Blizzard assets can't
  be committed; pure logic gets tiny synthetic fixtures.

## Phasing

1. **Terrain render (internal milestone: real terrain PNG via CLI — not yet a preview).**
   Scaffold both crates. Parsers: DDS container, CV5, `.dds.vr4`; BC1/BC3 decode;
   `TilesetDataSource` + `CascSource` + `DirSource`; terrain-only CPU render with draw-time
   scaling; `broodmap-cli render` subcommand. Verify real CASC paths/tiers here.
2. **Unit/sprite overlay (milestone: actual map preview — resources, preplaced
   buildings/units, doodads).** .dat/.rel/.tbl parsers (extracted from neobrood's gen_rules
   where applicable), HD/HD2 `.anim` (extracted from neobrood), the two-round `required_*` API,
   RenderOptions filtering, player colors, start location markers.
3. **SD + minimap + WASM ergonomics.** SD `mainSD.anim` parser (completes the
   `ArtStyle::Original` preview path); minimap: color-table generator bin + baked tables +
   zero-asset renderer (depends only on phase 1, can be pulled earlier if wanted);
   `MemorySource`/`FnSource`, encoder features, wasm32 CI build.
4. **Plan API + integrations.** Public serde-able render plan, a browser GPU example, neobrood
   migration to `broodmap-formats`.

## Open questions

- Exact CASC layouts to confirm in phase 1: tier path prefixes, HD anim filename scheme. (VX4/
  VX4EX/VR4/WPE parsers are needed after all — but only by the dev-time minimap table
  generator, not at render time.)
- SD `mainSD.anim` container details — reverse from community docs + real data in phase 3.
- Downscaling quality: visible artifacting has been observed in scaled-down terrain. Likely
  contributors: each tile is filtered independently (fractional sample boundaries don't line
  up across tiles -> seams, the same artifact bw-chk documents), averaging happens in sRGB
  rather than linear space, and pixel-art SD suffers at non-integer ratios. Candidate fixes
  (later phase): composite at tile-native resolution in strips and downscale across tile
  boundaries, gamma-correct averaging, and/or an integer-ratio/nearest mode for SD.
- `scale_rgba` averages channels independently (straight alpha) — exact for opaque terrain,
  but produces dark fringes on translucent sprite edges; switch to premultiplied-alpha
  filtering before phase 2 reuses it for `.anim` frames (TODO recorded in the code).
- ~~BW's exact minimap color algorithm~~ — resolved: OpenBW's `draw_minimap` (see Minimap
  section). Note the generator consequently needs classic VX4(EX)/VR4/WPE parsers after all
  (dev-time only, for table generation — still not needed at render time). bw-chk confirms
  the `.vx4ex` (32-bit entries) variant should be handled alongside classic `.vx4`.
- Crate naming: `broodmap-formats` vs. a standalone name (`scr-formats`); it can start in this
  workspace and be extracted later if neobrood adoption makes that cleaner.
- Publishing order: `broodmap-render` (with `casc` feature) can't hit crates.io until broodcasc
  does; core crates don't depend on it and are unblocked.
