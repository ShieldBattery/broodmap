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
  (SD) support follows, not leads (see phasing). (Postscript, phase 3: with the SD path complete,
  `Original` became the *default* style — at preview output sizes the resampler discards most of
  the HD art's extra resolution, the original art tiles better on open ground, and the SD asset
  set is by far the cheapest to fetch. Remastered support remains the differentiating capability;
  it's just opt-in rather than the default.)

## Non-goals

- The classic 1.16.1 pipeline (GRP + WPE palettes + VX4/VR4 minitile assembly, tunit.pcx
  remapping). We render exclusively from Remastered asset formats across all quality tiers. The
  data-source seam leaves the door open if someone wants this later. (Reading a classic GRP's
  6-byte *header* for its canvas dimensions — the SD placement anchor, see
  `broodmap-formats/src/grp.rs` — is metadata lookup, not this pipeline.)
- Animation: palette cycling, iscript playback, water/lava animation. Static first-frame renders
  only.
- Parsing or executing `scripts/iscript.bin` in any form, including a VM-free static extraction
  of its Init animations. This is a permanent non-goal, not a stopgap: everything iscript would
  normally drive at render time (shadow attachment being the running example — see "Shadows"
  below) is instead approximated with a data-only heuristic over `.dat`/`.rel`, verified against
  real data and, where useful, cross-checked offline against a real iscript disassembly as a
  development-time oracle. The oracle informs the heuristic; it is never wired into the crate.
- Game-accurate creep edge transitions (`.tmsk`) in v1. Creep rendering starts simple.
- Writing/authoring any of these formats.

## Crate structure

```
broodmap           CHK/MPQ parsing (unchanged)
broodmap-formats   SC:R asset format parsers: CV5, VF4, VX4/VX4EX, VR4, WPE, .dds.vr4,
                   .anim (HD + SD mainSD), .dat (units/sprites/flingy/images), .rel, .tbl,
                   .lo, DDS container, classic GRP (header only — SD canvas source)
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
single bundled `mainSD.anim` with a per-image entry directory — a different container, same
building blocks (parsed by `broodmap-formats/src/mainsd.rs`, phase 3; neobrood explicitly
doesn't support it). Player color uses the anim `teamcolor` mask layer composited with a
player color, replacing classic palette remapping.

Phase-2 facts pinned from neobrood + a real install (full specs live in the
`broodmap-formats` module docs):

- .dat files are column-major (struct-of-arrays); units.dat 228 entries/19876 B, flingy.dat
  209/3135, sprites.dat 517/3229, images.dat 999/37962. Some columns exist only for ID
  subranges (buildings 106..202, "units" 0..106; sprites 130..517 for selectable fields).
- `images.rel` is 999 x 8-byte records (u32 rel_type, u32 ref_image); `rel_type & 0x200` +
  a non-sentinel ref means "load this image's art from ref_image's anim instead".
- Anim paths: `{tier}anim/{pack}main_{id:03}.anim` (tier prefix ""/"HD2/", pack infix
  "Carbot/"; note the pack infix sits INSIDE anim/, unlike tilesets). `images.tbl` is NOT
  needed for anim resolution — but SD canvas resolution reads it (with `images.dat`'s grp
  column) to locate classic GRP headers; see the `mainSD.anim` bullet below.
- .anim container: 12 B header (magic ANIM, u8 scale, u8 type 1=SD/2=HD, layer/entry counts),
  fixed-size name table to 0x14C, frame-table header (count, ref_id, canvas w/h, table
  offset), per-layer 12 B texture records (absolute offset/size + dims, offset 0 = absent,
  payloads are embedded DDS/PNG — sniff), per-frame 12 B records. HD/HD2 frame coords are in
  fixed "4K" units (1 logical px = 4 units); embedded texels = 4K units / (4 / scale) — HD and
  HD2 literally share one byte-identical frame table authored in HD texels. Layers share one
  frame table. `ref_id != 0xFFFF` = inline reference (unsupported in the HD parser; the SD
  container is where references actually occur).
- `mainSD.anim` (phase 3, pinned empirically — full spec in `broodmap-formats/src/mainsd.rs`):
  same header/name region, but 0x14C holds a per-image entry offset directory (999 u32s, one
  per images.dat row) instead of a frame-table header. Real entries (868) carry HD-shaped
  layer records + frames; reference entries (131) are 12 bytes total — no layer records, no
  frames, everything from the (one-hop, verified chain-free) target. The refs duplicate
  `images.rel`'s 0x200 redirects exactly (131/131 agreement), so the rel-then-lookup flow
  needs no SD special case. Diffuse payloads are plain DDS (DXT1/DXT5); `teamcolor` is NOT
  DDS but a `"BMP "`-magic raw binary stencil (w*h bytes, each 0/255). Crucially the SD frame
  table is separately authored in the file's own SD texel space (divisor 1, not 4K units —
  verified via DXT content bounding boxes and atlas coverage); the parser normalizes entries
  x4 into canonical 4K units so consumers treat them exactly like any other `Anim`. Canvas is
  0x0 for all 999 entries; the true anchor turned out to be the **classic GRP header's
  declared width/height** — `mainSD.anim`'s frame tables are the classic GRP frame tables
  *verbatim* (byte-identical offsets/sizes across all 547 frames of 7 probed files), and BW
  anchors a GRP frame at `pos - grp_w/2 + frame_offset`. The SD draw path therefore reads the
  6-byte GRP header per image (image id -> `images.dat` grp column -> `images.tbl` path ->
  `unit/...grp` in CASC; `AssetRequest::{ImagesTbl, Grp}`; header-only parser in
  `broodmap-formats/src/grp.rs` — the classic GRP *pipeline* stays a non-goal) and falls back
  to per-frame content centering (`effective_canvas`, below) only when the GRP can't be
  resolved. Without the GRP canvas, shadows draw content-centered — i.e. perfectly hidden
  behind their owners — which is how this was discovered. This also corrects the earlier note
  that `images.tbl` was "not needed" for rendering: it wasn't for *anim resolution*, but SD
  canvas resolution reads it.
- Teamcolor compositing, pinned empirically in phase 2 (no reference implementation exists):
  the mask is the teamcolor layer's RED/grayscale channel, not alpha — all 155 teamcolor
  layers in the HD2 corpus are BC1 (no meaningful alpha; every diffuse layer is BC3). Blend is
  multiplicative, per channel: `out = diffuse * ((255 - m) + m * player_rgb / 255) / 255`,
  which preserves the art's shading (straight lerp collapses masked regions into flat
  shadeless color). See `apply_team_color` in `broodmap-render/src/overlay.rs`.
- 10 of 868 HD2 anims (all editor-only graphics, including 588, the start-location marker)
  declare a 0x0 canvas in their frame-table header; taking it literally misplaces the frame by
  its half-extent. Fall back to `2 * offset + size` per axis (see `effective_canvas`).
- THG2 "unit sprite" entries (DRAW_AS_SPRITE clear) hold UNIT ids and resolve through the
  full units.dat chain; only pure-sprite entries (flag set) are sprites.dat ids.
- Facing: units.dat `unit_direction` (0-31; 32 = random, rendered deterministically);
  directional images map direction to frame with horizontal flip for the mirrored half.
- Resource art: mineral frame by amount thresholds and geyser frame by tileset are bw-chk
  conventions (neobrood has neither); we follow bw-chk.

**Shadows.** In the real game, a unit/sprite's shadow is a separate image attached as an underlay
by an iscript `imgul` opcode — a VM this library deliberately never implements or runs (see
Non-goals). In its place, `show_shadows` (default on; `RenderOptions::show_shadows`, CLI
`--no-shadows`) uses exact data: `images.rel`'s `rel_type == 8` records mark every shadow image
and name its parent (owner) image in `ref_image`. `GameData` inverts that table once at load into
a parent -> shadow lookup (`shadow_image_pre_redirect`; 230 records, 218 distinct parents on a
real install; the 12 parents with two records — obscure neutral pickups with `*Shad`/`*Sha2`
variants and doodads listing one shadow GRP under two ids — take the lowest shadow id,
deterministically). The lookup is tried with the main image's PRE-`images.rel`-redirect ID first
(the ID that indexes `images.dat`) and falls back through the redirect, and the result is still
gated on the shadow slot's `images.dat` `render_style == 10` ("shadow" draw style) as a
belt-and-suspenders check: on real data the gate is a no-op (the type-8 set equals the
render-style-10 set exactly, 230 = 230), so it exists purely so a hostile/malformed rel-vs-dat
disagreement fails toward "no shadow" rather than tinting arbitrary art black. Applies uniformly
to `UNIT`-chunk units, THG2 unit-sprites, and THG2 doodad sprites; start-location graphics never
get a shadow. A shadow is emitted as its own [`Drawable`] carrying the owner's exact position,
frame index and flip (shadow anim frame tables mirror their owner's directional tables), pushed
immediately before the owner into the same painter-order sort key — genuine per-drawable ordering
(not a global shadows-first pass), relying on `Vec::sort_by_key`'s stability to keep it
immediately beneath its owner without a separate band. Compositing replaces the shadow anim's
diffuse RGB with black and scales its alpha by a constant (`SHADOW_ALPHA_SCALE`, `overlay.rs`),
calibrated visually to 0.5 (within the 0.4-0.6 range considered); no team color is ever applied
to a shadow, and its tile-cache key is forced onto the no-teamcolor sentinel plus its own
`is_shadow` bit, so every owner sharing a shadow image/frame/flip shares one cached tile. A
missing shadow `.anim` (e.g. the Cartooned/Carbot pack, which does not ship every shadow image)
is a silent per-drawable skip, exactly like a missing main-art anim — the owner still draws, just
without its shadow. Note the SD interplay: a shadow only *lands* correctly because SD frames are
anchored on their classic-GRP-header canvas (see the `mainSD.anim` facts above) — without a real
canvas, shadow and owner both content-center on the same point and the shadow is drawn perfectly
hidden behind its owner, which is exactly the bug that shipped briefly before the canvas fix.

History, kept because it establishes why the table is trustworthy: the first implementation was a
`+1`-slot heuristic (shadow conventionally lives at `main_image_id + 1`, gated on
`render_style == 10`, with a hardcoded `+2` for the vespene geyser whose `+1` is an art variant
of the geyser itself). It was verified against a real install (59.6% of units resolved a shadow:
81/127 non-building units, 49/95 buildings, 6/6 critters) and cross-checked against neobrood's
generated iscript disassembly as a development-time oracle — of 745 resolved unit/sprite image
IDs, only 2 real false positives, with under-coverage (buildings, turret-bearing units whose
shadow sits at `+2`) as the failure mode. The `rel_type == 8` table was then discovered during
the phase-3 `mainSD.anim` reversing and replaced the heuristic outright: it agrees with
everything the heuristic got right (172 of its 230 pairs are the `+1` cases; the geyser's record,
346 -> 344, encodes precisely what the special case hardcoded), covers what the heuristic
couldn't (the 46 non-`+1` parents are almost entirely building shadows — protoss nexus 179 ->
182, terran control tower 63 -> 277, ...), and lifts real-install unit coverage from 59.6% to
80.7% with no heuristic left. HD/HD2 per-image anims exist for the newly covered shadow images
(spot-verified in CASC), so drawing them matches SC:R's own behavior in every art style. The
real-data invariants live on as env-gated tests in `gamedata.rs` (type-8/render-style set
equality, coverage floor, geyser 344 -> 346 straight from the table).

Art style is the only user-facing quality knob: `ArtStyle { Original, Remastered, Cartooned }`.
The SD art is genuinely different art from the HD art (HD2 is the same art as HD at half
resolution), and Cartooned (the "StarCraft: Cartooned" / Carbot pack, CASC paths with a
`Carbot/` infix like `HD2/Carbot/TileSet/...`) ships the same HD/HD2 sizes as Remastered. The
concrete tier is always derived, never chosen: `Original` is SD (the only resolution that art
exists at); the Remastered-family styles use HD only when the effective px/tile exceeds HD2's
native 64 — including treating "no size cap" as HD2-native, since full HD is enormous
(128px/tile) and only worth fetching when the output actually shows it. Invalid style/tier
combinations (original at HD, SD Cartooned) are unrepresentable rather than runtime errors.
Requested output size is honored without ever materializing a full-resolution intermediate —
this matters because compositing a 256x256 map at full HD (128 px/tile) would be a 32k x 32k
image (~4 GB RGBA), and "1024x1024 rendered from HD assets" must not pay that. Sprite frames are
decoded and downscaled individually before blitting; terrain composites at native resolution but
only a strip at a time, then resamples (see "Terrain downscaling"). Either way peak memory stays
proportional to the output buffer plus a bounded working set.

## broodmap-render API

### Data sources

```rust
pub enum AssetRequest {
  Cv5(Tileset),
  TilesetDds(Tileset, AssetTier, ArtPack),
  Dat(DatKind),                 // units, sprites, flingy, images
  ImagesTbl,
  ImagesRel,
  Anim(u16, AssetTier),         // image ID (HD/HD2 per-image files)
  MainSdAnim,                   // the single bundled SD art container
  Grp { path: String },         // classic GRP (images.tbl path) — 6-byte header only,
                                // the SD canvas source
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

Round 1's own rule changed once the render's read pattern turned out wider than "only when unit
art is available": `required_preview_assets` also pulls in the `.dat`/`.rel` tables whenever
`start_locations` is `ColorBlock` (the default), because sizing that token reads `units.dat`'s
placebox even when the unit layer itself is `Original` (unit art unavailable). The tables are
omitted only when *neither* condition holds — i.e. `Original` units with start locations set to
`Sprite` or `Hidden`. `required_preview_assets_for_chk`/`required_preview_graphics_for_chk` wrap
both rounds for callers that already have a parsed `Chk`, so they can't drift from what
`render_chk_preview` actually reads.

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

- `start_locations: ColorBlock (default) | Sprite | Hidden` — the in-game start-location
  graphic reads poorly at preview scale, so the default is the pro-map-preview convention: a
  solid block in the owning player's resolved color, sized to the start location's footprint
  (its units.dat placebox, 4x3 tiles), centered on the placed position. `Sprite` uses the
  actual image (ID 588; note Carbot doesn't ship it — force the Standard pack for that one
  image, matching the game).
- toggles: critters, neutral buildings (`show_neutral_buildings`, implemented — drops
  neutral-owned `UNIT`-chunk entries whose `units.dat` special-ability flags mark them as a
  Building, but never resources or start locations), resources, THG2 doodad sprites
- `unit_filter: Melee (default) | AsPlaced` — previews overwhelmingly serve melee play, and
  the game applies melee rules regardless of what the map placed, so Melee is the default
  (`--as-placed` for the UMS view). Melee drops preplaced player-owned units (the game
  replaces them with starting workers) and clears units whose `units.dat` collision bounds
  overlap a start location's spawn area (the game destroys anything within the spawned HQ's
  bounds — we use the smallest of the three HQs' bounds, since mapmakers design for
  race-independent behavior; collision bounds, NOT the placebox, which is the larger
  placement-grid footprint). Neutral units/resources are kept. Units preplaced in the
  hallucinated state are dropped under BOTH filters — mappers use them to push starting
  workers into position and they expire moments into a real game.
- creep on/off
- `art_style: Original | Remastered | Cartooned`; target output size (the HD/HD2 tier is
  derived from these two — there is deliberately no explicit tier knob)
- `unit_style: Option<ArtStyle>` (phase 2): render the unit/sprite layer in a different style
  than the terrain (e.g. Cartooned terrain + Remastered units). Mixing tiers is cheap because
  all drawing normalizes through draw-time scaling: each tier's art has a fixed scale
  multiplier (SD 1x, HD2 2x, HD 4x; 1 tile = 32 logical px), so a layer's frames — and its
  anim frame offsets, which are authored in the same tier pixels — scale by
  `output_zoom / layer_scale`. `Original` units draw from `mainSD.anim` (phase 3): the
  renderer fetches/parses the bundle once per render (`AssetRequest::MainSdAnim`) and looks
  entries up per image, instead of one `.anim` fetch per image.
- `max_output_pixels` (default 64Mi px = 256 MiB RGBA): a hard output-buffer budget. The
  resolution is clamped down (before tier selection, so budget-shrunk renders also fetch the
  cheaper tier) rather than errored, so rendering always succeeds within bounded memory.

`required_terrain_assets(tileset, map_w, map_h, options)` exposes exactly which
`AssetRequest`s a render will make, so prefetching callers (WASM) don't duplicate
tier-selection logic. The phase-2 two-round `required_*` API extends this.

### Terrain downscaling

Resolved (was an open question). The terrain layer composites megatiles at the art's **native**
tile size into a rolling window of horizontal strips — one tile row live at a time — and resamples
the native image to the output size with a separable **Catmull-Rom** kernel evaluated in
**linear light**. When the output's px/tile already equals the native tile size there is a fast
path that skips filtering entirely, so native-resolution renders are unchanged.

The strip window is what makes whole-image resampling affordable: peak memory is one strip
(`map_w * tile_px` x `tile_px`, at most 16 MiB) plus a ring of a few resampled output rows (~2 MiB
worst case) plus the output buffer. The full native intermediate — 32Ki x 32Ki, 4 GiB, for a
256x256 map at HD — is never materialized, which was the constraint that forced per-tile
downscaling in the first place.

**What this fixes.** The old path box-filtered each megatile in isolation, in sRGB space. A box
filter is a poor low-pass filter, so detail above the output's Nyquist limit folds back in as
speckle; and a per-tile window cannot see the neighbouring tile's pixels that belong in an output
pixel straddling the seam. Together those broke up shorelines and cliff edges into stippled,
discontinuous runs, worst at non-integer ratios (a 64px tile into 7 output pixels: some output
pixels average 9 source rows, some 10, which beats against the art's own texture) but clearly
visible at aligned ratios too. sRGB-space averaging compounded it by darkening every
mixed-brightness pixel.

Measured against a linear-light Lanczos-3 downscale of the native render of Lost Temple (RMSE,
0-1 scale):

| output              | old: per-tile box | new: strip Catmull-Rom |
|---------------------|-------------------|------------------------|
| HD2 `--size 900`    | 0.00963           | 0.00363                |
| HD2 `--size 1024`   | 0.00916           | 0.00370                |
| HD2 `--size 2048`   | 0.00776           | 0.00361                |
| SD  `--size 900`    | 0.01239           | 0.00424                |

Catmull-Rom (B = 0, C = 1/2) was picked over the more usual Mitchell (B = C = 1/3) by
measurement: Mitchell scored 0.0066-0.0069 across the same cases — better than box, but its wide
main lobe visibly softens terrain — while Catmull-Rom's ringing stays under the output clamp even
on this art's highest-contrast content (cliff against water). Cost: the terrain render goes from
~87 ms to ~345 ms for a 128x128 map at 1024px (release, warm cache), because the whole native
image is now composited and filtered rather than one thumbnail per distinct megatile. That is
driven by the native resolution, not the output size, so it is roughly flat in `--size`.

**What this does not fix, and why we stopped.** Heavy downscales still make open ground look
repetitive: Brood War terrain draws from a small set of megatile variants, and the SC:R HD/HD2
art's variants differ from one another less than the original art's did. In game this is hidden
by zoom and by SC:R's `.fol` foliage overlays (an SC:R-only addition, a possible later
authenticity feature). It is a property of the art, not of the filter.

A deterministic "texture breakup" option was implemented and rejected: hashed per-tile luminance
jitter plus a low-frequency luminance field plus per-pixel dither, all keyed off tile/pixel
coordinates. It did what it claimed numerically — in a 128x128px patch of Lost Temple grass, the
old path produced 256 tile blocks with only 48 distinct values (208 exact duplicates), the
resampler alone brought that to 251 distinct, and the noise to 256 — but it did not read as an
improvement. The perceived grid comes from the *within-tile* pattern repeating at the tile pitch,
which a per-tile brightness offset cannot touch; at amplitudes low enough to be unobtrusive the
effect was invisible against the texture's own variance, and at amplitudes high enough to notice
it drew a soft checkerboard at exactly the tile pitch, i.e. more grid, not less. Don't re-attempt
this from the luminance-noise direction; if it's worth revisiting, the leverage is in the art
(foliage overlays, or per-instance variant selection), not in post-processing.

### Minimap

Not a downsampled preview, and not "1 output pixel per map tile" either: the real game builds its
minimap at a small **native** resolution and the UI magnifies that. Pinned by reverse-engineering
`StarCraft.1.23.10` and validated empirically against the real `(2)Space Madness.scm` minimap.

**Native resolution.** The native image is always `<= 128` px per side, `M = max(map_w, map_h)`
in tiles:

- **`M <= 64`**: `native_ppt = 2` — every tile becomes a 2x2 native block. The four pixels of
  that block sample **four distinct minitiles** of the tile's 4x4 minitile grid: TL = minitile
  `[0]`, TR = `[1]`, BL = `[4]`, BR = `[5]` (the grid's own top-left 2x2 quadrant). This
  multi-sampling is what avoids a dark-speckle artifact a single-sample-per-tile scheme would
  produce once upscaled.
- **`65 <= M <= 128`**: `native_ppt = 1` — one native pixel per tile, minitile `[0]` (TL) only.
- **`M > 128`**: subsampled — one native pixel per 2x2 *tile* block, taken from the top-left
  tile's minitile `[0]`. Base dims `ceil(map_w/2) x ceil(map_h/2)`.

**Per-minitile sampling.** Look the minitile up in VX4(EX), ignoring the horizontal-flip bit (the
game's minimap path deliberately doesn't apply it), then sample byte 55 (row 6, col 7) of that
minitile's 8x8 VR4 bitmap; the resulting palette index, through WPE, is the pixel's color. The
real game also runs the palette index through a small runtime remap LUT that could not be
recovered (built at runtime, not present in static data); empirically, using the index directly
(an identity LUT) matches the real minimap closely — a documented, minor, known divergence.

**Creep** needs no special handling: the game masks the creep flag (`tile & 0x7FFF`) and samples
the resulting tile id exactly like any other — creep megatiles are already baked into a map's
tile ids by the editor/game, so the ordinary per-tile-id table already covers them.

**Magnification.** The native image (terrain *and* dots together) is upscaled by an integer,
nearest-neighbor `scale` afterward — reproducing "a small fixed texture displayed larger" exactly,
rather than rendering at output resolution directly.

The baked tables (`broodmap-render/src/minimap/tables/*.bin`) are indexed by **CHK unified tile
id**, not megatile id — CV5's tile-id -> megatile mapping is folded in at bake time, keeping the
runtime path zero-asset. Per tileset: **four** `u8` quadrant palette indices per tile id (TL/TR/
BL/BR — minitiles 0/1/4/5, byte 55, flip ignored), then the 256-color WPE palette. Generated by
the hidden `broodmap-cli gen-minimap-tables` subcommand against a real install
(`build_minimap_table` is the pure core), with an env-gated drift test byte-comparing a fresh
Jungle generation against the committed blob — a color table derived from the art, not the art
itself.

Runtime (`render_minimap`/`render_chk_minimap`, `MinimapOptions`): `Chk` in, image out, nothing
fetched — usable on a server or in WASM. Dots (unit/THG2-unit-sprite in resolved player colors,
resources highlighted in SC:R's neutral cyan, start locations per the same options as the preview
renderer — `Sprite` degrades to `ColorBlock`, no art in this path) are drawn into the *native*
image, then upscaled along with the terrain. Dot sizing/border (RE'd from `sub_72a090`/
`sub_729ae0`): non-building units *and* resources are always exactly 2 native px — mineral fields
and vespene geysers size identically despite their differing `units.dat` placeboxes, matching the
real game; buildings scale their `units.dat` placebox to native px, clamped `2..=4`. Border is
opaque black, `round(0.5 * native_ppt)` native px thick with a minimum of 1 (a documented
deviation from the game's exact formula, which can hit 0 at `native_ppt == 1`). `GameData` is an
*optional* input: with it, buildings size from `units.dat` and melee's start-area clearing
applies; without it, every dot is the fixed 2-native-px non-building size and the data-free parts
of melee filtering (player-owned/hallucinated drops) still apply. `MinimapOptions::scale` (1-16,
default 1 = native/no upscale; the CLI `minimap` subcommand defaults to 4 for eyeballing) is the
only size knob.

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
  generator, not at render time. Done in phase 3: `broodmap-formats/src/{vx4,vr4,wpe}.rs`;
  SC:R's CASC confirmed to ship only `.vx4ex`, with classic `.vx4` support kept for other
  sources.)
- ~~SD `mainSD.anim` container details~~ — resolved: reversed from real data (Animosity's
  parser as a cross-reference) and verified across all 999 entries / 22,735 frames; full spec
  and the empirical evidence live in `broodmap-formats/src/mainsd.rs`'s module docs, and the
  headline facts (entry directory, 12-byte reference entries, `"BMP "` teamcolor stencils,
  SD-texel — not 4K-unit — frame coordinates) are summarized in the phase-2/3 facts list
  above. The follow-up question of where SD placement *canvases* come from (every entry
  declares 0x0) is also resolved — the classic GRP headers; see the facts list and
  `broodmap-formats/src/grp.rs`.
- ~~Downscaling quality~~ — resolved: see the "Terrain downscaling" section for the shipped
  pipeline (native-resolution strip compositing, linear-light Catmull-Rom resampling), the
  measurements behind the kernel choice, and the record of the texture-breakup experiment that
  was implemented and rejected. Terrain repetition at preview scale is *not* fixed and is not a
  filtering problem; that section says where the remaining leverage actually is.
- `scale_rgba` averages channels independently (straight alpha) — exact for opaque terrain,
  but produces dark fringes on translucent sprite edges; switch to premultiplied-alpha
  filtering before phase 2 reuses it for `.anim` frames (TODO recorded in the code).
- ~~BW's exact minimap color algorithm~~ — resolved twice: first from OpenBW's `draw_minimap`
  (one sample per tile at output resolution), then superseded by a reverse-engineering pass of
  the real `StarCraft.1.23.10` binary plus empirical validation (see the Minimap section), which
  found the OpenBW-derived model visibly wrong — the real game samples at a small *native*
  resolution (four quadrants per tile on typical maps) and magnifies afterward. The generator
  needs classic VX4(EX)/VR4/WPE parsers either way (dev-time only, for table generation — still
  not needed at render time). bw-chk confirms the `.vx4ex` (32-bit entries) variant should be
  handled alongside classic `.vx4`. One divergence remains open: a runtime palette-index remap
  LUT the real game applies couldn't be recovered from static data; an identity LUT matches
  empirically and is what's shipped.
- Crate naming: `broodmap-formats` vs. a standalone name (`scr-formats`); it can start in this
  workspace and be extracted later if neobrood adoption makes that cleaner.
- Publishing order: `broodmap-render` (with `casc` feature) can't hit crates.io until broodcasc
  does; core crates don't depend on it and are unblocked.
