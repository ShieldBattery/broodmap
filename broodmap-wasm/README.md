# broodmap-wasm

WebAssembly bindings for rendering StarCraft: Brood War map previews in the browser (or Node):
`MapRenderer` parses a map, names the assets it needs (the two-round prefetch API — asset keys
are CASC catalog paths, which double as URL suffixes), accepts the fetched bytes, and renders
previews, zero-asset minimaps, and render-plan JSON synchronously from memory.

## Browser example

```bash
pnpm install
pnpm run assets:analysis # terrain tables and units.dat (~2.2 MB); minimap-backed analysis needs no artwork
pnpm run assets   # fetch the style-complete SD asset set (~98 MB) into examples/assets
                  # (uses a local SC:R install; add -- --cdn for no install)
pnpm run dev      # build the wasm package and serve examples/ with vite
```

Pick a `.scm`/`.scx` map (e.g. `../broodmap/assets/lt.scm`) and render. The asset directory is
produced by `broodmap-cli fetch-assets`, which writes files at exactly the paths the bindings
request — any static file host works as an asset origin.

## Node

```bash
pnpm run build:node
node examples/node.mjs ../broodmap/assets/lt.scm examples/assets out.png
```

WASM renders are byte-identical to the native CLI's (verified against `broodmap-cli render`).

`pkg/`, `pkg-node/` and `examples/assets/` are build/game-data artifacts and are gitignored;
the assets are Blizzard's and must never be committed.

## Terrain inspector (experimental)

Choose a map and click **Analyze terrain**. That tileset's CV5/VF4 and the small `arr/units.dat` collision table are fetched.
The inspector starts with a minimap backdrop; **Render** adds the full preview artwork.
Toggle walkability, static obstacles, elevation, terrain buildability, or ramps; hover for cell attributes.
**Respect map obstacles** is enabled by default; turn it off to compare terrain-only routes.
Click two walkable cells to compare a ground route with the straight air-distance line.
A third click starts a new route. Parsing, rendering, and routing run in a worker.

Click **Find bases** to discover resource clusters and candidate depot footprints. Select **Base A**
and **Base B**, then **Compare bases** for an orange ground route and dashed air-distance line.
The obstacle toggle preserves base IDs and recomputes the selected comparison. Base labels include
mineral/geyser counts and nearby start players; they do not assign main/natural roles.

Discovery clusters resources by terrain connectivity and uses a stock 4x3 depot footprint.
Candidate scoring accounts for static blockers and balances mineral and gas access. Sites marked
`*` require clearing small mineral patches or overlapping buildings, highlighted in orange when
selected. Exact start sites can clear overlapping objects automatically when occupied, including
invincible ones. Current routes retain all these objects because this snapshot does not know the
active starting assignments. Creep/power, full placement
legality, and optimal gathering are not established. Small mineral groups may be omitted as bases,
and qualifying clusters with no suitable site are reported.

The routing model is an 8-pixel point grid with eight directions and no diagonal corner
cutting. Resources and grounded neutral buildings from UNIT/THG2 block intersecting cells
using caller-supplied units.dat collision bounds. It ignores moving units, mover sizes,
and exact engine movement; narrow gaps can be overblocked. Turning obstacles off ignores
all of them, including invincible buildings, rather than simulating their destruction. Ground distance uses fixed-point step costs (8 px orthogonal, 11.312 px diagonal),
not travel time. Clicks snap to cell centers; distances use logical pixels, not output-image
pixels (32 logical pixels = one map tile). Terrain buildability is a CV5 flag hint, not a
building-placement validator. See [the analysis design](../docs/analysis-design.md).

Existing terrain-only exports need `arr/units.dat` added. To refresh the small analysis tables:

```bash
pnpm run assets:analysis
# Or just one map's tileset, from the repository root:
cargo run -p broodmap-cli -- fetch-assets --analysis-only --out broodmap-wasm/examples/assets broodmap/assets/lt.scm
```

The usual `--install`, `--assets-dir`, and `--cdn` sources work with `--analysis-only`.
Full `fetch-assets` bundles now include VF4 as well. Nothing is uploaded by the demo.

Missing UNIT/THG2 chunks contribute no obstacles. The underlying parsers ignore incomplete
trailing records; `obstacleCount` is not a map-integrity check. Definitions with negative
collision extents are unsupported and skipped.

The independent WASM snapshot can also be used outside the example:

```js
const [cv5Path, vf4Path, unitsPath] = map.requiredMapAnalysisAssets()
const analysis = map.analyzeMap(
  await fetchBytes(cv5Path), await fetchBytes(vf4Path), await fetchBytes(unitsPath),
)
// analysis.setObstaclesEnabled(false) selects terrain-only routes and clearance.
const flags = analysis.cellFlags() // row-major; see generated TypeScript docs for bit layout
const clearance = analysis.clearancePixels() // Uint16Array, row-major square radii in logical pixels
// 0 = blocked; 4 = next to a blocker/map edge. Not unit passability or corridor width.
const route = JSON.parse(analysis.routeJson(10, 20, 30, 40)) // walk-cell coordinates
// null groundDistancePixels means disconnected; invalid/blocked endpoints throw.
const catalog = JSON.parse(analysis.findBasesJson('{}'))
// Options: {depotWidthTiles:4,depotHeightTiles:3,maxMineralBlockerAmount:8}; unknown options throw.
// Use maxMineralBlockerAmount:null, allowDestructibleClearing:false, and
// allowStartObstacleClearing:false for strict current placement. See each base.requiredMinerals,
// requiredObstacles, and startClearedObstacles.
const bases = catalog.bases.filter(base => base.routeAnchor !== null)
if (bases.length >= 2) {
  const baseRoute = JSON.parse(analysis.baseRouteJson(bases[0].id, bases[1].id))
}
analysis.free()
```

`requiredAnalysisAssets()` / `analyzeTerrain(cv5, vf4)` remain available for terrain-only callers.

Select **Clearance** in the demo overlay menu to inspect narrow passages and open areas.
Hover details include the numerical clearance radius with any overlay.
The heatmap and hover radius follow **Respect map obstacles**. **Color scale maximum** defaults
to 256px; choose a smaller range for tight gaps or a larger range for open areas. It changes
only the colors, not the measured radii. Radius measures a centered,
axis-aligned empty square and includes the map boundary; it is not an engine movement test.

For a static demo build, run `pnpm run build:demo`. The output is `examples/dist/`.
Game assets are not bundled: serve an exported asset directory separately and set the
asset-base field to its URL (with CORS enabled when hosted on a different origin).
