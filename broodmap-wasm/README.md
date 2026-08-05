# broodmap-wasm

WebAssembly bindings for rendering StarCraft: Brood War map previews in the browser (or Node):
`MapRenderer` parses a map, names the assets it needs (the two-round prefetch API — asset keys
are CASC catalog paths, which double as URL suffixes), accepts the fetched bytes, and renders
previews, zero-asset minimaps, and render-plan JSON synchronously from memory.

## Browser example

```bash
pnpm install
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
