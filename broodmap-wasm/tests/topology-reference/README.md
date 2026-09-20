# Frozen topology reference

These JavaScript modules preserve checkpoint `0125c2b` for differential testing. Their adjacent
unit tests exercise this reference only. Production topology decisions live in
`broodmap-analysis/src/topology`, whose Rust unit tests cover grouping, graph gates, ramp evidence,
and deterministic selection without external map assets.

From `broodmap-wasm`, `pnpm test` runs both these reference tests and the production demo tests.
For a comparison against the actual WASM pipeline, build the Node package and run:

```text
node tests/topology-parity.mjs <asset-directory> <map>...
```

Assets and maps are caller-supplied. The reference is not bundled into the demo or a supported API.
The Rust policy intentionally adds an 8192-observation limit and explicitly orders equal-cut
junction proposals by region IDs; the reference used first-seen order for that rare tie.
