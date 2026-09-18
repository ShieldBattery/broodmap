# broodmap

A pure Rust implementation of StarCraft 1 map parsing. This library works for both SCM and SCX
files, and offers handling for the archive format (MPQ) as well as the internal scenario description
(CHK).

## Terrain analysis experiment

`broodmap-analysis` resolves terrain flags, suggests resource-base locations, and computes routes
around static map obstacles. The [WASM demo](broodmap-wasm/README.md) shows overlays and compares
ground and air distances between points or base candidates. See the
[design and limitations](docs/analysis-design.md) for the heuristics and path toward wall analysis.

## WIP

This library is a work-in-progress, it is not yet complete or ready for use.

## License

Licensed under either of

* Apache License, Version 2.0
  ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
* MIT license
  ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
