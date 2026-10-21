# broodmap

A pure Rust implementation of StarCraft 1 map parsing. This library works for both SCM and SCX
files, and offers handling for the archive format (MPQ) as well as the internal scenario description
(CHK).

## Untrusted maps

The default `ResourceLimits` cap extracted files at 64 MiB and MPQ sector expansion at 500:1.
Every decompression stage uses the original sector's actual byte length, including intermediate
stages. A 4 KiB allowance preserves small sectors, and compatibility zero-padding also counts
against the expansion limit. This is a per-sector policy, not a whole-upload ratio guarantee.

Callers can adjust `max_mpq_compression_ratio` through `Mpq::from_bytes_with_limits` or
`extract_chk_from_map_with_limits`; `usize::MAX` disables only the ratio check.
`ResourceLimits::trusted()` disables all parser limits and is only for trusted inputs.
Network services should additionally bound upload sizes, concurrency, and processing time.

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
