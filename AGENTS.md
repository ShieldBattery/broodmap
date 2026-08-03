# broodmap

A pure Rust library for parsing StarCraft: Brood War map files (.scm, .scx). Extracts map data from MPQ archives and parses the CHK (scenario) format.

## Build & Test

```bash
cargo build                    # Build all workspace members
cargo test                     # Run all tests
cargo clippy -- -D warnings    # Lint (CI enforces warning-free)
cargo fmt --all -- --check     # Check formatting
```

The CLI (`broodmap-cli`) is currently a placeholder.

## Fuzzing

`fuzz/` is a cargo-fuzz crate (its own workspace, nightly-only — doesn't affect the library's MSRV) with targets `chk_parse`, `mpq_parse`, and `full_pipeline`. Because parsing is lazy, targets must call every lazy `Chk` accessor, not just construct the `Chk`.

```bash
cargo +nightly fuzz run chk_parse fuzz/corpus/chk_parse fuzz/seeds/chk_parse -- -max_len=65536
cargo run --bin seed_gen   # from fuzz/; regenerates committed seeds from broodmap/assets
```

CI runs a 20s smoke fuzz per target on push/PR and a 5-minute run weekly (`fuzz.yml`). When triaging findings, remember the invariant is "no panics/OOM" — permissively accepting garbage input is intentional, BW-compatible behavior, not a bug.

On Windows, linking fuzz targets needs an MSVC toolset that ships the clang ASan runtime (`clang_rt.asan*`); if the link fails with those libs missing, point `LIB`/`PATH` at a VS toolset version that includes them (the default Build Tools install may not).

## Project structure

```
broodmap/               Core library
  src/
    lib.rs              Public API entry point (extract_chk_from_map)
    mpq.rs              MPQ archive parsing
    chk/
      mod.rs            CHK file parsing, Chk struct, lazy field accessors
      chunk_type.rs     Chunk type definitions (VCOD, DIM, MTXM, etc.)
      dimensions.rs     Map width/height
      terrain.rs        Terrain tile data with creep flags
      placed_units.rs   Unit placement data (UNIT chunk)
      sprites.rs        Sprite/doodad placement (THG2 chunk)
      triggers.rs       Trigger system (conditions, actions, execution)
      briefing.rs       Mission briefing triggers
      forces.rs         Force settings and player assignments
      strings.rs        String table with multi-encoding support
      tileset.rs        Tileset enum (Badlands, Platform, etc.)
      format_version.rs Version detection
      scenario_props.rs Map name and description
      unit_settings.rs  Unit/weapon stat overrides
```

## Architecture notes

- **Entry point:** `extract_chk_from_map(map_bytes, locale, str_encoding)` returns `(Chk, Mpq)`
- **Lazy parsing:** Header/chunks are gathered eagerly, but terrain, units, strings, etc. are parsed on first access via `OnceLock`
- **Multi-encoding:** Supports Latin, Korean (EUC-KR), and UTF-8 string encodings with automatic detection
- **Parser combinators:** Uses `nom` for all binary parsing
- **Chunk handling:** Supports FullOverwrite, PartialOverwrite, and Append chunk merge strategies (matching BW's behavior)
- **Protected maps:** Handles various map protection schemes gracefully

## Code conventions

- Rust 2024 edition, MSRV 1.95
- `thiserror` for error types
- `bitflags!` for flag fields (UnitState, ForceFlags, SpriteFlags, etc.)
- `SmallVec` for chunk storage (most chunks appear once)
- `nom` parser combinators for binary format parsing
- 4-space indentation for Rust, 2-space for everything else
- Dual licensed: MIT or Apache 2.0

## Things to know

- Multiple chunks of the same type are merged according to chunk-specific strategies, matching how BW handles them
- String IDs are indices into a string table, not inline strings — use `Chk::strings()` to decode
- MPQ parsing supports the StarCraft-specific MPQ variant (not general-purpose MPQ)
- Test assets in `broodmap/assets/` include protected, corrupted, and multi-encoding maps
