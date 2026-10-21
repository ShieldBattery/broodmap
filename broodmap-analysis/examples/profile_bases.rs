//! Native release benchmark for bounded base discovery.
//!
//! ```text
//! cargo run --release -p broodmap-analysis --example profile_bases -- \
//!   broodmap-wasm/examples/assets 10 path/to/map.scx [more-maps...] [--stress] [--memory]
//! ```
//!
//! Each map is loaded through `analyze_bases`, which also serves as the unmeasured warmup; only the
//! following `repetitions` calls to `discover_bases` on its normalized inputs are timed, and every
//! result must equal the warmup result. Maps whose discovery was skipped (for example, dense
//! resource fields over the solver's limit) are reported as `skipped` rows without timing.
//! `--stress` appends a 256x256-tile synthetic case with 100 destructible obstacles arranged to
//! create many candidate-clearance groups. It is intentionally capped to one timed iteration so it
//! remains a quick profiling checkpoint.

use std::{
    alloc::{GlobalAlloc, Layout, System},
    env, fs,
    path::{Path, PathBuf},
    sync::atomic::{AtomicBool, AtomicI64, AtomicUsize, Ordering},
    time::{Duration, Instant},
};

use broodmap::chk::tileset::Tileset;
use broodmap::extract_chk_from_map;
use broodmap_analysis::{
    AnalysisAssets, BaseAnalysis, BaseDiscovery, BaseDiscoveryOutcome, BaseSearchOptions,
    MeleeBaseInputs, PixelRect, ResourceKind, ResourceNode, StaticObstacle, TerrainCell,
    TerrainGrid, analyze_bases, discover_bases,
};
use broodmap_formats::{parse_cv5, parse_units_dat, parse_vf4};

// Tracking stays disabled for timed iterations. The optional memory pass reports requested bytes
// relative to the moment tracking was enabled; negative net bytes are possible when the solver
// frees allocations made before that moment.
struct ProfilingAllocator;

#[global_allocator]
static ALLOCATOR: ProfilingAllocator = ProfilingAllocator;
static TRACK_ALLOCATIONS: AtomicBool = AtomicBool::new(false);
static OUTSTANDING_BYTES: AtomicI64 = AtomicI64::new(0);
static PEAK_DELTA_BYTES: AtomicI64 = AtomicI64::new(0);
static ALLOCATION_EVENTS: AtomicUsize = AtomicUsize::new(0);
static DEALLOCATION_EVENTS: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for ProfilingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc(layout) };
        if !pointer.is_null() {
            record_allocation(byte_delta(layout.size(), 0));
        }
        pointer
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc_zeroed(layout) };
        if !pointer.is_null() {
            record_allocation(byte_delta(layout.size(), 0));
        }
        pointer
    }

    unsafe fn dealloc(&self, pointer: *mut u8, layout: Layout) {
        unsafe { System.dealloc(pointer, layout) };
        record_deallocation(byte_delta(0, layout.size()));
    }

    unsafe fn realloc(&self, pointer: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        let replacement = unsafe { System.realloc(pointer, layout, new_size) };
        if !replacement.is_null() || new_size == 0 {
            record_reallocation(byte_delta(new_size, layout.size()));
        }
        replacement
    }
}

fn byte_delta(added: usize, removed: usize) -> i64 {
    let added = i64::try_from(added).unwrap_or(i64::MAX);
    let removed = i64::try_from(removed).unwrap_or(i64::MAX);
    added.saturating_sub(removed)
}

fn record_allocation(delta: i64) {
    if TRACK_ALLOCATIONS.load(Ordering::Relaxed) {
        ALLOCATION_EVENTS.fetch_add(1, Ordering::Relaxed);
        record_delta(delta);
    }
}

fn record_deallocation(delta: i64) {
    if TRACK_ALLOCATIONS.load(Ordering::Relaxed) {
        DEALLOCATION_EVENTS.fetch_add(1, Ordering::Relaxed);
        record_delta(delta);
    }
}

fn record_reallocation(delta: i64) {
    if TRACK_ALLOCATIONS.load(Ordering::Relaxed) {
        ALLOCATION_EVENTS.fetch_add(1, Ordering::Relaxed);
        DEALLOCATION_EVENTS.fetch_add(1, Ordering::Relaxed);
        record_delta(delta);
    }
}

fn record_delta(delta: i64) {
    let current = OUTSTANDING_BYTES.fetch_add(delta, Ordering::Relaxed) + delta;
    if current <= 0 {
        return;
    }
    let mut peak = PEAK_DELTA_BYTES.load(Ordering::Relaxed);
    while current > peak {
        match PEAK_DELTA_BYTES.compare_exchange_weak(
            peak,
            current,
            Ordering::Relaxed,
            Ordering::Relaxed,
        ) {
            Ok(_) => break,
            Err(observed) => peak = observed,
        }
    }
}

struct Case {
    name: String,
    analysis: BaseAnalysis,
}

fn main() {
    let (assets, repetitions, maps, stress, memory) = match parse_args() {
        Ok(arguments) => arguments,
        Err(message) => {
            eprintln!("{message}");
            eprintln!(
                "usage: profile_bases <assets-dir> <repetitions> <map paths...> [--stress] [--memory]"
            );
            std::process::exit(2);
        }
    };

    println!(
        "case\tstatus\ttiles\tresources\tobstacles\tstarts\tbases\tunplaced\titerations\tmedian_ms\tmin_ms\tmax_ms\tallocations\tdeallocations\tnet_bytes\tpeak_bytes\terror"
    );
    let mut failed = false;
    for map in maps {
        match load_case(&assets, &map) {
            Ok(case) => {
                if let Err(message) = print_case(&case, repetitions, memory) {
                    print_error_name(&case.name, &message);
                    failed = true;
                }
            }
            Err(message) => {
                print_error(&map, &message);
                failed = true;
            }
        }
    }
    if stress {
        let case = stress_case();
        if let Err(message) = print_case(&case, 1, memory) {
            print_error_name(&case.name, &message);
            failed = true;
        }
    }
    if failed {
        std::process::exit(1);
    }
}

fn parse_args() -> Result<(PathBuf, usize, Vec<PathBuf>, bool, bool), String> {
    let mut arguments = env::args_os().skip(1);
    let assets = arguments
        .next()
        .map(PathBuf::from)
        .ok_or_else(|| "missing assets directory".to_owned())?;
    let repetitions = arguments
        .next()
        .ok_or_else(|| "missing repetitions".to_owned())
        .and_then(|value| {
            value
                .to_string_lossy()
                .parse::<usize>()
                .map_err(|_| "repetitions must be a positive integer".to_owned())
        })?;
    if repetitions == 0 {
        return Err("repetitions must be a positive integer".to_owned());
    }
    let mut maps = Vec::new();
    let mut stress = false;
    let mut memory = false;
    for argument in arguments {
        if argument == "--stress" {
            stress = true;
        } else if argument == "--memory" {
            memory = true;
        } else {
            maps.push(PathBuf::from(argument));
        }
    }
    if maps.is_empty() && !stress {
        return Err("supply at least one map path or --stress".to_owned());
    }
    Ok((assets, repetitions, maps, stress, memory))
}

fn print_case(case: &Case, repetitions: usize, memory: bool) -> Result<(), String> {
    let terrain = &case.analysis.terrain;
    let inputs = &case.analysis.inputs;
    let expected = match &case.analysis.discovery {
        BaseDiscoveryOutcome::Complete(discovery) => discovery,
        BaseDiscoveryOutcome::Skipped(reason) => {
            let placeholders = ["-"; 10].join("\t");
            println!(
                "{}\tskipped\t{}x{}\t{}\t{}\t{}\t{placeholders}\t{}",
                sanitize(&case.name),
                terrain.width() / 4,
                terrain.height() / 4,
                inputs.resources.len(),
                inputs.obstacles.len(),
                inputs.starts.len(),
                sanitize(&reason.to_string()),
            );
            return Ok(());
        }
    };
    let durations = benchmark(case, expected, repetitions)?;
    let memory = match memory {
        true => Some(memory_pass(case, expected)?),
        false => None,
    };
    let (allocations, deallocations, net_bytes, peak_bytes) = memory
        .map(|stats| {
            (
                stats.allocations.to_string(),
                stats.deallocations.to_string(),
                stats.net_bytes.to_string(),
                stats.peak_bytes.to_string(),
            )
        })
        .unwrap_or_else(|| ("-".into(), "-".into(), "-".into(), "-".into()));
    println!(
        "{}\tok\t{}x{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t",
        sanitize(&case.name),
        terrain.width() / 4,
        terrain.height() / 4,
        inputs.resources.len(),
        inputs.obstacles.len(),
        inputs.starts.len(),
        expected.bases.len(),
        expected.unplaced_clusters,
        repetitions,
        milliseconds(median(&durations)),
        milliseconds(*durations.iter().min().expect("at least one duration")),
        milliseconds(*durations.iter().max().expect("at least one duration")),
        allocations,
        deallocations,
        net_bytes,
        peak_bytes,
    );
    Ok(())
}

fn print_error(map: &Path, error: &str) {
    print_error_name(&map.to_string_lossy(), error);
}

fn print_error_name(name: &str, error: &str) {
    let name = sanitize(name);
    let error = sanitize(error);
    let placeholders = ["-"; 14].join("\t");
    println!("{name}\terror\t{placeholders}\t{error}");
}

fn sanitize(value: &str) -> String {
    value.replace(['\t', '\n', '\r'], " ")
}

fn load_case(assets: &Path, map: &Path) -> Result<Case, String> {
    let bytes = fs::read(map).map_err(|error| error.to_string())?;
    let (chk, _) = extract_chk_from_map(&bytes, None, None).map_err(|error| error.to_string())?;
    let tileset = tileset_stem(chk.tileset());
    let cv5 = parse_cv5(&read_asset(assets, &format!("TileSet/{tileset}.cv5"))?);
    let vf4 = parse_vf4(&read_asset(assets, &format!("TileSet/{tileset}.vf4"))?);
    let units = parse_units_dat(&read_asset(assets, "arr/units.dat")?);
    let analysis = analyze_bases(
        &chk,
        AnalysisAssets {
            cv5: &cv5,
            vf4: &vf4,
            units: &units,
        },
        &BaseSearchOptions::default(),
    )
    .map_err(|error| error.to_string())?;
    Ok(Case {
        name: map.to_string_lossy().into_owned(),
        analysis,
    })
}

fn read_asset(root: &Path, relative: &str) -> Result<Vec<u8>, String> {
    fs::read(root.join(relative)).map_err(|error| format!("{relative}: {error}"))
}

fn tileset_stem(tileset: Tileset) -> &'static str {
    match tileset {
        Tileset::Badlands => "badlands",
        Tileset::SpacePlatform => "platform",
        Tileset::Installation => "install",
        Tileset::Ashworld => "ashworld",
        Tileset::Jungle => "jungle",
        Tileset::Desert => "desert",
        Tileset::Arctic => "ice",
        Tileset::Twilight => "twilight",
    }
}

fn discover(case: &Case) -> Result<BaseDiscovery, String> {
    let inputs = &case.analysis.inputs;
    discover_bases(
        &case.analysis.terrain,
        &inputs.resources,
        &inputs.obstacles,
        &inputs.start_positions(),
        &BaseSearchOptions::default(),
    )
    .map_err(|error| error.to_string())
}

fn benchmark(
    case: &Case,
    expected: &BaseDiscovery,
    repetitions: usize,
) -> Result<Vec<Duration>, String> {
    let mut durations = Vec::with_capacity(repetitions);
    for _ in 0..repetitions {
        let started = Instant::now();
        let result = discover(case)?;
        durations.push(started.elapsed());
        if result != *expected {
            return Err("base discovery returned a non-deterministic result".to_owned());
        }
    }
    Ok(durations)
}

struct MemoryStats {
    allocations: usize,
    deallocations: usize,
    net_bytes: i64,
    peak_bytes: i64,
}

fn memory_pass(case: &Case, expected: &BaseDiscovery) -> Result<MemoryStats, String> {
    OUTSTANDING_BYTES.store(0, Ordering::Relaxed);
    PEAK_DELTA_BYTES.store(0, Ordering::Relaxed);
    ALLOCATION_EVENTS.store(0, Ordering::Relaxed);
    DEALLOCATION_EVENTS.store(0, Ordering::Relaxed);
    TRACK_ALLOCATIONS.store(true, Ordering::Release);
    let result = discover(case);
    let result = match result {
        Ok(result) if result == *expected => result,
        Ok(_) => {
            TRACK_ALLOCATIONS.store(false, Ordering::Release);
            return Err(
                "base discovery returned a non-deterministic result in memory pass".to_owned(),
            );
        }
        Err(error) => {
            TRACK_ALLOCATIONS.store(false, Ordering::Release);
            return Err(error);
        }
    };
    drop(result);
    TRACK_ALLOCATIONS.store(false, Ordering::Release);
    Ok(MemoryStats {
        allocations: ALLOCATION_EVENTS.load(Ordering::Relaxed),
        deallocations: DEALLOCATION_EVENTS.load(Ordering::Relaxed),
        net_bytes: OUTSTANDING_BYTES.load(Ordering::Relaxed),
        peak_bytes: PEAK_DELTA_BYTES.load(Ordering::Relaxed),
    })
}

fn median(values: &[Duration]) -> Duration {
    let mut sorted = values.to_vec();
    sorted.sort_unstable();
    let upper = sorted.len() / 2;
    if sorted.len().is_multiple_of(2) {
        sorted[upper - 1] + (sorted[upper] - sorted[upper - 1]) / 2
    } else {
        sorted[upper]
    }
}

fn milliseconds(duration: Duration) -> String {
    format!("{:.3}", duration.as_secs_f64() * 1_000.0)
}

fn stress_case() -> Case {
    const TILES: u32 = 256;
    let cells = vec![
        TerrainCell {
            walkable: true,
            terrain_buildable: true,
            elevation: 0,
            ramp: false,
        };
        (TILES * 4 * TILES * 4) as usize
    ];
    let terrain = TerrainGrid::from_cells(TILES * 4, TILES * 4, cells)
        .expect("synthetic grid has a valid shape");
    let field_left = 4_096;
    let field_top = 4_096;
    let resources = [(0, 0), (32, 0), (0, 32), (32, 32)]
        .into_iter()
        .map(|(x, y)| ResourceNode {
            bounds: PixelRect {
                left: field_left + x,
                top: field_top + y,
                right: field_left + x + 16,
                bottom: field_top + y + 16,
            },
            kind: ResourceKind::Mineral,
            amount: Some(1_500),
        })
        .collect();
    let obstacles = (0..100)
        .map(|index| {
            let x = 116 + index % 10;
            let y = 116 + index / 10;
            StaticObstacle {
                bounds: PixelRect {
                    left: x * 32 + 12,
                    top: y * 32 + 12,
                    right: x * 32 + 20,
                    bottom: y * 32 + 20,
                },
                destructible: true,
            }
        })
        .collect();
    let inputs = MeleeBaseInputs {
        resources,
        obstacles,
        ..MeleeBaseInputs::default()
    };
    Case {
        name: "synthetic-256-clearance-groups".to_owned(),
        analysis: BaseAnalysis::from_inputs(terrain, inputs, &BaseSearchOptions::default())
            .expect("synthetic case is valid"),
    }
}
