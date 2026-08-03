//! Generates the committed seed corpus under `fuzz/seeds/<target>/`.
//!
//! Seeds are copied/derived from the real test map assets in `../broodmap/assets/` (these are
//! already committed test fixtures used by broodmap's own test suite, so reusing them here
//! doesn't introduce any new licensing concerns). Anything over `MAX_SEED_SIZE` is skipped to
//! keep the corpus modest.
//!
//! Run with `cargo run --bin seed_gen` from `fuzz/`.

use std::fs;
use std::path::{Path, PathBuf};

use broodmap::Chk;
use broodmap::extract_chk_from_map;

/// Seeds (and CHKs extracted from map seeds) larger than this are skipped.
const MAX_SEED_SIZE: u64 = 256 * 1024;

/// Map files (.scm/.scx) copied as-is for the `mpq_parse` and `full_pipeline` targets, chosen for
/// structural diversity: smallest known-valid map, a normal full-featured map, a map protector
/// output, an imploded (compressed) file, negative chunk table offsets, and a corrupted/malformed
/// header.
const MPQ_SEED_MAPS: &[&str] = &[
    "smallest.scm",
    "lt.scm",
    "protected-2.scx",
    "imploded.scm",
    "negativeoffsets.scx",
    "corrupted-0.scx",
];

/// Map files that CHK bytes get extracted from (via `extract_chk_from_map`) for the `chk_parse`
/// target. Chosen to cover the smallest map, a normal full-featured map, and a couple of
/// protector outputs -- other maps in `../broodmap/assets` either fail extraction (the corrupted
/// ones) or produce a CHK over `MAX_SEED_SIZE`.
const CHK_SEED_MAPS: &[&str] = &[
    "smallest.scm",
    "lt.scm",
    "protected-5.scm",
    "protected-3.scx",
];

fn main() {
    let assets_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("../broodmap/assets");
    let seeds_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("seeds");

    let mpq_dir = seeds_root.join("mpq_parse");
    let pipeline_dir = seeds_root.join("full_pipeline");
    let chk_dir = seeds_root.join("chk_parse");

    fs::create_dir_all(&mpq_dir).expect("create mpq_parse seed dir");
    fs::create_dir_all(&pipeline_dir).expect("create full_pipeline seed dir");
    fs::create_dir_all(&chk_dir).expect("create chk_parse seed dir");

    for name in MPQ_SEED_MAPS {
        let src = assets_dir.join(name);
        let bytes = fs::read(&src).unwrap_or_else(|e| panic!("read {}: {e}", src.display()));
        if bytes.len() as u64 > MAX_SEED_SIZE {
            println!(
                "skipping {name} for mpq_parse/full_pipeline: {} bytes exceeds {} byte cap",
                bytes.len(),
                MAX_SEED_SIZE
            );
            continue;
        }

        // Sanity check against the real parser so we don't commit a seed that can't even be
        // read back by the library (mirrors what the fuzz target itself will do).
        if let Err(e) = broodmap::Mpq::from_bytes(&bytes) {
            println!("note: {name} does not parse as a valid MPQ ({e}), seeding anyway");
        }

        write_seed(&mpq_dir, name, &bytes);
        write_seed(&pipeline_dir, name, &bytes);
    }

    for name in CHK_SEED_MAPS {
        let src = assets_dir.join(name);
        let bytes = fs::read(&src).unwrap_or_else(|e| panic!("read {}: {e}", src.display()));

        match extract_chk_from_map(&bytes, None, None) {
            Ok((chk, _mpq)) => {
                let chk_bytes = &chk.data;
                if chk_bytes.len() as u64 > MAX_SEED_SIZE {
                    println!(
                        "skipping {name} for chk_parse: extracted CHK is {} bytes, exceeds {} byte cap",
                        chk_bytes.len(),
                        MAX_SEED_SIZE
                    );
                    continue;
                }

                // Sanity check: the bytes we're about to write should parse cleanly with the
                // real CHK parser.
                if let Err(e) = Chk::from_bytes(chk_bytes.clone(), None) {
                    panic!("extracted CHK for {name} failed to re-parse: {e}");
                }

                let out_name = format!("{name}.chk");
                write_seed(&chk_dir, &out_name, chk_bytes);
            }
            Err(e) => {
                println!("skipping {name} for chk_parse: extraction failed ({e})");
            }
        }
    }

    println!("done");
}

fn write_seed(dir: &Path, name: &str, bytes: &[u8]) {
    let path = dir.join(name);
    fs::write(&path, bytes).unwrap_or_else(|e| panic!("write {}: {e}", path.display()));
    println!(
        "wrote {} ({} bytes)",
        relative(&path).display(),
        bytes.len()
    );
}

fn relative(path: &Path) -> PathBuf {
    path.strip_prefix(env!("CARGO_MANIFEST_DIR"))
        .unwrap_or(path)
        .to_path_buf()
}
