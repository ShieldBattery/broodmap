use broodmap::chk::Chk;
use broodmap::{Mpq, StringEncoding, extract_chk_from_map};
use divan::{AllocProfiler, Bencher, black_box};

#[global_allocator]
static ALLOC: AllocProfiler = AllocProfiler::system();

const MAP: &[u8] = include_bytes!("../assets/DSA_7.4.3a_Desert_Strike_Angel.scx");
const CHK: &[u8] = include_bytes!("../assets/DSA_7.4.3a_Desert_Strike_Angel.chk");

fn main() {
    divan::main();
}

#[divan::bench]
fn mpq_index(bencher: Bencher) {
    bencher.bench_local(|| Mpq::from_bytes(black_box(MAP)).unwrap());
}

#[divan::bench]
fn mpq_extract_chk(bencher: Bencher) {
    let mpq = Mpq::from_bytes(MAP).unwrap();
    bencher.bench_local(|| mpq.read_file(black_box(broodmap::CHK_PATH), None).unwrap());
}

#[divan::bench]
fn full_pipeline(bencher: Bencher) {
    bencher.bench_local(|| extract_chk_from_map(black_box(MAP), None, None).unwrap());
}

#[divan::bench]
fn chk_index(bencher: Bencher) {
    bencher.bench_local(|| {
        Chk::from_bytes(black_box(CHK.to_vec()), Some(StringEncoding::Utf8)).unwrap()
    });
}

#[divan::bench]
fn metadata_auto_encoding(bencher: Bencher) {
    bencher.bench_local(|| {
        let chk = Chk::from_bytes(black_box(CHK.to_vec()), None).unwrap();
        black_box(chk.scenario_props()).unwrap();
    });
}

#[divan::bench]
fn metadata_known_encoding(bencher: Bencher) {
    bencher.bench_local(|| {
        let chk = Chk::from_bytes(black_box(CHK.to_vec()), Some(StringEncoding::Utf8)).unwrap();
        black_box(chk.scenario_props()).unwrap();
    });
}

#[divan::bench]
fn all_lazy_accessors(bencher: Bencher) {
    bencher.bench_local(|| {
        let chk = Chk::from_bytes(black_box(CHK.to_vec()), None).unwrap();
        black_box(chk.strings());
        black_box(chk.scenario_props()).unwrap();
        black_box(chk.force_settings()).unwrap();
        black_box(chk.raw_triggers()).unwrap();
        black_box(chk.raw_briefing()).unwrap();
        black_box(chk.terrain()).unwrap();
        black_box(chk.sprites()).unwrap();
        black_box(chk.placed_units()).unwrap();
    });
}
