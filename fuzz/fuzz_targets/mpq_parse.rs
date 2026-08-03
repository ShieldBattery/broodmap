#![no_main]

use broodmap::Mpq;
use libfuzzer_sys::fuzz_target;

const CHK_PATH: &str = "staredit\\scenario.chk";

fuzz_target!(|data: &[u8]| {
    let Ok(mpq) = Mpq::from_bytes(data) else {
        return;
    };

    // Neutral locale, plus a couple of specific locale values to exercise the locale
    // fallback/search logic in `find_hash_table_entry`.
    let _ = mpq.read_file(CHK_PATH, None);
    let _ = mpq.read_file(CHK_PATH, Some(0x409));
    let _ = mpq.read_file(CHK_PATH, Some(0x412));
});
