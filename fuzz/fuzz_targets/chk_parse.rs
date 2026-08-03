#![no_main]

use broodmap::Chk;
use broodmap::chk::strings::StringId;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // Auto-detect the string encoding (`None`) -- this is the more interesting/complex path
    // versus a fixed encoding, since it inspects string contents across several chunks.
    let Ok(chk) = Chk::from_bytes(data.to_vec(), None) else {
        return;
    };

    exercise_lazy_accessors(&chk);
});

/// Touches every public lazy accessor on [Chk], discarding results. Parsing is lazy (each
/// accessor is backed by a `OnceLock`), so a successful `Chk::from_bytes` only proves the eagerly
/// parsed header chunks (VER/STR/STRx/DIM/ERA) are valid -- everything else is only exercised by
/// actually calling the accessor.
fn exercise_lazy_accessors(chk: &Chk) {
    let strings = chk.strings();
    for i in 0..32u32 {
        let _ = strings.get(StringId::from(i));
    }

    let _ = chk.format_version();
    let _ = chk.width();
    let _ = chk.height();
    let _ = chk.tileset();
    let _ = chk.scenario_props();
    let _ = chk.force_settings();
    let _ = chk.raw_triggers();
    let _ = chk.raw_briefing();
    let _ = chk.terrain();
    let _ = chk.sprites();
    let _ = chk.placed_units();
}
