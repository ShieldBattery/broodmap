#![no_main]

use broodmap::chk::strings::StringId;
use broodmap::extract_chk_from_map;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // Locale/encoding are kept fixed (both auto-detecting via `None`) so this target stays a
    // pure function of the input bytes; auto-detection internally exercises all the
    // encoding/locale paths anyway.
    let Ok((chk, _mpq)) = extract_chk_from_map(data, None, None) else {
        return;
    };

    exercise_lazy_accessors(&chk);
});

/// Touches every public lazy accessor on `Chk`, discarding results. Mirrors the accessor sweep in
/// `chk_parse.rs`, but reached via the full MPQ -> CHK extraction pipeline instead of raw CHK
/// bytes.
fn exercise_lazy_accessors(chk: &broodmap::Chk) {
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
