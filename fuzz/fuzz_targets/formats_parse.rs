#![no_main]

use broodmap_formats::{DdsVr4, Frame, parse_cv5, parse_dds, parse_vf4};
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // All four parsers are cheap and permissive (garbage in, garbage/error out, never a panic):
    // run each over the same bytes.
    let _ = parse_cv5(data);
    let _ = parse_vf4(data);
    let _ = parse_dds(data);

    let Ok(vr4) = DdsVr4::parse(data) else {
        return;
    };

    // Walk every parsed frame/tile. `frame_count()` is bounded by the u16 frame/tile count in
    // the container header, so this is always a cheap, bounded loop.
    for i in 0..vr4.frame_count() as u16 {
        match vr4.frame(i) {
            Some(Frame::Dds(payload)) => {
                let _ = parse_dds(payload);
            }
            Some(Frame::Paletted { .. }) | None => {}
        }
    }

    // Paletted (SD) containers carry an embedded palette; exercise every index (cheap: plain
    // array reads, `Palette::rgb` never fails for any `u8`).
    if let Some(palette) = vr4.palette() {
        for index in 0..=u8::MAX {
            let _ = palette.rgb(index);
        }
    }
});
