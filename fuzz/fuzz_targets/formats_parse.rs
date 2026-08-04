#![no_main]

use broodmap_formats::{
    DdsVr4, Frame, parse_cv5, parse_dds, parse_flingy_dat, parse_images_dat, parse_images_rel,
    parse_sprites_dat, parse_tbl, parse_units_dat, parse_vf4,
};
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

    // `.dat` table parsers (units/flingy/sprites/images): cheap, fixed-layout column readers that
    // are permissive on any input length (see broodmap-formats/src/dat.rs) -- run each over the
    // same bytes as everything else, then sweep entry lookups at ID 0, 1, the last valid ID, and
    // just out of range, to exercise both the happy path and the `None` path.
    let units = parse_units_dat(data);
    for id in [0u16, 1, 227, 228, u16::MAX] {
        let _ = units.entry(id);
    }

    let flingy = parse_flingy_dat(data);
    for id in [0u8, 1, 208, 209, u8::MAX] {
        let _ = flingy.sprite_id(id);
    }

    let sprites = parse_sprites_dat(data);
    for id in [0u16, 1, 516, 517, u16::MAX] {
        let _ = sprites.image_id(id);
    }

    let images = parse_images_dat(data);
    for id in [0u16, 1, 998, 999, u16::MAX] {
        let _ = images.entry(id);
    }

    // images.rel: art-redirect table, permissive on any length (trailing partial records are
    // ignored). Sweep a few IDs, including past the end of the parsed record list.
    let rel = parse_images_rel(data);
    for id in [0u16, 1, 998, 999, u16::MAX] {
        let _ = rel.resolve(id);
    }

    // .tbl string table: sweep index 0, 1, the file's self-declared count (interesting boundary --
    // may itself be out of range of what was actually parseable), and out of range.
    let tbl = parse_tbl(data);
    let declared_count = data
        .get(0..2)
        .map(|b| u16::from_le_bytes([b[0], b[1]]))
        .unwrap_or(0);
    for index in [0u16, 1, declared_count, u16::MAX] {
        let _ = tbl.get(index);
    }
});
