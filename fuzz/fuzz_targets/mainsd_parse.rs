#![no_main]

use broodmap_formats::{MainSdAnim, parse_dds, parse_teamcolor_mask};
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let Ok(sd_anim) = MainSdAnim::parse(data) else {
        return;
    };

    // num_entries is an untrusted u16 read straight from the header; cap the walk so a single
    // input can't force ~65k nontrivial entry() resolutions.
    let num_entries = sd_anim.num_entries().min(2048);

    for id in 0..num_entries {
        let Ok(anim) = sd_anim.entry(id as u16) else {
            continue;
        };

        let _ = anim.scale();
        let _ = anim.canvas_size();

        // Walk every layer actually parsed, mirroring how broodmap-render's overlay module
        // consumes SD entries: diffuse is a DDS payload (parse_dds), teamcolor is a raw
        // player-color stencil (parse_teamcolor_mask).
        for layer in anim.layers() {
            let _ = &layer.name;
            let _ = (layer.width, layer.height);
            let _ = parse_dds(layer.data);
            let _ = parse_teamcolor_mask(layer.data, layer.width, layer.height);
        }

        // Walk every parsed frame and its texel-rect conversion. `frame_count()` reflects only
        // frames actually parsed, so this is always a cheap, bounded loop.
        for i in 0..anim.frame_count() {
            let _ = anim.frame(i);
            let _ = anim.frame_texel_rect(i);
        }
    }
});
