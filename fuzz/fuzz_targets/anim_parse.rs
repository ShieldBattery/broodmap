#![no_main]

use broodmap_formats::{Anim, parse_dds};
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let Ok(anim) = Anim::parse(data) else {
        return;
    };

    let _ = anim.scale();
    let _ = anim.canvas_size();

    // Walk every layer actually parsed (bounded by `MAX_LAYERS` internally, see anim.rs), and feed
    // each one's raw payload into `parse_dds` -- mirroring how broodmap-render's overlay module
    // consumes anim layer data (its `decode_layer` helper does the same: layers are typically DDS,
    // occasionally PNG, and are left undecoded by this crate).
    for layer in anim.layers() {
        let _ = &layer.name;
        let _ = (layer.width, layer.height);
        let _ = parse_dds(layer.data);
    }

    // Walk every parsed frame and its texel-rect conversion. `frame_count()` reflects only frames
    // actually parsed (bounded, see anim.rs docs), so this is always a cheap, bounded loop.
    for i in 0..anim.frame_count() {
        let _ = anim.frame(i);
        let _ = anim.frame_texel_rect(i);
    }
});
