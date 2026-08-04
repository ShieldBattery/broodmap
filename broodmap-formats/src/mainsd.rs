//! Parser for SC:R's `mainSD.anim` container (zero-copy) — the SD counterpart to the HD/HD2
//! single-entry `.anim` layout parsed by [`crate::anim`]. Unlike an HD `.anim` file, which holds
//! exactly one animation, `mainSD.anim` is a single file bundling one entry per `images.dat` row
//! (999 entries in the real file), looked up by image id.
//!
//! # File layout
//!
//! All integers are little-endian; all stored offsets are **absolute from the start of the
//! file**.
//!
//! **File header (12 bytes at `0x00`)**, the same shape as HD's:
//!
//! | Field       | Type | Notes                                                              |
//! |-------------|------|--------------------------------------------------------------------|
//! | magic       | `[u8; 4]` | `"ANIM"`                                                       |
//! | scale       | `u8` | `1` in the real file (SD is definitionally 1 texel/logical px)        |
//! | ty          | `u8` | **`1`** for this SD multi-entry container (HD files use `2` — see [`crate::anim::Anim::parse`], which rejects `ty == 1`) |
//! | unknown     | `u16`| unparsed                                                               |
//! | num_layers  | `u16`| `2` in the real file (`diffuse`, `teamcolor`)                         |
//! | num_entries | `u16`| `999` in the real file, one per `images.dat` row                      |
//!
//! **Layer-name region (`0x0C..0x14C`):** byte-identical to HD's — 10 fixed slots of 32
//! NUL-padded bytes each, always occupying that full region regardless of `num_layers`. In the
//! real file, slot 0 is `"diffuse"`, slot 1 is `"teamcolor"`, the rest are zeroed.
//!
//! **Entry offset table (at `0x14C`):** `num_entries` consecutive `u32` absolute offsets, one per
//! entry, indexed by image id — this replaces HD's single frame-table header living at the same
//! offset. In the real file this table is strictly increasing and 4-aligned and its first entry
//! equals the offset immediately after the table itself, but **none of that is assumed** by this
//! parser: every offset read is bounds-checked against the file independently.
//!
//! **Entry header (12 bytes at the entry's offset):**
//!
//! | Field            | Type | Notes                                                          |
//! |------------------|------|-----------------------------------------------------------------|
//! | frame_count      | `u16`| number of [16-byte frame records](#frame-records)                |
//! | ref_id           | `u16`| `0xFFFF` = this is a real entry; otherwise the image id whose art to reuse |
//! | canvas_width      | `u16`| always `0` in the real file's 999 entries, parsed regardless    |
//! | canvas_height     | `u16`| always `0` in the real file's 999 entries, parsed regardless    |
//! | frame_arr_offset | `u32`| absolute offset of the frame array                                |
//!
//! **Reference entries** (`ref_id != 0xFFFF`): the entry is *exactly* those 12 bytes and nothing
//! more — no layer records, no frame array. `frame_count`/`canvas_width`/`canvas_height`/
//! `frame_arr_offset` are all `0` in every one of the real file's 131 reference entries. Reading
//! layer records after a reference header would read into the next entry's bytes, so this parser
//! never does that.
//!
//! In the real file there are 868 real entries and 131 reference entries; every reference target
//! is a real (non-reference) entry with frames, and there are zero chains, zero self-references,
//! and zero forward references (i.e. every observed structure resolves in one hop). A **hostile**
//! file is not bound by any of this and can contain chains, cycles, self-references, or
//! out-of-range targets, so [`MainSdAnim::entry`] resolves at most **one** hop: if the target
//! entry is itself a reference, or targets its own source id, or its id is out of range, or its
//! header can't even be read, that's a clean [`AnimError::InvalidEntry`] — never a loop or
//! recursion.
//!
//! **Real entry body:** immediately after the 12-byte header, `num_layers` layer texture records
//! (12 bytes each), in the *exact* layout HD uses (`u32` absolute offset, `0` = layer absent;
//! `u32` size; `u16` width; `u16` height).
//!
//! Layer payloads:
//! - `diffuse` is always an embedded DDS file (DXT1 or DXT5 in the real file), left undecoded as
//!   a raw slice — parse it with [`crate::parse_dds`], exactly like HD's `diffuse` layer.
//! - `teamcolor` (present on 155 of the 868 real entries) is **not** a DDS file. It's a raw binary
//!   player-color stencil: a 4-byte magic `"BMP "` (`0x42 0x4D 0x50 0x20`) followed by exactly
//!   `width * height` bytes, row-major top-down, each byte either `0` or `255`. Its `width`/
//!   `height` (from the layer record) always equal the entry's `diffuse` dimensions in real data.
//!   Use [`parse_teamcolor_mask`] to validate and slice out the mask bytes.
//!
//! **Frame records** (16 bytes each, `frame_count` of them at `frame_arr_offset`): the exact same
//! field layout as HD's (`u16` texture_x, `u16` texture_y, `i16` offset_x, `i16` offset_y, `u16`
//! width, `u16` height, `u32` unknown).
//!
//! Every one of the regions above is, in the real file, perfectly packed with zero gaps, zero
//! overlaps, and zero trailing bytes — but this parser never relies on that (e.g. it never derives
//! one entry's size from a neighboring entry's offset); every offset is read from its own stored
//! field and bounds-checked independently.
//!
//! # Coordinate space and normalization
//!
//! Unlike HD/HD2 `.anim` files, whose frame tables are authored in fixed "4K units" *regardless of
//! tier* (see [`crate::anim`]'s module docs), `mainSD.anim`'s frame table is a **separately
//! authored table in this file's own SD texel space** — divisor 1, i.e. 1 unit = 1 SD texel = 1
//! logical BW pixel. This was verified empirically two ways:
//!
//! - **Content bounding boxes.** Decoding a diffuse DXT payload's actual non-transparent content
//!   and comparing it against the frame union: e.g. image 344's diffuse texture is 240x232, its
//!   decoded content bounding box is `(4,4)-(236,228)`, and the frame union **divided by 1**
//!   matches that exactly — `(4,4)-(236,228)` — while dividing by 4 (the HD/HD2 divisor) gives an
//!   absurdly small `(1,1)-(59,57)` that doesn't cover the visible art at all.
//! - **Atlas coverage.** Across entries, frame rects at divisor 1 cover a mean 95% of their
//!   diffuse texture's area (as expected for a tightly packed sprite atlas); at divisor 4 that
//!   drops to a mean ~24%, which is not remotely consistent with a real atlas layout.
//!
//! (HD and HD2 `.anim` files, by contrast, share one byte-identical frame table authored in HD
//! texels, which is why HD/HD2 both use a fixed 4K-unit space independent of `scale`.)
//!
//! To let [`MainSdAnim::entry`]'s output be consumed identically to an HD/HD2 [`crate::anim::Anim`]
//! (in particular, so [`crate::anim::Anim::frame_texel_rect`] works unmodified), every SD-texel
//! value is **normalized into 4K units before being returned**: each frame's `texture_x`,
//! `texture_y`, `width`, `height`, `offset_x`, `offset_y`, and the entry's canvas width/height, are
//! all multiplied by 4 (saturating — untrusted input can otherwise overflow a `u16`/`i16`). The
//! returned [`crate::anim::Anim`] always reports `scale() == 1` (SD's raw scale byte is ignored;
//! SD is definitionally 1 texel/logical px, which is what the normalization assumes). Net effect:
//! `frame_texel_rect`'s `4 / scale` divisor becomes `4 / 1 = 4`, exactly undoing the x4
//! normalization — so it returns precisely the original SD texel values, and the returned `Anim`
//! is indistinguishable from a well-formed single-image SD-tier `.anim`.

use crate::anim::{
    self, Anim, AnimError, AnimFrame, HEADER_SIZE, LAYER_NAME_REGION_START, LAYER_NAME_SLOT_SIZE,
    LAYER_NAME_SLOTS, NO_REF_ID,
};

/// Container type byte for `mainSD.anim`. HD/HD2 `.anim` files use `2` (see
/// [`crate::anim::Anim::parse`]).
const TYPE_SD: u8 = 1;
/// Byte size of a single entry header (frame_count, ref_id, canvas_width, canvas_height,
/// frame_arr_offset).
const ENTRY_HEADER_SIZE: usize = 12;
/// Absolute offset where the entry offset table begins: immediately after the (fixed-size)
/// layer-name region, at the same offset HD's single frame-table header lives at.
const ENTRY_OFFSET_TABLE_START: usize =
    LAYER_NAME_REGION_START + LAYER_NAME_SLOTS * LAYER_NAME_SLOT_SIZE;
/// Byte size of a single entry offset table slot.
const ENTRY_OFFSET_SIZE: usize = 4;
/// 4-byte magic prefixing a `teamcolor` layer's payload, identifying it as a raw binary stencil
/// rather than a DDS file.
const TEAMCOLOR_MAGIC: &[u8; 4] = b"BMP ";

/// One entry's 12-byte header, already decoded.
#[derive(Debug, Clone, Copy)]
struct EntryHeader {
    frame_count: u16,
    ref_id: u16,
    canvas_width: u16,
    canvas_height: u16,
    frame_arr_offset: usize,
}

/// A parsed `mainSD.anim` container: a table of per-image animation entries, looked up by image
/// id (== `images.dat` row index). See the module docs for the full byte layout, the
/// reference-entry scheme, and the SD-texel-to-4K-unit normalization applied by [`Self::entry`].
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MainSdAnim<'a> {
    data: &'a [u8],
    num_layers: usize,
    num_entries: usize,
    layer_names: Vec<String>,
}

impl<'a> MainSdAnim<'a> {
    /// Parses a `mainSD.anim` file's header, magic, and layer-name region only — it does **not**
    /// walk the entry table, so this allocates just the small (`num_layers`-capped) name list.
    /// Entry lookup is lazy: see [`Self::entry`].
    pub fn parse(data: &'a [u8]) -> Result<MainSdAnim<'a>, AnimError> {
        if data.len() < HEADER_SIZE {
            return Err(AnimError::TooShort);
        }
        if &data[0..4] != b"ANIM" {
            return Err(AnimError::BadMagic);
        }

        let ty = data[5];
        if ty != TYPE_SD {
            return Err(AnimError::Unsupported(
                "only SD (ty == 1) mainSD.anim containers are supported by MainSdAnim::parse; \
                 HD/HD2 single-entry .anim files use ty == 2 — see Anim::parse",
            ));
        }

        let num_layers = u16::from_le_bytes(data[8..10].try_into().unwrap()) as usize;
        let num_entries = u16::from_le_bytes(data[10..12].try_into().unwrap()) as usize;

        let layer_names = anim::parse_layer_names(data, num_layers);

        Ok(MainSdAnim {
            data,
            num_layers,
            num_entries,
            layer_names,
        })
    }

    /// The number of entries declared by the file's header (untrusted; not validated against
    /// what's actually readable — [`Self::entry`] bounds-checks each lookup independently).
    pub fn num_entries(&self) -> usize {
        self.num_entries
    }

    /// Looks up the animation entry for `image_id` (an `images.dat` row index), resolving a
    /// single inline reference hop if present, and returns it normalized into an
    /// [`crate::anim::Anim`] in the same 4K-unit coordinate space HD/HD2 `.anim` files use — see
    /// the module docs' normalization section.
    ///
    /// Errors with [`AnimError::InvalidEntry`] if: `image_id` is out of range; the entry offset
    /// table read, entry header read, or (for a reference) the target's offset/header read run
    /// past the end of the file; the reference targets its own source id; the reference target id
    /// is out of range; or the reference target is itself a reference (chains are never
    /// followed).
    pub fn entry(&self, image_id: u16) -> Result<Anim<'a>, AnimError> {
        let id = image_id as usize;
        if id >= self.num_entries {
            return Err(AnimError::InvalidEntry("image id is out of range"));
        }
        let (offset, header) = self.resolve_entry(id)?;

        let (real_offset, real_header) = if header.ref_id == NO_REF_ID {
            (offset, header)
        } else {
            let target_id = header.ref_id as usize;
            if target_id == id {
                return Err(AnimError::InvalidEntry("reference entry targets itself"));
            }
            if target_id >= self.num_entries {
                return Err(AnimError::InvalidEntry(
                    "reference target id is out of range",
                ));
            }
            let (target_offset, target_header) = self.resolve_entry(target_id)?;
            if target_header.ref_id != NO_REF_ID {
                return Err(AnimError::InvalidEntry(
                    "reference target is itself a reference entry",
                ));
            }
            (target_offset, target_header)
        };

        let records_offset = real_offset
            .checked_add(ENTRY_HEADER_SIZE)
            .ok_or(AnimError::InvalidEntry("layer records offset overflows"))?;
        let layers = anim::parse_layer_records(self.data, records_offset, self.layer_names.clone());
        let frames: Vec<AnimFrame> = anim::parse_frame_records(
            self.data,
            real_header.frame_arr_offset,
            real_header.frame_count as usize,
        )
        .into_iter()
        .map(normalize_frame)
        .collect();

        let width = real_header.canvas_width.saturating_mul(4);
        let height = real_header.canvas_height.saturating_mul(4);

        Ok(Anim::from_parts(1, width, height, layers, frames))
    }

    /// Reads and decodes the offset table slot and entry header for entry `id`. Does not resolve
    /// references.
    fn resolve_entry(&self, id: usize) -> Result<(usize, EntryHeader), AnimError> {
        let offset = self.read_entry_offset(id)?;
        let header = self.read_entry_header(offset)?;
        Ok((offset, header))
    }

    fn read_entry_offset(&self, id: usize) -> Result<usize, AnimError> {
        let rec_start = id
            .checked_mul(ENTRY_OFFSET_SIZE)
            .and_then(|o| o.checked_add(ENTRY_OFFSET_TABLE_START))
            .ok_or(AnimError::InvalidEntry(
                "entry offset table index overflows",
            ))?;
        let rec_end = rec_start
            .checked_add(ENTRY_OFFSET_SIZE)
            .ok_or(AnimError::InvalidEntry(
                "entry offset table index overflows",
            ))?;
        if rec_end > self.data.len() {
            return Err(AnimError::InvalidEntry(
                "entry offset table entry is beyond end of file",
            ));
        }
        Ok(u32::from_le_bytes(self.data[rec_start..rec_end].try_into().unwrap()) as usize)
    }

    fn read_entry_header(&self, entry_offset: usize) -> Result<EntryHeader, AnimError> {
        let end = entry_offset
            .checked_add(ENTRY_HEADER_SIZE)
            .ok_or(AnimError::InvalidEntry("entry header offset overflows"))?;
        if end > self.data.len() {
            return Err(AnimError::InvalidEntry("entry header is truncated"));
        }
        let rec = &self.data[entry_offset..end];
        Ok(EntryHeader {
            frame_count: u16::from_le_bytes(rec[0..2].try_into().unwrap()),
            ref_id: u16::from_le_bytes(rec[2..4].try_into().unwrap()),
            canvas_width: u16::from_le_bytes(rec[4..6].try_into().unwrap()),
            canvas_height: u16::from_le_bytes(rec[6..8].try_into().unwrap()),
            frame_arr_offset: u32::from_le_bytes(rec[8..12].try_into().unwrap()) as usize,
        })
    }
}

/// Normalizes a single SD-texel-space frame into 4K units (see the module docs): every field is
/// multiplied by 4, saturating so a hostile file (e.g. `texture_x == u16::MAX`) can't overflow.
fn normalize_frame(frame: AnimFrame) -> AnimFrame {
    AnimFrame {
        texture_x: frame.texture_x.saturating_mul(4),
        texture_y: frame.texture_y.saturating_mul(4),
        offset_x: frame.offset_x.saturating_mul(4),
        offset_y: frame.offset_y.saturating_mul(4),
        width: frame.width.saturating_mul(4),
        height: frame.height.saturating_mul(4),
    }
}

/// Validates and returns the raw `width * height` mask bytes of an SD `teamcolor` layer payload
/// (see the module docs): a `"BMP "` magic followed by exactly `width * height` raw bytes,
/// row-major top-down, each byte `0` or `255`. Returns `None` if the magic doesn't match or
/// `data` is shorter than the magic plus the declared mask size.
///
/// All size arithmetic is checked so a hostile `width`/`height` (each up to `u16::MAX`) cannot
/// overflow, even on 32-bit targets: this crate compiles for `wasm32-unknown-unknown`, where
/// `usize` is 32 bits and `65535 * 65535 + 4` alone is within a few bytes of overflowing
/// `u32::MAX`.
pub fn parse_teamcolor_mask(data: &[u8], width: u16, height: u16) -> Option<&[u8]> {
    if data.len() < TEAMCOLOR_MAGIC.len() || &data[0..TEAMCOLOR_MAGIC.len()] != TEAMCOLOR_MAGIC {
        return None;
    }
    let mask_len = (width as usize).checked_mul(height as usize)?;
    let end = TEAMCOLOR_MAGIC.len().checked_add(mask_len)?;
    if end > data.len() {
        return None;
    }
    Some(&data[TEAMCOLOR_MAGIC.len()..end])
}

#[cfg(test)]
mod tests {
    use super::*;

    /// One entry to be assembled into a synthetic `mainSD.anim` file by [`MainSdBuilder`].
    enum EntrySpec {
        /// A real entry: per-layer payloads (`None` = absent, offset 0) in layer order, plus
        /// frames and a (usually zero) canvas size.
        Real {
            layers: Vec<Option<(Vec<u8>, u16, u16)>>,
            frames: Vec<AnimFrame>,
            canvas: (u16, u16),
        },
        /// A 12-byte inline-reference entry pointing at another entry's image id.
        Ref(u16),
    }

    /// Builds a synthetic `mainSD.anim` file: header, 10-slot layer-name region, entry offset
    /// table, and a sequence of entries (assigned sequential image ids in push order).
    struct MainSdBuilder {
        layer_names: Vec<&'static str>,
        entries: Vec<EntrySpec>,
    }

    impl MainSdBuilder {
        fn new(layer_names: Vec<&'static str>) -> Self {
            Self {
                layer_names,
                entries: Vec::new(),
            }
        }

        /// Adds a real entry, returning its assigned image id.
        fn real_entry(
            &mut self,
            layers: Vec<Option<(Vec<u8>, u16, u16)>>,
            frames: Vec<AnimFrame>,
        ) -> u16 {
            self.entries.push(EntrySpec::Real {
                layers,
                frames,
                canvas: (0, 0),
            });
            (self.entries.len() - 1) as u16
        }

        /// Adds a reference entry pointing at `target`, returning its assigned image id.
        fn ref_entry(&mut self, target: u16) -> u16 {
            self.entries.push(EntrySpec::Ref(target));
            (self.entries.len() - 1) as u16
        }

        fn build(&self) -> Vec<u8> {
            let num_layers = self.layer_names.len();
            let num_entries = self.entries.len();

            let offset_table_size = num_entries * ENTRY_OFFSET_SIZE;
            let mut data = vec![0u8; ENTRY_OFFSET_TABLE_START + offset_table_size];

            data[0..4].copy_from_slice(b"ANIM");
            data[4] = 1; // scale
            data[5] = TYPE_SD;
            data[6..8].copy_from_slice(&0u16.to_le_bytes()); // unknown
            data[8..10].copy_from_slice(&(num_layers as u16).to_le_bytes());
            data[10..12].copy_from_slice(&(num_entries as u16).to_le_bytes());

            for (i, name) in self.layer_names.iter().enumerate() {
                let start = LAYER_NAME_REGION_START + i * LAYER_NAME_SLOT_SIZE;
                data[start..start + name.len()].copy_from_slice(name.as_bytes());
            }

            let mut entry_offsets = Vec::with_capacity(num_entries);

            for spec in &self.entries {
                entry_offsets.push(data.len() as u32);
                match spec {
                    EntrySpec::Ref(target) => {
                        let mut hdr = [0u8; ENTRY_HEADER_SIZE];
                        hdr[2..4].copy_from_slice(&target.to_le_bytes());
                        data.extend_from_slice(&hdr);
                    }
                    EntrySpec::Real {
                        layers,
                        frames,
                        canvas,
                    } => {
                        let header_pos = data.len();
                        data.extend_from_slice(&[0u8; ENTRY_HEADER_SIZE]);

                        let layer_records_pos = data.len();
                        data.extend(std::iter::repeat_n(0u8, layers.len() * 12));

                        let frame_arr_offset = data.len();
                        data.extend(std::iter::repeat_n(0u8, frames.len() * 16));
                        for (fi, frame) in frames.iter().enumerate() {
                            let fo = frame_arr_offset + fi * 16;
                            data[fo..fo + 2].copy_from_slice(&frame.texture_x.to_le_bytes());
                            data[fo + 2..fo + 4].copy_from_slice(&frame.texture_y.to_le_bytes());
                            data[fo + 4..fo + 6].copy_from_slice(&frame.offset_x.to_le_bytes());
                            data[fo + 6..fo + 8].copy_from_slice(&frame.offset_y.to_le_bytes());
                            data[fo + 8..fo + 10].copy_from_slice(&frame.width.to_le_bytes());
                            data[fo + 10..fo + 12].copy_from_slice(&frame.height.to_le_bytes());
                            // unknown u32 left zeroed
                        }

                        for (li, layer) in layers.iter().enumerate() {
                            let rec_pos = layer_records_pos + li * 12;
                            if let Some((bytes, w, h)) = layer {
                                let payload_off = data.len();
                                data[rec_pos..rec_pos + 4]
                                    .copy_from_slice(&(payload_off as u32).to_le_bytes());
                                data[rec_pos + 4..rec_pos + 8]
                                    .copy_from_slice(&(bytes.len() as u32).to_le_bytes());
                                data[rec_pos + 8..rec_pos + 10].copy_from_slice(&w.to_le_bytes());
                                data[rec_pos + 10..rec_pos + 12].copy_from_slice(&h.to_le_bytes());
                                data.extend_from_slice(bytes);
                            }
                            // Absent layer (None): record stays zeroed (offset 0).
                        }

                        data[header_pos..header_pos + 2]
                            .copy_from_slice(&(frames.len() as u16).to_le_bytes());
                        data[header_pos + 2..header_pos + 4]
                            .copy_from_slice(&NO_REF_ID.to_le_bytes());
                        data[header_pos + 4..header_pos + 6]
                            .copy_from_slice(&canvas.0.to_le_bytes());
                        data[header_pos + 6..header_pos + 8]
                            .copy_from_slice(&canvas.1.to_le_bytes());
                        data[header_pos + 8..header_pos + 12]
                            .copy_from_slice(&(frame_arr_offset as u32).to_le_bytes());
                    }
                }
            }

            for (i, off) in entry_offsets.iter().enumerate() {
                let pos = ENTRY_OFFSET_TABLE_START + i * ENTRY_OFFSET_SIZE;
                data[pos..pos + ENTRY_OFFSET_SIZE].copy_from_slice(&off.to_le_bytes());
            }

            data
        }
    }

    fn frame(
        texture_x: u16,
        texture_y: u16,
        offset_x: i16,
        offset_y: i16,
        width: u16,
        height: u16,
    ) -> AnimFrame {
        AnimFrame {
            texture_x,
            texture_y,
            offset_x,
            offset_y,
            width,
            height,
        }
    }

    #[test]
    fn happy_path_real_and_ref_entries_match_with_normalization() {
        let mut builder = MainSdBuilder::new(vec!["diffuse", "teamcolor"]);
        let diffuse_payload = vec![0xAAu8; 8];
        let teamcolor_payload = {
            let mut p = TEAMCOLOR_MAGIC.to_vec();
            p.extend_from_slice(&[0u8, 255, 255, 0]); // 2x2 mask
            p
        };
        let real_id = builder.real_entry(
            vec![
                Some((diffuse_payload.clone(), 2, 2)),
                Some((teamcolor_payload.clone(), 2, 2)),
            ],
            vec![frame(8, 4, -2, 3, 10, 9), frame(0, 0, 0, 0, 4, 4)],
        );
        let ref_id = builder.ref_entry(real_id);
        let data = builder.build();

        let anim = MainSdAnim::parse(&data).unwrap();
        assert_eq!(anim.num_entries(), 2);

        for id in [real_id, ref_id] {
            let parsed = anim.entry(id).unwrap();
            assert_eq!(parsed.scale(), 1);
            assert_eq!(parsed.canvas_size(), (0, 0));
            assert_eq!(parsed.frame_count(), 2);

            let diffuse = parsed.layer("diffuse").unwrap();
            assert_eq!(diffuse.data, &diffuse_payload[..]);
            assert_eq!((diffuse.width, diffuse.height), (2, 2));

            let teamcolor = parsed.layer("teamcolor").unwrap();
            assert_eq!(teamcolor.data, &teamcolor_payload[..]);

            // Normalization: SD texel values x4, saturating.
            let f0 = parsed.frame(0).unwrap();
            assert_eq!(
                (
                    f0.texture_x,
                    f0.texture_y,
                    f0.offset_x,
                    f0.offset_y,
                    f0.width,
                    f0.height
                ),
                (32, 16, -8, 12, 40, 36)
            );

            // frame_texel_rect (divisor 4/scale=4/1=4) undoes the normalization exactly.
            assert_eq!(parsed.frame_texel_rect(0).unwrap(), (8, 4, 10, 9));
            assert_eq!(parsed.frame_texel_rect(1).unwrap(), (0, 0, 4, 4));
        }
    }

    #[test]
    fn ref_self_reference_errors_cleanly() {
        let mut builder = MainSdBuilder::new(vec!["diffuse"]);
        // Build a placeholder real entry first so id 0 exists, then a self-ref at id 1... but we
        // want id 0 itself to self-reference, so push it directly.
        builder.entries.push(EntrySpec::Ref(0));
        let data = builder.build();

        let anim = MainSdAnim::parse(&data).unwrap();
        assert_eq!(
            anim.entry(0),
            Err(AnimError::InvalidEntry("reference entry targets itself"))
        );
    }

    #[test]
    fn ref_chain_errors_cleanly() {
        let mut builder = MainSdBuilder::new(vec!["diffuse"]);
        let real_id = builder.real_entry(vec![None], vec![frame(0, 0, 0, 0, 1, 1)]);
        let mid_ref = builder.ref_entry(real_id);
        let chained_ref = builder.ref_entry(mid_ref);
        let data = builder.build();

        let anim = MainSdAnim::parse(&data).unwrap();
        // One hop resolves fine.
        assert!(anim.entry(mid_ref).is_ok());
        // Two hops must error, not recurse.
        assert_eq!(
            anim.entry(chained_ref),
            Err(AnimError::InvalidEntry(
                "reference target is itself a reference entry"
            ))
        );
    }

    #[test]
    fn ref_target_out_of_range_errors_cleanly() {
        let mut builder = MainSdBuilder::new(vec!["diffuse"]);
        builder.ref_entry(999);
        let data = builder.build();

        let anim = MainSdAnim::parse(&data).unwrap();
        assert_eq!(
            anim.entry(0),
            Err(AnimError::InvalidEntry(
                "reference target id is out of range"
            ))
        );
    }

    #[test]
    fn entry_id_out_of_range_errors() {
        let mut builder = MainSdBuilder::new(vec!["diffuse"]);
        builder.real_entry(vec![None], vec![]);
        let data = builder.build();

        let anim = MainSdAnim::parse(&data).unwrap();
        assert_eq!(
            anim.entry(5),
            Err(AnimError::InvalidEntry("image id is out of range"))
        );
    }

    #[test]
    fn offset_table_entry_beyond_eof_errors() {
        let mut builder = MainSdBuilder::new(vec!["diffuse"]);
        builder.real_entry(vec![None], vec![]);
        let mut data = builder.build();
        // Truncate right at the start of the (single) offset-table entry.
        data.truncate(ENTRY_OFFSET_TABLE_START + 1);

        let anim = MainSdAnim::parse(&data).unwrap();
        assert_eq!(
            anim.entry(0),
            Err(AnimError::InvalidEntry(
                "entry offset table entry is beyond end of file"
            ))
        );
    }

    #[test]
    fn truncated_entry_header_errors() {
        let mut builder = MainSdBuilder::new(vec!["diffuse"]);
        builder.real_entry(vec![None], vec![]);
        let mut data = builder.build();
        // The single entry's header lives right at ENTRY_OFFSET_TABLE_START + 4 (after the
        // 1-slot offset table). Truncate partway through it.
        let entry_offset = ENTRY_OFFSET_TABLE_START + ENTRY_OFFSET_SIZE;
        data.truncate(entry_offset + 4);

        let anim = MainSdAnim::parse(&data).unwrap();
        assert_eq!(
            anim.entry(0),
            Err(AnimError::InvalidEntry("entry header is truncated"))
        );
    }

    #[test]
    fn truncated_frame_array_keeps_earlier_frames() {
        let mut builder = MainSdBuilder::new(vec!["diffuse"]);
        let id = builder.real_entry(
            vec![None],
            vec![frame(1, 1, 0, 0, 4, 4), frame(2, 2, 0, 0, 4, 4)],
        );
        let mut data = builder.build();
        // Find the frame array offset from the entry header (id 0 -> offset table slot 0) and
        // truncate right after frame 0.
        let stored_offset = u32::from_le_bytes(
            data[ENTRY_OFFSET_TABLE_START..ENTRY_OFFSET_TABLE_START + 4]
                .try_into()
                .unwrap(),
        ) as usize;
        let frame_arr_offset = u32::from_le_bytes(
            data[stored_offset + 8..stored_offset + 12]
                .try_into()
                .unwrap(),
        ) as usize;
        data.truncate(frame_arr_offset + 16);

        let anim = MainSdAnim::parse(&data).unwrap();
        let parsed = anim.entry(id).unwrap();
        assert_eq!(parsed.frame_count(), 1);
        assert_eq!(parsed.frame_texel_rect(0).unwrap(), (1, 1, 4, 4));
    }

    #[test]
    fn lying_counts_do_not_panic_or_oom() {
        let mut builder = MainSdBuilder::new(vec!["diffuse"]);
        builder.real_entry(vec![None], vec![frame(0, 0, 0, 0, 1, 1)]);
        let mut data = builder.build();

        // Lie about num_entries (huge relative to actual table size) and num_layers.
        data[8..10].copy_from_slice(&u16::MAX.to_le_bytes()); // num_layers
        data[10..12].copy_from_slice(&u16::MAX.to_le_bytes()); // num_entries

        let anim = MainSdAnim::parse(&data).unwrap();
        assert_eq!(anim.num_entries(), u16::MAX as usize);
        // Entry 0's offset table slot still reads fine (it's within the tiny real table)...
        // but most ids will hit an out-of-bounds offset-table read. Neither panics nor hangs.
        assert!(anim.entry(0).is_ok());
        assert!(matches!(
            anim.entry(u16::MAX),
            Err(AnimError::InvalidEntry(_))
        ));

        // Now also lie about frame_count on the one real entry.
        let stored_offset = u32::from_le_bytes(
            data[ENTRY_OFFSET_TABLE_START..ENTRY_OFFSET_TABLE_START + 4]
                .try_into()
                .unwrap(),
        ) as usize;
        data[stored_offset..stored_offset + 2].copy_from_slice(&u16::MAX.to_le_bytes());
        let anim2 = MainSdAnim::parse(&data).unwrap();
        let parsed = anim2.entry(0).unwrap();
        // Only the one real frame backed by data was parsed, despite the lie.
        assert_eq!(parsed.frame_count(), 1);
    }

    #[test]
    fn saturating_frame_values_do_not_panic() {
        let mut builder = MainSdBuilder::new(vec!["diffuse"]);
        let id = builder.real_entry(
            vec![None],
            vec![frame(
                u16::MAX,
                u16::MAX,
                i16::MAX,
                i16::MIN,
                u16::MAX,
                u16::MAX,
            )],
        );
        let data = builder.build();

        let anim = MainSdAnim::parse(&data).unwrap();
        let parsed = anim.entry(id).unwrap();
        let f = parsed.frame(0).unwrap();
        assert_eq!(f.texture_x, u16::MAX);
        assert_eq!(f.width, u16::MAX);
        assert_eq!(f.offset_x, i16::MAX);
        assert_eq!(f.offset_y, i16::MIN);

        // Bounded, no panic.
        let (x, y, w, h) = parsed.frame_texel_rect(0).unwrap();
        assert!(x <= u16::MAX as u32 && y <= u16::MAX as u32);
        assert!(w <= u16::MAX as u32 && h <= u16::MAX as u32);
    }

    #[test]
    fn ty_2_is_unsupported_by_mainsd_parse() {
        let mut builder = MainSdBuilder::new(vec!["diffuse"]);
        builder.real_entry(vec![None], vec![]);
        let mut data = builder.build();
        data[5] = 2; // HD's ty
        assert!(matches!(
            MainSdAnim::parse(&data),
            Err(AnimError::Unsupported(_))
        ));
    }

    #[test]
    fn bad_magic_errors() {
        let mut builder = MainSdBuilder::new(vec!["diffuse"]);
        builder.real_entry(vec![None], vec![]);
        let mut data = builder.build();
        data[0] = b'X';
        assert_eq!(MainSdAnim::parse(&data), Err(AnimError::BadMagic));
    }

    #[test]
    fn too_short_errors() {
        assert_eq!(MainSdAnim::parse(&[]), Err(AnimError::TooShort));
        assert_eq!(MainSdAnim::parse(&[0u8; 4]), Err(AnimError::TooShort));
    }

    #[test]
    fn anim_parse_still_rejects_sd_ty() {
        // Sanity check: HD Anim::parse continues to reject ty == 1 (mainSD's ty), unmodified
        // behavior from before this module existed.
        let mut builder = MainSdBuilder::new(vec!["diffuse"]);
        builder.real_entry(vec![None], vec![]);
        let data = builder.build();
        assert!(matches!(Anim::parse(&data), Err(AnimError::Unsupported(_))));
    }

    #[test]
    fn parse_teamcolor_mask_valid_round_trip() {
        let mut payload = TEAMCOLOR_MAGIC.to_vec();
        payload.extend_from_slice(&[0, 255, 255, 0, 0, 255]); // 3x2 mask
        let mask = parse_teamcolor_mask(&payload, 3, 2).unwrap();
        assert_eq!(mask, &[0, 255, 255, 0, 0, 255]);
    }

    #[test]
    fn parse_teamcolor_mask_wrong_magic_is_none() {
        let mut payload = b"NOPE".to_vec();
        payload.extend_from_slice(&[0, 255, 255, 0]);
        assert!(parse_teamcolor_mask(&payload, 2, 2).is_none());
    }

    #[test]
    fn parse_teamcolor_mask_short_payload_is_none() {
        let mut payload = TEAMCOLOR_MAGIC.to_vec();
        payload.extend_from_slice(&[0, 255]); // needs 4 bytes for a 2x2 mask, only has 2
        assert!(parse_teamcolor_mask(&payload, 2, 2).is_none());
    }

    #[test]
    fn parse_teamcolor_mask_zero_dims_is_empty_slice() {
        let payload = TEAMCOLOR_MAGIC.to_vec();
        let mask = parse_teamcolor_mask(&payload, 0, 500).unwrap();
        assert!(mask.is_empty());
    }

    #[test]
    fn parse_teamcolor_mask_large_dims_do_not_overflow() {
        let payload = TEAMCOLOR_MAGIC.to_vec();
        // 65535 * 65535 + 4 is close to u32::MAX; must not overflow/panic, and with such a short
        // payload it's simply None.
        assert!(parse_teamcolor_mask(&payload, u16::MAX, u16::MAX).is_none());
    }
}
