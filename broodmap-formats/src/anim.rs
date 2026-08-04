//! Parser for SC:R's HD `.anim` container format (zero-copy).
//!
//! Only the HD single-entry layout (`ty == 2`, `num_entries == 1`, no inline reference) is
//! supported; SD's single-file layout and inline-reference entries are structurally different and
//! return [`AnimError::Unsupported`] for now.
//!
//! # Coordinate space
//!
//! All frame-table coordinates and the header's canvas width/height are in fixed **"4K" units**,
//! where 1 logical BW pixel = 4 units, *regardless of this file's tier*. The embedded layer
//! textures, however, are at this file's own `scale` (texels per... see below), so converting a
//! frame's 4K-unit rect into texel coordinates of the embedded textures requires dividing by
//! `4 / scale`:
//!
//! - `scale == 4` (HD): divisor 1 (1:1, no scaling)
//! - `scale == 2` (HD2): divisor 2
//! - `scale == 1` (SD): divisor 4
//!
//! [`Anim::frame_texel_rect`] performs this conversion, flooring `texture_x`/`texture_y` and
//! ceiling `width`/`height` (matching the reference implementation's rounding), while
//! `offset_x`/`offset_y` are left as exact 4K-unit values on [`AnimFrame`] for the consumer to
//! scale themselves.

use thiserror::Error;

/// Byte size of the fixed file header (magic, scale, ty, unknown, num_layers, num_entries).
const HEADER_SIZE: usize = 12;
/// Absolute offset where the (fixed-size) layer-name region begins.
const LAYER_NAME_REGION_START: usize = 0x0C;
/// Byte size of each layer-name slot (a NUL-padded name).
const LAYER_NAME_SLOT_SIZE: usize = 32;
/// Number of layer-name slots physically present in the file, regardless of `num_layers`.
const LAYER_NAME_SLOTS: usize = 10;
/// Absolute offset where the frame-table header begins. Fixed regardless of `num_layers` — the
/// layer-name region always occupies exactly this much space.
const FRAME_TABLE_HEADER_OFFSET: usize = 0x14C;
/// Byte size of the frame-table header (frame_count, ref_id, width, height, frame_arr_offset).
const FRAME_TABLE_HEADER_SIZE: usize = 12;
/// Absolute offset where the per-layer texture records begin.
const LAYER_RECORDS_OFFSET: usize = 0x158;
/// Byte size of a single layer texture record (offset, size, width, height).
const LAYER_RECORD_SIZE: usize = 12;
/// Byte size of a single frame-table entry (texture_x, texture_y, offset_x, offset_y, width,
/// height, unknown).
const FRAME_RECORD_SIZE: usize = 16;

/// Sane upper bound on `num_layers`. Untrusted input; real files have a small handful of layers
/// (diffuse, teamcolor, ...). Layers beyond this are simply not parsed, rather than driving an
/// unbounded allocation for a hostile/fuzzed count.
const MAX_LAYERS: usize = 0x200;
/// Upper bound on how many frame slots we'll preallocate based on the file's declared
/// `frame_count`. The count is untrusted input; more frames than this simply grow the `Vec`
/// normally as they're parsed (bounds-checked against the input regardless).
const MAX_PREALLOC_FRAMES: usize = 8192;

/// Container type byte. Only HD is supported.
const TYPE_HD: u8 = 2;
/// Sentinel `ref_id` meaning "this is a real frame table, not an inline reference".
const NO_REF_ID: u16 = 0xFFFF;

#[derive(Error, Debug, Clone, Eq, PartialEq)]
pub enum AnimError {
    #[error("Data too short to contain a valid ANIM header")]
    TooShort,
    #[error("Missing or invalid ANIM magic bytes")]
    BadMagic,
    #[error("Unsupported ANIM container: {0}")]
    Unsupported(&'static str),
}

/// One layer of a parsed `.anim` (e.g. "diffuse", "teamcolor"). Its `data` is the raw embedded
/// payload — typically a DDS file (parse with [`crate::parse_dds`]), occasionally PNG — left
/// undecoded.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AnimLayer<'a> {
    pub name: String,
    pub data: &'a [u8],
    pub width: u16,
    pub height: u16,
}

/// A single animation frame's placement, in raw 4K coordinate-space units (see the module docs).
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct AnimFrame {
    pub texture_x: u16,
    pub texture_y: u16,
    pub offset_x: i16,
    pub offset_y: i16,
    pub width: u16,
    pub height: u16,
}

/// A parsed SC:R HD `.anim` container: a set of layer textures plus a shared frame table
/// describing how each frame indexes into them.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Anim<'a> {
    scale: u8,
    width: u16,
    height: u16,
    layers: Vec<AnimLayer<'a>>,
    frames: Vec<AnimFrame>,
}

impl<'a> Anim<'a> {
    /// Parses an `.anim` file. See the module docs for which layouts are supported.
    pub fn parse(data: &'a [u8]) -> Result<Anim<'a>, AnimError> {
        if data.len() < HEADER_SIZE {
            return Err(AnimError::TooShort);
        }
        if &data[0..4] != b"ANIM" {
            return Err(AnimError::BadMagic);
        }

        let scale = data[4];
        let ty = data[5];
        // data[6..8] is an unknown u16, skipped.
        let num_layers = u16::from_le_bytes(data[8..10].try_into().unwrap()) as usize;
        let num_entries = u16::from_le_bytes(data[10..12].try_into().unwrap());

        if ty != TYPE_HD {
            return Err(AnimError::Unsupported(
                "only HD (ty == 2) .anim containers are supported; SD uses a different layout",
            ));
        }
        if num_entries != 1 {
            return Err(AnimError::Unsupported(
                "HD .anim containers must have exactly one entry",
            ));
        }

        let total_layers = num_layers.min(MAX_LAYERS);
        let named_layers = num_layers.min(LAYER_NAME_SLOTS);

        let mut layer_names: Vec<String> = Vec::with_capacity(total_layers);
        for i in 0..named_layers {
            let start = LAYER_NAME_REGION_START + i * LAYER_NAME_SLOT_SIZE;
            let name = if start < data.len() {
                let end = (start + LAYER_NAME_SLOT_SIZE).min(data.len());
                let raw = &data[start..end];
                let nul_pos = raw.iter().position(|&b| b == 0).unwrap_or(raw.len());
                String::from_utf8_lossy(&raw[..nul_pos]).into_owned()
            } else {
                String::new()
            };
            layer_names.push(name);
        }
        for i in named_layers..total_layers {
            layer_names.push(format!("Layer{i}"));
        }

        if data.len() < FRAME_TABLE_HEADER_OFFSET + FRAME_TABLE_HEADER_SIZE {
            return Err(AnimError::TooShort);
        }
        let fth =
            &data[FRAME_TABLE_HEADER_OFFSET..FRAME_TABLE_HEADER_OFFSET + FRAME_TABLE_HEADER_SIZE];
        let frame_count = u16::from_le_bytes(fth[0..2].try_into().unwrap()) as usize;
        let ref_id = u16::from_le_bytes(fth[2..4].try_into().unwrap());
        let width = u16::from_le_bytes(fth[4..6].try_into().unwrap());
        let height = u16::from_le_bytes(fth[6..8].try_into().unwrap());
        let frame_arr_offset = u32::from_le_bytes(fth[8..12].try_into().unwrap()) as usize;

        if ref_id != NO_REF_ID {
            return Err(AnimError::Unsupported(
                "inline-reference .anim entries are not yet supported",
            ));
        }

        let mut layers = Vec::with_capacity(total_layers);
        for (i, name) in layer_names.into_iter().enumerate() {
            let rec_start = LAYER_RECORDS_OFFSET + i * LAYER_RECORD_SIZE;
            let Some(rec_end) = rec_start.checked_add(LAYER_RECORD_SIZE) else {
                continue;
            };
            if rec_end > data.len() {
                continue;
            }
            let rec = &data[rec_start..rec_end];
            let offset = u32::from_le_bytes(rec[0..4].try_into().unwrap()) as usize;
            let size = u32::from_le_bytes(rec[4..8].try_into().unwrap()) as usize;
            let tex_width = u16::from_le_bytes(rec[8..10].try_into().unwrap());
            let tex_height = u16::from_le_bytes(rec[10..12].try_into().unwrap());

            if offset == 0 {
                // Layer absent.
                continue;
            }
            let Some(payload_end) = offset.checked_add(size) else {
                continue;
            };
            if payload_end > data.len() {
                continue;
            }

            layers.push(AnimLayer {
                name,
                data: &data[offset..payload_end],
                width: tex_width,
                height: tex_height,
            });
        }

        let mut frames = Vec::with_capacity(frame_count.min(MAX_PREALLOC_FRAMES));
        let mut offset = frame_arr_offset;
        for _ in 0..frame_count {
            let Some(end) = offset.checked_add(FRAME_RECORD_SIZE) else {
                break;
            };
            if end > data.len() {
                break;
            }
            let rec = &data[offset..end];
            let texture_x = u16::from_le_bytes(rec[0..2].try_into().unwrap());
            let texture_y = u16::from_le_bytes(rec[2..4].try_into().unwrap());
            let offset_x = i16::from_le_bytes(rec[4..6].try_into().unwrap());
            let offset_y = i16::from_le_bytes(rec[6..8].try_into().unwrap());
            let frame_width = u16::from_le_bytes(rec[8..10].try_into().unwrap());
            let frame_height = u16::from_le_bytes(rec[10..12].try_into().unwrap());
            // rec[12..16] is an unknown u32, skipped.

            frames.push(AnimFrame {
                texture_x,
                texture_y,
                offset_x,
                offset_y,
                width: frame_width,
                height: frame_height,
            });
            offset = end;
        }

        Ok(Anim {
            scale,
            width,
            height,
            layers,
            frames,
        })
    }

    /// The file's raw scale tier byte, as stored in the file: nominally 4 = HD, 2 = HD2, 1 = SD.
    /// This is untrusted input and is returned exactly as parsed, with no validation or
    /// clamping — a hostile/malformed file can make this any `u8` value (e.g. `0` or `255`).
    /// Consumers doing their own arithmetic with this value (rather than using
    /// [`Anim::frame_texel_rect`], which clamps internally) should treat out-of-domain values
    /// (anything other than 1, 2, or 4) as untrusted and guard accordingly.
    pub fn scale(&self) -> u8 {
        self.scale
    }

    /// The overall canvas size, in raw 4K coordinate-space units.
    pub fn canvas_size(&self) -> (u16, u16) {
        (self.width, self.height)
    }

    /// Looks up a layer by name (e.g. `"diffuse"`, `"teamcolor"`). `None` if there's no layer
    /// with that name (including layers whose texture record was absent/out-of-bounds).
    pub fn layer(&self, name: &str) -> Option<&AnimLayer<'a>> {
        self.layers.iter().find(|l| l.name == name)
    }

    /// Every layer actually parsed (bounded by `MAX_LAYERS`; layers whose texture record was
    /// absent/out-of-bounds are not present at all — see [`Anim::parse`]).
    pub fn layers(&self) -> &[AnimLayer<'a>] {
        &self.layers
    }

    /// The number of frames actually parsed (may be less than the file's declared `frame_count`
    /// if the data was truncated).
    pub fn frame_count(&self) -> usize {
        self.frames.len()
    }

    /// The frame at index `i`, in raw 4K coordinate-space units. `None` if out of range.
    pub fn frame(&self, i: usize) -> Option<&AnimFrame> {
        self.frames.get(i)
    }

    /// Frame `i`'s rect — `(x, y, width, height)` — in **texel coordinates of this file's
    /// embedded layer textures** (see the module docs for the 4K-unit -> texel conversion).
    /// `x`/`y` are floored, `width`/`height` are ceiled; `None` if `i` is out of range.
    pub fn frame_texel_rect(&self, i: usize) -> Option<(u32, u32, u32, u32)> {
        let frame = self.frame(i)?;
        // `scale` is nominally 1/2/4 (SD/HD2/HD); the raw stored byte is untrusted (a
        // fuzzed/malformed file can set it to anything, e.g. 0 or 255), so clamp it into the
        // meaningful 1..=4 domain before it drives any arithmetic. This makes `divisor` always a
        // sane, non-zero value derived from a valid scale, rather than an artifact of truncating
        // integer division on a garbage input (`4 / scale` would otherwise floor to 0 for any
        // scale > 4, which `.max(1)` alone would paper over without actually being meaningful).
        let scale = (self.scale as u32).clamp(1, 4);
        let divisor = (4 / scale).max(1);

        let x = frame.texture_x as u32 / divisor;
        let y = frame.texture_y as u32 / divisor;
        let width = frame.width as u32 + divisor - 1;
        let height = frame.height as u32 + divisor - 1;
        Some((x, y, width / divisor, height / divisor))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dds::parse_dds;

    /// Builds a full HD `.anim` file with `layer_names` (each getting its own texture record —
    /// pass `None` for an offset-0/absent layer) and `frames`.
    struct AnimBuilder {
        scale: u8,
        layer_names: Vec<&'static str>,
        /// Per layer: `Some((payload, width, height))` for a present layer, `None` for absent.
        layer_payloads: Vec<Option<(Vec<u8>, u16, u16)>>,
        frames: Vec<AnimFrame>,
    }

    impl AnimBuilder {
        fn new(scale: u8) -> Self {
            Self {
                scale,
                layer_names: Vec::new(),
                layer_payloads: Vec::new(),
                frames: Vec::new(),
            }
        }

        fn layer(mut self, name: &'static str, payload: Option<(Vec<u8>, u16, u16)>) -> Self {
            self.layer_names.push(name);
            self.layer_payloads.push(payload);
            self
        }

        fn frame(mut self, frame: AnimFrame) -> Self {
            self.frames.push(frame);
            self
        }

        fn build(self) -> Vec<u8> {
            let num_layers = self.layer_names.len();

            let mut data = vec![0u8; LAYER_RECORDS_OFFSET + num_layers * LAYER_RECORD_SIZE];
            data[0..4].copy_from_slice(b"ANIM");
            data[4] = self.scale;
            data[5] = TYPE_HD;
            data[6..8].copy_from_slice(&0u16.to_le_bytes()); // unknown
            data[8..10].copy_from_slice(&(num_layers as u16).to_le_bytes());
            data[10..12].copy_from_slice(&1u16.to_le_bytes()); // num_entries

            for (i, name) in self.layer_names.iter().enumerate() {
                let start = LAYER_NAME_REGION_START + i * LAYER_NAME_SLOT_SIZE;
                data[start..start + name.len()].copy_from_slice(name.as_bytes());
                // Rest of the 32-byte slot stays zero (NUL padding).
            }

            // Frame table header at FRAME_TABLE_HEADER_OFFSET.
            let canvas_width = 64u16;
            let canvas_height = 48u16;
            let frame_arr_offset = data.len(); // frames appended after layer records
            data[FRAME_TABLE_HEADER_OFFSET..FRAME_TABLE_HEADER_OFFSET + 2]
                .copy_from_slice(&(self.frames.len() as u16).to_le_bytes());
            data[FRAME_TABLE_HEADER_OFFSET + 2..FRAME_TABLE_HEADER_OFFSET + 4]
                .copy_from_slice(&NO_REF_ID.to_le_bytes());
            data[FRAME_TABLE_HEADER_OFFSET + 4..FRAME_TABLE_HEADER_OFFSET + 6]
                .copy_from_slice(&canvas_width.to_le_bytes());
            data[FRAME_TABLE_HEADER_OFFSET + 6..FRAME_TABLE_HEADER_OFFSET + 8]
                .copy_from_slice(&canvas_height.to_le_bytes());
            data[FRAME_TABLE_HEADER_OFFSET + 8..FRAME_TABLE_HEADER_OFFSET + 12]
                .copy_from_slice(&(frame_arr_offset as u32).to_le_bytes());

            // Layer texture records at LAYER_RECORDS_OFFSET, payloads appended right after the
            // (fixed-size) frame table, in layer order.
            let mut payload_cursor = frame_arr_offset + self.frames.len() * FRAME_RECORD_SIZE;
            let mut payloads = Vec::new();
            for (i, payload) in self.layer_payloads.iter().enumerate() {
                let rec_start = LAYER_RECORDS_OFFSET + i * LAYER_RECORD_SIZE;
                match payload {
                    Some((bytes, w, h)) => {
                        data[rec_start..rec_start + 4]
                            .copy_from_slice(&(payload_cursor as u32).to_le_bytes());
                        data[rec_start + 4..rec_start + 8]
                            .copy_from_slice(&(bytes.len() as u32).to_le_bytes());
                        data[rec_start + 8..rec_start + 10].copy_from_slice(&w.to_le_bytes());
                        data[rec_start + 10..rec_start + 12].copy_from_slice(&h.to_le_bytes());
                        payloads.push(bytes.clone());
                        payload_cursor += bytes.len();
                    }
                    None => {
                        // offset 0 => absent; leave the whole record zeroed.
                    }
                }
            }

            // Now grow `data` to hold the frame table and payloads, then fill the frame table.
            data.resize(payload_cursor, 0);
            let mut frame_offset = frame_arr_offset;
            for frame in &self.frames {
                data[frame_offset..frame_offset + 2]
                    .copy_from_slice(&frame.texture_x.to_le_bytes());
                data[frame_offset + 2..frame_offset + 4]
                    .copy_from_slice(&frame.texture_y.to_le_bytes());
                data[frame_offset + 4..frame_offset + 6]
                    .copy_from_slice(&frame.offset_x.to_le_bytes());
                data[frame_offset + 6..frame_offset + 8]
                    .copy_from_slice(&frame.offset_y.to_le_bytes());
                data[frame_offset + 8..frame_offset + 10]
                    .copy_from_slice(&frame.width.to_le_bytes());
                data[frame_offset + 10..frame_offset + 12]
                    .copy_from_slice(&frame.height.to_le_bytes());
                // unknown u32 left zeroed
                frame_offset += FRAME_RECORD_SIZE;
            }

            let mut payload_offset = frame_arr_offset + self.frames.len() * FRAME_RECORD_SIZE;
            for payload in &payloads {
                data[payload_offset..payload_offset + payload.len()].copy_from_slice(payload);
                payload_offset += payload.len();
            }

            data
        }
    }

    #[test]
    fn parses_happy_path_two_layers_one_absent_two_frames() {
        let data = AnimBuilder::new(2) // HD2
            .layer("diffuse", Some((vec![0xAAu8; 6], 32, 32)))
            .layer("teamcolor", None) // absent (offset 0)
            .frame(AnimFrame {
                texture_x: 8,
                texture_y: 4,
                offset_x: -2,
                offset_y: 3,
                width: 10,
                height: 9,
            })
            .frame(AnimFrame {
                texture_x: 0,
                texture_y: 0,
                offset_x: 0,
                offset_y: 0,
                width: 4,
                height: 4,
            })
            .build();

        let anim = Anim::parse(&data).unwrap();
        assert_eq!(anim.scale(), 2);
        assert_eq!(anim.canvas_size(), (64, 48));
        assert_eq!(anim.frame_count(), 2);

        let diffuse = anim.layer("diffuse").unwrap();
        assert_eq!(diffuse.data, &[0xAAu8; 6][..]);
        assert_eq!(diffuse.width, 32);
        assert_eq!(diffuse.height, 32);

        // The absent layer has no entry at all.
        assert!(anim.layer("teamcolor").is_none());

        let frame0 = anim.frame(0).unwrap();
        assert_eq!(frame0.offset_x, -2);
        assert_eq!(frame0.offset_y, 3);

        // scale=2 => divisor = 4/2 = 2. texture_x=8 -> 4 (floor), width=10 -> ceil(10/2)=5.
        let (x, y, w, h) = anim.frame_texel_rect(0).unwrap();
        assert_eq!((x, y, w, h), (4, 2, 5, 5));

        // height=9 is odd: ceil(9/2) = 5, proving ceiling (not floor/truncation) is used.
        assert_eq!(h, 5);

        assert!(anim.frame_texel_rect(2).is_none());
    }

    #[test]
    fn errors_on_too_short() {
        assert_eq!(Anim::parse(&[]), Err(AnimError::TooShort));
        assert_eq!(Anim::parse(&[0u8; 4]), Err(AnimError::TooShort));
    }

    #[test]
    fn errors_on_bad_magic() {
        let mut data = AnimBuilder::new(4).build();
        data[0] = b'X';
        assert_eq!(Anim::parse(&data), Err(AnimError::BadMagic));
    }

    #[test]
    fn errors_on_sd_type() {
        let mut data = AnimBuilder::new(1).build();
        data[5] = 1; // ty = SD
        assert!(matches!(Anim::parse(&data), Err(AnimError::Unsupported(_))));
    }

    #[test]
    fn errors_on_multiple_entries() {
        let mut data = AnimBuilder::new(4).build();
        data[10..12].copy_from_slice(&2u16.to_le_bytes());
        assert!(matches!(Anim::parse(&data), Err(AnimError::Unsupported(_))));
    }

    #[test]
    fn errors_on_inline_reference_entry() {
        let mut data = AnimBuilder::new(4).build();
        data[FRAME_TABLE_HEADER_OFFSET + 2..FRAME_TABLE_HEADER_OFFSET + 4]
            .copy_from_slice(&0u16.to_le_bytes()); // ref_id != 0xFFFF
        assert!(matches!(Anim::parse(&data), Err(AnimError::Unsupported(_))));
    }

    #[test]
    fn lying_layer_and_frame_counts_do_not_panic_or_oom() {
        let mut data = AnimBuilder::new(4).build();
        data[8..10].copy_from_slice(&u16::MAX.to_le_bytes()); // num_layers
        data[FRAME_TABLE_HEADER_OFFSET..FRAME_TABLE_HEADER_OFFSET + 2]
            .copy_from_slice(&u16::MAX.to_le_bytes()); // frame_count

        let anim = Anim::parse(&data).unwrap();
        // No backing data for any of the claimed layers/frames beyond what's actually present.
        assert_eq!(anim.frame_count(), 0);
        assert!(anim.layer("diffuse").is_none());
    }

    #[test]
    fn truncated_frame_table_keeps_earlier_frames() {
        let mut data = AnimBuilder::new(4)
            .frame(AnimFrame {
                texture_x: 1,
                texture_y: 1,
                offset_x: 0,
                offset_y: 0,
                width: 4,
                height: 4,
            })
            .frame(AnimFrame {
                texture_x: 2,
                texture_y: 2,
                offset_x: 0,
                offset_y: 0,
                width: 4,
                height: 4,
            })
            .build();
        // Truncate right after the first frame record.
        let frame_arr_offset = u32::from_le_bytes(
            data[FRAME_TABLE_HEADER_OFFSET + 8..FRAME_TABLE_HEADER_OFFSET + 12]
                .try_into()
                .unwrap(),
        ) as usize;
        data.truncate(frame_arr_offset + FRAME_RECORD_SIZE);

        let anim = Anim::parse(&data).unwrap();
        assert_eq!(anim.frame_count(), 1);
        assert_eq!(anim.frame(0).unwrap().texture_x, 1);
    }

    #[test]
    fn scale_zero_produces_sane_non_panicking_texel_rect() {
        let data = AnimBuilder::new(0)
            .frame(AnimFrame {
                texture_x: 8,
                texture_y: 4,
                offset_x: 0,
                offset_y: 0,
                width: 10,
                height: 9,
            })
            .build();

        let anim = Anim::parse(&data).unwrap();
        assert_eq!(anim.scale(), 0); // raw byte is returned as-is, unclamped.

        // scale 0 clamps to 1 internally => divisor = 4/1 = 4.
        let (x, y, w, h) = anim.frame_texel_rect(0).unwrap();
        assert_eq!((x, y, w, h), (2, 1, 3, 3));
    }

    #[test]
    fn scale_255_produces_sane_non_panicking_texel_rect() {
        let data = AnimBuilder::new(255)
            .frame(AnimFrame {
                texture_x: 8,
                texture_y: 4,
                offset_x: 0,
                offset_y: 0,
                width: 10,
                height: 9,
            })
            .build();

        let anim = Anim::parse(&data).unwrap();
        assert_eq!(anim.scale(), 255); // raw byte is returned as-is, unclamped.

        // scale 255 clamps to 4 internally => divisor = 4/4 = 1 (no scaling).
        let (x, y, w, h) = anim.frame_texel_rect(0).unwrap();
        assert_eq!((x, y, w, h), (8, 4, 10, 9));
    }

    #[test]
    fn layer_payload_sniffs_as_dds() {
        // Build a minimal real DDS payload to prove layer data round-trips as raw bytes suitable
        // for `parse_dds` (not decoded by this module).
        let mut dds_payload = Vec::new();
        dds_payload.extend_from_slice(b"DDS ");
        dds_payload.extend_from_slice(&124u32.to_le_bytes());
        dds_payload.extend_from_slice(&[0u8; 120]); // rest of the header, zeroed

        let data = AnimBuilder::new(4)
            .layer("diffuse", Some((dds_payload.clone(), 16, 16)))
            .build();

        let anim = Anim::parse(&data).unwrap();
        let diffuse = anim.layer("diffuse").unwrap();
        assert_eq!(diffuse.data, &dds_payload[..]);
        // Sniffs as a (mostly empty) DDS file without erroring on magic.
        assert!(parse_dds(diffuse.data).is_ok());
    }
}
