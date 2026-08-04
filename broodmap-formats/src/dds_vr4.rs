use thiserror::Error;

/// Size in bytes of the container's common header: `u32` file size, `u16` frame/tile count, `u16`
/// format code.
const FILE_HEADER_SIZE: usize = 8;
/// Size in bytes of each DDS-layout frame's header: `u32` zero, `u16` width, `u16` height, `u32`
/// payload size.
const FRAME_HEADER_SIZE: usize = 12;
/// Size in bytes of the paletted-layout dimensions fields (`u16` tile width, `u16` tile height)
/// that immediately follow the common header.
const PALETTED_DIMS_SIZE: usize = 4;
/// Size in bytes of the paletted-layout's embedded palette: 256 entries of `{r, g, b, pad}`.
const PALETTE_SIZE: usize = 1024;
/// Offset from the start of the file where the paletted layout's tile table begins (common
/// header + dims + palette).
const PALETTED_TILES_OFFSET: usize = FILE_HEADER_SIZE + PALETTED_DIMS_SIZE + PALETTE_SIZE;
/// Bit in the format code selecting the raw-paletted fixed-size-frame layout. When clear, the
/// file uses the DDS-record layout instead. This is the only bit dispatch should key off of: the
/// low nibble (scale factor: 1=32px, 2=64px, 4=128px) varies independently and does not indicate
/// which layout is in play (e.g. SD `.dds.grp` uses format code `0x1001`, DDS-record layout at
/// 32px).
const FORMAT_PALETTED_BIT: u16 = 0x10;
/// Upper bound on tile width/height (in pixels) used when computing the paletted layout's tile
/// stride. Untrusted input; real files are 32x32. Keeps a hostile/fuzzed header from driving an
/// unbounded per-tile size.
const MAX_TILE_DIM: u32 = 1024;
/// Upper bound on how many frame/tile slots we'll preallocate based on the file's declared count.
/// The count is untrusted input, so this keeps a malicious/fuzzed value from driving an
/// unbounded allocation; more frames than this simply grow the `Vec` normally as they're parsed.
const MAX_PREALLOC_FRAMES: usize = 8192;

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum DdsVr4Error {
    #[error("Data too short to contain a dds.vr4 file header")]
    TooShort,
}

/// An embedded 256-color palette from a paletted (SD) `.dds.vr4` container: 256 entries of
/// `{r, g, b, pad}`, 4 bytes each, in RGB order. The `pad` byte is always zero in observed files
/// and carries no data; consumers should force alpha to 255 rather than reading it. Index 0 is
/// opaque black in practice, but it is a real, used palette entry, not a transparency sentinel.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Palette(pub [u8; PALETTE_SIZE]);

impl Palette {
    /// Returns the `[r, g, b]` bytes of palette entry `index`. Every `u8` index is in range
    /// (256 entries), so this never fails.
    pub fn rgb(&self, index: u8) -> [u8; 3] {
        let offset = index as usize * 4;
        [self.0[offset], self.0[offset + 1], self.0[offset + 2]]
    }
}

/// A single megatile frame, in whichever representation this container's tier uses.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Frame<'a> {
    /// A standard DDS file (HD/HD2 tiers, and SD's `.dds.grp` variant). Parse with
    /// [`crate::parse_dds`].
    Dds(&'a [u8]),
    /// Raw 8-bit palette indices (SD `.dds.vr4` tier), row-major, exactly `width * height` bytes.
    /// Look up colors via [`DdsVr4::palette`].
    Paletted {
        indices: &'a [u8],
        width: u32,
        height: u32,
    },
}

/// The two container layouts a `.dds.vr4` file can use, selected by `format_code & 0x10`.
#[derive(Debug, Clone, Eq, PartialEq)]
enum Frames<'a> {
    Dds(Vec<&'a [u8]>),
    Paletted {
        // Boxed so this variant doesn't blow up `Frames`'s size relative to the `Dds` variant
        // (a `Palette` is 1024 bytes).
        palette: Box<Palette>,
        width: u32,
        height: u32,
        tiles: Vec<&'a [u8]>,
    },
}

/// A parsed `.dds.vr4` container: SC:R's per-tileset megatile texture container, holding one
/// frame per mega-tile ID.
///
/// The container has a common 8-byte header (`u32` file size — informational, not validated
/// against the actual data length; `u16` frame/tile count; `u16` format code), followed by one
/// of two layouts depending on `format_code & 0x10`:
///
/// - **Clear (DDS-record layout, HD/HD2 tiers and SD's `.dds.grp`):** a sequence of
///   `frame_count` records, each `u32` zero + `u16` width + `u16` height + `u32` payload size,
///   followed by that many bytes of a standard DDS file. Frame index == megatile ID.
/// - **Set (paletted layout, SD's `.dds.vr4`):** `u16` tile width + `u16` tile height, then a
///   1024-byte embedded palette (256 x `{r, g, b, pad}`), then `frame_count` tiles of
///   `width * height` raw palette-index bytes each, row-major. Tile index == megatile ID.
///
/// The format code's low nibble (1/2/4) is a scale factor (32/64/128px) and varies independently
/// of the layout bit, so dispatch must only ever key off `0x10`.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DdsVr4<'a> {
    frames: Frames<'a>,
}

impl<'a> DdsVr4<'a> {
    /// Parses a `.dds.vr4` container, eagerly building a table of frame slices. Parsing is
    /// permissive: all offset/size arithmetic is bounds-checked, and if a frame's/tile's declared
    /// size would run past the end of `data`, parsing stops there and returns the frames/tiles
    /// gathered so far rather than erroring.
    pub fn parse(data: &'a [u8]) -> Result<DdsVr4<'a>, DdsVr4Error> {
        if data.len() < FILE_HEADER_SIZE {
            return Err(DdsVr4Error::TooShort);
        }

        let count = u16::from_le_bytes(data[4..6].try_into().unwrap()) as usize;
        let format_code = u16::from_le_bytes(data[6..8].try_into().unwrap());

        if format_code & FORMAT_PALETTED_BIT != 0 {
            Self::parse_paletted(data, count)
        } else {
            Self::parse_dds_records(data, count)
        }
    }

    fn parse_dds_records(data: &'a [u8], frame_count: usize) -> Result<DdsVr4<'a>, DdsVr4Error> {
        let mut frames = Vec::with_capacity(frame_count.min(MAX_PREALLOC_FRAMES));

        let mut offset = FILE_HEADER_SIZE;
        for _ in 0..frame_count {
            let Some(payload_start) = offset.checked_add(FRAME_HEADER_SIZE) else {
                break;
            };
            if payload_start > data.len() {
                break;
            }

            let size_offset = offset + 8;
            let size =
                u32::from_le_bytes(data[size_offset..size_offset + 4].try_into().unwrap()) as usize;

            let Some(payload_end) = payload_start.checked_add(size) else {
                break;
            };
            if payload_end > data.len() {
                break;
            }

            frames.push(&data[payload_start..payload_end]);
            offset = payload_end;
        }

        Ok(DdsVr4 {
            frames: Frames::Dds(frames),
        })
    }

    fn parse_paletted(data: &'a [u8], tile_count: usize) -> Result<DdsVr4<'a>, DdsVr4Error> {
        if data.len() < PALETTED_TILES_OFFSET {
            // Not enough data for the dims + palette: treat as zero frames rather than erroring,
            // matching this parser's permissive philosophy.
            return Ok(DdsVr4 {
                frames: Frames::Paletted {
                    palette: Box::new(Palette([0u8; PALETTE_SIZE])),
                    width: 0,
                    height: 0,
                    tiles: Vec::new(),
                },
            });
        }

        let width = u16::from_le_bytes(data[8..10].try_into().unwrap()) as u32;
        let height = u16::from_le_bytes(data[10..12].try_into().unwrap()) as u32;

        let mut palette_bytes = [0u8; PALETTE_SIZE];
        palette_bytes.copy_from_slice(&data[12..12 + PALETTE_SIZE]);
        let palette = Box::new(Palette(palette_bytes));

        // Cap the dims: untrusted input, real files are 32x32. The capped values are also what
        // gets stored/returned, so a frame's declared dimensions always match its stride.
        let width = width.min(MAX_TILE_DIM);
        let height = height.min(MAX_TILE_DIM);
        let stride = width as usize * height as usize;

        let mut tiles = Vec::with_capacity(tile_count.min(MAX_PREALLOC_FRAMES));
        let mut offset = PALETTED_TILES_OFFSET;
        for _ in 0..tile_count {
            let Some(tile_end) = offset.checked_add(stride) else {
                break;
            };
            if tile_end > data.len() {
                break;
            }
            tiles.push(&data[offset..tile_end]);
            offset = tile_end;
        }

        Ok(DdsVr4 {
            frames: Frames::Paletted {
                palette,
                width,
                height,
                tiles,
            },
        })
    }

    /// The number of frames actually parsed (may be less than the file's declared count if the
    /// data was truncated).
    pub fn frame_count(&self) -> usize {
        match &self.frames {
            Frames::Dds(frames) => frames.len(),
            Frames::Paletted { tiles, .. } => tiles.len(),
        }
    }

    /// The frame for a megatile ID (frame/tile index == megatile ID). `None` if out of range.
    pub fn frame(&self, mega_tile_id: u16) -> Option<Frame<'a>> {
        match &self.frames {
            Frames::Dds(frames) => frames.get(mega_tile_id as usize).copied().map(Frame::Dds),
            Frames::Paletted {
                width,
                height,
                tiles,
                ..
            } => tiles
                .get(mega_tile_id as usize)
                .copied()
                .map(|indices| Frame::Paletted {
                    indices,
                    width: *width,
                    height: *height,
                }),
        }
    }

    /// The embedded palette, for paletted (SD) containers. `None` for DDS-layout containers.
    pub fn palette(&self) -> Option<&Palette> {
        match &self.frames {
            Frames::Dds(_) => None,
            Frames::Paletted { palette, .. } => Some(palette),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn file_header(frame_count: u16, format_code: u16) -> Vec<u8> {
        let mut data = Vec::with_capacity(FILE_HEADER_SIZE);
        data.extend_from_slice(&0u32.to_le_bytes()); // file size (unvalidated)
        data.extend_from_slice(&frame_count.to_le_bytes());
        data.extend_from_slice(&format_code.to_le_bytes());
        data
    }

    fn frame_header_and_payload(payload: &[u8]) -> Vec<u8> {
        let mut data = Vec::with_capacity(FRAME_HEADER_SIZE + payload.len());
        data.extend_from_slice(&0u32.to_le_bytes()); // zero
        data.extend_from_slice(&0u16.to_le_bytes()); // width (unused by the DDS-record layout)
        data.extend_from_slice(&0u16.to_le_bytes()); // height (unused by the DDS-record layout)
        data.extend_from_slice(&(payload.len() as u32).to_le_bytes());
        data.extend_from_slice(payload);
        data
    }

    /// Builds a paletted-layout (SD) `.dds.vr4` file: format code `0x1011` (scale 1 = 32px, bit
    /// `0x10` set), given tile dims, a full 1024-byte palette, and raw tile bytes.
    fn paletted_file(
        width: u16,
        height: u16,
        palette: &[u8; PALETTE_SIZE],
        tiles: &[&[u8]],
    ) -> Vec<u8> {
        let mut data = file_header(tiles.len() as u16, 0x1011);
        data.extend_from_slice(&width.to_le_bytes());
        data.extend_from_slice(&height.to_le_bytes());
        data.extend_from_slice(palette);
        for tile in tiles {
            data.extend_from_slice(tile);
        }
        data
    }

    fn distinctive_palette() -> [u8; PALETTE_SIZE] {
        let mut palette = [0u8; PALETTE_SIZE];
        // Entry 0: opaque black in practice, still a real used entry.
        palette[0..4].copy_from_slice(&[0, 0, 0, 0]);
        // Entry 1: distinctive red-ish color, pad byte nonzero to prove it's ignored.
        palette[4..8].copy_from_slice(&[0x12, 0x34, 0x56, 0xFF]);
        // Entry 255: another distinctive color.
        palette[255 * 4..255 * 4 + 4].copy_from_slice(&[0xAA, 0xBB, 0xCC, 0x00]);
        palette
    }

    #[test]
    fn parses_two_dds_frames() {
        let payload_a = [0xAAu8; 5];
        let payload_b = [0xBBu8; 12];

        let mut data = file_header(2, 0x1004); // HD-style code, bit 0x10 clear
        data.extend(frame_header_and_payload(&payload_a));
        data.extend(frame_header_and_payload(&payload_b));

        let vr4 = DdsVr4::parse(&data).unwrap();
        assert_eq!(vr4.frame_count(), 2);
        assert_eq!(vr4.frame(0), Some(Frame::Dds(&payload_a[..])));
        assert_eq!(vr4.frame(1), Some(Frame::Dds(&payload_b[..])));
        assert!(vr4.frame(2).is_none());
        assert!(vr4.palette().is_none());
    }

    #[test]
    fn format_code_0x1001_parses_as_dds_layout() {
        // SD's `.dds.grp` variant: scale nibble 1 (32px) but bit 0x10 clear.
        let payload = [0xCCu8; 6];
        let mut data = file_header(1, 0x1001);
        data.extend(frame_header_and_payload(&payload));

        let vr4 = DdsVr4::parse(&data).unwrap();
        assert_eq!(vr4.frame_count(), 1);
        assert_eq!(vr4.frame(0), Some(Frame::Dds(&payload[..])));
    }

    #[test]
    fn truncated_frame_keeps_earlier_frames() {
        let payload_a = [0xAAu8; 4];

        let mut data = file_header(2, 0x1004);
        data.extend(frame_header_and_payload(&payload_a));
        // Second frame claims a huge payload size but there's no data backing it.
        data.extend_from_slice(&[0u8; 8]); // zero + width + height
        data.extend_from_slice(&0xFFFF_FFFFu32.to_le_bytes()); // impossible size

        let vr4 = DdsVr4::parse(&data).unwrap();
        assert_eq!(vr4.frame_count(), 1);
        assert_eq!(vr4.frame(0), Some(Frame::Dds(&payload_a[..])));
        assert!(vr4.frame(1).is_none());
    }

    #[test]
    fn truncated_frame_header_stops_cleanly() {
        let mut data = file_header(1, 0x1004);
        data.extend_from_slice(&[0u8; 5]); // not even a full frame header

        let vr4 = DdsVr4::parse(&data).unwrap();
        assert_eq!(vr4.frame_count(), 0);
    }

    #[test]
    fn lying_large_frame_count_does_not_panic_or_oom() {
        let data = file_header(u16::MAX, 0x1004);
        let vr4 = DdsVr4::parse(&data).unwrap();
        assert_eq!(vr4.frame_count(), 0);
    }

    #[test]
    fn errors_on_too_short() {
        assert_eq!(DdsVr4::parse(&[]), Err(DdsVr4Error::TooShort));
        assert_eq!(DdsVr4::parse(&[0u8; 4]), Err(DdsVr4Error::TooShort));
    }

    #[test]
    fn parses_paletted_frames_and_palette() {
        let palette = distinctive_palette();
        let tile_a = [1u8; 4]; // 2x2 tile, all index 1
        let tile_b = [255u8; 4]; // 2x2 tile, all index 255

        let data = paletted_file(2, 2, &palette, &[&tile_a, &tile_b]);

        let vr4 = DdsVr4::parse(&data).unwrap();
        assert_eq!(vr4.frame_count(), 2);
        assert_eq!(
            vr4.frame(0),
            Some(Frame::Paletted {
                indices: &tile_a[..],
                width: 2,
                height: 2,
            })
        );
        assert_eq!(
            vr4.frame(1),
            Some(Frame::Paletted {
                indices: &tile_b[..],
                width: 2,
                height: 2,
            })
        );
        assert!(vr4.frame(2).is_none());

        let parsed_palette = vr4.palette().unwrap();
        assert_eq!(parsed_palette.rgb(1), [0x12, 0x34, 0x56]);
        assert_eq!(parsed_palette.rgb(255), [0xAA, 0xBB, 0xCC]);
        assert_eq!(parsed_palette.rgb(0), [0, 0, 0]);
    }

    #[test]
    fn paletted_truncated_tile_table_keeps_complete_tiles() {
        let palette = distinctive_palette();
        let tile_a = [7u8; 4]; // 2x2 tile
        let mut data = paletted_file(2, 2, &palette, &[&tile_a]);
        // Claim a second tile but only supply half of it.
        data.extend_from_slice(&[9u8; 2]);
        // Rewrite the declared tile count to 2 without adding a full second tile's worth of data.
        data[4..6].copy_from_slice(&2u16.to_le_bytes());

        let vr4 = DdsVr4::parse(&data).unwrap();
        assert_eq!(vr4.frame_count(), 1);
        assert_eq!(
            vr4.frame(0),
            Some(Frame::Paletted {
                indices: &tile_a[..],
                width: 2,
                height: 2,
            })
        );
        assert!(vr4.frame(1).is_none());
    }

    #[test]
    fn paletted_too_short_for_palette_yields_zero_frames() {
        // Format code claims paletted layout, but there isn't even enough data for the dims +
        // palette.
        let mut data = file_header(5, 0x1011);
        data.extend_from_slice(&[0u8; 10]); // well short of PALETTED_TILES_OFFSET

        let vr4 = DdsVr4::parse(&data).unwrap();
        assert_eq!(vr4.frame_count(), 0);
        assert!(vr4.frame(0).is_none());
    }

    #[test]
    fn paletted_lying_large_tile_count_does_not_panic_or_oom() {
        let palette = distinctive_palette();
        let data = paletted_file(32, 32, &palette, &[]);
        let mut data = data;
        data[4..6].copy_from_slice(&u16::MAX.to_le_bytes());

        let vr4 = DdsVr4::parse(&data).unwrap();
        assert_eq!(vr4.frame_count(), 0);
    }
}
