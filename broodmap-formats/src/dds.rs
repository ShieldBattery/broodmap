use thiserror::Error;

/// Size in bytes of the `DDS ` magic plus the 124-byte `DDS_HEADER`.
const HEADER_END: usize = 4 + 124;
/// Size in bytes of the optional `DDS_HEADER_DXT10` extension.
const DX10_HEADER_SIZE: usize = 20;
/// Offset of the `DDS_HEADER_DXT10::dxgiFormat` field, when present.
const DX10_FORMAT_OFFSET: usize = HEADER_END;
/// Offset where pixel data begins when a DXT10 header is present.
const DX10_PAYLOAD_OFFSET: usize = HEADER_END + DX10_HEADER_SIZE;

/// `DDPF_RGB`: the pixel format contains uncompressed RGB data.
const DDPF_RGB: u32 = 0x40;

// DXGI_FORMAT values we care about (from dxgiformat.h).
const DXGI_FORMAT_BC1_UNORM: u32 = 71;
const DXGI_FORMAT_BC1_UNORM_SRGB: u32 = 72;
const DXGI_FORMAT_BC2_UNORM: u32 = 74;
const DXGI_FORMAT_BC2_UNORM_SRGB: u32 = 75;
const DXGI_FORMAT_BC3_UNORM: u32 = 77;
const DXGI_FORMAT_BC3_UNORM_SRGB: u32 = 78;
const DXGI_FORMAT_BC4_UNORM: u32 = 80;
const DXGI_FORMAT_BC4_SNORM: u32 = 81;

/// The pixel format of a DDS file's payload, as far as we can determine from its header.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum DdsFormat {
    /// BC1 / DXT1 block-compressed (opaque or 1-bit alpha).
    Bc1,
    /// BC2 / DXT2-3 block-compressed (explicit alpha).
    Bc2,
    /// BC3 / DXT4-5 block-compressed (interpolated alpha).
    Bc3,
    /// BC4 single-channel block-compressed.
    Bc4,
    /// Uncompressed 32bpp RGBA (channel order is not validated, see module docs).
    Rgba8,
    /// A format we don't recognize; holds the raw FourCC bytes (or, for a DX10 container with an
    /// unrecognized `dxgiFormat`, the `DX10` FourCC itself) for callers that want to inspect it.
    Unknown([u8; 4]),
}

/// A parsed DDS container: header fields plus a borrowed slice of the pixel payload (mip 0 first).
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DdsFile<'a> {
    pub width: u32,
    pub height: u32,
    pub mip_count: u32,
    pub format: DdsFormat,
    pub payload: &'a [u8],
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum DdsError {
    #[error("Data too short to contain a DDS header")]
    TooShort,
    #[error("Missing or invalid DDS magic bytes")]
    BadMagic,
    #[error("Invalid DDS header size")]
    BadHeaderSize,
}

/// Parses a DDS container's header, returning its dimensions, pixel format, and a slice of the
/// payload starting at mip 0. Payload length is not validated against `width`/`height`/`mip_count`
/// — some SC:R files are quirky here, and downstream decoders handle short data themselves.
pub fn parse_dds(data: &[u8]) -> Result<DdsFile<'_>, DdsError> {
    if data.len() < 4 {
        return Err(DdsError::TooShort);
    }
    if &data[0..4] != b"DDS " {
        return Err(DdsError::BadMagic);
    }
    if data.len() < HEADER_END {
        return Err(DdsError::TooShort);
    }

    let header_size = u32::from_le_bytes(data[4..8].try_into().unwrap());
    if header_size != 124 {
        return Err(DdsError::BadHeaderSize);
    }

    let height = u32::from_le_bytes(data[12..16].try_into().unwrap());
    let width = u32::from_le_bytes(data[16..20].try_into().unwrap());
    let mip_map_count = u32::from_le_bytes(data[28..32].try_into().unwrap());
    let mip_count = if mip_map_count == 0 { 1 } else { mip_map_count };

    let pf_flags = u32::from_le_bytes(data[80..84].try_into().unwrap());
    let four_cc: [u8; 4] = data[84..88].try_into().unwrap();
    let rgb_bit_count = u32::from_le_bytes(data[88..92].try_into().unwrap());
    let r_mask = u32::from_le_bytes(data[92..96].try_into().unwrap());
    let g_mask = u32::from_le_bytes(data[96..100].try_into().unwrap());
    let b_mask = u32::from_le_bytes(data[100..104].try_into().unwrap());
    let a_mask = u32::from_le_bytes(data[104..108].try_into().unwrap());

    let (format, payload_offset) = if four_cc == *b"DX10" {
        let dxgi_format = if data.len() >= DX10_FORMAT_OFFSET + 4 {
            u32::from_le_bytes(
                data[DX10_FORMAT_OFFSET..DX10_FORMAT_OFFSET + 4]
                    .try_into()
                    .unwrap(),
            )
        } else {
            0
        };

        let format = match dxgi_format {
            DXGI_FORMAT_BC1_UNORM | DXGI_FORMAT_BC1_UNORM_SRGB => DdsFormat::Bc1,
            DXGI_FORMAT_BC2_UNORM | DXGI_FORMAT_BC2_UNORM_SRGB => DdsFormat::Bc2,
            DXGI_FORMAT_BC3_UNORM | DXGI_FORMAT_BC3_UNORM_SRGB => DdsFormat::Bc3,
            DXGI_FORMAT_BC4_UNORM | DXGI_FORMAT_BC4_SNORM => DdsFormat::Bc4,
            _ => DdsFormat::Unknown(four_cc),
        };
        (format, DX10_PAYLOAD_OFFSET)
    } else {
        let format = match &four_cc {
            b"DXT1" => DdsFormat::Bc1,
            b"DXT2" | b"DXT3" => DdsFormat::Bc2,
            b"DXT4" | b"DXT5" => DdsFormat::Bc3,
            b"BC4U" | b"ATI1" => DdsFormat::Bc4,
            // Only the canonical byte-order-RGBA mask arrangement qualifies as Rgba8:
            // classifying every 32bpp DDPF_RGB image this way would silently render e.g. the
            // (more common) BGRA arrangement with red/blue swapped. Anything else is Unknown
            // until a consumer actually needs channel-mask interpretation.
            _ if pf_flags & DDPF_RGB != 0
                && rgb_bit_count == 32
                && r_mask == 0x0000_00FF
                && g_mask == 0x0000_FF00
                && b_mask == 0x00FF_0000
                && a_mask == 0xFF00_0000 =>
            {
                DdsFormat::Rgba8
            }
            _ => DdsFormat::Unknown(four_cc),
        };
        (format, HEADER_END)
    };

    let payload_offset = payload_offset.min(data.len());
    let payload = &data[payload_offset..];

    Ok(DdsFile {
        width,
        height,
        mip_count,
        format,
        payload,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Builds a minimal DDS header (magic + 124-byte `DDS_HEADER`) with the given height, width,
    /// mip count, pixel-format flags, FourCC, and RGB bit count.
    fn make_header(
        height: u32,
        width: u32,
        mip_map_count: u32,
        pf_flags: u32,
        four_cc: &[u8; 4],
        rgb_bit_count: u32,
    ) -> Vec<u8> {
        let mut data = Vec::with_capacity(HEADER_END);
        data.extend_from_slice(b"DDS ");
        data.extend_from_slice(&124u32.to_le_bytes()); // dwSize
        data.extend_from_slice(&0u32.to_le_bytes()); // dwFlags
        data.extend_from_slice(&height.to_le_bytes());
        data.extend_from_slice(&width.to_le_bytes());
        data.extend_from_slice(&0u32.to_le_bytes()); // dwPitchOrLinearSize
        data.extend_from_slice(&0u32.to_le_bytes()); // dwDepth
        data.extend_from_slice(&mip_map_count.to_le_bytes());
        data.extend_from_slice(&[0u8; 44]); // dwReserved1[11]
        debug_assert_eq!(data.len(), 76);
        data.extend_from_slice(&32u32.to_le_bytes()); // pixel format dwSize
        data.extend_from_slice(&pf_flags.to_le_bytes());
        data.extend_from_slice(four_cc);
        data.extend_from_slice(&rgb_bit_count.to_le_bytes());
        data.extend_from_slice(&0u32.to_le_bytes()); // dwRBitMask
        data.extend_from_slice(&0u32.to_le_bytes()); // dwGBitMask
        data.extend_from_slice(&0u32.to_le_bytes()); // dwBBitMask
        data.extend_from_slice(&0u32.to_le_bytes()); // dwABitMask
        debug_assert_eq!(data.len(), 108);
        data.extend_from_slice(&0u32.to_le_bytes()); // dwCaps
        data.extend_from_slice(&0u32.to_le_bytes()); // dwCaps2
        data.extend_from_slice(&0u32.to_le_bytes()); // dwCaps3
        data.extend_from_slice(&0u32.to_le_bytes()); // dwCaps4
        data.extend_from_slice(&0u32.to_le_bytes()); // dwReserved2
        assert_eq!(data.len(), HEADER_END);
        data
    }

    #[test]
    fn parses_dxt1() {
        let mut data = make_header(64, 128, 3, 0x4, b"DXT1", 0);
        data.extend_from_slice(&[0xAA; 16]); // fake payload

        let dds = parse_dds(&data).unwrap();
        assert_eq!(dds.width, 128);
        assert_eq!(dds.height, 64);
        assert_eq!(dds.mip_count, 3);
        assert_eq!(dds.format, DdsFormat::Bc1);
        assert_eq!(dds.payload, &[0xAA; 16]);
    }

    #[test]
    fn zero_mip_count_becomes_one() {
        let data = make_header(4, 4, 0, 0x4, b"DXT5", 0);
        let dds = parse_dds(&data).unwrap();
        assert_eq!(dds.mip_count, 1);
        assert_eq!(dds.format, DdsFormat::Bc3);
    }

    /// Overwrites the four channel masks (R, G, B, A at offsets 92/96/100/104) in a header
    /// built by [`make_header`].
    fn set_masks(data: &mut [u8], r: u32, g: u32, b: u32, a: u32) {
        data[92..96].copy_from_slice(&r.to_le_bytes());
        data[96..100].copy_from_slice(&g.to_le_bytes());
        data[100..104].copy_from_slice(&b.to_le_bytes());
        data[104..108].copy_from_slice(&a.to_le_bytes());
    }

    #[test]
    fn parses_uncompressed_rgba_with_canonical_masks() {
        let mut data = make_header(4, 4, 1, DDPF_RGB, &[0, 0, 0, 0], 32);
        set_masks(
            &mut data,
            0x0000_00FF,
            0x0000_FF00,
            0x00FF_0000,
            0xFF00_0000,
        );
        let dds = parse_dds(&data).unwrap();
        assert_eq!(dds.format, DdsFormat::Rgba8);
    }

    #[test]
    fn uncompressed_with_noncanonical_masks_is_unknown() {
        // BGRA masks (the most common 32bpp arrangement) must NOT be classified as Rgba8 —
        // copying its bytes as RGBA would swap red and blue.
        let mut data = make_header(4, 4, 1, DDPF_RGB, &[0, 0, 0, 0], 32);
        set_masks(
            &mut data,
            0x00FF_0000,
            0x0000_FF00,
            0x0000_00FF,
            0xFF00_0000,
        );
        let dds = parse_dds(&data).unwrap();
        assert_eq!(dds.format, DdsFormat::Unknown([0, 0, 0, 0]));

        // Zero masks (as some exporters write for fourCC-less files) are likewise Unknown.
        let data = make_header(4, 4, 1, DDPF_RGB, &[0, 0, 0, 0], 32);
        let dds = parse_dds(&data).unwrap();
        assert_eq!(dds.format, DdsFormat::Unknown([0, 0, 0, 0]));
    }

    #[test]
    fn unrecognized_fourcc_is_unknown() {
        let data = make_header(4, 4, 1, 0x4, b"WXYZ", 0);
        let dds = parse_dds(&data).unwrap();
        assert_eq!(dds.format, DdsFormat::Unknown(*b"WXYZ"));
    }

    #[test]
    fn parses_dx10_header() {
        let mut data = make_header(8, 8, 1, 0x4, b"DX10", 0);
        data.extend_from_slice(&DXGI_FORMAT_BC3_UNORM.to_le_bytes()); // dxgiFormat
        data.extend_from_slice(&3u32.to_le_bytes()); // resourceDimension
        data.extend_from_slice(&0u32.to_le_bytes()); // miscFlag
        data.extend_from_slice(&1u32.to_le_bytes()); // arraySize
        data.extend_from_slice(&0u32.to_le_bytes()); // miscFlags2
        assert_eq!(data.len(), DX10_PAYLOAD_OFFSET);
        data.extend_from_slice(&[0x11; 8]); // fake payload

        let dds = parse_dds(&data).unwrap();
        assert_eq!(dds.format, DdsFormat::Bc3);
        assert_eq!(dds.payload, &[0x11; 8]);
    }

    #[test]
    fn dx10_unrecognized_format_is_unknown() {
        let mut data = make_header(8, 8, 1, 0x4, b"DX10", 0);
        data.extend_from_slice(&9999u32.to_le_bytes());
        data.extend_from_slice(&[0u8; 16]); // rest of DXT10 header

        let dds = parse_dds(&data).unwrap();
        assert_eq!(dds.format, DdsFormat::Unknown(*b"DX10"));
    }

    #[test]
    fn dx10_header_truncated_does_not_panic() {
        // Claims DX10 but doesn't have enough bytes for the DXT10 extension header.
        let data = make_header(8, 8, 1, 0x4, b"DX10", 0);
        let dds = parse_dds(&data).unwrap();
        assert_eq!(dds.format, DdsFormat::Unknown(*b"DX10"));
        assert!(dds.payload.is_empty());
    }

    #[test]
    fn errors_on_too_short() {
        assert_eq!(parse_dds(&[]), Err(DdsError::TooShort));
        assert_eq!(parse_dds(b"DDS "), Err(DdsError::TooShort));
        assert_eq!(
            parse_dds(&make_header(1, 1, 1, 0, b"DXT1", 0)[..HEADER_END - 1]),
            Err(DdsError::TooShort)
        );
    }

    #[test]
    fn errors_on_bad_magic() {
        let mut data = make_header(1, 1, 1, 0, b"DXT1", 0);
        data[0] = b'X';
        assert_eq!(parse_dds(&data), Err(DdsError::BadMagic));
    }

    #[test]
    fn errors_on_bad_header_size() {
        let mut data = make_header(1, 1, 1, 0, b"DXT1", 0);
        data[4..8].copy_from_slice(&123u32.to_le_bytes());
        assert_eq!(parse_dds(&data), Err(DdsError::BadHeaderSize));
    }
}
