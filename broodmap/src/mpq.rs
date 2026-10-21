//! An implementation of the MPQ file format, for the features used by SCM/SCX files in StarCraft.
//! All features that are not used by the game are not implemented, and the implementation matches
//! the game's behavior as closely as possible (including bugs, which are often exploited by
//! so-called "map protectors" to prevent 3rd party editors from opening map files).

use crate::limits::{
    Resource, ResourceLimitError, ResourceLimits, allocation_error, ensure_within,
};
use bitflags::bitflags;
use nom::combinator::{map, verify};
use nom::{IResult, Parser};
use smallvec::SmallVec;
use std::borrow::Cow;
use std::io::Read;

use thiserror::Error;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MpqHeader {
    /// The offset the header was found at, relative to the start of the input data.
    pub offset: u32,
    /// Power of two exponent specifying the number of 512-byte disk sectors in each logical sector
    /// in the archive. The size of each logical sector in the archive is 512 * 2^block_size.
    pub sector_size_shift: u16,
    /// Offset to the beginning of the hash table, relative to the start of the MPQ header. NOTE:
    /// This may be behind you! The BW implementation handles negative offsets here.
    pub hash_table_pos: i32,
    /// Offset to the beginning of the block table, relative to the start of the MPQ header. NOTE:
    /// This may be behind you! The BW implementation handles negative offsets here.
    pub block_table_pos: i32,
    /// Number of entries in the hash table. Must be a power of 2, and must be less than 2^16.
    pub hash_table_size: u32,
    /// Number of entries in the block table.
    pub block_table_size: u32,
}

impl MpqHeader {
    /// Returns the absolute offset of the hash table, from the beginning of the file.
    pub fn hash_table_offset(&self) -> usize {
        assert!(self.hash_table_pos >= -(self.offset as i32));
        if self.hash_table_pos >= 0 {
            (self.offset as usize) + (self.hash_table_pos as usize)
        } else {
            (self.offset - self.hash_table_pos.unsigned_abs()) as usize
        }
    }

    /// Returns the absolute offset of the block table, from the beginning of the file.
    pub fn block_table_offset(&self) -> usize {
        assert!(self.block_table_pos >= -(self.offset as i32));
        if self.block_table_pos >= 0 {
            (self.offset as usize) + (self.block_table_pos as usize)
        } else {
            (self.offset - self.block_table_pos.unsigned_abs()) as usize
        }
    }

    /// Returns the size of each block of file data (each file is split into blocks of this size
    /// for storage).
    pub fn sector_size(&self) -> usize {
        512usize * 2usize.pow(self.sector_size_shift as u32)
    }
}

fn mpq_header_size(input: &[u8]) -> IResult<&[u8], u32> {
    use nom::number::streaming::le_u32;

    // NOTE(tec27): BW's implementation just cares that this is at least 32 bytes, it will never
    // read more than that. If this value is less than 32 bytes, it will continue searching for the
    // MPQ header later in the file.
    verify(le_u32, |size: &u32| *size >= 32).parse(input)
}

fn mpq_header_offset(input: &[u8]) -> IResult<&[u8], u32> {
    let mut offset = 0usize;
    loop {
        if input.len().saturating_sub(offset) < 8 {
            return Err(nom::Err::Incomplete(nom::Needed::Unknown));
        }

        let candidate = &input[offset..];
        if candidate.starts_with(b"MPQ\x1A") && mpq_header_size(&candidate[4..]).is_ok() {
            let offset = u32::try_from(offset).map_err(|_| {
                nom::Err::Failure(nom::error::Error::new(
                    candidate,
                    nom::error::ErrorKind::TooLarge,
                ))
            })?;
            return Ok((&candidate[8..], offset));
        }

        if input.len().saturating_sub(offset) < 512 {
            return Err(nom::Err::Incomplete(nom::Needed::Unknown));
        }
        offset += 512;
    }
}

fn mpq_table_pos(min_pos: i32) -> impl Fn(&[u8]) -> IResult<&[u8], i32> {
    use nom::number::streaming::le_i32;

    // This checks to ensure that the table is placed at the start of the file or after (but can't
    // validate that it is before the end of the file, since we may not know what that is yet)
    move |input| verify(le_i32, |pos: &i32| *pos >= min_pos).parse(input)
}

/// Find and parse an [MpqHeader] from the given input. This is a streaming parser, and may return
/// Incomplete if the full header cannot be found.
fn mpq_header(input: &[u8]) -> IResult<&[u8], MpqHeader> {
    use nom::bytes::streaming::take;
    use nom::number::streaming::{le_u16, le_u32};

    let (input, offset) = mpq_header_offset(input)?;
    // NOTE(tec27): BW's code ignores format version and archive size entirely
    let (input, (_archive_size, _format_version)) = (take(4usize), take(2usize)).parse(input)?;

    map(
        (
            le_u16,
            mpq_table_pos(-(offset as i32)),
            mpq_table_pos(-(offset as i32)),
            le_u32,
            le_u32,
        ),
        move |(sector_size, hash_table_pos, block_table_pos, hash_table_size, block_table_size)| {
            let mut sector_size = sector_size & 0xFF;
            // Certain maps (see smallest.scm) set very high sector sizes that overflow the bounds
            // of the u32 it's stored in. This is undefined behavior in C++ so I'm uncertain
            // whether these maps work everywhere anyway (they seem to be broken for SC:R, but that
            // may be an unrelated problem), but we set the value back down to try and make them
            // work
            if sector_size > 22 {
                sector_size = 3
            }

            MpqHeader {
                offset,
                sector_size_shift: sector_size,
                hash_table_pos,
                block_table_pos,
                // Discard the high 4 bits of hash_table_size, as C code multiplying that u32 by
                // hash table entry size (0x10) will cause them to overflow out of the u32 range,
                // so we reproduce that bug for compatibility
                hash_table_size: hash_table_size & 0x0FFFFFFF,
                block_table_size,
            }
        },
    )
    .parse(input)
}

const fn generate_crypt_table() -> [u32; 1280] {
    let mut result = [0u32; 1280];
    let mut seed: u32 = 0x0010_0001;

    let mut index_a = 0;
    while index_a < 256 {
        let mut index_b = index_a;
        let mut i = 0;
        while i < 5 {
            seed = (seed * 125 + 3) % 0x002A_AAAB;
            let temp_a = (seed & 0xFFFF) << 16;
            seed = (seed * 125 + 3) % 0x002A_AAAB;
            let temp_b = seed & 0xFFFF;

            result[index_b] = temp_a | temp_b;

            i += 1;
            index_b += 256;
        }

        index_a += 1;
    }

    result
}

const CRYPT_TABLE: [u32; 1280] = generate_crypt_table();

struct Decrypter {
    seed: u32,
    key: u32,
}

impl Decrypter {
    /// Creates a new [Decrypter] with a key calculated from the given string.
    fn from_str(key: &str) -> Self {
        Self {
            seed: 0xEEEE_EEEE,
            key: hash_str(key, MpqHashType::FileKey),
        }
    }

    /// Creates a new [Decrypter] from a precalculated encryption key.
    fn from_key_value(key: u32) -> Self {
        Self {
            seed: 0xEEEE_EEEE,
            key,
        }
    }

    fn decrypt_u32(&mut self, value: u32) -> u32 {
        self.seed = self
            .seed
            .wrapping_add(CRYPT_TABLE[(0x400 + (self.key & 0xFF)) as usize]);
        let value = value ^ (self.seed.wrapping_add(self.key));
        self.key = (!self.key << 0x15).wrapping_add(0x1111_1111) | (self.key >> 0x0B);
        self.seed = self
            .seed
            .wrapping_add(self.seed << 5)
            .wrapping_add(value)
            .wrapping_add(3);

        value
    }

    fn decrypt_bytes(&mut self, data: &[u8]) -> Vec<u8> {
        // We always decrypt by u32s, any extra bytes are output without any extra decoding
        let (data, remainder): (&[u8], &[u8]) = if data.len().is_multiple_of(4) {
            (data, &[])
        } else {
            let extra = data.len() % 4;
            (&data[0..data.len() - extra], &data[data.len() - extra..])
        };

        data.as_chunks::<4>()
            .0
            .iter()
            .flat_map(|val| u32::to_le_bytes(self.decrypt_u32(u32::from_le_bytes(*val))))
            .chain(remainder.iter().copied())
            .collect()
    }
}

enum MpqHashType {
    TableOffset,
    NameA,
    NameB,
    FileKey,
}

impl MpqHashType {
    fn to_crypt_table_multiplier(&self) -> usize {
        match self {
            MpqHashType::TableOffset => 0,
            MpqHashType::NameA => 1,
            MpqHashType::NameB => 2,
            MpqHashType::FileKey => 3,
        }
    }
}

fn hash_str(str: &str, hash_type: MpqHashType) -> u32 {
    let multiplier = hash_type.to_crypt_table_multiplier();

    let mut seed_a: u32 = 0x7FED_7FED;
    let mut seed_b: u32 = 0xEEEE_EEEE;

    for c in str.chars() {
        let c = c.to_ascii_uppercase();
        seed_a = CRYPT_TABLE[multiplier * 256 + (c as usize)] ^ (seed_a.wrapping_add(seed_b));
        seed_b = seed_b
            .wrapping_add(seed_b << 5)
            .wrapping_add(c as u32)
            .wrapping_add(seed_a)
            .wrapping_add(3)
    }

    seed_a
}

/// Indicates that the hash table entry is empty and always has been, terminates the search for a
/// file.
const BLOCK_INDEX_EMPTY: u32 = 0xFFFF_FFFF;
/// Indicates that the hash table entry is empty but is due to a deleted file, does not terminate
/// the search for a file.
const BLOCK_INDEX_DELETED: u32 = 0xFFFF_FFFE;

/// The size of each MPQ hash table entry in the file, in bytes.
const MPQ_HASH_TABLE_ENTRY_SIZE: usize = 16;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MpqHashTableEntry {
    /// The hash of the full file name with constant A
    pub hash_a: u32,
    /// The hash of the full file name with constant B
    pub hash_b: u32,
    /// The language of the file (uses Windows LANGID values). 0 indicates the default language
    /// (American English), and is used if there are not language-specific versions for the current
    /// locale.
    pub locale: u16,
    /// The platform the file is used for. 0 indicates the default platform. No other values are
    /// current known.
    pub platform: u16,
    /// The index of this entry's corresponding [MpqBlockTableEntry] in the block table.
    pub block_index: u32,
}

/// Parse the contents of an MPQ hash table. This is a complete parser, it expects that all data for
/// the hash table is present, and that the input begins at the start of the table and ends at the
/// end.
///
/// Note that BW's parser allows the hash table to be shorter than expected at the end of the file
/// (so this does as well).
fn mpq_hash_table(input: &[u8], max_entries: usize) -> Result<Vec<MpqHashTableEntry>, MpqError> {
    let entry_count = input.len() / MPQ_HASH_TABLE_ENTRY_SIZE;
    ensure_within(Resource::MpqHashTableEntries, entry_count, max_entries)?;
    let mut decrypter = Decrypter::from_str("(hash table)");
    let mut result = Vec::new();
    result
        .try_reserve_exact(entry_count)
        .map_err(|_| allocation_error(Resource::MpqHashTableEntries, entry_count))?;
    for entry in input.as_chunks::<MPQ_HASH_TABLE_ENTRY_SIZE>().0 {
        let hash_a = decrypter.decrypt_u32(u32::from_le_bytes(entry[0..4].try_into().unwrap()));
        let hash_b = decrypter.decrypt_u32(u32::from_le_bytes(entry[4..8].try_into().unwrap()));
        let locale_platform =
            decrypter.decrypt_u32(u32::from_le_bytes(entry[8..12].try_into().unwrap()));
        let block_index =
            decrypter.decrypt_u32(u32::from_le_bytes(entry[12..16].try_into().unwrap()));

        let block_index = if block_index != BLOCK_INDEX_EMPTY && block_index != BLOCK_INDEX_DELETED
        {
            // Mask out high bits that will overflow out when doing uint32 multiplication by 0x10,
            // which protections abuse to mess up block table length checks outside of BW
            block_index & 0x0FFF_FFFF
        } else {
            block_index
        };

        result.push(MpqHashTableEntry {
            hash_a,
            hash_b,
            locale: (locale_platform & 0x0000_FFFF) as u16,
            platform: (locale_platform >> 16) as u16,
            block_index,
        });
    }

    Ok(result)
}

/// The size of each MPQ hash table entry in the file, in bytes.
const MPQ_BLOCK_TABLE_ENTRY_SIZE: usize = 16;

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct MpqBlockFlags: u32 {
        const IMPLODED = 0x0000_0100;
        const COMPRESSED = 0x0000_0200;
        const ENCRYPTED = 0x0001_0000;
        const ADJUSTED_KEY = 0x0002_0000;
        const DELETED = 0x0200_0000;
        const EXISTS = 0x8000_0000;
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MpqBlockTableEntry {
    /// Offset to the beginning of the file data, relative to the start of the MPQ header. NOTE:
    /// This may be behind you! The BW implementation handles negative offsets here.
    pub offset: i32,
    /// The size of the compressed file data
    pub compressed_size: u32,
    /// The size of the uncompressed file data
    pub size: u32,
    /// Information about how the file is stored
    pub flags: MpqBlockFlags,
}

impl MpqBlockTableEntry {
    /// Returns the offset of a block's file data, from the beginning of the file. If the block's
    /// offset is not in the file, [None] will be returned. Note that this cannot bounds check the
    /// end of the file, only the beginning.
    pub fn absolute_offset(&self, mpq_header: &MpqHeader) -> Option<usize> {
        if self.offset < -(mpq_header.offset as i32) {
            None
        } else if self.offset >= 0 {
            (self.offset as usize).checked_add(mpq_header.offset as usize)
        } else {
            Some((mpq_header.offset - self.offset.unsigned_abs()) as usize)
        }
    }

    /// Returns the encryption key to be used for a given block ([None] if the block is not
    /// encrypted).
    fn encryption_key(&self, path: &str) -> Option<u32> {
        if !self
            .flags
            .intersects(MpqBlockFlags::ADJUSTED_KEY | MpqBlockFlags::ENCRYPTED)
        {
            return None;
        }

        let (_, filename) = path.rsplit_once('\\').unwrap_or(("", path));
        let key = hash_str(filename, MpqHashType::FileKey);
        if self.flags.contains(MpqBlockFlags::ADJUSTED_KEY) {
            Some(key.wrapping_add(self.offset as u32) ^ self.size)
        } else {
            Some(key)
        }
    }
}

/// Parse the contents of an MPQ block table. This is a complete parser, it expects that all data
/// for the block table is present, and that the input begins at the start of the table and ends at
/// the end.
///
/// Note that BW's parser allows the block table to be shorter than expected at the end of the file
/// (so this does as well).
fn mpq_block_table(input: &[u8], max_entries: usize) -> Result<Vec<MpqBlockTableEntry>, MpqError> {
    let entry_count = input.len() / MPQ_BLOCK_TABLE_ENTRY_SIZE;
    ensure_within(Resource::MpqBlockTableEntries, entry_count, max_entries)?;
    let mut decrypter = Decrypter::from_str("(block table)");
    let mut result = Vec::new();
    result
        .try_reserve_exact(entry_count)
        .map_err(|_| allocation_error(Resource::MpqBlockTableEntries, entry_count))?;
    for entry in input.as_chunks::<MPQ_BLOCK_TABLE_ENTRY_SIZE>().0 {
        result.push(MpqBlockTableEntry {
            offset: decrypter.decrypt_u32(u32::from_le_bytes(entry[0..4].try_into().unwrap()))
                as i32,
            compressed_size: decrypter
                .decrypt_u32(u32::from_le_bytes(entry[4..8].try_into().unwrap())),
            size: decrypter.decrypt_u32(u32::from_le_bytes(entry[8..12].try_into().unwrap())),
            flags: MpqBlockFlags::from_bits_truncate(
                decrypter.decrypt_u32(u32::from_le_bytes(entry[12..16].try_into().unwrap())),
            ),
        });
    }

    Ok(result)
}

/// Parse the contents of an MPQ sector table for a particular file, decrypting it if needed.
fn mpq_sector_table(
    input: &[u8],
    num_sectors: usize,
    encryption_key: Option<u32>,
) -> Result<Vec<i32>, MpqError> {
    let byte_len = num_sectors
        .checked_mul(4)
        .ok_or(MpqError::MalformedSectorTable)?;
    if input.len() < byte_len {
        return Err(MpqError::MalformedSectorTable);
    }
    let mut decrypter = encryption_key.map(Decrypter::from_key_value);
    let mut result = Vec::new();
    result
        .try_reserve_exact(num_sectors)
        .map_err(|_| allocation_error(Resource::MpqFileSectors, num_sectors))?;
    for bytes in input[..byte_len].as_chunks::<4>().0 {
        let value = u32::from_le_bytes(*bytes);
        result.push(match decrypter {
            Some(ref mut decrypter) => decrypter.decrypt_u32(value) as i32,
            None => value as i32,
        });
    }
    Ok(result)
}

// TODO(tec27): Support more compression algorithms, this is not a complete list of what BW supports
// NOTE: Huffman (0x01), ADPCM (0x40/0x80) and sparse (0x20) are also defined by the MPQ format, but
// are only used for sound or non-SC archives, so they are rejected as unsupported. Newer MPQ
// versions also treat the exact mask 0x12 as LZMA, but SC:R's Storm interprets each bit on its own,
// so 0x12 is decompressed as bzip2 followed by zlib.
const COMPRESSION_ZLIB: u8 = 0x02;
const COMPRESSION_IMPLODE: u8 = 0x08;
const COMPRESSION_BZIP2: u8 = 0x10;

/// The compression types a sector's mask can combine, in the order they are decompressed (the
/// reverse of the order they were applied in when compressing), matching SC:R's Storm.
const DECOMPRESSION_ORDER: [u8; 3] = [COMPRESSION_IMPLODE, COMPRESSION_BZIP2, COMPRESSION_ZLIB];

/// Size of the scratch buffer decompressors write into before output is bounds-checked.
const DECOMPRESSION_CHUNK_SIZE: usize = 4096;

/// Decompresses a file sector using the algorithms it specifies. Sectors specify a mask of the
/// algorithms that were applied to them with the first byte of their data.
fn decompress_sector_into(
    data: &[u8],
    max_output: usize,
    max_append: usize,
    max_expansion: usize,
    output: &mut Vec<u8>,
) -> Result<usize, MpqError> {
    if data.len() <= 1 {
        return Err(MpqError::MalformedSectorTable);
    }

    let mask = data[0];
    let supported = DECOMPRESSION_ORDER.iter().fold(0, |acc, c| acc | c);
    if mask & !supported != 0 {
        return Err(MpqError::UnsupportedCompressionType(mask));
    }

    let stages: SmallVec<[u8; 3]> = DECOMPRESSION_ORDER
        .iter()
        .copied()
        .filter(|&c| mask & c != 0)
        .collect();
    let Some((&last, intermediate)) = stages.split_last() else {
        return Err(MpqError::UnsupportedCompressionType(mask));
    };

    let mut input = Cow::from(&data[1..]);
    for &compression in intermediate {
        // Each intermediate result is bounded by the same per-sector ceiling as the final output
        let mut next = Vec::new();
        decompress_with(
            compression,
            input.as_ref(),
            &mut SectorOutput::new(max_output, max_output, max_expansion, &mut next),
        )?;
        input = Cow::from(next);
    }

    let mut sink = SectorOutput::new(max_output, max_append, max_expansion, output);
    decompress_with(last, input.as_ref(), &mut sink)?;
    Ok(sink.decompressed_len)
}

fn decompress_with(
    compression: u8,
    data: &[u8],
    sink: &mut SectorOutput<'_>,
) -> Result<(), MpqError> {
    match compression {
        COMPRESSION_BZIP2 => bunzip_to(data, sink),
        COMPRESSION_IMPLODE => explode_to(data, sink),
        COMPRESSION_ZLIB => inflate_to(data, sink),
        c => Err(MpqError::UnsupportedCompressionType(c)),
    }
}

/// Receives the output of a decompressor, enforcing the per-sector output ceiling as data is
/// produced and appending at most `max_append` bytes of it to `output`.
struct SectorOutput<'a> {
    max_output: usize,
    max_append: usize,
    max_expansion: usize,
    decompressed_len: usize,
    appended_len: usize,
    output: &'a mut Vec<u8>,
}

impl<'a> SectorOutput<'a> {
    fn new(
        max_output: usize,
        max_append: usize,
        max_expansion: usize,
        output: &'a mut Vec<u8>,
    ) -> Self {
        Self {
            max_output,
            max_append,
            max_expansion,
            decompressed_len: 0,
            appended_len: 0,
            output,
        }
    }

    fn write(&mut self, data: &[u8]) -> Result<(), MpqError> {
        self.decompressed_len =
            self.decompressed_len
                .checked_add(data.len())
                .ok_or(ResourceLimitError::Exceeded {
                    resource: Resource::MpqSectorOutput,
                    observed: usize::MAX,
                    limit: self.max_output,
                })?;
        ensure_within(
            Resource::MpqSectorOutput,
            self.decompressed_len,
            self.max_output,
        )?;
        ensure_within(
            Resource::MpqSectorExpansion,
            self.decompressed_len,
            self.max_expansion,
        )?;

        let append_len = data
            .len()
            .min(self.max_append.saturating_sub(self.appended_len));
        self.output
            .try_reserve(append_len)
            .map_err(|_| allocation_error(Resource::MpqFileBytes, append_len))?;
        self.output.extend_from_slice(&data[..append_len]);
        self.appended_len += append_len;
        Ok(())
    }
}

fn explode_into(
    data: &[u8],
    max_output: usize,
    max_append: usize,
    max_expansion: usize,
    output: &mut Vec<u8>,
) -> Result<usize, MpqError> {
    let mut sink = SectorOutput::new(max_output, max_append, max_expansion, output);
    explode_to(data, &mut sink)?;
    Ok(sink.decompressed_len)
}

fn explode_to(data: &[u8], sink: &mut SectorOutput<'_>) -> Result<(), MpqError> {
    let mut reader = explode::ExplodeReader::new(data);
    let mut buffer = [0u8; DECOMPRESSION_CHUNK_SIZE];

    loop {
        let read = reader.read(&mut buffer).map_err(map_explode_reader_error)?;
        if read == 0 {
            return Ok(());
        }
        sink.write(&buffer[..read])?;
    }
}

/// Decompresses zlib-wrapped deflate data (including verifying its checksum).
fn inflate_to(data: &[u8], sink: &mut SectorOutput<'_>) -> Result<(), MpqError> {
    use miniz_oxide::inflate::stream::{InflateState, inflate};
    use miniz_oxide::{DataFormat, MZError, MZFlush, MZStatus};

    let mut state = InflateState::new_boxed(DataFormat::Zlib);
    let mut input = data;
    let mut buffer = [0u8; DECOMPRESSION_CHUNK_SIZE];

    loop {
        let result = inflate(&mut state, input, &mut buffer, MZFlush::None);
        input = &input[result.bytes_consumed.min(input.len())..];
        sink.write(&buffer[..result.bytes_written])?;

        match result.status {
            Ok(MZStatus::StreamEnd) => return Ok(()),
            Ok(_) if result.bytes_consumed == 0 && result.bytes_written == 0 => {
                return Err(MpqError::TruncatedCompressedData);
            }
            Ok(_) => {}
            // In streaming mode, this means the input ran out before the end of the stream
            Err(MZError::Buf) => return Err(MpqError::TruncatedCompressedData),
            Err(e) => return Err(MpqError::ZlibError(e)),
        }
    }
}

fn bunzip_to(data: &[u8], sink: &mut SectorOutput<'_>) -> Result<(), MpqError> {
    let mut decompress = bzip2::Decompress::new(false);
    let mut input = data;
    let mut buffer = [0u8; DECOMPRESSION_CHUNK_SIZE];

    loop {
        let (in_before, out_before) = (decompress.total_in(), decompress.total_out());
        let status = decompress
            .decompress(input, &mut buffer)
            .map_err(MpqError::Bzip2Error)?;
        // Both deltas are bounded by the lengths of the slices just passed in
        let consumed = (decompress.total_in() - in_before) as usize;
        let written = (decompress.total_out() - out_before) as usize;
        input = &input[consumed.min(input.len())..];
        sink.write(&buffer[..written.min(buffer.len())])?;

        match status {
            bzip2::Status::StreamEnd => return Ok(()),
            bzip2::Status::MemNeeded => return Err(MpqError::Bzip2OutOfMemory),
            _ if consumed == 0 && written == 0 => return Err(MpqError::TruncatedCompressedData),
            _ => {}
        }
    }
}

#[cfg(test)]
fn explode_data(data: &[u8], max_output: usize) -> Result<Vec<u8>, MpqError> {
    let mut result = Vec::new();
    explode_into(
        data,
        max_output,
        max_output,
        ResourceLimits::default().mpq_sector_expansion_limit(data.len()),
        &mut result,
    )?;
    Ok(result)
}

#[cfg(test)]
fn decompress_sector(data: &[u8], max_output: usize) -> Result<Vec<u8>, MpqError> {
    let mut result = Vec::new();
    decompress_sector_into(
        data,
        max_output,
        max_output,
        ResourceLimits::default().mpq_sector_expansion_limit(data.len()),
        &mut result,
    )?;
    Ok(result)
}

fn map_explode_reader_error(error: std::io::Error) -> MpqError {
    if error
        .get_ref()
        .is_some_and(|inner| inner.is::<explode::Error>())
    {
        let kind = error.kind();
        match error
            .into_inner()
            .and_then(|inner| inner.downcast::<explode::Error>().ok())
        {
            Some(error) => MpqError::ExplodeError(*error),
            None => MpqError::ExplodeIoError(std::io::Error::new(
                kind,
                "explode reader error changed type",
            )),
        }
    } else {
        MpqError::ExplodeIoError(error)
    }
}

#[derive(Error, Debug)]
pub enum MpqError {
    #[error("header not found")]
    HeaderNotFound,
    #[error("file not found")]
    FileNotFound,
    #[error("MPQ paths cannot contain characters outside the single-byte range")]
    InvalidPath,
    #[error("malformed header")]
    MalformedHeader,
    #[error("file had malformed sector table")]
    MalformedSectorTable,
    #[error("unsupported compression type: {0:x}")]
    UnsupportedCompressionType(u8),
    #[error("problem decompressing with explode: {0}")]
    ExplodeError(explode::Error),
    #[error("I/O error while decompressing with explode: {0}")]
    ExplodeIoError(std::io::Error),
    #[error("problem decompressing with zlib: {0:?}")]
    ZlibError(miniz_oxide::MZError),
    #[error("problem decompressing with bzip2: {0}")]
    Bzip2Error(bzip2::Error),
    #[error("bzip2 decompressor could not allocate its working memory")]
    Bzip2OutOfMemory,
    #[error("compressed sector data ended before the end of its stream")]
    TruncatedCompressedData,
    #[error(transparent)]
    ResourceLimit(#[from] ResourceLimitError),
    #[error("parser error")]
    ParserError(nom::error::ErrorKind),
}

// TODO(tec27): Write a version of this that works with BufReader or similar
#[derive(Debug)]
pub struct Mpq<'a> {
    /// The file data of this MPQ.
    data: &'a [u8],
    /// The header of the MPQ, containing metadata about its structure.
    header: MpqHeader,
    /// A hash table containing information about the files present in this MPQ archive, allowing
    /// a file's position in the [block_table] to be located.
    hash_table: Vec<MpqHashTableEntry>,
    /// A table for file metadata, containing 1 entry for each file present in this MPQ archive.
    block_table: Vec<MpqBlockTableEntry>,
    limits: ResourceLimits,
}

impl<'a> Mpq<'a> {
    /// Initializes an [Mpq] from the given data in-memory, which must be the entire MPQ file. This
    /// method will eagerly parse the MPQ metadata, which will be used to fulfill later requests for
    /// specific files.
    pub fn from_bytes(data: &'a [u8]) -> Result<Self, MpqError> {
        Self::from_bytes_with_limits(data, &ResourceLimits::default())
    }

    /// Initializes an MPQ from in-memory data using explicit resource limits.
    pub fn from_bytes_with_limits(
        data: &'a [u8],
        limits: &ResourceLimits,
    ) -> Result<Self, MpqError> {
        ensure_within(Resource::MapBytes, data.len(), limits.max_map_bytes)?;
        let header = match mpq_header(data) {
            Ok((_, header)) => Ok(header),
            Err(nom::Err::Error(e)) => Err(MpqError::ParserError(e.code)),
            Err(nom::Err::Failure(e)) => Err(MpqError::ParserError(e.code)),
            // This is a Not Found, since we have all the data already
            Err(nom::Err::Incomplete(_)) => Err(MpqError::HeaderNotFound),
        }?;

        let mut hash_table = {
            let hash_table_offset = header.hash_table_offset();
            if hash_table_offset >= data.len() {
                return Err(MpqError::MalformedHeader);
            }
            let hash_table_size = (header.hash_table_size as usize)
                .saturating_mul(MPQ_HASH_TABLE_ENTRY_SIZE)
                .min(data.len() - hash_table_offset);
            ensure_within(
                Resource::MpqHashTableEntries,
                hash_table_size / MPQ_HASH_TABLE_ENTRY_SIZE,
                limits.max_mpq_table_entries,
            )?;
            let hash_data = &data[hash_table_offset..hash_table_offset + hash_table_size];
            mpq_hash_table(hash_data, limits.max_mpq_table_entries)
        }?;

        let mut block_table = {
            let block_table_offset = header.block_table_offset();
            if block_table_offset >= data.len() {
                return Err(MpqError::MalformedHeader);
            }
            let block_table_size = (header.block_table_size as usize)
                .saturating_mul(MPQ_BLOCK_TABLE_ENTRY_SIZE)
                .min(data.len() - block_table_offset);
            ensure_within(
                Resource::MpqBlockTableEntries,
                block_table_size / MPQ_BLOCK_TABLE_ENTRY_SIZE,
                limits.max_mpq_table_entries,
            )?;
            let block_data = &data[block_table_offset..block_table_offset + block_table_size];
            mpq_block_table(block_data, limits.max_mpq_table_entries)
        }?;

        // Shrink the hash and block table to remove invalid entries at the end. This saves some
        // memory/time with certain map protectors, which create tables that are "the whole file"
        // but really just a normal number of slots + a bunch of junk entries (would be nicer to
        // save this memory *before* we allocate it, but what are you gonna do? :) )
        // TODO(tec27): I guess one thing we could do is parse the tables in reverse
        let mut last_valid = block_table.len();
        let data_len = data.len();
        for (i, entry) in block_table.iter().enumerate().rev() {
            // This is unfortunately not a perfect check, and these protections sometimes set the
            // right bit + also randomly end up with an offset inside the file, but it will work
            // most of the time
            if entry.flags.contains(MpqBlockFlags::EXISTS)
                && entry.absolute_offset(&header).unwrap_or(data_len) < data_len
            {
                // Once we find a valid entry, that's as far as we can shrink the table
                last_valid = i;
                break;
            }
        }
        let shrink_size = last_valid + 1;
        if shrink_size < block_table.len() {
            block_table.truncate(shrink_size);
            block_table.shrink_to_fit();
        }

        let mut last_valid = hash_table.len();
        for (i, entry) in hash_table.iter().enumerate().rev() {
            if (entry.block_index as usize) < block_table.len() {
                // Once we find a valid entry, that's as far as we can shrink the table
                last_valid = i;
                break;
            }
        }
        let shrink_size = last_valid + 1;
        if shrink_size < hash_table.len() {
            hash_table.truncate(shrink_size);
            hash_table.shrink_to_fit();
        }

        Ok(Self {
            data,
            header,
            hash_table,
            block_table,
            limits: *limits,
        })
    }

    /// Returns the complete archive bytes.
    pub fn data(&self) -> &'a [u8] {
        self.data
    }

    /// Returns the validated archive header.
    pub fn header(&self) -> &MpqHeader {
        &self.header
    }

    /// Returns the parsed hash table.
    pub fn hash_table(&self) -> &[MpqHashTableEntry] {
        &self.hash_table
    }

    /// Returns the parsed block table.
    pub fn block_table(&self) -> &[MpqBlockTableEntry] {
        &self.block_table
    }

    /// Returns the resource policy retained by this archive for lazy file extraction.
    pub fn limits(&self) -> ResourceLimits {
        self.limits
    }

    pub fn read_file(&self, path: &str, locale: Option<u16>) -> Result<Vec<u8>, MpqError> {
        if path
            .chars()
            .any(|character| u32::from(character) > u8::MAX.into())
        {
            return Err(MpqError::InvalidPath);
        }
        let best = self
            .find_hash_table_entry(path, locale)
            .ok_or(MpqError::FileNotFound)?;

        if best.block_index as usize >= self.block_table.len() {
            return Err(MpqError::FileNotFound);
        }
        let block = &self.block_table[best.block_index as usize];
        if block.flags.contains(MpqBlockFlags::DELETED)
            || !block.flags.contains(MpqBlockFlags::EXISTS)
        {
            return Err(MpqError::FileNotFound);
        }
        ensure_within(
            Resource::MpqFileBytes,
            block.size as usize,
            self.limits.max_mpq_file_bytes,
        )?;

        let offset = block
            .absolute_offset(&self.header)
            .ok_or(MpqError::FileNotFound)?;
        if offset >= self.data.len() {
            return Err(MpqError::FileNotFound);
        }

        let encryption_key = block.encryption_key(path);
        let sector_table = self.read_sector_offset_table(block, encryption_key)?;
        let sector_size = self.header.sector_size();

        let mut bytes_left = block.size as usize;
        let mut result = Vec::new();
        // Grow only after checking actual output. Reserving the attacker-declared file size
        // here would allocate up to the file ceiling before the first ratio check.

        // NOTE(tec27): BW is okay with file data being truncated by the end of the file, so we
        // reproduce that handling
        for (i, w) in sector_table.windows(2).enumerate() {
            if bytes_left == 0 {
                break;
            }

            let (sector_offset, next_sector_offset) = (w[0], w[1]);
            // The sector table was validated as non-decreasing, but the difference between two
            // entries can still exceed i32::MAX, so it has to be computed as an i64
            let cur_sector_size = (next_sector_offset as i64 - sector_offset as i64) as usize;
            ensure_within(
                Resource::MpqFileBytes,
                cur_sector_size,
                self.limits.max_mpq_file_bytes,
            )?;
            // Convert to an absolute offset in the data
            let start = if sector_offset >= 0 {
                offset.saturating_add(sector_offset as usize)
            } else if sector_offset >= -(offset as i32) {
                offset - sector_offset.unsigned_abs() as usize
            } else {
                // Sector was before the start of the file, but BW's implementation seems to be okay
                // with this happening, so just return what we have
                return Err(MpqError::MalformedSectorTable);
            };
            // Sectors can also claim to start past the end of the file; BW tolerates truncated
            // file data, so treat everything past the end as empty
            let start = start.min(self.data.len());

            let use_compression = cur_sector_size < sector_size && cur_sector_size < bytes_left;
            let sector_compressed =
                block.flags.contains(MpqBlockFlags::COMPRESSED) && use_compression;
            let sector_imploded = block.flags.contains(MpqBlockFlags::IMPLODED) && use_compression;

            let end = start.saturating_add(cur_sector_size).min(self.data.len());
            let mut sector = Cow::from(&self.data[start..end]);
            // Use actual input bytes, not offsets that may extend beyond EOF. Reuse this budget
            // for every codec stage and any zero-padding; intermediate ratios must not compound.
            let max_expansion = self.limits.mpq_sector_expansion_limit(sector.len());
            // MPQ compressors normally expand every sector to the archive's full sector size,
            // including the final sector; the final output is truncated to the file's declared
            // length below. Keep that compatibility while still applying the file-wide ceiling.
            let expected_output = sector_size.min(self.limits.max_mpq_file_bytes);

            if let Some(key) = encryption_key {
                let mut d = Decrypter::from_key_value(key.wrapping_add(i as u32));
                sector = Cow::from(d.decrypt_bytes(sector.as_ref()));
            }
            let sector_output_len = if sector_compressed {
                decompress_sector_into(
                    sector.as_ref(),
                    expected_output,
                    bytes_left,
                    max_expansion,
                    &mut result,
                )?
            } else if sector_imploded {
                explode_into(
                    sector.as_ref(),
                    expected_output,
                    bytes_left,
                    max_expansion,
                    &mut result,
                )?
            } else {
                // Some protectors add extra data past the file's stated length. Truncate it while
                // appending to the final buffer.
                let slice_len = sector.len().min(bytes_left);
                result
                    .try_reserve(slice_len)
                    .map_err(|_| allocation_error(Resource::MpqFileBytes, slice_len))?;
                result.extend_from_slice(&sector[..slice_len]);
                sector.len()
            };

            // BW expects that every decompression will result in sectorSize bytes of data (except,
            // possibly, for the very last sector). This is never verified, however, which means map
            // protection schemes can compress less data. When reading it back out, BW will always
            // give sectorSize bytes anyway, so we need to pad the buffer in those cases.
            let is_last_sector = i == sector_table.len() - 2;
            if !is_last_sector && sector_output_len < sector_size {
                ensure_within(Resource::MpqSectorExpansion, sector_size, max_expansion)?;
                let padding = sector_size - sector_output_len;
                let new_len =
                    result
                        .len()
                        .checked_add(padding)
                        .ok_or(ResourceLimitError::Exceeded {
                            resource: Resource::MpqFileBytes,
                            observed: usize::MAX,
                            limit: self.limits.max_mpq_file_bytes,
                        })?;
                ensure_within(
                    Resource::MpqFileBytes,
                    new_len,
                    self.limits.max_mpq_file_bytes,
                )?;
                result
                    .try_reserve(padding)
                    .map_err(|_| allocation_error(Resource::MpqFileBytes, padding))?;
                result.resize(new_len, 0);
                bytes_left -= sector_size.min(bytes_left);
            } else {
                bytes_left -= sector_output_len.min(bytes_left);
            }
        }

        Ok(result)
    }

    fn find_hash_table_entry(&self, path: &str, locale: Option<u16>) -> Option<&MpqHashTableEntry> {
        // Searching for locale 0 is equivalent to not searching for a specific locale
        let locale = match locale {
            Some(0) => None,
            l => l,
        };

        // A malformed/truncated file can produce an empty hash table, in which case no file can
        // ever be found (this also means a non-empty table has hash_table_size >= 1, keeping the
        // mask below safe from underflow)
        if self.hash_table.is_empty() {
            return None;
        }

        let mut table_offset = hash_str(path, MpqHashType::TableOffset) as usize;
        table_offset &= self.header.hash_table_size as usize - 1;
        // NOTE(tec27): Since hash tables can be truncated from the size the header states, we have
        // to check that we're within the bounds. Entries outside the existing table are treated
        // as BLOCK_INDEX_DELETED
        if table_offset < self.hash_table.len()
            && self.hash_table[table_offset].block_index == BLOCK_INDEX_EMPTY
        {
            return None;
        }

        if table_offset >= self.hash_table.len() {
            table_offset = 0
        }

        let hash_a = hash_str(path, MpqHashType::NameA);
        let hash_b = hash_str(path, MpqHashType::NameB);

        let mut best = None;
        let mut i = table_offset;
        loop {
            let cur = &self.hash_table[i];
            if cur.block_index == BLOCK_INDEX_EMPTY {
                // Once we encounter an empty block, the search is over
                break;
            } else if cur.block_index != BLOCK_INDEX_DELETED
                && cur.hash_a == hash_a
                && cur.hash_b == hash_b
            {
                if let Some(locale) = locale {
                    if locale == cur.locale {
                        // If we're searching for a specific locale and this matches, we can return
                        // this directly
                        return Some(cur);
                    } else if cur.locale == 0 {
                        // We will fall back to the neutral locale version of a file (the last one
                        // seen) if we fail to find a file with the requested locale
                        best = Some(cur);
                    }
                } else {
                    // If we're searching for a file in locale 0, the game will return the last file
                    // matching that path (regardless of its locale), so we set the current one as
                    // the best option and continue
                    best = Some(cur);
                }
            }

            i = (i + 1) % self.hash_table.len();

            if i == table_offset {
                break;
            }
        }

        best
    }

    /// Returns a table of sizes to read for each sector. If the file is not compressed, these sizes
    /// will match the MPQ's sector size. Otherwise, these sizes will (generally) be less than the
    /// sector size.
    fn read_sector_offset_table(
        &self,
        block: &MpqBlockTableEntry,
        encryption_key: Option<u32>,
    ) -> Result<Vec<i32>, MpqError> {
        // Adjust the key to be able to decrypt the offset table
        let encryption_key = encryption_key.map(|k| k.wrapping_sub(1));

        let sector_size = self.header.sector_size();
        let num_sectors = (block.size as usize).div_ceil(sector_size);
        ensure_within(
            Resource::MpqFileSectors,
            num_sectors,
            self.limits.max_mpq_file_sectors,
        )?;
        // NOTE(tec27): BW's implementation doesn't support the "SINGLE_UNIT" flag, so there are
        // less reasons to not have a sector table
        let has_sector_table = block
            .flags
            .intersects(MpqBlockFlags::COMPRESSED | MpqBlockFlags::IMPLODED);

        // There is one extra entry in the table, used purely for measuring the size of the
        // last section. Section N's size is calculated as:
        // offset(section n+1) - offset(section n)
        let num_sectors = num_sectors
            .checked_add(1)
            .ok_or(MpqError::MalformedSectorTable)?;

        if has_sector_table {
            let absolute_offset = block
                .absolute_offset(&self.header)
                .unwrap_or(self.data.len());
            let table_bytes = num_sectors
                .checked_mul(4)
                .ok_or(MpqError::MalformedSectorTable)?;
            if absolute_offset
                .checked_add(table_bytes)
                .is_none_or(|end| end >= self.data.len())
            {
                // The sector table extends past the end of the file
                return Err(MpqError::MalformedSectorTable);
            }

            let sector_table =
                mpq_sector_table(&self.data[absolute_offset..], num_sectors, encryption_key)?;

            // Validate the table to ensure the sectors always move forward
            if sector_table.windows(2).any(|w| w[0] > w[1]) {
                Err(MpqError::MalformedSectorTable)
            } else {
                Ok(sector_table)
            }
        } else {
            Ok((0..num_sectors - 1)
                .map(|s| (s * sector_size) as i32)
                .chain(std::iter::once(block.size as i32))
                .collect())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_ok::assert_ok;
    use rstest::rstest;

    const CHK_PATH: &str = "staredit\\scenario.chk";

    const LT: &[u8] = include_bytes!("../assets/lt.scm");
    const LT_CHK: &[u8] = include_bytes!("../assets/lt.chk");
    const LT_OFFSET_FROM_START: &[u8] = include_bytes!("../assets/lt-offset-from-start.scm");

    const NEGATIVE_OFFSETS: &[u8] = include_bytes!("../assets/negativeoffsets.scx");
    const NEGATIVE_OFFSETS_CHK: &[u8] = include_bytes!("../assets/negativeoffsets.chk");

    const IMPLODED: &[u8] = include_bytes!("../assets/imploded.scm");
    const IMPLODED_CHK: &[u8] = include_bytes!("../assets/imploded.chk");

    const PROTECTED_0: &[u8] = include_bytes!("../assets/protected-0.scx");
    const PROTECTED_0_CHK: &[u8] = include_bytes!("../assets/protected-0.chk");

    const PROTECTED_1: &[u8] = include_bytes!("../assets/protected-1.scm");
    const PROTECTED_1_CHK: &[u8] = include_bytes!("../assets/protected-1.chk");

    const PROTECTED_2: &[u8] = include_bytes!("../assets/protected-2.scx");
    const PROTECTED_2_CHK: &[u8] = include_bytes!("../assets/protected-2.chk");

    const PROTECTED_3: &[u8] = include_bytes!("../assets/protected-3.scx");
    const PROTECTED_3_CHK: &[u8] = include_bytes!("../assets/protected-3.chk");

    const PROTECTED_4: &[u8] = include_bytes!("../assets/protected-4.scx");
    const PROTECTED_4_CHK: &[u8] = include_bytes!("../assets/protected-4.chk");

    const PROTECTED_5: &[u8] = include_bytes!("../assets/protected-5.scm");
    const PROTECTED_5_CHK: &[u8] = include_bytes!("../assets/protected-5.chk");

    const PROTECTED_6: &[u8] = include_bytes!("../assets/protected-6.scx");
    const PROTECTED_6_CHK: &[u8] = include_bytes!("../assets/protected-6.chk");

    const SMALLEST: &[u8] = include_bytes!("../assets/smallest.scm");
    const SMALLEST_CHK: &[u8] = include_bytes!("../assets/smallest.chk");

    const OCOC_BOUND_2: &[u8] = include_bytes!("../assets/OcOc_Bound_2(p).scx");
    const OCOC_BOUND_2_CHK: &[u8] = include_bytes!("../assets/OcOc_Bound_2(p).chk");

    const SNIPER_SEED: &[u8] = include_bytes!("../assets/Sniper_Seed_vA.scx");
    const SNIPER_SEED_CHK: &[u8] = include_bytes!("../assets/Sniper_Seed_vA.chk");

    #[test]
    fn header_normal() {
        let result = mpq_header(&LT[..32]);
        assert_eq!(
            result,
            Ok((
                &b""[..],
                MpqHeader {
                    offset: 0,
                    sector_size_shift: 3,
                    hash_table_pos: 69637,
                    block_table_pos: 86021,
                    hash_table_size: 1024,
                    block_table_size: 4,
                }
            ))
        );

        let (_, result) = result.unwrap();
        assert_eq!(result.hash_table_offset(), 69637);
        assert_eq!(result.block_table_offset(), 86021);
    }

    #[test]
    fn header_offset() {
        let result = mpq_header(&LT_OFFSET_FROM_START[..544]);
        assert_eq!(
            result,
            Ok((
                &b""[..],
                MpqHeader {
                    offset: 512,
                    sector_size_shift: 3,
                    hash_table_pos: 69637,
                    block_table_pos: 86021,
                    hash_table_size: 1024,
                    block_table_size: 4,
                }
            ))
        );

        let (_, result) = result.unwrap();
        assert_eq!(result.hash_table_offset(), 69637 + 512);
        assert_eq!(result.block_table_offset(), 86021 + 512);
    }

    #[test]
    fn header_negative_table_pos() {
        let result = mpq_header(&NEGATIVE_OFFSETS[..0x4220]);
        assert_eq!(
            result,
            Ok((
                &b""[..],
                MpqHeader {
                    offset: 0x4200,
                    sector_size_shift: 3,
                    hash_table_pos: -16672,
                    block_table_pos: -288,
                    hash_table_size: 1024,
                    block_table_size: 18,
                }
            ))
        );

        let (_, result) = result.unwrap();
        assert_eq!(result.hash_table_offset(), 0x4200 - 16672);
        assert_eq!(result.block_table_offset(), 0x4200 - 288);
    }

    #[test]
    fn hash_strings() {
        // Values from: http://www.zezula.net/en/mpq/techinfo.html#hashes
        assert_eq!(
            hash_str("arr\\units.dat", MpqHashType::TableOffset),
            0xF4E6_C69D
        );
        assert_eq!(
            hash_str("unit\\neutral\\acritter.grp", MpqHashType::TableOffset),
            0xA260_67F3
        );
    }

    #[test]
    fn hash_table() {
        let result = mpq_hash_table(
            &LT[69637..69637 + (1024 * 16)],
            ResourceLimits::trusted().max_mpq_table_entries,
        );

        assert!(result.is_ok());

        let result = result.unwrap();
        assert_eq!(result.len(), 1024);

        let nonempty = result
            .into_iter()
            .filter(|e| e.block_index != BLOCK_INDEX_EMPTY)
            .collect::<Vec<_>>();

        assert_eq!(
            nonempty,
            vec![
                MpqHashTableEntry {
                    hash_a: 4251285776,
                    hash_b: 1318820007,
                    locale: 0,
                    platform: 0,
                    block_index: 2
                },
                MpqHashTableEntry {
                    hash_a: 3070322030,
                    hash_b: 4244315885,
                    locale: 0,
                    platform: 0,
                    block_index: 1
                },
                MpqHashTableEntry {
                    hash_a: 3633698129,
                    hash_b: 1663536459,
                    locale: 0,
                    platform: 0,
                    block_index: BLOCK_INDEX_DELETED
                },
                MpqHashTableEntry {
                    hash_a: 2173454238,
                    hash_b: 3755855017,
                    locale: 0,
                    platform: 0,
                    block_index: BLOCK_INDEX_DELETED
                },
                MpqHashTableEntry {
                    hash_a: 3548657611,
                    hash_b: 132115180,
                    locale: 0,
                    platform: 0,
                    block_index: 3
                },
                MpqHashTableEntry {
                    hash_a: 846314731,
                    hash_b: 823494219,
                    locale: 0,
                    platform: 0,
                    block_index: 0
                },
                MpqHashTableEntry {
                    hash_a: 3479609644,
                    hash_b: 1703330056,
                    locale: 0,
                    platform: 0,
                    block_index: BLOCK_INDEX_DELETED
                },
                MpqHashTableEntry {
                    hash_a: 1503620431,
                    hash_b: 2997229249,
                    locale: 0,
                    platform: 0,
                    block_index: BLOCK_INDEX_DELETED
                }
            ]
        );
    }

    #[test]
    fn block_table() {
        let result = mpq_block_table(
            &LT[86021..86021 + (4 * 16)],
            ResourceLimits::trusted().max_mpq_table_entries,
        );

        assert!(result.is_ok());

        let result = result.unwrap();
        assert_eq!(
            result,
            vec![
                MpqBlockTableEntry {
                    offset: 69489,
                    compressed_size: 50,
                    size: 42,
                    flags: MpqBlockFlags::COMPRESSED | MpqBlockFlags::EXISTS
                },
                MpqBlockTableEntry {
                    offset: 32,
                    compressed_size: 69457,
                    size: 193831,
                    flags: MpqBlockFlags::COMPRESSED
                        | MpqBlockFlags::ENCRYPTED
                        | MpqBlockFlags::EXISTS
                },
                MpqBlockTableEntry {
                    offset: 69539,
                    compressed_size: 45,
                    size: 37,
                    flags: MpqBlockFlags::COMPRESSED | MpqBlockFlags::EXISTS
                },
                MpqBlockTableEntry {
                    offset: 69584,
                    compressed_size: 53,
                    size: 56,
                    flags: MpqBlockFlags::COMPRESSED | MpqBlockFlags::EXISTS
                }
            ]
        );
    }

    #[test]
    fn from_bytes_lt() {
        let result = Mpq::from_bytes(LT);
        assert_ok!(result);
    }

    #[test]
    fn from_bytes_invalid() {
        let bytes = b"MPQ\x1aDEAD";
        let result = Mpq::from_bytes(bytes);
        assert!(matches!(result, Err(MpqError::HeaderNotFound)));
    }

    #[test]
    fn hash_table_lookup() {
        let mpq = assert_ok!(Mpq::from_bytes(LT));
        let entry = mpq.find_hash_table_entry(CHK_PATH, None);
        assert_eq!(
            entry,
            Some(&MpqHashTableEntry {
                hash_a: hash_str(CHK_PATH, MpqHashType::NameA),
                hash_b: hash_str(CHK_PATH, MpqHashType::NameB),
                locale: 0,
                platform: 0,
                block_index: 1,
            })
        );
    }

    #[test]
    fn hash_table_lookup_falls_back_to_neutral() {
        let mpq = assert_ok!(Mpq::from_bytes(LT));
        let entry = mpq.find_hash_table_entry(CHK_PATH, Some(0x0409));
        assert_eq!(
            entry,
            Some(&MpqHashTableEntry {
                hash_a: hash_str(CHK_PATH, MpqHashType::NameA),
                hash_b: hash_str(CHK_PATH, MpqHashType::NameB),
                locale: 0,
                platform: 0,
                block_index: 1,
            })
        );
    }

    #[test]
    fn sector_offset_table() {
        let mpq = assert_ok!(Mpq::from_bytes(LT));
        let block = &mpq.block_table[1];
        let encryption_key = block.encryption_key(CHK_PATH);
        let table = assert_ok!(mpq.read_sector_offset_table(block, encryption_key));
        assert_eq!(
            table,
            vec![
                196, 3239, 5627, 8040, 10540, 13079, 15752, 18393, 20670, 21490, 21550, 22240,
                23646, 24752, 25925, 27241, 28609, 30156, 31678, 33198, 34737, 36412, 37970, 39550,
                41090, 42640, 43625, 44830, 46564, 48836, 51246, 53625, 55898, 58417, 60946, 63322,
                65739, 66033, 66069, 66105, 66141, 66411, 66482, 66533, 67053, 67963, 68044, 69222,
                69457
            ]
        );
    }

    #[rstest]
    #[case::lt(LT, LT_CHK)]
    #[case::negative(NEGATIVE_OFFSETS, NEGATIVE_OFFSETS_CHK)]
    #[case::imploded(IMPLODED, IMPLODED_CHK)]
    #[case::protected_0(PROTECTED_0, PROTECTED_0_CHK)]
    #[case::protected_1(PROTECTED_1, PROTECTED_1_CHK)]
    #[case::protected_2(PROTECTED_2, PROTECTED_2_CHK)]
    #[case::protected_3(PROTECTED_3, PROTECTED_3_CHK)]
    #[case::protected_4(PROTECTED_4, PROTECTED_4_CHK)]
    #[case::protected_5(PROTECTED_5, PROTECTED_5_CHK)]
    #[case::protected_6(PROTECTED_6, PROTECTED_6_CHK)]
    #[case::smallest(SMALLEST, SMALLEST_CHK)]
    #[case::ococ_bound_2(OCOC_BOUND_2, OCOC_BOUND_2_CHK)]
    #[case::sniper_seed(SNIPER_SEED, SNIPER_SEED_CHK)]
    fn extract_chk(#[case] input: &[u8], #[case] expected: &[u8]) {
        let mpq = assert_ok!(Mpq::from_bytes(input));
        let chk = assert_ok!(mpq.read_file(CHK_PATH, None));
        assert_eq!(chk.as_slice(), expected);
    }

    const DESERT_STRIKE: &[u8] = include_bytes!("../assets/DSA_7.4.3a_Desert_Strike_Angel.scx");
    const DESERT_STRIKE_CHK: &[u8] = include_bytes!("../assets/DSA_7.4.3a_Desert_Strike_Angel.chk");

    #[test]
    fn extract_chk_in_language() {
        // This file has a CHK that's only under English
        let mpq = assert_ok!(Mpq::from_bytes(DESERT_STRIKE));
        let chk = assert_ok!(mpq.read_file(CHK_PATH, Some(0x409)));
        assert_eq!(chk.as_slice(), DESERT_STRIKE_CHK);
    }

    const DOUBLE_CHK_IN_HASH_TABLE: &[u8] = include_bytes!("../assets/Impossible_Scen._Future.scx");
    const DOUBLE_CHK_IN_HASH_TABLE_CHK: &[u8] =
        include_bytes!("../assets/Impossible_Scen._Future.chk");

    #[test]
    fn double_chk_in_hash_table_regression() {
        // This file has 2 entries for the CHK file in its hash table, only one of which is in the
        // "correct" slot. If our search logic doesn't cut off properly when encountering empty
        // slots, it will pick the wrong one
        let mpq = assert_ok!(Mpq::from_bytes(DOUBLE_CHK_IN_HASH_TABLE));
        let chk = assert_ok!(mpq.read_file(CHK_PATH, None));
        assert_eq!(chk.as_slice(), DOUBLE_CHK_IN_HASH_TABLE_CHK);
    }

    const CORRUPTED_0: &[u8] = include_bytes!("../assets/corrupted-0.scx");
    const CORRUPTED_1: &[u8] = include_bytes!("../assets/corrupted-1.scx");

    #[rstest]
    #[case(CORRUPTED_0)]
    #[case(CORRUPTED_1)]
    fn corrupted_map_doesnt_panic(#[case] input: &[u8]) {
        let result = Mpq::from_bytes(input);
        assert!(matches!(result, Err(MpqError::MalformedHeader)));
    }

    #[test]
    fn zero_size_hash_table_doesnt_panic() {
        // A header can claim a zero-size hash table, producing an empty table after parsing.
        // Looking up any file in it must return FileNotFound rather than panicking (found by
        // fuzzing: the table offset mask underflowed)
        let mut data = vec![];
        data.extend_from_slice(b"MPQ\x1A");
        data.extend_from_slice(&32u32.to_le_bytes()); // header size
        data.extend_from_slice(&[0u8; 4]); // archive size (ignored)
        data.extend_from_slice(&[0u8; 2]); // format version (ignored)
        data.extend_from_slice(&3u16.to_le_bytes()); // sector size shift
        data.extend_from_slice(&32i32.to_le_bytes()); // hash table pos
        data.extend_from_slice(&32i32.to_le_bytes()); // block table pos
        data.extend_from_slice(&0u32.to_le_bytes()); // hash table size
        data.extend_from_slice(&0u32.to_le_bytes()); // block table size
        data.extend_from_slice(&[0u8; 8]); // padding to keep the table offsets in bounds

        let mpq = assert_ok!(Mpq::from_bytes(&data));
        assert!(matches!(
            mpq.read_file(CHK_PATH, None),
            Err(MpqError::FileNotFound)
        ));
    }

    #[test]
    fn wide_character_file_path_is_rejected_instead_of_panicking() {
        let mpq = assert_ok!(Mpq::from_bytes(LT));
        assert!(matches!(
            mpq.read_file("staredit\\café.chk", None),
            Err(MpqError::FileNotFound)
        ));
        assert!(matches!(
            mpq.read_file("staredit\\シナリオ.chk", None),
            Err(MpqError::InvalidPath)
        ));
    }

    #[test]
    fn map_size_limit_is_checked_before_parsing() {
        let limits = ResourceLimits {
            max_map_bytes: LT.len() - 1,
            ..ResourceLimits::default()
        };
        let result = Mpq::from_bytes_with_limits(LT, &limits);
        assert!(matches!(
            result,
            Err(MpqError::ResourceLimit(ResourceLimitError::Exceeded {
                resource: Resource::MapBytes,
                ..
            }))
        ));
    }

    #[test]
    fn declared_file_size_is_checked_before_sector_allocation() {
        let data = [0u8; 1];
        let limits = ResourceLimits {
            max_mpq_file_bytes: 1024,
            ..ResourceLimits::default()
        };
        let mpq = Mpq {
            data: &data,
            header: MpqHeader {
                offset: 0,
                sector_size_shift: 0,
                hash_table_pos: 0,
                block_table_pos: 0,
                hash_table_size: 1,
                block_table_size: 1,
            },
            hash_table: vec![MpqHashTableEntry {
                hash_a: hash_str(CHK_PATH, MpqHashType::NameA),
                hash_b: hash_str(CHK_PATH, MpqHashType::NameB),
                locale: 0,
                platform: 0,
                block_index: 0,
            }],
            block_table: vec![MpqBlockTableEntry {
                offset: 0,
                compressed_size: 1,
                size: 1025,
                flags: MpqBlockFlags::EXISTS,
            }],
            limits,
        };

        assert!(matches!(
            mpq.read_file(CHK_PATH, None),
            Err(MpqError::ResourceLimit(ResourceLimitError::Exceeded {
                resource: Resource::MpqFileBytes,
                observed: 1025,
                limit: 1024,
            }))
        ));
    }

    #[test]
    fn implode_output_is_bounded_while_streaming() {
        // This tiny stream expands to "AIAIAIAIAIAIA".
        let compressed = [0x00, 0x04, 0x82, 0x24, 0x25, 0x8f, 0x80, 0x7f];
        assert!(matches!(
            explode_data(&compressed, 4),
            Err(MpqError::ResourceLimit(ResourceLimitError::Exceeded {
                resource: Resource::MpqSectorOutput,
                limit: 4,
                ..
            }))
        ));
    }

    #[test]
    fn compression_marker_without_payload_is_an_error() {
        assert!(matches!(
            decompress_sector(&[COMPRESSION_IMPLODE], 4096),
            Err(MpqError::MalformedSectorTable)
        ));
    }

    /// Encodes `data` as a PKWARE implode stream made only of literals (binary mode, 1 KiB
    /// dictionary), which is enough to exercise implode as a stage of a multi-compressed sector.
    fn implode_literals(data: &[u8]) -> Vec<u8> {
        let mut out = vec![0x00, 0x04];
        let mut bit_buf = 0u32;
        let mut bit_count = 0;
        let mut put_bits = |out: &mut Vec<u8>, value: u32, count: u32| {
            bit_buf |= value << bit_count;
            bit_count += count;
            while bit_count >= 8 {
                out.push(bit_buf as u8);
                bit_buf >>= 8;
                bit_count -= 8;
            }
        };
        for &byte in data {
            // A 0 flag bit followed by the raw literal byte
            put_bits(&mut out, (byte as u32) << 1, 9);
        }
        // End of stream: a 1 flag bit, the (inverted) 7-bit code for length symbol 15, then 8
        // extra bits of 0xFF, giving the reserved length 519
        put_bits(&mut out, 1, 1);
        put_bits(&mut out, 0, 7);
        put_bits(&mut out, 0xFF, 8);
        put_bits(&mut out, 0, 7);
        out
    }

    fn zlib(data: &[u8]) -> Vec<u8> {
        miniz_oxide::deflate::compress_to_vec_zlib(data, 6)
    }

    fn bzip2(data: &[u8]) -> Vec<u8> {
        let mut out = Vec::new();
        bzip2::read::BzEncoder::new(data, bzip2::Compression::best())
            .read_to_end(&mut out)
            .unwrap();
        out
    }

    fn sector(mask: u8, payload: &[u8]) -> Vec<u8> {
        let mut sector = vec![mask];
        sector.extend_from_slice(payload);
        sector
    }

    fn sample_data(len: usize) -> Vec<u8> {
        (0..len).map(|i| (i * 7 % 23) as u8 + b'A').collect()
    }

    #[test]
    fn implode_literal_helper_round_trips() {
        let data = sample_data(300);
        assert_eq!(
            assert_ok!(explode_data(&implode_literals(&data), 4096)),
            data
        );
    }

    #[test]
    fn zlib_sector_decompresses() {
        let data = sample_data(4096);
        let result = decompress_sector(&sector(COMPRESSION_ZLIB, &zlib(&data)), 4096);
        assert_eq!(assert_ok!(result), data);
    }

    #[test]
    fn bzip2_sector_decompresses() {
        let data = sample_data(4096);
        let result = decompress_sector(&sector(COMPRESSION_BZIP2, &bzip2(&data)), 4096);
        assert_eq!(assert_ok!(result), data);
    }

    #[rstest]
    #[case::implode_zlib(COMPRESSION_IMPLODE | COMPRESSION_ZLIB)]
    #[case::bzip2_zlib(COMPRESSION_BZIP2 | COMPRESSION_ZLIB)]
    #[case::implode_bzip2(COMPRESSION_IMPLODE | COMPRESSION_BZIP2)]
    #[case::implode_bzip2_zlib(COMPRESSION_IMPLODE | COMPRESSION_BZIP2 | COMPRESSION_ZLIB)]
    fn multi_compressed_sector_decompresses_in_storm_order(#[case] mask: u8) {
        let data = sample_data(2000);
        // Compression is applied in the reverse of the decompression order: zlib, bzip2, implode
        let mut payload = data.clone();
        if mask & COMPRESSION_ZLIB != 0 {
            payload = zlib(&payload);
        }
        if mask & COMPRESSION_BZIP2 != 0 {
            payload = bzip2(&payload);
        }
        if mask & COMPRESSION_IMPLODE != 0 {
            payload = implode_literals(&payload);
        }

        let result = decompress_sector(&sector(mask, &payload), 4096);
        assert_eq!(assert_ok!(result), data);
    }

    #[rstest]
    #[case::none(0x00)]
    #[case::huffman(0x01)]
    #[case::huffman_zlib(0x01 | COMPRESSION_ZLIB)]
    #[case::sparse(0x20)]
    #[case::adpcm_mono(0x40)]
    #[case::adpcm_stereo(0x80)]
    fn unsupported_compression_mask_is_an_error(#[case] mask: u8) {
        let result = decompress_sector(&sector(mask, &zlib(b"data")), 4096);
        assert!(
            matches!(result, Err(MpqError::UnsupportedCompressionType(m)) if m == mask),
            "{result:?}"
        );
    }

    #[rstest]
    #[case::zlib(COMPRESSION_ZLIB, zlib(&[0u8; 65536]))]
    #[case::bzip2(COMPRESSION_BZIP2, bzip2(&[0u8; 65536]))]
    // The intermediate (bzip2) stage is bounded too, not just the final one
    #[case::intermediate(COMPRESSION_BZIP2 | COMPRESSION_ZLIB, bzip2(&[0u8; 65536]))]
    fn decompressed_output_is_bounded_while_streaming(#[case] mask: u8, #[case] payload: Vec<u8>) {
        assert!(matches!(
            decompress_sector(&sector(mask, &payload), 4096),
            Err(MpqError::ResourceLimit(ResourceLimitError::Exceeded {
                resource: Resource::MpqSectorOutput,
                limit: 4096,
                ..
            }))
        ));
    }

    #[test]
    fn decompressed_output_is_truncated_to_max_append() {
        let data = sample_data(4096);
        let mut output = Vec::new();
        let decompressed_len = assert_ok!(decompress_sector_into(
            &sector(COMPRESSION_ZLIB, &zlib(&data)),
            4096,
            100,
            usize::MAX,
            &mut output,
        ));
        assert_eq!(decompressed_len, 4096);
        assert_eq!(output, &data[..100]);
    }

    #[rstest]
    #[case::zlib(COMPRESSION_ZLIB, zlib(&sample_data(4096)))]
    #[case::bzip2(COMPRESSION_BZIP2, bzip2(&sample_data(4096)))]
    fn truncated_stream_is_an_error(#[case] mask: u8, #[case] payload: Vec<u8>) {
        let truncated = &payload[..payload.len() / 2];
        assert!(matches!(
            decompress_sector(&sector(mask, truncated), 4096),
            Err(MpqError::TruncatedCompressedData)
        ));
    }

    #[test]
    fn corrupt_zlib_stream_is_an_error() {
        let mut payload = zlib(&sample_data(4096));
        // Break the Adler-32 checksum at the end of the stream
        *payload.last_mut().unwrap() ^= 0xFF;
        assert!(matches!(
            decompress_sector(&sector(COMPRESSION_ZLIB, &payload), 4096),
            Err(MpqError::ZlibError(_))
        ));
    }

    #[test]
    fn corrupt_bzip2_stream_is_an_error() {
        let mut payload = bzip2(&sample_data(4096));
        // Break the stream magic
        payload[0] = b'X';
        assert!(matches!(
            decompress_sector(&sector(COMPRESSION_BZIP2, &payload), 4096),
            Err(MpqError::Bzip2Error(_))
        ));
    }

    #[test]
    fn read_file_with_zlib_sectors() {
        // 512-byte sectors (shift 0); the file spans one full sector plus a partial one
        let contents = sample_data(700);
        let sectors = [
            sector(COMPRESSION_ZLIB, &zlib(&contents[..512])),
            sector(COMPRESSION_ZLIB, &zlib(&contents[512..])),
        ];
        let table_len = 3 * 4;
        let mut data = Vec::new();
        let mut offset = table_len as u32;
        data.extend_from_slice(&offset.to_le_bytes());
        for s in &sectors {
            offset += s.len() as u32;
            data.extend_from_slice(&offset.to_le_bytes());
        }
        for s in &sectors {
            data.extend_from_slice(s);
        }

        let mpq = Mpq {
            data: &data,
            header: MpqHeader {
                offset: 0,
                sector_size_shift: 0,
                hash_table_pos: 0,
                block_table_pos: 0,
                hash_table_size: 1,
                block_table_size: 1,
            },
            hash_table: vec![MpqHashTableEntry {
                hash_a: hash_str(CHK_PATH, MpqHashType::NameA),
                hash_b: hash_str(CHK_PATH, MpqHashType::NameB),
                locale: 0,
                platform: 0,
                block_index: 0,
            }],
            block_table: vec![MpqBlockTableEntry {
                offset: 0,
                compressed_size: offset,
                size: contents.len() as u32,
                flags: MpqBlockFlags::COMPRESSED | MpqBlockFlags::EXISTS,
            }],
            limits: ResourceLimits::default(),
        };

        assert_eq!(assert_ok!(mpq.read_file(CHK_PATH, None)), contents);
    }
    // One-sector compressed archives or arbitrary raw archives, exercising the public extraction
    // path while keeping metadata construction independent of the archive parser.
    fn ratio_archive_read(
        payload: &[u8],
        flags: MpqBlockFlags,
        sector_size_shift: u16,
        declared_size: u32,
        limits: ResourceLimits,
    ) -> Result<Vec<u8>, MpqError> {
        let mut data = Vec::new();
        if flags.intersects(MpqBlockFlags::COMPRESSED | MpqBlockFlags::IMPLODED) {
            assert!(declared_size as usize <= 512usize << sector_size_shift);
            data.extend_from_slice(&8u32.to_le_bytes());
            data.extend_from_slice(&(8 + payload.len() as u32).to_le_bytes());
        }
        data.extend_from_slice(payload);
        let mpq = Mpq {
            data: &data,
            header: MpqHeader {
                offset: 0,
                sector_size_shift,
                hash_table_pos: 0,
                block_table_pos: 0,
                hash_table_size: 1,
                block_table_size: 1,
            },
            hash_table: vec![MpqHashTableEntry {
                hash_a: hash_str(CHK_PATH, MpqHashType::NameA),
                hash_b: hash_str(CHK_PATH, MpqHashType::NameB),
                locale: 0,
                platform: 0,
                block_index: 0,
            }],
            block_table: vec![MpqBlockTableEntry {
                offset: 0,
                compressed_size: data.len() as u32,
                size: declared_size,
                flags: flags | MpqBlockFlags::EXISTS,
            }],
            limits,
        };
        mpq.read_file(CHK_PATH, None)
    }

    fn assert_expansion_limit<T>(result: Result<T, MpqError>, expected: usize) {
        // A regression may return megabytes of output; report only whether it succeeded.
        let result = result.map(|_| ());
        assert!(
            matches!(result, Err(MpqError::ResourceLimit(ResourceLimitError::Exceeded {
                resource: Resource::MpqSectorExpansion, limit, observed,
            })) if limit == expected && observed > limit),
            "{result:?}"
        );
    }

    #[rstest]
    #[case::zlib(COMPRESSION_ZLIB)]
    #[case::bzip2(COMPRESSION_BZIP2)]
    #[case::masked_implode(COMPRESSION_IMPLODE)]
    #[case::legacy_implode(0)]
    fn read_file_enforces_ratio_for_every_codec(#[case] mask: u8) {
        let contents = sample_data(8192);
        let encoded = match mask {
            COMPRESSION_ZLIB => zlib(&contents),
            COMPRESSION_BZIP2 => bzip2(&contents),
            _ => implode_literals(&contents),
        };
        let (payload, flags) = if mask == 0 {
            (encoded, MpqBlockFlags::IMPLODED)
        } else {
            (sector(mask, &encoded), MpqBlockFlags::COMPRESSED)
        };
        // Zero allows only the 4 KiB floor, including literal-only implode streams.
        let limits = ResourceLimits {
            max_mpq_compression_ratio: 0,
            ..ResourceLimits::default()
        };
        assert_expansion_limit(ratio_archive_read(&payload, flags, 5, 16384, limits), 4096);
        assert_eq!(
            assert_ok!(ratio_archive_read(
                &payload,
                flags,
                5,
                16384,
                ResourceLimits::trusted()
            )),
            contents
        );
    }

    #[test]
    fn default_ratio_rejects_small_bzip2_bomb_below_absolute_limit() {
        let contents = vec![0; 4 * 1024 * 1024];
        let payload = sector(COMPRESSION_BZIP2, &bzip2(&contents));
        assert!(payload.len() < 128, "expected a tiny compressed fixture");
        let flags = MpqBlockFlags::COMPRESSED;
        let size = contents.len() as u32;
        let limits = ResourceLimits::default();
        assert_expansion_limit(
            ratio_archive_read(&payload, flags, 13, size, limits),
            payload.len() * limits.max_mpq_compression_ratio,
        );
        let relaxed = ResourceLimits {
            max_mpq_compression_ratio: 100_000,
            ..limits
        };
        assert_eq!(
            assert_ok!(ratio_archive_read(&payload, flags, 13, size, relaxed)),
            contents
        );
    }

    #[test]
    fn ratio_budget_uses_original_sector_across_stages() {
        let contents = sample_data(16384);
        let mut inner = zlib(&contents);
        // Trailing bytes are accepted after zlib's stream end. They let the first decoder
        // expand to exactly the allowance while the final decoder expands beyond it.
        inner.resize(4096, 0);
        let payload = sector(COMPRESSION_BZIP2 | COMPRESSION_ZLIB, &bzip2(&inner));
        let limits = ResourceLimits {
            max_mpq_compression_ratio: 4,
            ..ResourceLimits::default()
        };
        assert!(payload.len() * 4 < 4096);
        assert_eq!(
            assert_ok!(ratio_archive_read(
                &payload,
                MpqBlockFlags::COMPRESSED,
                5,
                16384,
                ResourceLimits::trusted(),
            )),
            contents
        );
        // Resetting the denominator to the intermediate 4096 bytes would allow all 16384.
        assert_expansion_limit(
            ratio_archive_read(&payload, MpqBlockFlags::COMPRESSED, 5, 16384, limits),
            4096,
        );
    }

    #[test]
    fn ratio_rejects_intermediate_expansion_before_invalid_next_codec() {
        let payload = sector(COMPRESSION_BZIP2 | COMPRESSION_ZLIB, &bzip2(&[0; 8192]));
        let limits = ResourceLimits {
            max_mpq_compression_ratio: 4,
            ..ResourceLimits::default()
        };
        // The expanded bytes are not zlib. An error from zlib would mean the first stage
        // was materialized without enforcing its expansion budget.
        assert_expansion_limit(
            ratio_archive_read(&payload, MpqBlockFlags::COMPRESSED, 5, 16384, limits),
            4096,
        );
    }

    #[rstest]
    #[case::zlib(COMPRESSION_ZLIB, zlib(&sample_data(8192)))]
    #[case::bzip2(COMPRESSION_BZIP2, bzip2(&sample_data(8192)))]
    #[case::implode(COMPRESSION_IMPLODE, implode_literals(&sample_data(8192)))]
    fn truncating_retained_output_does_not_bypass_ratio(
        #[case] mask: u8,
        #[case] encoded: Vec<u8>,
    ) {
        let mut output = Vec::new();
        assert_expansion_limit(
            decompress_sector_into(&sector(mask, &encoded), 16384, 1, 4096, &mut output),
            4096,
        );
        assert!(output.len() <= 1);
    }

    #[test]
    fn ratio_small_sector_allowance_includes_boundary_only() {
        let limits = ResourceLimits {
            max_mpq_compression_ratio: 0,
            ..ResourceLimits::default()
        };
        for len in [4096, 4097] {
            let contents = vec![0; len];
            let payload = sector(COMPRESSION_ZLIB, &zlib(&contents));
            let result = ratio_archive_read(&payload, MpqBlockFlags::COMPRESSED, 4, 8192, limits);
            if len == 4096 {
                assert_eq!(assert_ok!(result), contents);
            } else {
                assert_expansion_limit(result, 4096);
            }
        }
    }

    #[rstest]
    #[case::truncated_first_sector(vec![7], 8193, 8192)]
    #[case::empty_second_sector(vec![7; 8192], 16385, 16384)]
    fn compatibility_padding_uses_actual_bytes_and_respects_ratio(
        #[case] payload: Vec<u8>,
        #[case] declared_size: u32,
        #[case] trusted_size: usize,
    ) {
        // Raw sector offsets advertise 8192 bytes even when EOF leaves only one or zero.
        assert_expansion_limit(
            ratio_archive_read(
                &payload,
                MpqBlockFlags::empty(),
                4,
                declared_size,
                ResourceLimits::default(),
            ),
            4096,
        );
        let result = assert_ok!(ratio_archive_read(
            &payload,
            MpqBlockFlags::empty(),
            4,
            declared_size,
            ResourceLimits::trusted(),
        ));
        assert_eq!(result.len(), trusted_size);
        assert_eq!(&result[..payload.len()], payload);
        assert!(result[payload.len()..].iter().all(|&byte| byte == 0));
    }

    #[test]
    fn ratio_limit_above_allowance_accepts_exact_boundary() {
        let limits = ResourceLimits {
            max_mpq_compression_ratio: 2,
            ..ResourceLimits::default()
        };
        for len in [10000, 10001] {
            let contents = vec![0; len];
            let mut payload = sector(COMPRESSION_ZLIB, &zlib(&contents));
            // Count all actual original sector bytes, including bytes after the stream.
            payload.resize(5000, 0);
            let result = ratio_archive_read(&payload, MpqBlockFlags::COMPRESSED, 5, 16384, limits);
            if len == 10000 {
                assert_eq!(assert_ok!(result), contents);
            } else {
                assert_expansion_limit(result, 10000);
            }
        }
    }

    #[test]
    fn ratio_counts_decoded_bytes_and_padding_together() {
        let payload = vec![7; 4096];
        let limits = ResourceLimits {
            max_mpq_compression_ratio: 1,
            ..ResourceLimits::default()
        };
        // The copied bytes and the padding each fit separately, but together occupy 8192 bytes.
        assert_expansion_limit(
            ratio_archive_read(&payload, MpqBlockFlags::empty(), 4, 8193, limits),
            4096,
        );
    }

    #[test]
    fn large_ratios_saturate_without_disabling_absolute_limits() {
        let contents = sample_data(8192);
        let payload = sector(COMPRESSION_ZLIB, &zlib(&contents));
        for ratio in [usize::MAX - 1, usize::MAX] {
            let limits = ResourceLimits {
                max_mpq_compression_ratio: ratio,
                ..ResourceLimits::default()
            };
            assert_eq!(
                assert_ok!(ratio_archive_read(
                    &payload,
                    MpqBlockFlags::COMPRESSED,
                    5,
                    16384,
                    limits,
                )),
                contents
            );
            let limited = ResourceLimits {
                max_mpq_file_bytes: 1024,
                ..limits
            };
            assert!(matches!(
                ratio_archive_read(&payload, MpqBlockFlags::COMPRESSED, 5, 16384, limited,),
                Err(MpqError::ResourceLimit(ResourceLimitError::Exceeded {
                    resource: Resource::MpqFileBytes,
                    limit: 1024,
                    ..
                }))
            ));
        }
    }
}
