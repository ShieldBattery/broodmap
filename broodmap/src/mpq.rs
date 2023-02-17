//! An implementation of the MPQ file format, for the features used by SCM/SCX files in StarCraft.
//! All features that are not used by the game are not implemented, and the implementation matches
//! the game's behavior as closely as possible (including bugs, which are often exploited by
//! so-called "map protectors" to prevent 3rd party editors from opening map files).

use bitflags::bitflags;
use nom::combinator::{map, verify};
use nom::multi::{count, many0, many_till};
use nom::sequence::tuple;
use nom::IResult;
use std::borrow::Cow;

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
    verify(le_u32, |size: &u32| *size >= 32)(input)
}

fn mpq_header_offset(input: &[u8]) -> IResult<&[u8], u32> {
    use nom::bytes::streaming::{tag, take};

    map(
        many_till(take(512usize), tuple((tag(b"MPQ\x1A"), mpq_header_size))),
        |(takes, (_magic, _header_size))| (takes.len() * 512) as u32,
    )(input)
}

fn mpq_table_pos(min_pos: i32) -> impl Fn(&[u8]) -> IResult<&[u8], i32> {
    use nom::number::streaming::le_i32;

    // This checks to ensure that the table is placed at the start of the file or after (but can't
    // validate that it is before the end of the file, since we may not know what that is yet)
    move |input| verify(le_i32, |pos: &i32| *pos >= min_pos)(input)
}

/// Find and parse an [MpqHeader] from the given input. This is a streaming parser, and may return
/// Incomplete if the full header cannot be found.
fn mpq_header(input: &[u8]) -> IResult<&[u8], MpqHeader> {
    use nom::bytes::streaming::take;
    use nom::number::streaming::{le_u16, le_u32};

    let (input, offset) = mpq_header_offset(input)?;
    // NOTE(tec27): BW's code ignores format version and archive size entirely
    let (input, (_archive_size, _format_version)) = tuple((take(4usize), take(2usize)))(input)?;

    map(
        tuple((
            le_u16,
            mpq_table_pos(-(offset as i32)),
            mpq_table_pos(-(offset as i32)),
            le_u32,
            le_u32,
        )),
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
    )(input)
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
        let (data, remainder): (&[u8], &[u8]) = if data.len() % 4 == 0 {
            (data, &[])
        } else {
            let extra = data.len() % 4;
            (&data[0..data.len() - extra], &data[data.len() - extra..])
        };

        data.chunks_exact(4)
            .flat_map(|val| {
                u32::to_le_bytes(self.decrypt_u32(u32::from_le_bytes(val.try_into().unwrap())))
            })
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
    ///
    pub block_index: u32,
}

/// Parse the contents of an MPQ hash table. This is a complete parser, it expects that all data for
/// the hash table is present, and that the input begins at the start of the table and ends at the
/// end.
///
/// Note that BW's parser allows the hash table to be shorter than expected at the end of the file
/// (so this does as well).
fn mpq_hash_table(input: &[u8]) -> IResult<&[u8], Vec<MpqHashTableEntry>> {
    use nom::number::complete::le_u32;

    let mut decrypter = Decrypter::from_str("(hash table)");
    let r = many0(map(count(le_u32, 4), |entries: Vec<u32>| {
        assert_eq!(entries.len(), 4);
        let hash_a = decrypter.decrypt_u32(entries[0]);
        let hash_b = decrypter.decrypt_u32(entries[1]);
        let locale_platform = decrypter.decrypt_u32(entries[2]);
        let block_index = decrypter.decrypt_u32(entries[3]);

        let block_index = if block_index != BLOCK_INDEX_EMPTY && block_index != BLOCK_INDEX_DELETED
        {
            // Mask out high bits that will overflow out when doing uint32 multiplication by 0x10,
            // which protections abuse to mess up block table length checks outside of BW
            block_index & 0x0FFF_FFFF
        } else {
            block_index
        };

        MpqHashTableEntry {
            hash_a,
            hash_b,
            locale: (locale_platform & 0x0000_FFFF) as u16,
            platform: (locale_platform >> 16) as u16,
            block_index,
        }
    }))(input);

    #[allow(clippy::let_and_return)] // Necessary so decrypter lives long enough
    r
}

/// The size of each MPQ hash table entry in the file, in bytes.
const MPQ_BLOCK_TABLE_ENTRY_SIZE: usize = 16;

bitflags! {
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
            Some((self.offset as usize) + (mpq_header.offset as usize))
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
            Some((key + (self.offset as u32)) ^ (self.size))
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
fn mpq_block_table(input: &[u8]) -> IResult<&[u8], Vec<MpqBlockTableEntry>> {
    use nom::number::complete::le_u32;

    let mut decrypter = Decrypter::from_str("(block table)");
    let r = many0(map(count(le_u32, 4), |entries: Vec<u32>| {
        assert_eq!(entries.len(), 4);

        MpqBlockTableEntry {
            offset: decrypter.decrypt_u32(entries[0]) as i32,
            compressed_size: decrypter.decrypt_u32(entries[1]),
            size: decrypter.decrypt_u32(entries[2]),
            flags: MpqBlockFlags::from_bits_truncate(decrypter.decrypt_u32(entries[3])),
        }
    }))(input);

    #[allow(clippy::let_and_return)] // Necessary so decrypter lives long enough
    r
}

/// Parse the contents of an MPQ sector table for a particular file, decrypting it if needed.
fn mpq_sector_table(
    input: &[u8],
    num_sectors: usize,
    encryption_key: Option<u32>,
) -> IResult<&[u8], Vec<i32>> {
    use nom::number::complete::le_u32;

    let mut decrypter = encryption_key.map(Decrypter::from_key_value);
    let r = map(count(le_u32, num_sectors), |entries: Vec<u32>| {
        if let Some(ref mut d) = decrypter {
            entries
                .iter()
                .map(|val| d.decrypt_u32(*val) as i32)
                .collect()
        } else {
            entries.iter().map(|val| *val as i32).collect()
        }
    })(input);

    #[allow(clippy::let_and_return)] // Necessary so decrypter lives long enough
    r
}

// TODO(tec27): Support more compression algorithms, this is not a complete list of what BW supports
const COMPRESSION_IMPLODE: u8 = 0x08;

/// Decompresses a file sector using the algorithm it specifies. Sectors specify an algorithm with
/// the first byte of their data.
fn decompress_sector(data: &[u8]) -> Result<Vec<u8>, MpqError> {
    if data.len() <= 1 {
        panic!("tried to decompress empty sector");
    }

    match data[0] {
        COMPRESSION_IMPLODE => explode_data(&data[1..]),
        a => Err(MpqError::UnsupportedCompressionType(a)),
    }
}

fn explode_data(data: &[u8]) -> Result<Vec<u8>, MpqError> {
    explode::explode(data).map_err(MpqError::ExplodeError)
}

#[derive(Error, Debug)]
pub enum MpqError {
    #[error("header not found")]
    HeaderNotFound,
    #[error("file not found")]
    FileNotFound,
    #[error("malformed header")]
    MalformedHeader,
    #[error("file had malformed sector table")]
    MalformedSectorTable,
    #[error("unsupported compression type: {0:x}")]
    UnsupportedCompressionType(u8),
    #[error("problem decompressing with explode: {0}")]
    ExplodeError(explode::Error),
    #[error("parser error")]
    ParserError(nom::error::ErrorKind),
}

// TODO(tec27): Write a version of this that works with BufReader or similar
#[derive(Debug)]
pub struct Mpq<'a> {
    /// The file data of this MPQ.
    pub data: &'a [u8],
    /// The header of the MPQ, containing metadata about its structure.
    pub header: MpqHeader,
    /// A hash table containing information about the files present in this MPQ archive, allowing
    /// a file's position in the [block_table] to be located.
    pub hash_table: Vec<MpqHashTableEntry>,
    /// A table for file metadata, containing 1 entry for each file present in this MPQ archive.
    pub block_table: Vec<MpqBlockTableEntry>,
}

impl<'a> Mpq<'a> {
    /// Initializes an [Mpq] from the given data in-memory, which must be the entire MPQ file. This
    /// method will eagerly parse the MPQ metadata, which will be used to fulfill later requests for
    /// specific files.
    pub fn from_bytes(data: &'a [u8]) -> Result<Self, MpqError> {
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
            let hash_table_size = ((header.hash_table_size as usize) * MPQ_HASH_TABLE_ENTRY_SIZE)
                .clamp(0, data.len() - hash_table_offset);
            let hash_data = &data[hash_table_offset..hash_table_offset + hash_table_size];
            match mpq_hash_table(hash_data) {
                Ok((_, hash_table)) => Ok(hash_table),
                Err(nom::Err::Error(e)) => Err(MpqError::ParserError(e.code)),
                Err(nom::Err::Failure(e)) => Err(MpqError::ParserError(e.code)),
                Err(nom::Err::Incomplete(_)) => {
                    // We're using complete parsers here so this should never happen
                    unreachable!()
                }
            }
        }?;

        let mut block_table = {
            let block_table_offset = header.block_table_offset();
            if block_table_offset >= data.len() {
                return Err(MpqError::MalformedHeader);
            }
            let block_table_size = ((header.block_table_size as usize)
                * MPQ_BLOCK_TABLE_ENTRY_SIZE)
                .clamp(0, data.len() - block_table_offset);
            let block_data = &data[block_table_offset..block_table_offset + block_table_size];
            match mpq_block_table(block_data) {
                Ok((_, block_table)) => Ok(block_table),
                Err(nom::Err::Error(e)) => Err(MpqError::ParserError(e.code)),
                Err(nom::Err::Failure(e)) => Err(MpqError::ParserError(e.code)),
                Err(nom::Err::Incomplete(_)) => {
                    // We're using complete parsers here so this should never happen
                    unreachable!()
                }
            }
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
        })
    }

    pub fn read_file(&self, path: &str, locale: Option<u16>) -> Result<Vec<u8>, MpqError> {
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
        let mut result = vec![];

        // NOTE(tec27): BW is okay with file data being truncated by the end of the file, so we
        // reproduce that handling
        for (i, w) in sector_table.windows(2).enumerate() {
            if bytes_left == 0 {
                break;
            }

            let (sector_offset, next_sector_offset) = (w[0], w[1]);
            let cur_sector_size = (next_sector_offset - sector_offset) as usize;
            // Convert to an absolute offset in the data
            let start = if sector_offset >= 0 {
                offset + (sector_offset as usize)
            } else if sector_offset >= -(offset as i32) {
                offset - sector_offset.unsigned_abs() as usize
            } else {
                // Sector was before the start of the file, but BW's implementation seems to be okay
                // with this happening, so just return what we have
                return Err(MpqError::MalformedSectorTable);
            };

            let use_compression = cur_sector_size < sector_size && cur_sector_size < bytes_left;
            let sector_compressed =
                block.flags.contains(MpqBlockFlags::COMPRESSED) && use_compression;
            let sector_imploded = block.flags.contains(MpqBlockFlags::IMPLODED) && use_compression;

            let end = (start + cur_sector_size).clamp(start, self.data.len());
            let mut sector = Cow::from(&self.data[start..end]);

            if let Some(key) = encryption_key {
                let mut d = Decrypter::from_key_value(key.wrapping_add(i as u32));
                sector = Cow::from(d.decrypt_bytes(sector.as_ref()));
            }
            if sector_compressed {
                if sector.is_empty() {
                    return Err(MpqError::MalformedSectorTable);
                }
                sector = Cow::from(decompress_sector(sector.as_ref())?);
            }
            if sector_imploded {
                sector = Cow::from(explode_data(sector.as_ref())?);
            }

            // Some protectors will add extra data to the end of the file that extends past its
            // stated length. We truncate the last sector to the correct size in that case.
            let slice_len = sector.len().min(bytes_left);

            result.extend_from_slice(&sector[..slice_len]);

            // BW expects that every decompression will result in sectorSize bytes of data (except,
            // possibly, for the very last sector). This is never verified, however, which means map
            // protection schemes can compress less data. When reading it back out, BW will always
            // give sectorSize bytes anyway, so we need to pad the buffer in those cases.
            let is_last_sector = i == sector_table.len() - 2;
            if !is_last_sector && sector.len() < sector_size {
                result.resize(result.len() + (sector_size - sector.len()), 0);
                bytes_left -= sector_size.min(bytes_left);
            } else {
                bytes_left -= sector.len().min(bytes_left);
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
        let num_sectors = (block.size as usize - 1) / sector_size + 1;
        // NOTE(tec27): BW's implementation doesn't support the "SINGLE_UNIT" flag, so there are
        // less reasons to not have a sector table
        let has_sector_table = block
            .flags
            .intersects(MpqBlockFlags::COMPRESSED | MpqBlockFlags::IMPLODED);

        // There is one extra entry in the table, used purely for measuring the size of the
        // last section. Section N's size is calculated as:
        // offset(section n+1) - offset(section n)
        let num_sectors = num_sectors + 1;

        if has_sector_table {
            let absolute_offset = block
                .absolute_offset(&self.header)
                .unwrap_or(self.data.len());
            if absolute_offset + num_sectors * 4 >= self.data.len() {
                // The sector table extends past the end of the file
                return Err(MpqError::MalformedSectorTable);
            }

            let sector_table = match mpq_sector_table(
                &self.data[absolute_offset..],
                num_sectors,
                encryption_key,
            ) {
                Ok((_, table)) => Ok(table),
                Err(nom::Err::Error(e)) => Err(MpqError::ParserError(e.code)),
                Err(nom::Err::Failure(e)) => Err(MpqError::ParserError(e.code)),
                Err(nom::Err::Incomplete(_)) => {
                    // We're using complete parsers here so this should never happen
                    unreachable!()
                }
            }?;

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
        let result = mpq_hash_table(&LT[69637..69637 + (1024 * 16)]);

        assert!(matches!(result, Ok(_)));

        let (_, result) = result.unwrap();
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
        let result = mpq_block_table(&LT[86021..86021 + (4 * 16)]);

        assert!(matches!(result, Ok(_)));

        let (_, result) = result.unwrap();
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
}
