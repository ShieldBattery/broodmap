//! An implementation of the MPQ file format, for the features used by SCM/SCX files in StarCraft.
//! All features that are not used by the game are not implemented, and the implementation matches
//! the game's behavior as closely as possible (including bugs, which are often exploited by
//! so-called "map protectors" to prevent 3rd party editors from opening map files).

use bitflags::bitflags;
use nom::combinator::{map, verify};
use nom::multi::{count, many0, many_till};
use nom::sequence::tuple;
use nom::IResult;

use thiserror::Error;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MpqHeader {
    /// The offset the header was found at, relative to the start of the input data.
    pub offset: u32,
    /// Power of two exponent specifying the number of 512-byte disk sectors in each logical sector
    /// in the archive. The size of each logical sector in the archive is 512 * 2^block_size.
    pub block_size: u16,
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
    fn hash_table_offset(&self) -> usize {
        assert!(self.hash_table_pos >= -(self.offset as i32));
        if self.hash_table_pos >= 0 {
            (self.offset as usize) + (self.hash_table_pos as usize)
        } else {
            (self.offset - self.hash_table_pos.abs() as u32) as usize
        }
    }

    /// Returns the absolute offset of the block table, from the beginning of the file.
    fn block_table_offset(&self) -> usize {
        assert!(self.block_table_pos >= -(self.offset as i32));
        if self.block_table_pos >= 0 {
            (self.offset as usize) + (self.block_table_pos as usize)
        } else {
            (self.offset - self.block_table_pos.abs() as u32) as usize
        }
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
        move |(block_size, hash_table_pos, block_table_pos, hash_table_size, block_table_size)| {
            MpqHeader {
                offset,
                block_size,
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
    fn new<'a>(key: &str) -> Self {
        Self {
            seed: 0xEEEE_EEEE,
            key: hash_str(key, MpqHashType::FileKey),
        }
    }

    fn decrypt(&mut self, value: u32) -> u32 {
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

    let mut decrypter = Decrypter::new("(hash table)");
    let r = many0(map(count(le_u32, 4), |entries: Vec<u32>| {
        assert_eq!(entries.len(), 4);
        let hash_a = decrypter.decrypt(entries[0]);
        let hash_b = decrypter.decrypt(entries[1]);
        let locale_platform = decrypter.decrypt(entries[2]);
        let block_index = decrypter.decrypt(entries[3]);

        MpqHashTableEntry {
            hash_a,
            hash_b,
            locale: (locale_platform & 0x0000_FFFF) as u16,
            platform: (locale_platform >> 16) as u16,
            block_index,
        }
    }))(input);

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

/// Parse the contents of an MPQ block table. This is a complete parser, it expects that all data
/// for the block table is present, and that the input begins at the start of the table and ends at
/// the end.
///
/// Note that BW's parser allows the block table to be shorter than expected at the end of the file
/// (so this does as well).
fn mpq_block_table(input: &[u8]) -> IResult<&[u8], Vec<MpqBlockTableEntry>> {
    use nom::number::complete::le_u32;

    let mut decrypter = Decrypter::new("(block table)");
    let r = many0(map(count(le_u32, 4), |entries: Vec<u32>| {
        assert_eq!(entries.len(), 4);

        MpqBlockTableEntry {
            offset: decrypter.decrypt(entries[0]) as i32,
            compressed_size: decrypter.decrypt(entries[1]),
            size: decrypter.decrypt(entries[2]),
            flags: MpqBlockFlags::from_bits_truncate(decrypter.decrypt(entries[3])),
        }
    }))(input);

    r
}

#[derive(Error, Debug)]
pub enum MpqError {
    #[error("header not found")]
    HeaderNotFound,
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

        let hash_table = {
            let hash_table_offset = header.hash_table_offset();
            let hash_table_size = ((header.hash_table_size as usize) * MPQ_HASH_TABLE_ENTRY_SIZE)
                .clamp(0, data.len() - hash_table_offset);
            let hash_data = &data[hash_table_offset..hash_table_offset + hash_table_size];
            match mpq_hash_table(hash_data) {
                Ok((_, hash_table)) => Ok(hash_table),
                Err(nom::Err::Error(e)) => Err(MpqError::ParserError(e.code)),
                Err(nom::Err::Failure(e)) => Err(MpqError::ParserError(e.code)),
                Err(nom::Err::Incomplete(_)) => {
                    panic!("incomplete returned from a complete parser")
                }
            }
        }?;

        let block_table = {
            let block_table_offset = header.block_table_offset();
            let block_table_size = ((header.block_table_size as usize)
                * MPQ_BLOCK_TABLE_ENTRY_SIZE)
                .clamp(0, data.len() - block_table_offset);
            let block_data = &data[block_table_offset..block_table_offset + block_table_size];
            match mpq_block_table(block_data) {
                Ok((_, block_table)) => Ok(block_table),
                Err(nom::Err::Error(e)) => Err(MpqError::ParserError(e.code)),
                Err(nom::Err::Failure(e)) => Err(MpqError::ParserError(e.code)),
                Err(nom::Err::Incomplete(_)) => {
                    panic!("incomplete returned from a complete parser")
                }
            }
        }?;

        Ok(Self {
            data,
            header,
            hash_table,
            block_table,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const LT: &[u8] = include_bytes!("../assets/lt.scm");
    const LT_OFFSET_FROM_START: &[u8] = include_bytes!("../assets/lt-offset-from-start.scm");
    const NEGATIVE_OFFSETS: &[u8] = include_bytes!("../assets/negativeoffsets.scx");

    #[test]
    fn header_normal() {
        let result = mpq_header(&LT[..32]);
        assert_eq!(
            result,
            Ok((
                &b""[..],
                MpqHeader {
                    offset: 0,
                    block_size: 3,
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
                    block_size: 3,
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
                    block_size: 3,
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
        assert!(matches!(result, Ok(_)));
    }

    #[test]
    fn from_bytes_invalid() {
        let bytes = b"MPQ\x1aDEAD";
        let result = Mpq::from_bytes(bytes);
        assert!(matches!(result, Err(MpqError::HeaderNotFound)));
    }
}
