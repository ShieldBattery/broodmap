//! An implementation of the MPQ file format, for the features used by SCM/SCX files in StarCraft.
//! All features that are not used by the game are not implemented, and the implementation matches
//! the game's behavior as closely as possible (including bugs, which are often exploited by
//! so-called "map protectors" to prevent 3rd party editors from opening map files).

use bitflags::bitflags;
use nom::combinator::{map, verify};
use nom::multi::{count, many0, many_till};
use nom::sequence::tuple;
use nom::IResult;

#[derive(Clone, Debug, PartialEq, Eq)]
struct MpqHeader {
    /// The offset the header was found at, relative to the start of the input data.
    offset: u32,
    /// Power of two exponent specifying the number of 512-byte disk sectors in each logical sector
    /// in the archive. The size of each logical sector in the archive is 512 * 2^block_size.
    block_size: u16,
    /// Offset to the beginning of the hash table, relative to the start of the MPQ header. NOTE:
    /// This may be behind you! The BW implementation handles negative offsets here.
    hash_table_pos: i32,
    /// Offset to the beginning of the block table, relative to the start of the MPQ header. NOTE:
    /// This may be behind you! The BW implementation handles negative offsets here.
    block_table_pos: i32,
    /// Number of entries in the hash table. Must be a power of 2, and must be less than 2^16.
    hash_table_size: u32,
    /// Number of entries in the block table.
    block_table_size: u32,
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
    let mut seed_a: u32 = 0x7FED_7FED;
    let mut seed_b: u32 = 0xEEEE_EEEE;

    for c in str.chars() {
        let c = c.to_ascii_uppercase();
        seed_a = CRYPT_TABLE[hash_type.to_crypt_table_multiplier() * 256 + (c as usize)]
            ^ (seed_a.wrapping_add(seed_b));
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

#[derive(Clone, Debug, PartialEq, Eq)]
struct MpqHashTableEntry {
    /// The hash of the full file name with constant A
    hash_a: u32,
    /// The hash of the full file name with constant B
    hash_b: u32,
    /// The language of the file (uses Windows LANGID values). 0 indicates the default language
    /// (American English), and is used if there are not language-specific versions for the current
    /// locale.
    locale: u16,
    /// The platform the file is used for. 0 indicates the default platform. No other values are
    /// current known.
    platform: u16,
    ///
    block_index: u32,
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

bitflags! {
    struct MpqBlockFlags: u32 {
        const IMPLODED = 0x0000_0100;
        const COMPRESSED = 0x0000_0200;
        const ENCRYPTED = 0x0001_0000;
        const ADJUSTED_KEY = 0x0002_0000;
        const DELETED = 0x0200_0000;
        const EXISTS = 0x8000_0000;
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct MpqBlockTableEntry {
    /// Offset to the beginning of the file data, relative to the start of the MPQ header. NOTE:
    /// This may be behind you! The BW implementation handles negative offsets here.
    offset: i32,
    /// The size of the compressed file data
    compressed_size: u32,
    /// The size of the uncompressed file data
    size: u32,
    /// Information about how the file is stored
    flags: MpqBlockFlags,
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

#[cfg(test)]
mod tests {
    use super::*;

    const LT: &[u8] = include_bytes!("../assets/lt.scm");
    const LT_OFFSET_FROM_START: &[u8] = include_bytes!("../assets/lt-offset-from-start.scm");
    const NEGATIVE_OFFSETS: &[u8] = include_bytes!("../assets/negativeoffsets.scx");

    #[test]
    fn header_normal() {
        assert_eq!(
            mpq_header(&LT[..32]),
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
    }

    #[test]
    fn header_offset() {
        assert_eq!(
            mpq_header(&LT_OFFSET_FROM_START[..544]),
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
    }

    #[test]
    fn header_negative_table_pos() {
        assert_eq!(
            mpq_header(&NEGATIVE_OFFSETS[..0x4220]),
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
}
