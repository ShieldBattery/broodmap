//! An implementation of the MPQ file format, for the features used by SCM/SCX files in StarCraft.
//! All features that are not used by the game are not implemented, and the implementation matches
//! the game's behavior as closely as possible (including bugs, which are often exploited by
//! so-called "map protectors" to prevent 3rd party editors from opening map files).

use nom::bytes::streaming::{tag, take};
use nom::combinator::{map, verify};
use nom::multi::many_till;
use nom::number::streaming::{le_i32, le_u16, le_u32};
use nom::sequence::tuple;
use nom::IResult;

const MPQ_MAGIC: &[u8] = b"MPQ\x1A";

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
    // NOTE(tec27): BW's implementation just cares that this is at least 32 bytes, it will never
    // read more than that. If this value is less than 32 bytes, it will continue searching for the
    // MPQ header later in the file.
    verify(le_u32, |size: &u32| *size >= 32)(input)
}

fn mpq_header_offset(input: &[u8]) -> IResult<&[u8], u32> {
    map(
        many_till(take(512usize), tuple((tag(MPQ_MAGIC), mpq_header_size))),
        |(takes, (_magic, _header_size))| (takes.len() * 512) as u32,
    )(input)
}

fn mpq_table_pos(min_pos: i32) -> impl Fn(&[u8]) -> IResult<&[u8], i32> {
    // This checks to ensure that the table is placed at the start of the file or after (but can't
    // validate that it is before the end of the file, since we may not know what that is yet)
    move |input| verify(le_i32, |pos: &i32| *pos >= min_pos)(input)
}

fn mpq_header(input: &[u8]) -> IResult<&[u8], MpqHeader> {
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

#[cfg(test)]
mod tests {
    use super::*;

    const LT: &[u8] = include_bytes!("../assets/lt.scm");
    const LT_OFFSET_FROM_START: &[u8] = include_bytes!("../assets/lt-offset-from-start.scm");

    #[test]
    fn headers() {
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
}
