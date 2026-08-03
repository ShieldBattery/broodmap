use std::fmt::{Display, Formatter};

use thiserror::Error;

const MIB: usize = 1024 * 1024;

/// Resource ceilings applied while parsing and extracting map data.
///
/// The defaults are intentionally finite and suitable for untrusted input. Callers loading data
/// from a trusted source may opt in to the format's historical permissiveness with
/// [`ResourceLimits::trusted`]. Network services must also limit request bodies before collecting
/// them into the byte slice passed to this crate.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub struct ResourceLimits {
    /// Maximum size of an MPQ archive passed to [`crate::Mpq::from_bytes`].
    pub max_map_bytes: usize,
    /// Maximum number of entries materialized for either MPQ metadata table.
    pub max_mpq_table_entries: usize,
    /// Maximum declared and extracted size of one file in an MPQ archive.
    pub max_mpq_file_bytes: usize,
    /// Maximum number of sectors used to represent one MPQ file.
    pub max_mpq_file_sectors: usize,
    /// Maximum size of raw CHK data passed to [`crate::Chk::from_bytes`].
    pub max_chk_bytes: usize,
    /// Maximum number of CHK headers visited, including jump chunks.
    pub max_chk_chunks: usize,
    /// Maximum retained chunks sharing one tag.
    pub max_chk_chunks_per_tag: usize,
    /// Maximum number of bytes produced by merging chunks with the same tag.
    pub max_merged_chunk_bytes: usize,
}

impl ResourceLimits {
    /// Limits for trusted local inputs, preserving the parser's previous effective behavior.
    ///
    /// This profile must not be used for network-provided or otherwise untrusted data.
    pub const fn trusted() -> Self {
        Self {
            max_map_bytes: usize::MAX,
            max_mpq_table_entries: usize::MAX,
            max_mpq_file_bytes: usize::MAX,
            max_mpq_file_sectors: usize::MAX,
            max_chk_bytes: usize::MAX,
            max_chk_chunks: usize::MAX,
            max_chk_chunks_per_tag: usize::MAX,
            max_merged_chunk_bytes: usize::MAX,
        }
    }
}

impl Default for ResourceLimits {
    fn default() -> Self {
        Self {
            max_map_bytes: 256 * MIB,
            max_mpq_table_entries: 512 * 1024,
            max_mpq_file_bytes: 64 * MIB,
            max_mpq_file_sectors: 128 * 1024,
            max_chk_bytes: 64 * MIB,
            max_chk_chunks: 64 * 1024,
            max_chk_chunks_per_tag: 16 * 1024,
            max_merged_chunk_bytes: 64 * MIB,
        }
    }
}

/// The resource whose configured limit was exceeded.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum Resource {
    MapBytes,
    MpqHashTableEntries,
    MpqBlockTableEntries,
    MpqFileBytes,
    MpqFileSectors,
    MpqSectorOutput,
    ChkBytes,
    ChkChunks,
    ChkChunksPerTag,
    MergedChunkBytes,
}

impl Display for Resource {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::MapBytes => "map bytes",
            Self::MpqHashTableEntries => "MPQ hash table entries",
            Self::MpqBlockTableEntries => "MPQ block table entries",
            Self::MpqFileBytes => "MPQ file bytes",
            Self::MpqFileSectors => "MPQ file sectors",
            Self::MpqSectorOutput => "MPQ sector output",
            Self::ChkBytes => "CHK bytes",
            Self::ChkChunks => "CHK chunks",
            Self::ChkChunksPerTag => "CHK chunks per tag",
            Self::MergedChunkBytes => "merged CHK bytes",
        })
    }
}

/// A deterministic resource bound or fallible allocation failed.
#[derive(Debug, Error)]
#[non_exhaustive]
pub enum ResourceLimitError {
    #[error("{resource} limit exceeded: observed {observed}, limit {limit}")]
    Exceeded {
        resource: Resource,
        observed: usize,
        limit: usize,
    },
    #[error("failed to reserve capacity for {requested} additional {resource}")]
    Allocation {
        resource: Resource,
        requested: usize,
    },
}

pub(crate) fn ensure_within(
    resource: Resource,
    observed: usize,
    limit: usize,
) -> Result<(), ResourceLimitError> {
    if observed > limit {
        Err(ResourceLimitError::Exceeded {
            resource,
            observed,
            limit,
        })
    } else {
        Ok(())
    }
}

pub(crate) fn allocation_error(resource: Resource, requested: usize) -> ResourceLimitError {
    ResourceLimitError::Allocation {
        resource,
        requested,
    }
}
