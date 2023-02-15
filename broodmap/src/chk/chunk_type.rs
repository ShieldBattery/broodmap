use std::fmt::{Display, Formatter};

pub type ChunkTag = [u8; 4];

/// Specifies how multiple entries of the same chunk type should be handled.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MultiChunkHandling {
    /// Only the last entry of this type is used.
    FullOverwrite,
    /// Later entries overwrite earlier entries, but leave earlier data intact if they are shorter.
    PartialOverwrite,
    /// All entries are included, behaving as if they were a single entry.
    Append,
}

/// A type of chunk in a StarCraft map file.
///
/// For a description of the different types and their data format, see the
/// [staredit.net wiki](http://www.staredit.net/wiki/index.php/Scenario.chk).
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ChunkType {
    /// Map type
    TYPE,
    /// Format version
    VER,
    /// Map version
    IVER,
    /// Map version (additional)
    IVE2,
    /// Verification code
    VCOD,
    /// StarEdit player types
    IOWN,
    /// StarCraft player types
    OWNR,
    /// Tileset
    ERA,
    /// Map dimensions
    DIM,
    /// Player races
    SIDE,
    /// StarCraft terrain
    MTXM,
    /// Player unit restrictions
    PUNI,
    /// Upgrade restrictions
    UPGR,
    /// Tech restrictions
    PTEC,
    /// Placed units
    UNIT,
    /// Isometric terrain
    ISOM,
    /// StarEdit terrain
    TILE,
    /// StarEdit sprites (doodads)
    DD2,
    /// StarCraft sprites
    THG2,
    /// Fog of war layer
    MASK,
    /// String data
    STR,
    /// String data (SC:R)
    STRx,
    /// Create units with properties slots
    UPRP,
    /// Create units with properties slots used
    UPUS,
    /// Locations
    MRGN,
    /// Triggers
    TRIG,
    /// Mission briefings
    MBRF,
    /// Scenario properties
    SPRP,
    /// Force settings
    FORC,
    /// WAV string indexes
    WAV,
    /// Unit settings
    UNIS,
    /// Upgrade settings
    UPGS,
    /// Tech settings
    TECS,
    /// Switch names
    SWNM,
    /// Player colors
    COLR,
    /// Player colors (SC:R)
    CRGB,
    /// Upgrade restrictions (BW)
    PUPx,
    /// Tech restrictions (BW)
    PTEx,
    /// Unit settings (BW)
    UNIx,
    /// Upgrade settings (BW)
    UPGx,
    /// Tech settings (BW)
    TECx,
    /// A chunk type without built-in handling
    Custom([u8; 4]),
}

impl ChunkType {
    /// Returns the minimum size of data for this chunk type. Data sections smaller than this size
    /// will be skipped.
    pub fn min_size(&self) -> Option<usize> {
        // These should generally match BW's behavior (but I took these from bw-chk for the moment,
        // so these should probably be double-checked at some point).
        match self {
            ChunkType::VER => Some(2),
            ChunkType::STR => Some(2),
            ChunkType::STRx => Some(4),
            ChunkType::ERA => Some(2),
            ChunkType::OWNR => Some(12),
            ChunkType::SIDE => Some(12),
            ChunkType::SPRP => Some(4),
            ChunkType::DIM => Some(4),
            ChunkType::UNIS => Some(4048),
            ChunkType::UNIx => Some(4168),
            _ => None,
        }
    }

    /// Returns the maximum size of data for this chunk type. Data sections larger than this size
    /// will be truncated. TODO(tec27): Does that match BW's behavior, or does it just fail?
    pub fn max_size(&self) -> Option<usize> {
        match self {
            ChunkType::ERA => Some(2),
            ChunkType::FORC => Some(20),
            ChunkType::OWNR => Some(12),
            ChunkType::SIDE => Some(12),
            ChunkType::SPRP => Some(4),
            ChunkType::DIM => Some(4),
            ChunkType::MTXM => Some(256 * 256 * 2),
            _ => None,
        }
    }

    pub fn multi_chunk_handling(&self) -> MultiChunkHandling {
        match self {
            ChunkType::MTXM => MultiChunkHandling::PartialOverwrite,
            ChunkType::STR => MultiChunkHandling::PartialOverwrite,
            ChunkType::STRx => MultiChunkHandling::PartialOverwrite,
            ChunkType::UNIT => MultiChunkHandling::Append,
            ChunkType::THG2 => MultiChunkHandling::Append,
            ChunkType::TRIG => MultiChunkHandling::Append,
            ChunkType::MBRF => MultiChunkHandling::Append,
            _ => MultiChunkHandling::FullOverwrite,
        }
    }
}

impl From<&ChunkTag> for ChunkType {
    fn from(value: &[u8; 4]) -> Self {
        use ChunkType::*;
        match value {
            b"TYPE" => TYPE,
            b"VER " => VER,
            b"IVER" => IVER,
            b"IVE2" => IVE2,
            b"VCOD" => VCOD,
            b"IOWN" => IOWN,
            b"OWNR" => OWNR,
            b"ERA " => ERA,
            b"DIM " => DIM,
            b"SIDE" => SIDE,
            b"MTXM" => MTXM,
            b"PUNI" => PUNI,
            b"UPGR" => UPGR,
            b"PTEC" => PTEC,
            b"UNIT" => UNIT,
            b"ISOM" => ISOM,
            b"TILE" => TILE,
            b"DD2 " => DD2,
            b"THG2" => THG2,
            b"MASK" => MASK,
            b"STR " => STR,
            b"STRx" => STRx,
            b"UPRP" => UPRP,
            b"UPUS" => UPUS,
            b"MRGN" => MRGN,
            b"TRIG" => TRIG,
            b"MBRF" => MBRF,
            b"SPRP" => SPRP,
            b"FORC" => FORC,
            b"WAV " => WAV,
            b"UNIS" => UNIS,
            b"UPGS" => UPGS,
            b"TECS" => TECS,
            b"SWNM" => SWNM,
            b"COLR" => COLR,
            b"CRGB" => CRGB,
            b"PUPx" => PUPx,
            b"PTEx" => PTEx,
            b"UNIx" => UNIx,
            b"UPGx" => UPGx,
            b"TECx" => TECx,
            tag => Custom(*tag),
        }
    }
}

impl From<ChunkTag> for ChunkType {
    fn from(value: ChunkTag) -> Self {
        (&value).into()
    }
}

impl From<ChunkType> for ChunkTag {
    fn from(value: ChunkType) -> Self {
        match value {
            ChunkType::TYPE => *b"TYPE",
            ChunkType::VER => *b"VER ",
            ChunkType::IVER => *b"IVER",
            ChunkType::IVE2 => *b"IVE2",
            ChunkType::VCOD => *b"VCOD",
            ChunkType::IOWN => *b"IOWN",
            ChunkType::OWNR => *b"OWNR",
            ChunkType::ERA => *b"ERA ",
            ChunkType::DIM => *b"DIM ",
            ChunkType::SIDE => *b"SIDE",
            ChunkType::MTXM => *b"MTXM",
            ChunkType::PUNI => *b"PUNI",
            ChunkType::UPGR => *b"UPGR",
            ChunkType::PTEC => *b"PTEC",
            ChunkType::UNIT => *b"UNIT",
            ChunkType::ISOM => *b"ISOM",
            ChunkType::TILE => *b"TILE",
            ChunkType::DD2 => *b"DD2 ",
            ChunkType::THG2 => *b"THG2",
            ChunkType::MASK => *b"MASK",
            ChunkType::STR => *b"STR ",
            ChunkType::STRx => *b"STRx",
            ChunkType::UPRP => *b"UPRP",
            ChunkType::UPUS => *b"UPUS",
            ChunkType::MRGN => *b"MRGN",
            ChunkType::TRIG => *b"TRIG",
            ChunkType::MBRF => *b"MBRF",
            ChunkType::SPRP => *b"SPRP",
            ChunkType::FORC => *b"FORC",
            ChunkType::WAV => *b"WAV ",
            ChunkType::UNIS => *b"UNIS",
            ChunkType::UPGS => *b"UPGS",
            ChunkType::TECS => *b"TECS",
            ChunkType::SWNM => *b"SWNM",
            ChunkType::COLR => *b"COLR",
            ChunkType::CRGB => *b"CRGB",
            ChunkType::PUPx => *b"PUPx",
            ChunkType::PTEx => *b"PTEx",
            ChunkType::UNIx => *b"UNIx",
            ChunkType::UPGx => *b"UPGx",
            ChunkType::TECx => *b"TECx",
            ChunkType::Custom(tag) => tag,
        }
    }
}

impl Display for ChunkType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ChunkType::TYPE => write!(f, "Map type (TYPE)"),
            ChunkType::VER => write!(f, "Format version (VER )"),
            ChunkType::IVER => write!(f, "Map version (IVER)"),
            ChunkType::IVE2 => write!(f, "Map version [additional] (IVE2)"),
            ChunkType::VCOD => write!(f, "Verification code (VCOD)"),
            ChunkType::IOWN => write!(f, "StarEdit player types (IOWN)"),
            ChunkType::OWNR => write!(f, "StarCraft player types (OWNR)"),
            ChunkType::ERA => write!(f, "Tileset (ERA )"),
            ChunkType::DIM => write!(f, "Map dimensions (DIM )"),
            ChunkType::SIDE => write!(f, "Player races (SIDE)"),
            ChunkType::MTXM => write!(f, "StarCraft terrain (MTXM)"),
            ChunkType::PUNI => write!(f, "Player unit restrictions (PUNI)"),
            ChunkType::UPGR => write!(f, "Upgrade restrictions (UPGR)"),
            ChunkType::PTEC => write!(f, "Tech restrictions (PTEC)"),
            ChunkType::UNIT => write!(f, "Placed units (UNIT)"),
            ChunkType::ISOM => write!(f, "Isometric terrain (ISOM)"),
            ChunkType::TILE => write!(f, "StarEdit terrain (TILE)"),
            ChunkType::DD2 => write!(f, "StarEdit sprites (doodads) (DD2 )"),
            ChunkType::THG2 => write!(f, "StarCraft sprites (THG2)"),
            ChunkType::MASK => write!(f, "Fog of war layer (MASK)"),
            ChunkType::STR => write!(f, "String data (STR )"),
            ChunkType::STRx => write!(f, "String data [SC:R] (STRx)"),
            ChunkType::UPRP => write!(f, "Create units with properties slots (UPRP)"),
            ChunkType::UPUS => write!(f, "Create units with properties slots used (UPUS)"),
            ChunkType::MRGN => write!(f, "Locations (MRGN)"),
            ChunkType::TRIG => write!(f, "Triggers (TRIG)"),
            ChunkType::MBRF => write!(f, "Mission briefings (MBRF)"),
            ChunkType::SPRP => write!(f, "Scenario properties (SPRP)"),
            ChunkType::FORC => write!(f, "Force settings (FORC)"),
            ChunkType::WAV => write!(f, "WAV string indexes (WAV )"),
            ChunkType::UNIS => write!(f, "Unit settings (UNIS)"),
            ChunkType::UPGS => write!(f, "Upgrade settings (UPGS)"),
            ChunkType::TECS => write!(f, "Tech settings (TECS)"),
            ChunkType::SWNM => write!(f, "Switch names (SWNM)"),
            ChunkType::COLR => write!(f, "Player colors (COLR)"),
            ChunkType::CRGB => write!(f, "Player colors [SC:R] (CRGB)"),
            ChunkType::PUPx => write!(f, "Upgrade restrictions [BW] (PUPx)"),
            ChunkType::PTEx => write!(f, "Tech restrictions [BW] (PTEx)"),
            ChunkType::UNIx => write!(f, "Unit settings [BW] (UNIx)"),
            ChunkType::UPGx => write!(f, "Upgrade settings [BW] (UPGx)"),
            ChunkType::TECx => write!(f, "Tech settings [BW] (TECx)"),
            ChunkType::Custom(tag) => write!(f, "Custom chunk ({})", String::from_utf8_lossy(tag)),
        }
    }
}
