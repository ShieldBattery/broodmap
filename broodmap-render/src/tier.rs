//! Asset quality tiers and art style.
//!
//! SC:R ships terrain (and unit/sprite) art at three quality tiers, matched to the CASC
//! directory layout: `SD/` (original-resolution art, 32px tiles), `HD2/` (half-resolution
//! Remastered art, 64px tiles), and the Remastered art at the CASC root (128px tiles). See
//! `docs/render-design.md`, "Asset formats and tiers".

/// A quality tier of pre-rendered SC:R terrain/sprite art.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum AssetTier {
    /// Original 1.16.1-resolution art, re-packaged for Remastered. 32px tiles.
    Sd,
    /// Remastered art at half resolution. 64px tiles.
    Hd2,
    /// Remastered art at full resolution. 128px tiles.
    Hd,
}

impl AssetTier {
    /// The native tile size, in pixels, for this tier.
    pub fn tile_px(&self) -> u32 {
        match self {
            AssetTier::Sd => 32,
            AssetTier::Hd2 => 64,
            AssetTier::Hd => 128,
        }
    }

    /// The CASC path prefix for this tier's tileset directory (see `docs/render-design.md`).
    /// HD assets live at the storage root, so this is empty for [`AssetTier::Hd`].
    pub(crate) fn casc_prefix(&self) -> &'static str {
        match self {
            AssetTier::Sd => "SD/",
            AssetTier::Hd2 => "HD2/",
            AssetTier::Hd => "",
        }
    }
}

/// The asset pack an art style's graphics come from, matching the CASC directory layout: the
/// standard art, or the "StarCraft: Cartooned" art under `Carbot/` subdirectories.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Default)]
pub enum ArtPack {
    /// The standard art (original or Remastered).
    #[default]
    Standard,
    /// The StarCraft: Cartooned art (CASC directories name it `Carbot`). HD/HD2 only.
    Carbot,
}

impl ArtPack {
    /// The CASC path infix for this pack, inserted between the tier prefix and `TileSet/...`
    /// (e.g. `HD2/Carbot/TileSet/jungle.dds.vr4`).
    pub(crate) fn casc_infix(&self) -> &'static str {
        match self {
            ArtPack::Standard => "",
            ArtPack::Carbot => "Carbot/",
        }
    }
}

/// The art a caller wants rendered. This is the only rendering-quality knob: the concrete
/// [`AssetTier`] is always derived from the style plus the requested output size (see
/// [`crate::options::RenderOptions`]), because the (style, tier) pairings are fixed by what
/// assets actually exist — the original art only exists at SD, while the Remastered and
/// Cartooned art only exist at HD/HD2 (with HD2 being the same art at half resolution, HD only
/// worth fetching for very large outputs).
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
pub enum ArtStyle {
    /// The original, 1.16.1-style art (re-packaged by Remastered as the `SD` tier — the only
    /// resolution this art exists at).
    Original,
    /// Remastered art (HD or HD2, picked from the requested output size).
    #[default]
    Remastered,
    /// StarCraft: Cartooned art (the `Carbot/` asset pack). Ships the same HD/HD2 sizes as
    /// Remastered, with the tier picked the same way.
    Cartooned,
}

impl ArtStyle {
    /// The asset pack this style's graphics come from.
    pub(crate) fn pack(&self) -> ArtPack {
        match self {
            ArtStyle::Original | ArtStyle::Remastered => ArtPack::Standard,
            ArtStyle::Cartooned => ArtPack::Carbot,
        }
    }
}
