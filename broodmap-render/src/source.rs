//! The asset data-source seam: `AssetRequest` describes *what* is needed (game-data knowledge
//! stays in this crate), `TilesetDataSource` fetches raw bytes for it. See
//! `docs/render-design.md`, "Data sources".

use std::collections::HashMap;
use thiserror::Error;

use broodmap::chk::tileset::Tileset;

use crate::tier::{ArtPack, AssetTier};

/// Which of BW's `.dat` stat tables an [`AssetRequest::Dat`] refers to. All of them live under
/// `arr/` in the CASC catalog.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum DatKind {
    /// `arr/units.dat` — per-unit-type stats (including the flingy each unit uses).
    Units,
    /// `arr/flingy.dat` — movement types; maps a flingy to a sprite.
    Flingy,
    /// `arr/sprites.dat` — maps a sprite to an image.
    Sprites,
    /// `arr/images.dat` — per-image rendering metadata.
    Images,
}

impl DatKind {
    /// The filename stem of this table (`"units"`, `"flingy"`, ...).
    fn stem(&self) -> &'static str {
        match self {
            DatKind::Units => "units",
            DatKind::Flingy => "flingy",
            DatKind::Sprites => "sprites",
            DatKind::Images => "images",
        }
    }
}

/// A request for a specific piece of SC:R asset data, keyed by what's needed rather than by
/// path. `Eq + Hash` so it can key caches (and `MemorySource`'s prefetch map) directly.
///
/// `Hash` is implemented by hand rather than derived: `broodmap::chk::tileset::Tileset` doesn't
/// implement `Hash` itself, so this hashes its (stable, explicitly-numbered) discriminant
/// instead.
#[derive(Debug, Clone, Eq, PartialEq)]
#[non_exhaustive]
pub enum AssetRequest {
    /// A tileset's CV5 tile-group table (shared by every tier and art pack).
    Cv5(Tileset),
    /// A tileset's pre-rendered megatile textures at a given quality tier, from a given art
    /// pack.
    TilesetDds(Tileset, AssetTier, ArtPack),
    /// One of BW's `.dat` stat tables (`arr/units.dat` and friends), needed to resolve a placed
    /// unit or THG2 sprite to the image whose art should be drawn.
    Dat(DatKind),
    /// `images.rel`, the per-image art redirection table.
    ImagesRel,
    /// A single image's `.anim` art container at a given tier/pack.
    ///
    /// [`AssetTier::Sd`] doesn't have per-image files at all: all SD art lives in one bundled
    /// `SD/mainSD.anim`, so the renderer never issues an SD request through this variant — it
    /// uses [`AssetRequest::MainSdAnim`] instead. The `Sd` -> `"SD/mainSD.anim"` path mapping
    /// stays on [`Self::casc_path`] for compatibility (a caller matching on `tier` alone still
    /// gets the right path).
    Anim {
        image_id: u16,
        tier: AssetTier,
        pack: ArtPack,
    },
    /// The single bundled SD art container (`SD/mainSD.anim`): every image's SD art lives in
    /// this one file, looked up by image id (see [`broodmap_formats::MainSdAnim::entry`]). Unlike
    /// [`AssetRequest::Anim`], there's no per-image or per-pack variation to key on — SD has no
    /// art packs, and the image id only indexes *into* this one file rather than selecting a
    /// path.
    MainSdAnim,
    /// The image-id -> GRP filename table (`arr/images.tbl`), needed (together with
    /// `images.dat`'s `grp` column) to locate the classic GRP file whose header supplies an SD
    /// (`ArtStyle::Original`) render's true canvas dimensions — see [`AssetRequest::Grp`] and
    /// `broodmap_formats::grp`'s module docs for why `mainSD.anim`'s own (always-zero) declared
    /// canvas can't be used directly.
    ImagesTbl,
    /// A classic (pre-Remastered) GRP file, fetched in SD renders solely for its 6-byte header
    /// (see [`broodmap_formats::parse_grp_header`]) — never its frame table or pixel data, which
    /// `mainSD.anim` already supplies pre-decoded. `path` is the `images.tbl` string verbatim,
    /// backslash path separators and all (e.g. `"terran\\marine.grp"`); [`Self::casc_path`]
    /// converts it to the forward-slash CASC path.
    Grp { path: String },
}

impl std::hash::Hash for AssetRequest {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            AssetRequest::Cv5(tileset) => {
                0u8.hash(state);
                tileset_discriminant(*tileset).hash(state);
            }
            AssetRequest::TilesetDds(tileset, tier, pack) => {
                1u8.hash(state);
                tileset_discriminant(*tileset).hash(state);
                tier.hash(state);
                pack.hash(state);
            }
            AssetRequest::Dat(kind) => {
                2u8.hash(state);
                kind.hash(state);
            }
            AssetRequest::ImagesRel => {
                3u8.hash(state);
            }
            AssetRequest::Anim {
                image_id,
                tier,
                pack,
            } => {
                4u8.hash(state);
                image_id.hash(state);
                tier.hash(state);
                pack.hash(state);
            }
            AssetRequest::MainSdAnim => {
                5u8.hash(state);
            }
            AssetRequest::ImagesTbl => {
                6u8.hash(state);
            }
            AssetRequest::Grp { path } => {
                7u8.hash(state);
                path.hash(state);
            }
        }
    }
}

/// `Tileset`'s variants have explicit, stable discriminants (0..=7); used as a `Hash`-able
/// stand-in since the type itself doesn't implement `Hash`.
fn tileset_discriminant(tileset: Tileset) -> u8 {
    tileset as u8
}

impl AssetRequest {
    /// The case-preserved SC:R CASC catalog path for this request, using forward slashes (e.g.
    /// `"TileSet/jungle.cv5"`, `"SD/TileSet/jungle.dds.vr4"`,
    /// `"HD2/Carbot/TileSet/jungle.dds.vr4"`).
    pub fn casc_path(&self) -> String {
        match self {
            AssetRequest::Cv5(tileset) => format!("TileSet/{}.cv5", tileset_stem(*tileset)),
            AssetRequest::TilesetDds(tileset, tier, pack) => format!(
                "{}{}TileSet/{}.dds.vr4",
                tier.casc_prefix(),
                pack.casc_infix(),
                tileset_stem(*tileset)
            ),
            AssetRequest::Dat(kind) => format!("arr/{}.dat", kind.stem()),
            AssetRequest::ImagesRel => "images.rel".to_string(),
            // NOTE the pack infix sits *inside* `anim/` here (`HD2/anim/Carbot/main_005.anim`),
            // unlike tilesets where it sits before the directory (`HD2/Carbot/TileSet/...`).
            // Both spellings are verified against a real install's catalog.
            AssetRequest::Anim {
                image_id,
                tier,
                pack,
            } => match tier {
                AssetTier::Sd => "SD/mainSD.anim".to_string(),
                _ => format!(
                    "{}anim/{}main_{:03}.anim",
                    tier.casc_prefix(),
                    pack.casc_infix(),
                    image_id
                ),
            },
            AssetRequest::MainSdAnim => "SD/mainSD.anim".to_string(),
            AssetRequest::ImagesTbl => "arr/images.tbl".to_string(),
            AssetRequest::Grp { path } => format!("unit/{}", path.replace('\\', "/")),
        }
    }
}

/// The filename stem (without extension) used for a tileset's asset files.
fn tileset_stem(tileset: Tileset) -> &'static str {
    match tileset {
        Tileset::Badlands => "badlands",
        Tileset::SpacePlatform => "platform",
        Tileset::Installation => "install",
        Tileset::Ashworld => "ashworld",
        Tileset::Jungle => "jungle",
        Tileset::Desert => "desert",
        Tileset::Arctic => "ice",
        Tileset::Twilight => "twilight",
    }
}

/// Errors returned by a [`TilesetDataSource`]. Deliberately minimal and WASM-safe (no
/// non-`Send`/non-portable types), since sources may run in a browser worker.
#[derive(Error, Debug, Clone, Eq, PartialEq)]
pub enum SourceError {
    /// The requested asset doesn't exist in this source.
    #[error("asset not found")]
    NotFound,
    /// Any other failure reading the asset (I/O failure, transport error, etc), stringified so
    /// the error type stays simple and portable.
    #[error("I/O error: {0}")]
    Io(String),
}

/// A synchronous source of raw asset bytes, keyed by [`AssetRequest`]. All game-data knowledge
/// (paths, tiers, filenames) lives in this crate; implementors are dumb byte fetchers. See
/// `docs/render-design.md`, "Data sources".
///
/// Reads return `Arc<[u8]>` so sources holding assets in memory (notably [`MemorySource`],
/// where `.dds.vr4` files can be tens of MB) can hand them out without copying.
pub trait TilesetDataSource {
    /// Reads the bytes for `req`, or `SourceError::NotFound` if this source doesn't have it.
    fn read(&self, req: &AssetRequest) -> Result<std::sync::Arc<[u8]>, SourceError>;
}

/// A prefilled, in-memory [`TilesetDataSource`]. The primary WASM pattern: the trait stays
/// sync while browser fetches are async, so callers prefetch into a `MemorySource` and then
/// render synchronously. Reads are zero-copy (shared `Arc`s).
#[derive(Debug, Clone, Default)]
pub struct MemorySource {
    assets: HashMap<AssetRequest, std::sync::Arc<[u8]>>,
}

impl MemorySource {
    /// Creates an empty source.
    pub fn new() -> Self {
        Self::default()
    }

    /// Inserts (or replaces) the bytes for `req` (accepts a `Vec<u8>`, boxed slice, or
    /// pre-shared `Arc<[u8]>`), returning any previous value.
    pub fn insert(
        &mut self,
        req: AssetRequest,
        bytes: impl Into<std::sync::Arc<[u8]>>,
    ) -> Option<std::sync::Arc<[u8]>> {
        self.assets.insert(req, bytes.into())
    }
}

impl<B: Into<std::sync::Arc<[u8]>>> FromIterator<(AssetRequest, B)> for MemorySource {
    fn from_iter<T: IntoIterator<Item = (AssetRequest, B)>>(iter: T) -> Self {
        Self {
            assets: iter.into_iter().map(|(req, b)| (req, b.into())).collect(),
        }
    }
}

impl TilesetDataSource for MemorySource {
    fn read(&self, req: &AssetRequest) -> Result<std::sync::Arc<[u8]>, SourceError> {
        self.assets.get(req).cloned().ok_or(SourceError::NotFound)
    }
}

/// A [`TilesetDataSource`] backed by a plain directory of extracted files (e.g. a CascView
/// export, or test fixtures), read via `std::fs`. Not available on `wasm32-unknown-unknown`.
#[cfg(feature = "fs")]
pub struct DirSource {
    root: std::path::PathBuf,
}

#[cfg(feature = "fs")]
impl DirSource {
    /// Creates a source rooted at `root`, mirroring the CASC catalog's directory layout
    /// underneath it (e.g. `root/TileSet/jungle.cv5`).
    pub fn new(root: impl Into<std::path::PathBuf>) -> Self {
        Self { root: root.into() }
    }
}

#[cfg(feature = "fs")]
impl TilesetDataSource for DirSource {
    fn read(&self, req: &AssetRequest) -> Result<std::sync::Arc<[u8]>, SourceError> {
        let path = req.casc_path();
        match std::fs::read(self.root.join(&path)) {
            Ok(bytes) => Ok(bytes.into()),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                // CascView-style extractions sometimes lowercase directory names; retry once
                // with `TileSet` lowered before giving up. On case-insensitive filesystems
                // (Windows, default macOS) this never matters, but it does on Linux.
                let lowered = path.replace("TileSet", "tileset");
                if lowered != path {
                    match std::fs::read(self.root.join(&lowered)) {
                        Ok(bytes) => return Ok(bytes.into()),
                        Err(e2) if e2.kind() == std::io::ErrorKind::NotFound => {
                            return Err(SourceError::NotFound);
                        }
                        Err(e2) => return Err(SourceError::Io(e2.to_string())),
                    }
                }
                Err(SourceError::NotFound)
            }
            Err(e) => Err(SourceError::Io(e.to_string())),
        }
    }
}

/// A [`TilesetDataSource`] backed by a SC:R install (or CDN) via `broodcasc`.
///
/// Generic over the storage provider so it can wrap either a local install
/// (`broodcasc::io::FsProvider`, the default) or another `broodcasc::io::StorageProvider`
/// implementation (e.g. a CDN-backed one).
#[cfg(feature = "casc")]
pub struct CascSource<P: broodcasc::io::StorageProvider = broodcasc::io::FsProvider> {
    storage: broodcasc::Storage<P>,
}

#[cfg(feature = "casc")]
impl CascSource<broodcasc::io::FsProvider> {
    /// Opens a SC:R install directory (the one containing `.build.info`) as a data source.
    pub fn open(dir: impl Into<std::path::PathBuf>) -> Result<Self, SourceError> {
        let storage = broodcasc::Storage::open(dir).map_err(map_casc_error)?;
        Ok(Self { storage })
    }
}

#[cfg(feature = "casc")]
impl<P: broodcasc::io::StorageProvider> CascSource<P> {
    /// Wraps an already-opened `broodcasc::Storage`.
    pub fn new(storage: broodcasc::Storage<P>) -> Self {
        Self { storage }
    }
}

#[cfg(feature = "casc")]
impl<P: broodcasc::io::StorageProvider> TilesetDataSource for CascSource<P> {
    fn read(&self, req: &AssetRequest) -> Result<std::sync::Arc<[u8]>, SourceError> {
        self.storage
            .read_file(&req.casc_path())
            .map(Into::into)
            .map_err(map_casc_error)
    }
}

#[cfg(feature = "casc")]
fn map_casc_error(err: broodcasc::CascError) -> SourceError {
    match err {
        broodcasc::CascError::NotFound(_) | broodcasc::CascError::NotInstalled(_) => {
            SourceError::NotFound
        }
        other => SourceError::Io(other.to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use broodmap::chk::tileset::Tileset;

    #[test]
    fn cv5_path_matches_expected_layout() {
        assert_eq!(
            AssetRequest::Cv5(Tileset::Jungle).casc_path(),
            "TileSet/jungle.cv5"
        );
        assert_eq!(
            AssetRequest::Cv5(Tileset::Arctic).casc_path(),
            "TileSet/ice.cv5"
        );
    }

    #[test]
    fn tileset_dds_path_includes_tier_prefix_and_pack_infix() {
        use crate::tier::ArtPack;

        assert_eq!(
            AssetRequest::TilesetDds(Tileset::Jungle, AssetTier::Sd, ArtPack::Standard).casc_path(),
            "SD/TileSet/jungle.dds.vr4"
        );
        assert_eq!(
            AssetRequest::TilesetDds(Tileset::Jungle, AssetTier::Hd2, ArtPack::Standard)
                .casc_path(),
            "HD2/TileSet/jungle.dds.vr4"
        );
        assert_eq!(
            AssetRequest::TilesetDds(Tileset::Jungle, AssetTier::Hd, ArtPack::Standard).casc_path(),
            "TileSet/jungle.dds.vr4"
        );
        // Cartooned: pack infix goes between the tier prefix and TileSet/ (verified against a
        // real install's catalog: Carbot/TileSet/... and HD2/Carbot/TileSet/...).
        assert_eq!(
            AssetRequest::TilesetDds(Tileset::Jungle, AssetTier::Hd, ArtPack::Carbot).casc_path(),
            "Carbot/TileSet/jungle.dds.vr4"
        );
        assert_eq!(
            AssetRequest::TilesetDds(Tileset::Jungle, AssetTier::Hd2, ArtPack::Carbot).casc_path(),
            "HD2/Carbot/TileSet/jungle.dds.vr4"
        );
    }

    /// Verified against a real SC:R install's catalog listing.
    #[test]
    fn anim_dat_and_rel_paths_match_the_real_catalog() {
        use crate::tier::ArtPack;

        let anim = |image_id, tier, pack| {
            AssetRequest::Anim {
                image_id,
                tier,
                pack,
            }
            .casc_path()
        };

        assert_eq!(
            anim(5, AssetTier::Hd, ArtPack::Standard),
            "anim/main_005.anim"
        );
        assert_eq!(
            anim(5, AssetTier::Hd2, ArtPack::Standard),
            "HD2/anim/main_005.anim"
        );
        // The Carbot infix goes *inside* anim/, unlike the tileset layout.
        assert_eq!(
            anim(899, AssetTier::Hd, ArtPack::Carbot),
            "anim/Carbot/main_899.anim"
        );
        assert_eq!(
            anim(955, AssetTier::Hd2, ArtPack::Carbot),
            "HD2/anim/Carbot/main_955.anim"
        );
        // SD art is one bundled file, regardless of image ID or pack. The renderer requests it
        // via `AssetRequest::MainSdAnim` rather than `Anim { tier: Sd, .. }`, but the `Anim`
        // path mapping is kept for compatibility.
        assert_eq!(anim(5, AssetTier::Sd, ArtPack::Standard), "SD/mainSD.anim");
        assert_eq!(anim(900, AssetTier::Sd, ArtPack::Carbot), "SD/mainSD.anim");
        assert_eq!(AssetRequest::MainSdAnim.casc_path(), "SD/mainSD.anim");

        assert_eq!(AssetRequest::ImagesTbl.casc_path(), "arr/images.tbl");
        assert_eq!(
            AssetRequest::Grp {
                path: "terran\\marine.grp".to_string(),
            }
            .casc_path(),
            "unit/terran/marine.grp"
        );
        assert_eq!(
            AssetRequest::Grp {
                path: "neutral\\geyShad.grp".to_string(),
            }
            .casc_path(),
            "unit/neutral/geyShad.grp"
        );

        assert_eq!(
            AssetRequest::Dat(DatKind::Units).casc_path(),
            "arr/units.dat"
        );
        assert_eq!(
            AssetRequest::Dat(DatKind::Flingy).casc_path(),
            "arr/flingy.dat"
        );
        assert_eq!(
            AssetRequest::Dat(DatKind::Sprites).casc_path(),
            "arr/sprites.dat"
        );
        assert_eq!(
            AssetRequest::Dat(DatKind::Images).casc_path(),
            "arr/images.dat"
        );
        assert_eq!(AssetRequest::ImagesRel.casc_path(), "images.rel");
    }

    /// The hand-written `Hash` impl must agree with `Eq` across every variant (a mismatch would
    /// silently break `MemorySource`'s prefetch map and the renderer's caches).
    #[test]
    fn hash_agrees_with_eq_across_variants() {
        use crate::tier::ArtPack;
        use std::collections::HashSet;

        let requests = [
            AssetRequest::Cv5(Tileset::Jungle),
            AssetRequest::Cv5(Tileset::Desert),
            AssetRequest::TilesetDds(Tileset::Jungle, AssetTier::Hd2, ArtPack::Standard),
            AssetRequest::TilesetDds(Tileset::Jungle, AssetTier::Hd2, ArtPack::Carbot),
            AssetRequest::Dat(DatKind::Units),
            AssetRequest::Dat(DatKind::Images),
            AssetRequest::ImagesRel,
            AssetRequest::Anim {
                image_id: 5,
                tier: AssetTier::Hd2,
                pack: ArtPack::Standard,
            },
            AssetRequest::Anim {
                image_id: 6,
                tier: AssetTier::Hd2,
                pack: ArtPack::Standard,
            },
            AssetRequest::Anim {
                image_id: 5,
                tier: AssetTier::Hd,
                pack: ArtPack::Standard,
            },
            AssetRequest::Anim {
                image_id: 5,
                tier: AssetTier::Hd2,
                pack: ArtPack::Carbot,
            },
            AssetRequest::MainSdAnim,
            AssetRequest::ImagesTbl,
            AssetRequest::Grp {
                path: "terran\\marine.grp".to_string(),
            },
            AssetRequest::Grp {
                path: "neutral\\geyShad.grp".to_string(),
            },
        ];

        let set: HashSet<&AssetRequest> = requests.iter().collect();
        assert_eq!(
            set.len(),
            requests.len(),
            "distinct requests must not alias"
        );
        for req in &requests {
            assert!(set.contains(req));
        }
    }

    #[test]
    fn memory_source_round_trips_and_reports_not_found() {
        let mut source = MemorySource::new();
        let req = AssetRequest::Cv5(Tileset::Badlands);
        assert_eq!(source.read(&req), Err(SourceError::NotFound));

        source.insert(req.clone(), vec![1, 2, 3]);
        assert_eq!(source.read(&req).unwrap().as_ref(), &[1, 2, 3]);
    }

    #[test]
    fn memory_source_from_iterator() {
        let req = AssetRequest::Cv5(Tileset::Desert);
        let source: MemorySource = [(req.clone(), vec![9u8])].into_iter().collect();
        assert_eq!(source.read(&req).unwrap().as_ref(), &[9u8]);
    }

    #[cfg(feature = "fs")]
    #[test]
    fn dir_source_reads_exact_path_and_falls_back_to_lowercase() {
        use std::time::{SystemTime, UNIX_EPOCH};

        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let root = std::env::temp_dir().join(format!(
            "broodmap-render-dirsource-test-{}-{nonce}",
            std::process::id()
        ));
        std::fs::create_dir_all(root.join("TileSet")).unwrap();
        std::fs::write(root.join("TileSet").join("jungle.cv5"), b"exact").unwrap();

        let source = DirSource::new(&root);
        let req = AssetRequest::Cv5(Tileset::Jungle);
        assert_eq!(source.read(&req).unwrap().as_ref(), b"exact");

        let missing = AssetRequest::Cv5(Tileset::Desert);
        assert_eq!(source.read(&missing), Err(SourceError::NotFound));

        // Now test the lowercase fallback with a separate root that only has "tileset".
        let root2 = std::env::temp_dir().join(format!(
            "broodmap-render-dirsource-test2-{}-{nonce}",
            std::process::id()
        ));
        std::fs::create_dir_all(root2.join("tileset")).unwrap();
        std::fs::write(root2.join("tileset").join("jungle.cv5"), b"lowered").unwrap();
        let source2 = DirSource::new(&root2);
        assert_eq!(source2.read(&req).unwrap().as_ref(), b"lowered");

        std::fs::remove_dir_all(&root).unwrap();
        std::fs::remove_dir_all(&root2).unwrap();
    }
}
