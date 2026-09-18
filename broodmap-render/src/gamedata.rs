//! The `.dat`/`.rel` game-data tables and the unit/sprite -> image resolution chain they encode.
//!
//! Drawing a placed unit needs to know which `.anim` file holds its art, and BW spreads that
//! answer across four tables plus a redirect list:
//!
//! ```text
//! unit ID --units.dat--> flingy --flingy.dat--> sprite --sprites.dat--> image
//!                                                                        |
//!                                                        images.rel (art redirect)
//!                                                                        v
//!                                                                  anim image ID
//! ```
//!
//! THG2 entries start partway along that chain: "pure sprite" entries (doodads) are `sprites.dat`
//! IDs, so they skip straight to the `sprites.dat` step, while "unit sprite" entries are unit IDs
//! and take the full chain (matching how BW and `bw-chk` interpret the THG2 flags).
//!
//! A fifth table, `images.tbl` (an image id -> classic-GRP-filename string table), is also loaded
//! here: it's not part of the unit/sprite resolution chain above, but SD (`ArtStyle::Original`)
//! renders need it (with `images.dat`'s `grp` column) to locate the classic GRP whose header
//! supplies the true canvas dimensions `mainSD.anim` itself can't — see [`GameData::grp_path`]
//! and `broodmap_formats::grp`'s module docs.

use std::borrow::Cow;
use std::collections::HashMap;

use broodmap_formats::{
    FlingyDat, ImagesDat, ImagesRel, SpritesDat, UnitsDat, parse_flingy_dat, parse_images_dat,
    parse_images_rel, parse_sprites_dat, parse_tbl, parse_units_dat,
};

use crate::error::RenderError;
use crate::source::{AssetRequest, DatKind, TilesetDataSource};
use crate::tier::{ArtPack, AssetTier};

/// The unit ID of a start location. Never a real drawable unit: the renderer handles it
/// separately (see [`crate::StartLocations`]).
pub(crate) const UNIT_ID_START_LOCATION: u16 = 214;

/// The image ID of the in-game start-location graphic.
///
/// The Carbot art pack does not ship this image, so any request for it must fall back to the
/// standard pack — see [`anim_request`].
pub(crate) const IMAGE_ID_START_LOCATION: u16 = 588;

/// The three mineral-field unit IDs (Mineral Field Type 1/2/3), which pick their frame from the
/// placed resource amount rather than a facing direction.
pub(crate) const UNIT_IDS_MINERAL_FIELD: [u16; 3] = [176, 177, 178];

/// The Vespene Geyser unit ID, whose frame is the map's tileset.
pub(crate) const UNIT_ID_VESPENE_GEYSER: u16 = 188;

/// The critter unit IDs, verified against `bw-chk`'s `units.js` GRP table and its `isCritter`
/// predicate (`index.js`): 89 Rhynadon (`neutral\bcritter.grp`), 90 Bengalaas
/// (`neutral\jcritter.grp`), 93 Scantid, 94 Kakaru, 95 Ragnasaur, 96 Ursadon. IDs 91/92 in
/// between are unused units, deliberately not included.
pub(crate) const UNIT_IDS_CRITTER: [u16; 6] = [89, 90, 93, 94, 95, 96];

/// The `units.dat` `special_ability_flags` bit meaning "this unit is a building".
const SPECIAL_ABILITY_FLAG_BUILDING: u32 = 0x0000_0001;

/// `images.dat`'s raw `render_style` code for BW's "shadow" draw function (see
/// [`broodmap_formats::ImageEntry::render_style`]'s docs). Used by
/// [`GameData::shadow_image_pre_redirect`] as a belt-and-suspenders gate on the `images.rel`
/// shadow table (see [`GameData::build_shadow_images`]).
const RENDER_STYLE_SHADOW: u8 = 10;

/// Builds the [`AssetRequest`] for an image's art at a given tier/pack, applying the Carbot
/// start-location quirk.
///
/// [`AssetTier::Sd`] always resolves to [`AssetRequest::MainSdAnim`] — the single bundled
/// `SD/mainSD.anim` container every SD image's art lives in — regardless of `image_id`/`pack`
/// (checked before the Carbot fallback below, which only concerns HD-family paths).
///
/// The Cartooned art pack ships no `main_588.anim`, so the game falls back to the standard
/// pack's start-location graphic there; requesting the Carbot path would just 404. Applying the
/// fallback here (at request-construction time) keeps prefetch lists and render-time reads in
/// agreement.
pub(crate) fn anim_request(image_id: u16, tier: AssetTier, pack: ArtPack) -> AssetRequest {
    if tier == AssetTier::Sd {
        return AssetRequest::MainSdAnim;
    }
    let pack = if image_id == IMAGE_ID_START_LOCATION {
        ArtPack::Standard
    } else {
        pack
    };
    AssetRequest::Anim {
        image_id,
        tier,
        pack,
    }
}

/// Whether `unit_id` is a mineral field or a vespene geyser.
pub(crate) fn is_resource(unit_id: u16) -> bool {
    UNIT_IDS_MINERAL_FIELD.contains(&unit_id) || unit_id == UNIT_ID_VESPENE_GEYSER
}

/// Whether `unit_id` is one of the six critters.
pub(crate) fn is_critter(unit_id: u16) -> bool {
    UNIT_IDS_CRITTER.contains(&unit_id)
}

/// The parsed `.dat`/`.rel` tables a preview render resolves unit and sprite art through.
///
/// Loading is a one-shot, whole-file affair (these are small, static files), so this is cheap to
/// build once and reuse across renders.
#[derive(Debug, Clone, Default)]
pub struct GameData {
    units: UnitsDat,
    flingy: FlingyDat,
    sprites: SpritesDat,
    images: ImagesDat,
    rel: ImagesRel,
    /// Parent (owner) image ID -> its shadow image ID, inverted from `images.rel`'s type-8
    /// shadow records at construction time (see [`Self::build_shadow_images`]). Built once and
    /// reused rather than scanned per lookup since it's derived from a whole-file table that
    /// doesn't change after load.
    shadow_images: HashMap<u16, u16>,
    /// Every `images.tbl` entry, decoded and owned up front (`Tbl<'a>` itself borrows from the
    /// source bytes, which don't outlive a single `load` call, so this can't just hold a `Tbl`).
    /// Indexed 0-based, matching `Tbl::get`; [`Self::grp_path`] applies `images.dat`'s 1-based
    /// `grp` column offset.
    images_tbl: Vec<String>,
}

impl GameData {
    /// Reads and parses all six tables from `source`.
    ///
    /// Unlike individual `.anim`/GRP files (whose absence just drops a drawable or a canvas
    /// override), these are whole-file dependencies of the unit layer: a missing one is a
    /// [`RenderError`].
    pub fn load(source: &dyn TilesetDataSource) -> Result<GameData, RenderError> {
        let units = source.read(&AssetRequest::Dat(DatKind::Units))?;
        let flingy = source.read(&AssetRequest::Dat(DatKind::Flingy))?;
        let sprites = source.read(&AssetRequest::Dat(DatKind::Sprites))?;
        let images = source.read(&AssetRequest::Dat(DatKind::Images))?;
        let rel = source.read(&AssetRequest::ImagesRel)?;
        let images_tbl = source.read(&AssetRequest::ImagesTbl)?;

        let images = parse_images_dat(images.as_ref());
        let rel = parse_images_rel(rel.as_ref());
        let shadow_images = build_shadow_images(&rel, &images);

        Ok(GameData {
            units: parse_units_dat(units.as_ref()),
            flingy: parse_flingy_dat(flingy.as_ref()),
            sprites: parse_sprites_dat(sprites.as_ref()),
            images,
            rel,
            shadow_images,
            images_tbl: parse_tbl_owned(images_tbl.as_ref()),
        })
    }

    /// Builds a [`GameData`] from already-parsed tables, for callers that fetched and parsed the
    /// bytes themselves (prefetching WASM flows, tests). `images_tbl` is every entry of
    /// `images.tbl`, decoded and 0-indexed (e.g. via [`parse_tbl_owned`] over the raw bytes, or
    /// [`broodmap_formats::Tbl::get`] over every index up to the file's own declared count).
    pub fn from_parts(
        units: UnitsDat,
        flingy: FlingyDat,
        sprites: SpritesDat,
        images: ImagesDat,
        rel: ImagesRel,
        images_tbl: Vec<String>,
    ) -> GameData {
        let shadow_images = build_shadow_images(&rel, &images);
        GameData {
            units,
            flingy,
            sprites,
            images,
            rel,
            shadow_images,
            images_tbl,
        }
    }

    /// Resolves a unit ID to the image ID whose `.anim` art should be drawn for it, following
    /// `units.dat` -> `flingy.dat` -> `sprites.dat` and then the `images.rel` art redirect.
    /// `None` if any link in the chain is out of range.
    pub fn unit_image(&self, unit_id: u16) -> Option<u16> {
        Some(self.rel.resolve(self.unit_image_pre_redirect(unit_id)?))
    }

    /// Resolves a THG2 "pure sprite" ID (a `sprites.dat` ID) to the image ID whose `.anim` art
    /// should be drawn for it. `None` if the sprite ID is out of range.
    pub fn sprite_image(&self, sprite_id: u16) -> Option<u16> {
        Some(self.rel.resolve(self.sprite_image_pre_redirect(sprite_id)?))
    }

    /// The unit's image ID *before* the `images.rel` art redirect is applied. This is the ID that
    /// indexes `images.dat`: the redirect only says where the art lives, not which image's
    /// rendering metadata (directional frames, draw style) applies.
    pub(crate) fn unit_image_pre_redirect(&self, unit_id: u16) -> Option<u16> {
        let flingy = self.units.entry(unit_id)?.flingy;
        let sprite = self.flingy.sprite_id(flingy)?;
        self.sprites.image_id(sprite)
    }

    /// The sprite's image ID before the `images.rel` art redirect (see
    /// [`Self::unit_image_pre_redirect`]).
    pub(crate) fn sprite_image_pre_redirect(&self, sprite_id: u16) -> Option<u16> {
        self.sprites.image_id(sprite_id)
    }

    /// Applies the `images.rel` art redirect to an image ID.
    pub(crate) fn resolve_art(&self, image_id: u16) -> u16 {
        self.rel.resolve(image_id)
    }

    /// The parsed `units.dat`, for the columns the renderer needs directly (facing direction,
    /// placebox size).
    pub fn units_dat(&self) -> &UnitsDat {
        &self.units
    }

    /// The parsed `images.dat`, for the columns the renderer needs directly (directional
    /// frames).
    pub fn images_dat(&self) -> &ImagesDat {
        &self.images
    }

    /// Whether `unit_id`'s `units.dat` entry has the Building special-ability flag set. `false`
    /// for an out-of-range ID or a table with no data for it (permissive: an unresolvable unit
    /// is simply not treated as a building, matching how the rest of this chain handles missing
    /// data).
    pub(crate) fn is_building(&self, unit_id: u16) -> bool {
        self.units
            .entry(unit_id)
            .is_some_and(|e| e.special_ability_flags & SPECIAL_ABILITY_FLAG_BUILDING != 0)
    }

    /// The image whose `.anim` art should be drawn as `main_pre_redirect`'s shadow underlay, if
    /// any.
    ///
    /// In the real game, a drawable's shadow is a separate image attached by an iscript `imgul`
    /// ("image underlay") opcode -- a VM this library deliberately does not implement or run (see
    /// `docs/render-design.md`'s Non-goals). Instead of running iscript, this looks the shadow up
    /// directly: `images.rel` carries an exact, data-driven parent -> shadow mapping (its type-8
    /// records; see [`broodmap_formats::ImagesRel::shadow_parent`]'s docs), inverted once at
    /// construction time into [`Self::shadow_images`] (see [`build_shadow_images`]). A handful of
    /// parents (12, in a real install) have two shadow records -- e.g. obscure neutral pickups
    /// with `*Shad`/`*Sha2` GRP variants -- and [`build_shadow_images`] deterministically keeps
    /// the lowest shadow image ID among them.
    ///
    /// `main_pre_redirect` must be the image ID *before* the `images.rel` art redirect (the same
    /// ID that indexes `images.dat`'s other rendering metadata -- see
    /// [`Self::unit_image_pre_redirect`]'s docs), since that's the ID the shadow table is keyed
    /// by. If that direct lookup misses, this falls back to looking up the *redirected* image
    /// instead (`self.rel.resolve(main_pre_redirect)`), so a unit whose own main image is itself
    /// redirected to different art still finds its shadow. Either way, the returned ID is itself
    /// pre-redirect w.r.t. art resolution -- callers apply [`Self::resolve_art`] to it, mirroring
    /// how the main image's art is resolved (see [`crate::overlay::push_with_shadow`]).
    ///
    /// As a belt-and-suspenders check against a hostile or malformed `images.rel`/`images.dat`
    /// pair disagreeing with each other, the resulting shadow ID's `images.dat` entry must have
    /// `render_style == 10` (BW's "shadow" draw style) or this returns `None` -- the safe
    /// direction for a disagreement to fail toward. On real data every entry in the table passes
    /// this gate (230/230), so it's a no-op there.
    pub(crate) fn shadow_image_pre_redirect(&self, main_pre_redirect: u16) -> Option<u16> {
        let shadow_id = self
            .shadow_images
            .get(&main_pre_redirect)
            .or_else(|| {
                let resolved = self.rel.resolve(main_pre_redirect);
                self.shadow_images.get(&resolved)
            })
            .copied()?;
        self.images
            .entry(shadow_id)
            .filter(|e| e.render_style == RENDER_STYLE_SHADOW)
            .map(|_| shadow_id)
    }

    /// Resolves `image_id`'s classic GRP filename (an `images.tbl` string, backslash separators
    /// and all -- see [`crate::source::AssetRequest::Grp`]), via `images.dat`'s `grp` column: a
    /// 1-based index into `images.tbl`, `0` meaning "none". `None` if `image_id` is out of range,
    /// its `grp` column is `0`, or the (0-based, `grp - 1`) index doesn't resolve to a real
    /// `images.tbl` entry.
    ///
    /// Used only by SD (`ArtStyle::Original`) renders, to locate the classic GRP whose header
    /// supplies the true canvas dimensions `mainSD.anim`'s own (always-zero) declared canvas
    /// can't -- see `crate::overlay`'s SD canvas override and `broodmap_formats::grp`'s module
    /// docs for the full story.
    pub(crate) fn grp_path(&self, image_id: u16) -> Option<&str> {
        let entry = self.images.entry(image_id)?;
        if entry.grp == 0 {
            return None;
        }
        let index = usize::try_from(entry.grp - 1).ok()?;
        self.images_tbl.get(index).map(String::as_str)
    }
}

/// Inverts `rel`'s type-8 shadow records (shadow ID -> parent ID) into a parent -> shadow lookup,
/// gated against `images` for hostile/malformed input: a record whose parent ID doesn't resolve
/// to a real `images.dat` entry, or whose parent ID is its own shadow ID (a self-reference), is
/// skipped entirely -- real `images.rel` files never do either, but the bytes are untrusted. When
/// a parent has more than one shadow record (12, in a real install), the lowest shadow ID wins,
/// applied via `Entry::or_insert`/`min` regardless of record order.
fn build_shadow_images(rel: &ImagesRel, images: &ImagesDat) -> HashMap<u16, u16> {
    let mut shadow_images: HashMap<u16, u16> = HashMap::new();
    for (shadow_id, parent_id) in rel.shadow_pairs() {
        if parent_id == shadow_id || images.entry(parent_id).is_none() {
            continue;
        }
        shadow_images
            .entry(parent_id)
            .and_modify(|existing| *existing = (*existing).min(shadow_id))
            .or_insert(shadow_id);
    }
    shadow_images
}

/// Eagerly decodes every entry of a `.tbl` file (here, always `images.tbl`) into an owned
/// `Vec<String>`, 0-indexed, so [`GameData`] can stay `'static`-owned rather than borrowing from
/// the source bytes the way `broodmap_formats::Tbl<'a>` does. The entry count comes from the
/// file's own leading `u16` (see `broodmap_formats::tbl`'s module docs) -- a hostile/huge count is
/// naturally capped at `u16::MAX` entries, the same bound `Tbl`'s own offset table allocates to.
pub(crate) fn parse_tbl_owned(data: &[u8]) -> Vec<String> {
    let count = data
        .get(0..2)
        .map(|b| u16::from_le_bytes([b[0], b[1]]))
        .unwrap_or(0);
    let tbl = parse_tbl(data);
    (0..count)
        .map(|i| tbl.get(i).map(Cow::into_owned).unwrap_or_default())
        .collect()
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::source::MemorySource;

    /// Raw bytes for the six tables: units, flingy, sprites, images, rel, images.tbl.
    pub(crate) type DatBytes = (Vec<u8>, Vec<u8>, Vec<u8>, Vec<u8>, Vec<u8>, Vec<u8>);

    /// Builds a minimal valid `.tbl` byte buffer (see `broodmap_formats::tbl`'s module docs for
    /// the format) from a list of entries, matching that module's own test helper.
    pub(crate) fn build_tbl(entries: &[&str]) -> Vec<u8> {
        let header_size = 2 + entries.len() * 2;
        let mut offsets = Vec::with_capacity(entries.len());
        let mut strings_blob = Vec::new();
        for s in entries {
            offsets.push((header_size + strings_blob.len()) as u16);
            strings_blob.extend_from_slice(s.as_bytes());
            strings_blob.push(0);
        }

        let mut data = Vec::with_capacity(header_size + strings_blob.len());
        data.extend_from_slice(&(entries.len() as u16).to_le_bytes());
        for offset in offsets {
            data.extend_from_slice(&offset.to_le_bytes());
        }
        data.extend_from_slice(&strings_blob);
        data
    }

    /// Builds the five `.dat`/`.rel` tables such that unit `unit_id` resolves through the given
    /// chain, plus an optional `images.rel` redirect on the final image, plus an empty (but
    /// valid) `images.tbl` -- tests that need a real `grp_path` resolution poke the returned
    /// `images.dat`/`images.tbl` bytes (indices 3 and 5) directly.
    pub(crate) fn synthetic_parts(
        unit_id: u16,
        flingy_id: u8,
        sprite_id: u16,
        image_id: u16,
        redirect: Option<(u16, u16)>,
    ) -> DatBytes {
        // units.dat: the `flingy` column is first, one u8 per unit.
        let mut units = vec![0u8; 19876];
        units[unit_id as usize] = flingy_id;

        // flingy.dat: the `sprite` column is first, one u16 per flingy.
        let mut flingy = vec![0u8; 3135];
        let off = flingy_id as usize * 2;
        flingy[off..off + 2].copy_from_slice(&sprite_id.to_le_bytes());

        // sprites.dat: the `image` column is first, one u16 per sprite.
        let mut sprites = vec![0u8; 3229];
        let off = sprite_id as usize * 2;
        sprites[off..off + 2].copy_from_slice(&image_id.to_le_bytes());

        let images = vec![0u8; 37962];

        // images.rel: 999 records of {u32 rel_type, u32 ref_image}; 0x200 = "redirect".
        let mut rel = vec![0u8; 999 * 8];
        for rec in rel.as_chunks_mut::<8>().0 {
            rec[4..8].copy_from_slice(&0xFFFF_FFFFu32.to_le_bytes());
        }
        if let Some((from, to)) = redirect {
            let off = from as usize * 8;
            rel[off..off + 4].copy_from_slice(&0x200u32.to_le_bytes());
            rel[off + 4..off + 8].copy_from_slice(&(to as u32).to_le_bytes());
        }

        let tbl = build_tbl(&[]);

        (units, flingy, sprites, images, rel, tbl)
    }

    /// `images.rel`'s type value for a shadow-image record (`broodmap_formats::rel`'s private
    /// `SHADOW_TYPE`, mirrored here the same way `synthetic_parts` mirrors `REDIRECT_FLAG` as a
    /// literal `0x200`).
    const SHADOW_REL_TYPE: u32 = 8;

    /// Plants an `images.rel` type-8 (shadow) record directly into raw `rel` bytes (index 4 of a
    /// [`DatBytes`], the same tuple [`synthetic_parts`] returns): `shadow_id`'s record marks it
    /// as `parent_id`'s shadow. Callers poke `synthetic_parts`' `.4` field with this the same way
    /// existing tests poke `.3` (images.dat) directly for `render_style`.
    pub(crate) fn plant_shadow_record(rel: &mut [u8], shadow_id: u16, parent_id: u16) {
        let off = shadow_id as usize * 8;
        rel[off..off + 4].copy_from_slice(&SHADOW_REL_TYPE.to_le_bytes());
        rel[off + 4..off + 8].copy_from_slice(&(parent_id as u32).to_le_bytes());
    }

    pub(crate) fn synthetic_source(parts: DatBytes) -> MemorySource {
        let (units, flingy, sprites, images, rel, tbl) = parts;
        [
            (AssetRequest::Dat(DatKind::Units), units),
            (AssetRequest::Dat(DatKind::Flingy), flingy),
            (AssetRequest::Dat(DatKind::Sprites), sprites),
            (AssetRequest::Dat(DatKind::Images), images),
            (AssetRequest::ImagesRel, rel),
            (AssetRequest::ImagesTbl, tbl),
        ]
        .into_iter()
        .collect()
    }

    #[test]
    fn unit_image_walks_the_full_chain() {
        let source = synthetic_source(synthetic_parts(7, 3, 250, 600, None));
        let data = GameData::load(&source).unwrap();
        assert_eq!(data.unit_image(7), Some(600));
        assert_eq!(data.unit_image_pre_redirect(7), Some(600));
        // A unit with no configured chain still resolves (everything points at entry 0).
        assert_eq!(data.unit_image(8), Some(0));
        // Out of range.
        assert_eq!(data.unit_image(228), None);
        assert_eq!(data.unit_image(u16::MAX), None);
    }

    #[test]
    fn unit_image_applies_the_rel_redirect_but_pre_redirect_does_not() {
        let source = synthetic_source(synthetic_parts(7, 3, 250, 600, Some((600, 601))));
        let data = GameData::load(&source).unwrap();
        assert_eq!(data.unit_image(7), Some(601));
        assert_eq!(data.unit_image_pre_redirect(7), Some(600));
    }

    #[test]
    fn sprite_image_skips_to_the_sprites_dat_step() {
        let source = synthetic_source(synthetic_parts(7, 3, 250, 600, Some((600, 42))));
        let data = GameData::load(&source).unwrap();
        assert_eq!(data.sprite_image(250), Some(42));
        assert_eq!(data.sprite_image_pre_redirect(250), Some(600));
        assert_eq!(data.sprite_image(517), None);
    }

    #[test]
    fn load_reports_a_missing_table_as_an_error() {
        let source = MemorySource::new();
        assert!(GameData::load(&source).is_err());
    }

    #[test]
    fn from_parts_matches_load() {
        let parts = synthetic_parts(7, 3, 250, 600, None);
        let data = GameData::from_parts(
            parse_units_dat(&parts.0),
            parse_flingy_dat(&parts.1),
            parse_sprites_dat(&parts.2),
            parse_images_dat(&parts.3),
            parse_images_rel(&parts.4),
            parse_tbl_owned(&parts.5),
        );
        assert_eq!(data.unit_image(7), Some(600));
    }

    #[test]
    fn grp_path_resolves_via_the_one_based_grp_column_and_the_tbl() {
        let mut parts = synthetic_parts(0, 0, 0, 5, None);
        // images.dat's `grp` column is first (a u32 per image); image 5's grp = 1 (1-based),
        // pointing at images.tbl entry 0.
        let grp_off = 5 * 4;
        parts.3[grp_off..grp_off + 4].copy_from_slice(&1u32.to_le_bytes());
        parts.5 = build_tbl(&["terran\\marine.grp"]);
        let data = GameData::load(&synthetic_source(parts)).unwrap();

        assert_eq!(data.grp_path(5), Some("terran\\marine.grp"));
        // grp == 0 means "none".
        assert_eq!(data.grp_path(6), None);
        // Out-of-range image id.
        assert_eq!(data.grp_path(999), None);
    }

    #[test]
    fn grp_path_is_none_when_the_grp_index_does_not_resolve_in_the_tbl() {
        let mut parts = synthetic_parts(0, 0, 0, 5, None);
        let grp_off = 5 * 4;
        // grp = 3 (1-based -> tbl index 2), but the tbl only has one entry.
        parts.3[grp_off..grp_off + 4].copy_from_slice(&3u32.to_le_bytes());
        parts.5 = build_tbl(&["terran\\marine.grp"]);
        let data = GameData::load(&synthetic_source(parts)).unwrap();

        assert_eq!(data.grp_path(5), None);
    }

    #[test]
    fn critter_and_resource_id_sets_match_bw_chk() {
        for id in [89u16, 90, 93, 94, 95, 96] {
            assert!(is_critter(id), "{id} should be a critter");
        }
        for id in [88u16, 91, 92, 97] {
            assert!(!is_critter(id), "{id} should not be a critter");
        }
        for id in [176u16, 177, 178, 188] {
            assert!(is_resource(id), "{id} should be a resource");
        }
        for id in [175u16, 179, 187, 189] {
            assert!(!is_resource(id), "{id} should not be a resource");
        }
    }

    /// Byte offset of `images.dat`'s `render_style` column: the `grp` u32 column plus the four
    /// single-byte columns ahead of it (`has_directional_frames`, `clickable`,
    /// `use_full_iscript`, `always_visible`), for 999 images -- see
    /// `broodmap_formats::dat`'s `IMAGES_COLUMN_SIZES`, whose own test independently pins this
    /// same offset at 7992; `broodmap_render::overlay`'s tests derive it the same way.
    const IMAGES_RENDER_STYLE_COLUMN: usize = 999 * 4 + 999 * 4;

    /// Sets `images.dat[image_id]`'s `render_style` column directly on raw bytes (index 3 of a
    /// [`DatBytes`]).
    fn set_render_style(images: &mut [u8], image_id: u16, render_style: u8) {
        images[IMAGES_RENDER_STYLE_COLUMN + image_id as usize] = render_style;
    }

    #[test]
    fn shadow_image_pre_redirect_maps_a_parent_to_its_shadow_at_an_arbitrary_delta() {
        // Mirrors the real protoss nexus (image 179 -> shadow 182, not +1): a shadow can sit
        // anywhere images.rel says it does, not just at parent + 1.
        let mut parts = synthetic_parts(0, 0, 0, 179, None);
        plant_shadow_record(&mut parts.4, 182, 179);
        set_render_style(&mut parts.3, 182, RENDER_STYLE_SHADOW);
        let data = GameData::load(&synthetic_source(parts)).unwrap();

        assert_eq!(data.shadow_image_pre_redirect(179), Some(182));
    }

    #[test]
    fn shadow_image_pre_redirect_rejects_a_rel_dat_mismatch() {
        // A type-8 record names 182 as 179's shadow, but 182's images.dat entry isn't
        // render_style 10 -- a hostile/malformed rel-vs-dat disagreement, which must fail toward
        // no shadow rather than trusting the rel table blindly.
        let mut parts = synthetic_parts(0, 0, 0, 179, None);
        plant_shadow_record(&mut parts.4, 182, 179);
        // render_style defaults to 0 (not RENDER_STYLE_SHADOW) -- no need to set it explicitly.
        let data = GameData::load(&synthetic_source(parts)).unwrap();

        assert_eq!(data.shadow_image_pre_redirect(179), None);
    }

    #[test]
    fn shadow_image_pre_redirect_picks_the_lowest_shadow_id_among_duplicates() {
        // Two shadow records for the same parent (like the 12 real multi-shadow parents):
        // planted out of ID order, to prove the tiebreak isn't just "last one wins".
        let mut parts = synthetic_parts(0, 0, 0, 50, None);
        plant_shadow_record(&mut parts.4, 60, 50);
        plant_shadow_record(&mut parts.4, 55, 50);
        set_render_style(&mut parts.3, 60, RENDER_STYLE_SHADOW);
        set_render_style(&mut parts.3, 55, RENDER_STYLE_SHADOW);
        let data = GameData::load(&synthetic_source(parts)).unwrap();

        assert_eq!(
            data.shadow_image_pre_redirect(50),
            Some(55),
            "the lower shadow ID must win"
        );
    }

    #[test]
    fn shadow_image_pre_redirect_skips_a_hostile_self_referencing_record() {
        // A record whose ref_image is its own index: real images.rel never does this, but the
        // bytes are untrusted, so it must be dropped when building the inverse map rather than
        // resolving an image as its own shadow.
        let mut parts = synthetic_parts(0, 0, 0, 5, None);
        plant_shadow_record(&mut parts.4, 5, 5);
        set_render_style(&mut parts.3, 5, RENDER_STYLE_SHADOW);
        let data = GameData::load(&synthetic_source(parts)).unwrap();

        assert_eq!(data.shadow_image_pre_redirect(5), None);
    }

    #[test]
    fn shadow_image_pre_redirect_falls_back_through_the_rel_redirect() {
        // Main image 100 is itself redirected (via the ordinary 0x200 redirect flag) to art
        // image 200; the shadow table only has an entry for 200, not 100. The direct lookup on
        // 100 misses, so this must retry through `rel.resolve(100) == 200`.
        let mut parts = synthetic_parts(0, 0, 0, 100, Some((100, 200)));
        plant_shadow_record(&mut parts.4, 201, 200);
        set_render_style(&mut parts.3, 201, RENDER_STYLE_SHADOW);
        let data = GameData::load(&synthetic_source(parts)).unwrap();

        assert_eq!(
            data.shadow_image_pre_redirect(100),
            Some(201),
            "a redirected main image should still find its shadow via the redirect target"
        );
    }

    /// Verifies the `images.rel` shadow table against real data: the set of type-8 record
    /// indices (shadow images) must equal the set of `images.dat` `render_style == 10` images
    /// exactly, confirming [`ImagesRel::shadow_parent`]'s and [`build_shadow_images`]'s gate is
    /// a no-op on real data (matching [`GameData::shadow_image_pre_redirect`]'s docs).
    ///
    /// Gated on `BROODMAP_TEST_SCR_DIR` like `tests/real_assets.rs`; run with:
    /// `BROODMAP_TEST_SCR_DIR='C:\Program Files (x86)\StarCraft' cargo test -p broodmap-render --features casc shadow_type_records_match_render_style -- --nocapture`
    #[cfg(feature = "casc")]
    #[test]
    fn shadow_type_records_match_render_style_shadow_images() {
        let Some(dir) = std::env::var_os("BROODMAP_TEST_SCR_DIR") else {
            eprintln!("skipping: BROODMAP_TEST_SCR_DIR not set");
            return;
        };
        let source = crate::source::CascSource::open(dir)
            .expect("BROODMAP_TEST_SCR_DIR should be a valid SC:R install");
        let rel = parse_images_rel(source.read(&AssetRequest::ImagesRel).unwrap().as_ref());
        let images = parse_images_dat(
            source
                .read(&AssetRequest::Dat(DatKind::Images))
                .unwrap()
                .as_ref(),
        );

        let type_8: std::collections::HashSet<u16> = rel.shadow_pairs().map(|(id, _)| id).collect();
        let render_style_10: std::collections::HashSet<u16> = (0u16..999)
            .filter(|&id| {
                images
                    .entry(id)
                    .is_some_and(|e| e.render_style == RENDER_STYLE_SHADOW)
            })
            .collect();

        eprintln!(
            "type-8 records: {}, render_style==10 images: {}",
            type_8.len(),
            render_style_10.len()
        );
        assert_eq!(
            type_8, render_style_10,
            "the type-8 record set must equal the render_style==10 image set exactly"
        );
    }

    /// Verifies the new exact `images.rel`-driven shadow lookup against real data: it must
    /// resolve strictly more coverage than the old `+1` heuristic (which the replaced test
    /// measured at ~59.6% of units), including buildings the old heuristic largely missed (e.g.
    /// the protoss nexus), and the vespene geyser must resolve straight from the table with no
    /// special case.
    ///
    /// Gated on `BROODMAP_TEST_SCR_DIR` like the rest of this module's real-data tests; run with:
    /// `BROODMAP_TEST_SCR_DIR='C:\Program Files (x86)\StarCraft' cargo test -p broodmap-render --features casc shadow_table_covers_more -- --nocapture`
    #[cfg(feature = "casc")]
    #[test]
    fn shadow_table_covers_more_than_the_old_plus_one_heuristic() {
        let Some(dir) = std::env::var_os("BROODMAP_TEST_SCR_DIR") else {
            eprintln!("skipping: BROODMAP_TEST_SCR_DIR not set");
            return;
        };
        let source = crate::source::CascSource::open(dir)
            .expect("BROODMAP_TEST_SCR_DIR should be a valid SC:R install");
        let data = GameData::load(&source).expect("the .dat tables should load");

        let mut total = 0u32;
        let mut with_shadow = 0u32;
        let mut old_plus_one_with_shadow = 0u32;
        for unit_id in 0u16..228 {
            let Some(main_pre_redirect) = data.unit_image_pre_redirect(unit_id) else {
                continue;
            };
            total += 1;
            if data.shadow_image_pre_redirect(main_pre_redirect).is_some() {
                with_shadow += 1;
            }
            let old_plus_one = data
                .images_dat()
                .entry(main_pre_redirect + 1)
                .map(|e| e.render_style)
                == Some(RENDER_STYLE_SHADOW);
            if old_plus_one {
                old_plus_one_with_shadow += 1;
            }
        }
        let coverage = 100.0 * with_shadow as f64 / total.max(1) as f64;
        let old_coverage = 100.0 * old_plus_one_with_shadow as f64 / total.max(1) as f64;
        eprintln!(
            "units: {with_shadow}/{total} ({coverage:.1}%) resolve a shadow via images.rel, \
             vs {old_plus_one_with_shadow}/{total} ({old_coverage:.1}%) under the old +1 heuristic"
        );
        assert!(
            coverage > old_coverage,
            "the exact table must cover at least as much as the old +1 heuristic (old {old_coverage:.1}%, new {coverage:.1}%)"
        );
        assert!(
            coverage > 59.6,
            "the exact table should exceed the old heuristic's measured ~59.6% unit coverage, got {coverage:.1}%"
        );

        // Buildings are the headline improvement: the protoss nexus (unit 154) never resolved a
        // +1 shadow (its real shadow is 179 -> 182, not +1) but must resolve through the table.
        let nexus_main = data
            .unit_image_pre_redirect(154)
            .expect("the protoss nexus should resolve a main image");
        assert!(
            data.shadow_image_pre_redirect(nexus_main).is_some(),
            "the protoss nexus (image {nexus_main}) should now resolve a shadow"
        );

        // The vespene geyser resolves straight from the table -- no +2 special case anymore.
        let geyser_main = data
            .unit_image_pre_redirect(UNIT_ID_VESPENE_GEYSER)
            .expect("the vespene geyser should resolve a main image");
        eprintln!("geyser main image: {geyser_main}");
        assert_eq!(
            data.shadow_image_pre_redirect(geyser_main),
            Some(geyser_main + 2),
            "the geyser's shadow should still land on +2 ({}), now via the table, not a special case",
            geyser_main + 2
        );
    }

    /// Targeted real-data check mirroring the design doc's numbers directly: image 344 (the
    /// vespene geyser's main art) resolves its shadow to 346 (`neutral\geyShad.grp`) straight
    /// from the `images.rel` table -- no unit-ID special case involved.
    #[cfg(feature = "casc")]
    #[test]
    fn geyser_shadow_resolves_via_the_table_with_no_special_case() {
        let Some(dir) = std::env::var_os("BROODMAP_TEST_SCR_DIR") else {
            eprintln!("skipping: BROODMAP_TEST_SCR_DIR not set");
            return;
        };
        let source = crate::source::CascSource::open(dir)
            .expect("BROODMAP_TEST_SCR_DIR should be a valid SC:R install");
        let data = GameData::load(&source).expect("the .dat tables should load");

        assert_eq!(data.shadow_image_pre_redirect(344), Some(346));
    }

    #[test]
    fn is_building_reads_the_special_ability_flags_bit() {
        // special_ability_flags is units.dat's 23rd column (a u32 per unit): the sum of every
        // column ahead of it. See `broodmap_formats::dat`'s `UNITS_COLUMN_SIZES` for the full
        // layout; `broodmap_render::overlay`'s tests derive the same offset independently (for
        // `placebox`, one column further) as a cross-check.
        const UNITS_COUNT: usize = 228;
        const BUILDINGS_COUNT: usize = 96;
        const SPECIAL_ABILITY_FLAGS_COLUMN: usize = UNITS_COUNT // flingy: u8
            + UNITS_COUNT * 2   // sub_unit_1: u16
            + UNITS_COUNT * 2   // sub_unit_2: u16
            + BUILDINGS_COUNT * 2 // infestation: u16 (buildings-only)
            + UNITS_COUNT * 4   // construction_image: u32
            + UNITS_COUNT * 2   // unit_direction, shield_enabled: u8 each
            + UNITS_COUNT * 2   // shield_amount: i16
            + UNITS_COUNT * 4   // hit_points: i32
            + UNITS_COUNT * 3   // elevation_level, unknown, sub_label: u8 each
            + UNITS_COUNT * 5   // five AI/order columns: u8 each
            + UNITS_COUNT * 5; // ground_weapon..ai_internal: u8 each

        let mut units = vec![0u8; 19876];
        units[SPECIAL_ABILITY_FLAGS_COLUMN..SPECIAL_ABILITY_FLAGS_COLUMN + 4]
            .copy_from_slice(&SPECIAL_ABILITY_FLAG_BUILDING.to_le_bytes());

        let mut parts = synthetic_parts(0, 0, 0, 0, None);
        parts.0 = units;
        let data = GameData::load(&synthetic_source(parts)).unwrap();

        assert!(data.is_building(0));
        assert!(!data.is_building(1), "unit 1's flags are all zero");
        assert!(
            !data.is_building(9999),
            "out-of-range unit IDs are never buildings"
        );
    }
}
