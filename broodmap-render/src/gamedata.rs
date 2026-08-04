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

use broodmap_formats::{
    FlingyDat, ImagesDat, ImagesRel, SpritesDat, UnitsDat, parse_flingy_dat, parse_images_dat,
    parse_images_rel, parse_sprites_dat, parse_units_dat,
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
/// [`broodmap_formats::ImageEntry::render_style`]'s docs). Used by [`GameData::shadow_image_pre_redirect`]
/// to gate the shadow-image heuristic.
const RENDER_STYLE_SHADOW: u8 = 10;

/// Builds the [`AssetRequest::Anim`] for an image, applying the Carbot start-location quirk.
///
/// The Cartooned art pack ships no `main_588.anim`, so the game falls back to the standard
/// pack's start-location graphic there; requesting the Carbot path would just 404. Applying the
/// fallback here (at request-construction time) keeps prefetch lists and render-time reads in
/// agreement.
pub(crate) fn anim_request(image_id: u16, tier: AssetTier, pack: ArtPack) -> AssetRequest {
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
}

impl GameData {
    /// Reads and parses all five tables from `source`.
    ///
    /// Unlike individual `.anim` files (whose absence just drops a drawable), these are
    /// whole-file dependencies of the unit layer: a missing one is a [`RenderError`].
    pub fn load(source: &dyn TilesetDataSource) -> Result<GameData, RenderError> {
        let units = source.read(&AssetRequest::Dat(DatKind::Units))?;
        let flingy = source.read(&AssetRequest::Dat(DatKind::Flingy))?;
        let sprites = source.read(&AssetRequest::Dat(DatKind::Sprites))?;
        let images = source.read(&AssetRequest::Dat(DatKind::Images))?;
        let rel = source.read(&AssetRequest::ImagesRel)?;

        Ok(GameData {
            units: parse_units_dat(units.as_ref()),
            flingy: parse_flingy_dat(flingy.as_ref()),
            sprites: parse_sprites_dat(sprites.as_ref()),
            images: parse_images_dat(images.as_ref()),
            rel: parse_images_rel(rel.as_ref()),
        })
    }

    /// Builds a [`GameData`] from already-parsed tables, for callers that fetched and parsed the
    /// bytes themselves (prefetching WASM flows, tests).
    pub fn from_parts(
        units: UnitsDat,
        flingy: FlingyDat,
        sprites: SpritesDat,
        images: ImagesDat,
        rel: ImagesRel,
    ) -> GameData {
        GameData {
            units,
            flingy,
            sprites,
            images,
            rel,
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
    /// `docs/render-design.md`'s Non-goals). Instead, this applies a data-only heuristic in its
    /// place: a shadow conventionally lives at `main_pre_redirect + 1`, gated on that slot's
    /// `images.dat` entry being flagged with BW's "shadow" draw style (`render_style == 10`).
    /// This was verified against a real SC:R install (228 units + 517 THG2 sprite IDs) and
    /// cross-checked against neobrood's generated iscript disassembly as a development-time
    /// oracle -- see `docs/render-design.md`'s "Shadows" note for the numbers.
    ///
    /// One case gets a hardcoded offset instead of the general `+1` rule: the vespene geyser
    /// (`unit_id == `[`UNIT_ID_VESPENE_GEYSER`]) uses `+2`, because `+1` there is a same-GRP art
    /// variant of the geyser itself (`render_style` other than 10), not a shadow -- verified
    /// against a real install (image 344's `+1`, 345, is a render_style-0 variant; its real
    /// shadow is 346, `neutral\geyShad.grp`, render_style 10). This mirrors the existing
    /// resource-specific stand-ins for iscript behavior this library doesn't run (mineral frame
    /// by amount, geyser frame by tileset — see `select_unit_frame`), rather than a general
    /// mechanism: units whose own turret/overlay image pushes their real shadow to `+2` are not
    /// covered and simply get no shadow (an accepted gap -- see `docs/render-design.md`'s
    /// "Shadows" note).
    ///
    /// `main_pre_redirect` must be the image ID *before* the `images.rel` redirect (the same ID
    /// that indexes `images.dat`'s other rendering metadata -- see
    /// [`Self::unit_image_pre_redirect`]'s docs), since that's what the render-style gate is
    /// checked against. The redirect is then applied to the shadow slot itself (mirroring how the
    /// main image's art is resolved) to get the ID whose `.anim` actually holds the shadow art.
    ///
    /// `unit_id` is the drawable's own unit ID where one exists (placed units and THG2
    /// unit-sprites); `None` for THG2 doodad sprites, which have no unit ID and so never match
    /// the geyser special case.
    pub(crate) fn shadow_image_pre_redirect(
        &self,
        main_pre_redirect: u16,
        unit_id: Option<u16>,
    ) -> Option<u16> {
        let offset: u16 = if unit_id == Some(UNIT_ID_VESPENE_GEYSER) {
            2
        } else {
            1
        };
        let shadow_id = main_pre_redirect.checked_add(offset)?;
        self.images
            .entry(shadow_id)
            .filter(|e| e.render_style == RENDER_STYLE_SHADOW)
            .map(|_| shadow_id)
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::source::MemorySource;

    /// Raw bytes for the five tables: units, flingy, sprites, images, rel.
    pub(crate) type DatBytes = (Vec<u8>, Vec<u8>, Vec<u8>, Vec<u8>, Vec<u8>);

    /// Builds the five tables such that unit `unit_id` resolves through the given chain, plus an
    /// optional `images.rel` redirect on the final image.
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
        for rec in rel.chunks_exact_mut(8) {
            rec[4..8].copy_from_slice(&0xFFFF_FFFFu32.to_le_bytes());
        }
        if let Some((from, to)) = redirect {
            let off = from as usize * 8;
            rel[off..off + 4].copy_from_slice(&0x200u32.to_le_bytes());
            rel[off + 4..off + 8].copy_from_slice(&(to as u32).to_le_bytes());
        }

        (units, flingy, sprites, images, rel)
    }

    pub(crate) fn synthetic_source(parts: DatBytes) -> MemorySource {
        let (units, flingy, sprites, images, rel) = parts;
        [
            (AssetRequest::Dat(DatKind::Units), units),
            (AssetRequest::Dat(DatKind::Flingy), flingy),
            (AssetRequest::Dat(DatKind::Sprites), sprites),
            (AssetRequest::Dat(DatKind::Images), images),
            (AssetRequest::ImagesRel, rel),
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
        );
        assert_eq!(data.unit_image(7), Some(600));
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

    /// Verifies the shadow-image heuristic (used by the overlay's shadow rendering, see
    /// `crate::overlay`) against real data: a drawable's shadow art is conventionally
    /// `image_id + 1` (the PRE-`images.rel`-redirect ID, since that's what indexes `images.dat`'s
    /// rendering metadata — see [`GameData::unit_image_pre_redirect`]'s docs), gated on
    /// `images.dat[main + 1].render_style == 10` (BW's "shadow" draw function). There's no
    /// iscript VM here to confirm this by actually running `imgul` opcodes, so this is the
    /// substitute: it walks every unit ID (0..228) and every THG2 "pure sprite" ID (0..517,
    /// doodads like trees) through the resolution chain and reports how often the `+1` slot is
    /// actually flagged as a shadow.
    ///
    /// Gated on `BROODMAP_TEST_SCR_DIR` like `tests/real_assets.rs`; run with:
    /// `BROODMAP_TEST_SCR_DIR='C:\Program Files (x86)\StarCraft' cargo test -p broodmap-render --features casc shadow_plus_one_convention -- --nocapture`
    #[cfg(feature = "casc")]
    #[test]
    fn shadow_plus_one_convention_holds_broadly() {
        let Some(dir) = std::env::var_os("BROODMAP_TEST_SCR_DIR") else {
            eprintln!("skipping: BROODMAP_TEST_SCR_DIR not set");
            return;
        };
        let source = crate::source::CascSource::open(dir)
            .expect("BROODMAP_TEST_SCR_DIR should be a valid SC:R install");
        let data = GameData::load(&source).expect("the .dat tables should load");

        let mut total = 0u32;
        let mut with_shadow = 0u32;
        let mut building_total = 0u32;
        let mut building_with_shadow = 0u32;
        let mut critter_total = 0u32;
        let mut critter_with_shadow = 0u32;
        let mut resource_total = 0u32;
        let mut resource_with_shadow = 0u32;
        let mut plain_total = 0u32;
        let mut plain_with_shadow = 0u32;
        let mut exceptions: Vec<String> = Vec::new();
        for unit_id in 0u16..228 {
            let Some(main_pre_redirect) = data.unit_image_pre_redirect(unit_id) else {
                continue;
            };
            total += 1;
            let shadow_id = main_pre_redirect + 1;
            let render_style = data.images_dat().entry(shadow_id).map(|e| e.render_style);
            let has_shadow = render_style == Some(10);

            let is_building = data.is_building(unit_id);
            let is_critter = is_critter(unit_id);
            let is_resource = is_resource(unit_id);
            if is_building {
                building_total += 1;
                building_with_shadow += has_shadow as u32;
            } else if is_critter {
                critter_total += 1;
                critter_with_shadow += has_shadow as u32;
            } else if is_resource {
                resource_total += 1;
                resource_with_shadow += has_shadow as u32;
            } else {
                plain_total += 1;
                plain_with_shadow += has_shadow as u32;
            }

            if has_shadow {
                with_shadow += 1;
            } else {
                exceptions.push(format!(
                    "unit {unit_id} (building={is_building} critter={is_critter} resource={is_resource}) \
                     image {main_pre_redirect}, +1={shadow_id} render_style {render_style:?}"
                ));
            }
        }
        eprintln!(
            "units overall: {with_shadow}/{total} ({:.1}%) resolve a +1 shadow (render_style == 10)",
            100.0 * with_shadow as f64 / total.max(1) as f64
        );
        eprintln!(
            "  plain units:  {plain_with_shadow}/{plain_total} ({:.1}%)",
            100.0 * plain_with_shadow as f64 / plain_total.max(1) as f64
        );
        eprintln!(
            "  buildings:    {building_with_shadow}/{building_total} ({:.1}%)",
            100.0 * building_with_shadow as f64 / building_total.max(1) as f64
        );
        eprintln!(
            "  critters:     {critter_with_shadow}/{critter_total} ({:.1}%)",
            100.0 * critter_with_shadow as f64 / critter_total.max(1) as f64
        );
        eprintln!(
            "  resources:    {resource_with_shadow}/{resource_total} ({:.1}%)",
            100.0 * resource_with_shadow as f64 / resource_total.max(1) as f64
        );
        eprintln!(
            "all unit exceptions ({}): {exceptions:#?}",
            exceptions.len()
        );

        let mut sprite_total = 0u32;
        let mut sprite_with_shadow = 0u32;
        let mut sprite_exceptions: Vec<String> = Vec::new();
        for sprite_id in 0u16..517 {
            let Some(main_pre_redirect) = data.sprite_image_pre_redirect(sprite_id) else {
                continue;
            };
            sprite_total += 1;
            let shadow_id = main_pre_redirect + 1;
            let render_style = data.images_dat().entry(shadow_id).map(|e| e.render_style);
            if render_style == Some(10) {
                sprite_with_shadow += 1;
            } else if sprite_exceptions.len() < 60 {
                sprite_exceptions.push(format!(
                    "sprite {sprite_id} (image {main_pre_redirect}, +1={shadow_id} render_style {render_style:?})"
                ));
            }
        }
        eprintln!(
            "sprites: {sprite_with_shadow}/{sprite_total} ({:.1}%) resolve a +1 shadow",
            100.0 * sprite_with_shadow as f64 / sprite_total.max(1) as f64
        );
        eprintln!("sprite exceptions (up to 60): {sprite_exceptions:#?}");

        // Regression guard, not a hardcoded golden count (the exact split was measured manually
        // when this heuristic was chosen and cross-checked against a real iscript disassembly —
        // see `docs/render-design.md`'s "Shadows" note): critters and plain (non-building) units
        // should keep resolving a `+1` shadow for a solid majority. Buildings and THG2 sprites
        // legitimately sit much lower (many buildings don't use this convention at all; see the
        // docs note), so they aren't asserted here — only that a future asset change didn't
        // collapse the parts of the heuristic that carry the most weight in a typical preview.
        assert!(
            critter_with_shadow as f64 / critter_total.max(1) as f64 > 0.9,
            "critters should almost always resolve a +1 shadow"
        );
        assert!(
            plain_with_shadow as f64 / plain_total.max(1) as f64 > 0.5,
            "non-building units should resolve a +1 shadow more often than not"
        );
    }

    /// Targeted real-data check for the vespene geyser special case in
    /// [`GameData::shadow_image_pre_redirect`]: image 344's `+1` (345) is a same-GRP art variant,
    /// not a shadow, and its real shadow (`neutral\geyShad.grp`, `render_style == 10`) is at
    /// `+2` (346). Gated on `BROODMAP_TEST_SCR_DIR` like the rest of this module's real-data
    /// tests.
    #[cfg(feature = "casc")]
    #[test]
    fn geyser_shadow_resolves_to_the_real_plus_two_image() {
        let Some(dir) = std::env::var_os("BROODMAP_TEST_SCR_DIR") else {
            eprintln!("skipping: BROODMAP_TEST_SCR_DIR not set");
            return;
        };
        let source = crate::source::CascSource::open(dir)
            .expect("BROODMAP_TEST_SCR_DIR should be a valid SC:R install");
        let data = GameData::load(&source).expect("the .dat tables should load");

        let main = data
            .unit_image_pre_redirect(UNIT_ID_VESPENE_GEYSER)
            .expect("the vespene geyser should resolve a main image");
        eprintln!("geyser main image: {main}");

        // +1 is a same-GRP variant, not a shadow.
        let plus_one = data.images_dat().entry(main + 1);
        assert_ne!(
            plus_one.map(|e| e.render_style),
            Some(RENDER_STYLE_SHADOW),
            "the geyser's +1 image ({}) should be a variant, not a shadow",
            main + 1
        );

        // shadow_image_pre_redirect must skip it and land on +2.
        let shadow = data.shadow_image_pre_redirect(main, Some(UNIT_ID_VESPENE_GEYSER));
        assert_eq!(
            shadow,
            Some(main + 2),
            "the geyser's shadow should resolve to +2 ({}), not +1",
            main + 2
        );
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
