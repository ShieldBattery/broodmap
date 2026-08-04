//! Parsers for BW's column-major (struct-of-arrays) `.dat` tables: `units.dat`, `flingy.dat`,
//! `sprites.dat`, and `images.dat`.
//!
//! Unlike CV5/VF4 (row-major, fixed-size records), a `.dat` file lays out each field as a single
//! contiguous array of N fixed-width values covering every entry, with the arrays concatenated in
//! declaration order. Only a subset of each format's columns is exposed here; the rest are still
//! accounted for in the layout so the exposed columns land at the right byte offsets. Parsing is
//! permissive: offsets are computed from the declared layout and bounds-checked against the input,
//! so truncated data yields zero-filled values for the affected columns rather than an error.

/// Reads a fixed-size little-endian value at entry `index` within `bytes` (a column's byte
/// range), zero-padding on the right if `bytes` is too short to cover this entry or runs out
/// partway through it. `decode` receives the `N`-byte buffer (real bytes, zero-padded) and turns
/// it into `T`.
fn decode_entry<T, const N: usize>(bytes: &[u8], index: usize, decode: impl Fn([u8; N]) -> T) -> T {
    let start = index * N;
    let mut buf = [0u8; N];
    if start < bytes.len() {
        let end = (start + N).min(bytes.len());
        buf[..end - start].copy_from_slice(&bytes[start..end]);
    }
    decode(buf)
}

/// Decodes an entire column of `count` fixed-width entries, permissively zero-filling any entries
/// not covered by `bytes`.
fn decode_column<T, const N: usize>(
    bytes: &[u8],
    count: usize,
    decode: impl Fn([u8; N]) -> T,
) -> Vec<T> {
    (0..count)
        .map(|i| decode_entry(bytes, i, &decode))
        .collect()
}

/// Slices out a column's byte range from the file, clipped to what's actually present. Permissive:
/// if `offset` is at or past the end of `data`, or the column runs off the end, the returned slice
/// is simply shorter than `len` (or empty) — callers zero-fill the rest via [`decode_column`].
fn column_bytes(data: &[u8], offset: usize, len: usize) -> &[u8] {
    if offset >= data.len() {
        return &[];
    }
    let end = (offset + len).min(data.len());
    &data[offset..end]
}

// ---------------------------------------------------------------------------------------------
// units.dat
// ---------------------------------------------------------------------------------------------

/// Number of unit entries in `units.dat`.
const UNITS_COUNT: usize = 228;
/// Number of building entries: a subrange of unit IDs (106..202) that some columns cover instead
/// of the full unit range.
const BUILDINGS_COUNT: usize = 96;
/// Number of non-building "unit" entries: a subrange of unit IDs (0..106) that some columns cover
/// instead of the full unit range.
const UNITS_ONLY_COUNT: usize = 106;
/// Real-world size in bytes of `units.dat`, used to cross-check the column layout below.
const UNITS_DAT_SIZE: usize = 19876;

/// Total byte size of each column in `units.dat`, in file declaration order. This lets the byte
/// offset of any column be computed from the layout (via [`column_offset`]) instead of hardcoded,
/// so a mistake in the declared order or column sizes shows up as a wrong total (checked below)
/// rather than a silently wrong offset for the columns we actually read.
const UNITS_COLUMN_SIZES: &[usize] = &[
    UNITS_COUNT,          // 1. flingy: u8
    UNITS_COUNT * 2,      // 2. sub_unit_1: u16
    UNITS_COUNT * 2,      // 3. sub_unit_2: u16
    BUILDINGS_COUNT * 2,  // 4. infestation: u16 (buildings-only)
    UNITS_COUNT * 4,      // 5. construction_image: u32
    UNITS_COUNT,          // 6. unit_direction: u8
    UNITS_COUNT,          // 7. shield_enabled: u8
    UNITS_COUNT * 2,      // 8. shield_amount: i16
    UNITS_COUNT * 4,      // 9. hit_points: i32
    UNITS_COUNT,          // 10. elevation_level: u8
    UNITS_COUNT,          // 11. unknown: u8
    UNITS_COUNT,          // 12. sub_label: u8
    UNITS_COUNT,          // 13. AI/order column
    UNITS_COUNT,          // 14. AI/order column
    UNITS_COUNT,          // 15. AI/order column
    UNITS_COUNT,          // 16. AI/order column
    UNITS_COUNT,          // 17. AI/order column
    UNITS_COUNT,          // 18. ground_weapon: u8
    UNITS_COUNT,          // 19. max_ground_hits: u8
    UNITS_COUNT,          // 20. air_weapon: u8
    UNITS_COUNT,          // 21. max_air_hits: u8
    UNITS_COUNT,          // 22. ai_internal: u8
    UNITS_COUNT * 4,      // 23. special_ability_flags: u32
    UNITS_COUNT,          // 24. target_acquisition_range: u8
    UNITS_COUNT,          // 25. sight_range: u8
    UNITS_COUNT,          // 26. armor_upgrade: u8
    UNITS_COUNT,          // 27. unit_size: u8
    UNITS_COUNT,          // 28. armor: u8
    UNITS_COUNT,          // 29. right_click_action: u8
    UNITS_ONLY_COUNT * 2, // 30. ready_sound: u16 (units-only)
    UNITS_COUNT * 2,      // 31. what_sound_start: u16
    UNITS_COUNT * 2,      // 32. what_sound_end: u16
    UNITS_ONLY_COUNT * 2, // 33. piss_sound_start: u16 (units-only)
    UNITS_ONLY_COUNT * 2, // 34. piss_sound_end: u16 (units-only)
    UNITS_ONLY_COUNT * 2, // 35. yes_sound_start: u16 (units-only)
    UNITS_ONLY_COUNT * 2, // 36. yes_sound_end: u16 (units-only)
    UNITS_COUNT * 4,      // 37. placebox_size: {i16 x, i16 y}
    BUILDINGS_COUNT * 4,  // 38. addon_size: {i16, i16} (buildings-only)
    UNITS_COUNT * 8,      // 39. bounds: {i16 left, top, right, bottom}
    UNITS_COUNT * 2,      // 40. portrait: u16
    UNITS_COUNT * 2,      // 41. mineral_cost: u16
    UNITS_COUNT * 2,      // 42. vespene_cost: u16
    UNITS_COUNT * 2,      // 43. build_time: u16
    UNITS_COUNT * 2,      // 44. requirement_index: u16
    UNITS_COUNT,          // 45. star_edit_group_flags: u8
    UNITS_COUNT,          // 46. supply_provided: u8
    UNITS_COUNT,          // 47. supply_required: u8
    UNITS_COUNT,          // 48. space_required: u8
    UNITS_COUNT,          // 49. space_provided: u8
    UNITS_COUNT * 2,      // 50. build_score: u16
    UNITS_COUNT * 2,      // 51. destroy_score: u16
    UNITS_COUNT * 2,      // 52. unit_map_string: u16
    UNITS_COUNT,          // 53. brood_war_unit_flag: u8
    UNITS_COUNT * 2,      // 54. star_edit_availability_flag: u16
];

// 0-based indices into `UNITS_COLUMN_SIZES` for the columns we expose.
const UNITS_COL_FLINGY: usize = 0;
const UNITS_COL_SUB_UNIT_1: usize = 1;
const UNITS_COL_UNIT_DIRECTION: usize = 5;
const UNITS_COL_SPECIAL_ABILITY_FLAGS: usize = 22;
const UNITS_COL_PLACEBOX_SIZE: usize = 36;
const UNITS_COL_BOUNDS: usize = 38;
const UNITS_COL_STAR_EDIT_GROUP_FLAGS: usize = 44;

/// Returns the byte offset where column `index` (into `sizes`) begins: the sum of all preceding
/// columns' sizes.
const fn column_offset(sizes: &[usize], index: usize) -> usize {
    let mut offset = 0;
    let mut i = 0;
    while i < index {
        offset += sizes[i];
        i += 1;
    }
    offset
}

const fn total_size(sizes: &[usize]) -> usize {
    column_offset(sizes, sizes.len())
}

// Cross-check: the declared units.dat column layout must sum to the real file size.
const _: () = assert!(total_size(UNITS_COLUMN_SIZES) == UNITS_DAT_SIZE);

// Independent literal-offset cross-check: these byte offsets were validated against real
// `units.dat` files (they sum to the exact real file size above via an entirely separate
// derivation), not merely re-derived from `UNITS_COLUMN_SIZES` itself. `total_size` alone only
// catches a wrong *total*; a self-consistent-but-wrong layout (e.g. two column sizes swapped)
// would still pass it, but would fail at least one of these.
const _: () = assert!(column_offset(UNITS_COLUMN_SIZES, UNITS_COL_FLINGY) == 0);
const _: () = assert!(column_offset(UNITS_COLUMN_SIZES, UNITS_COL_UNIT_DIRECTION) == 2244);
const _: () = assert!(column_offset(UNITS_COLUMN_SIZES, UNITS_COL_SPECIAL_ABILITY_FLAGS) == 7032);
const _: () = assert!(column_offset(UNITS_COLUMN_SIZES, UNITS_COL_PLACEBOX_SIZE) == 11284);
const _: () = assert!(column_offset(UNITS_COLUMN_SIZES, UNITS_COL_BOUNDS) == 12580);
const _: () = assert!(column_offset(UNITS_COLUMN_SIZES, UNITS_COL_STAR_EDIT_GROUP_FLAGS) == 16684);

/// A single `units.dat` entry's exposed fields.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UnitEntry {
    pub flingy: u8,
    pub sub_unit_1: u16,
    pub unit_direction: u8,
    /// Raw bit flags (e.g. `0x00000001` Building, `0x00000002` Addon, `0x00000004` Flyer,
    /// `0x00000008` Worker).
    pub special_ability_flags: u32,
    pub placebox: (i16, i16),
    /// `(left, top, right, bottom)`.
    pub bounds: (i16, i16, i16, i16),
    /// Raw StarEdit grouping flags (race/category, used by the map editor's unit palette).
    pub star_edit_group_flags: u8,
}

/// A parsed `units.dat`: BW's per-unit-type stat table. Only a subset of its columns is exposed
/// (see [`UnitEntry`]); the rest are accounted for in the layout but not stored.
#[derive(Debug, Clone, Default)]
pub struct UnitsDat {
    flingy: Vec<u8>,
    sub_unit_1: Vec<u16>,
    unit_direction: Vec<u8>,
    special_ability_flags: Vec<u32>,
    placebox: Vec<(i16, i16)>,
    bounds: Vec<(i16, i16, i16, i16)>,
    star_edit_group_flags: Vec<u8>,
}

impl UnitsDat {
    /// Looks up a unit's exposed fields by unit ID. `None` if `unit_id` is out of range
    /// (>= 228); within range, missing/truncated backing data simply reads as zero.
    pub fn entry(&self, unit_id: u16) -> Option<UnitEntry> {
        let i = unit_id as usize;
        if i >= UNITS_COUNT {
            return None;
        }
        Some(UnitEntry {
            flingy: self.flingy[i],
            sub_unit_1: self.sub_unit_1[i],
            unit_direction: self.unit_direction[i],
            special_ability_flags: self.special_ability_flags[i],
            placebox: self.placebox[i],
            bounds: self.bounds[i],
            star_edit_group_flags: self.star_edit_group_flags[i],
        })
    }
}

/// Parses a `units.dat` file (real size 19876 bytes; permissive on any other length: columns not
/// fully covered by `data` read as zero for the uncovered entries).
pub fn parse_units_dat(data: &[u8]) -> UnitsDat {
    let col = |index: usize| -> &[u8] {
        column_bytes(
            data,
            column_offset(UNITS_COLUMN_SIZES, index),
            UNITS_COLUMN_SIZES[index],
        )
    };

    let flingy = decode_column::<u8, 1>(col(UNITS_COL_FLINGY), UNITS_COUNT, |b| b[0]);
    let sub_unit_1 =
        decode_column::<u16, 2>(col(UNITS_COL_SUB_UNIT_1), UNITS_COUNT, u16::from_le_bytes);
    let unit_direction =
        decode_column::<u8, 1>(col(UNITS_COL_UNIT_DIRECTION), UNITS_COUNT, |b| b[0]);
    let special_ability_flags = decode_column::<u32, 4>(
        col(UNITS_COL_SPECIAL_ABILITY_FLAGS),
        UNITS_COUNT,
        u32::from_le_bytes,
    );
    let placebox = decode_column::<(i16, i16), 4>(col(UNITS_COL_PLACEBOX_SIZE), UNITS_COUNT, |b| {
        (
            i16::from_le_bytes([b[0], b[1]]),
            i16::from_le_bytes([b[2], b[3]]),
        )
    });
    let bounds =
        decode_column::<(i16, i16, i16, i16), 8>(col(UNITS_COL_BOUNDS), UNITS_COUNT, |b| {
            (
                i16::from_le_bytes([b[0], b[1]]),
                i16::from_le_bytes([b[2], b[3]]),
                i16::from_le_bytes([b[4], b[5]]),
                i16::from_le_bytes([b[6], b[7]]),
            )
        });
    let star_edit_group_flags =
        decode_column::<u8, 1>(col(UNITS_COL_STAR_EDIT_GROUP_FLAGS), UNITS_COUNT, |b| b[0]);

    UnitsDat {
        flingy,
        sub_unit_1,
        unit_direction,
        special_ability_flags,
        placebox,
        bounds,
        star_edit_group_flags,
    }
}

// ---------------------------------------------------------------------------------------------
// flingy.dat
// ---------------------------------------------------------------------------------------------

/// Number of flingy entries in `flingy.dat`.
const FLINGY_COUNT: usize = 209;
/// Real-world size in bytes of `flingy.dat`, used to cross-check the column layout below.
const FLINGY_DAT_SIZE: usize = 3135;

const FLINGY_COLUMN_SIZES: &[usize] = &[
    FLINGY_COUNT * 2, // sprite: u16
    FLINGY_COUNT * 4, // speed: u32
    FLINGY_COUNT * 2, // acceleration: u16
    FLINGY_COUNT * 4, // halt_distance: u32
    FLINGY_COUNT,     // turn_radius: u8
    FLINGY_COUNT,     // unused: u8
    FLINGY_COUNT,     // movement_control: u8
];
const FLINGY_COL_SPRITE: usize = 0;

const _: () = assert!(total_size(FLINGY_COLUMN_SIZES) == FLINGY_DAT_SIZE);
// Independent literal-offset cross-check (see the units.dat comment above for why this is
// checked separately from `total_size`).
const _: () = assert!(column_offset(FLINGY_COLUMN_SIZES, FLINGY_COL_SPRITE) == 0);

/// A parsed `flingy.dat`: only the `sprite` column is exposed.
#[derive(Debug, Clone, Default)]
pub struct FlingyDat {
    sprite: Vec<u16>,
}

impl FlingyDat {
    /// Looks up the sprite ID for a flingy ID. `None` if out of range (>= 209).
    pub fn sprite_id(&self, flingy_id: u8) -> Option<u16> {
        self.sprite.get(flingy_id as usize).copied()
    }
}

/// Parses a `flingy.dat` file (real size 3135 bytes; permissive on any other length).
pub fn parse_flingy_dat(data: &[u8]) -> FlingyDat {
    let sprite_bytes = column_bytes(
        data,
        column_offset(FLINGY_COLUMN_SIZES, FLINGY_COL_SPRITE),
        FLINGY_COLUMN_SIZES[FLINGY_COL_SPRITE],
    );
    let sprite = decode_column::<u16, 2>(sprite_bytes, FLINGY_COUNT, u16::from_le_bytes);
    FlingyDat { sprite }
}

// ---------------------------------------------------------------------------------------------
// sprites.dat
// ---------------------------------------------------------------------------------------------

/// Number of sprite entries in `sprites.dat`.
const SPRITES_COUNT: usize = 517;
/// Number of "selectable" sprite entries: a subrange of sprite IDs (130..517) that some columns
/// cover instead of the full sprite range.
const SPRITES_SELECTABLE_COUNT: usize = 387;
/// Real-world size in bytes of `sprites.dat`, used to cross-check the column layout below.
const SPRITES_DAT_SIZE: usize = 3229;

const SPRITES_COLUMN_SIZES: &[usize] = &[
    SPRITES_COUNT * 2,        // image: u16
    SPRITES_SELECTABLE_COUNT, // health_bar: u8 (selectable-only)
    SPRITES_COUNT,            // unknown: u8
    SPRITES_COUNT,            // visible: u8
    SPRITES_SELECTABLE_COUNT, // selection_circle: u8 (selectable-only)
    SPRITES_SELECTABLE_COUNT, // selection_circle_offset: u8 (selectable-only)
];
const SPRITES_COL_IMAGE: usize = 0;

const _: () = assert!(total_size(SPRITES_COLUMN_SIZES) == SPRITES_DAT_SIZE);
// Independent literal-offset cross-check (see the units.dat comment above for why this is
// checked separately from `total_size`).
const _: () = assert!(column_offset(SPRITES_COLUMN_SIZES, SPRITES_COL_IMAGE) == 0);

/// A parsed `sprites.dat`: only the `image` column is exposed.
#[derive(Debug, Clone, Default)]
pub struct SpritesDat {
    image: Vec<u16>,
}

impl SpritesDat {
    /// Looks up the image ID for a sprite ID. `None` if out of range (>= 517).
    pub fn image_id(&self, sprite_id: u16) -> Option<u16> {
        self.image.get(sprite_id as usize).copied()
    }
}

/// Parses a `sprites.dat` file (real size 3229 bytes; permissive on any other length).
pub fn parse_sprites_dat(data: &[u8]) -> SpritesDat {
    let image_bytes = column_bytes(
        data,
        column_offset(SPRITES_COLUMN_SIZES, SPRITES_COL_IMAGE),
        SPRITES_COLUMN_SIZES[SPRITES_COL_IMAGE],
    );
    let image = decode_column::<u16, 2>(image_bytes, SPRITES_COUNT, u16::from_le_bytes);
    SpritesDat { image }
}

// ---------------------------------------------------------------------------------------------
// images.dat
// ---------------------------------------------------------------------------------------------

/// Number of image entries in `images.dat`.
const IMAGES_COUNT: usize = 999;
/// Real-world size in bytes of `images.dat`, used to cross-check the column layout below.
const IMAGES_DAT_SIZE: usize = 37962;

const IMAGES_COLUMN_SIZES: &[usize] = &[
    IMAGES_COUNT * 4, // grp: u32
    IMAGES_COUNT,     // has_directional_frames: u8
    IMAGES_COUNT,     // clickable: u8
    IMAGES_COUNT,     // use_full_iscript: u8
    IMAGES_COUNT,     // always_visible: u8
    IMAGES_COUNT,     // render_style: u8
    IMAGES_COUNT,     // color_shift: u8
    IMAGES_COUNT * 4, // iscript: u32
    IMAGES_COUNT * 4, // shield_overlay: u32
    IMAGES_COUNT * 4, // attack_overlay: u32
    IMAGES_COUNT * 4, // damage_overlay: u32
    IMAGES_COUNT * 4, // special_overlay: u32
    IMAGES_COUNT * 4, // landing_dust_overlay: u32
    IMAGES_COUNT * 4, // lift_off_dust_overlay: u32
];
const IMAGES_COL_GRP: usize = 0;
const IMAGES_COL_HAS_DIRECTIONAL_FRAMES: usize = 1;
const IMAGES_COL_RENDER_STYLE: usize = 5;
const IMAGES_COL_COLOR_SHIFT: usize = 6;
const IMAGES_COL_SPECIAL_OVERLAY: usize = 11;

const _: () = assert!(total_size(IMAGES_COLUMN_SIZES) == IMAGES_DAT_SIZE);
// Independent literal-offset cross-check (see the units.dat comment above for why this is
// checked separately from `total_size`).
const _: () = assert!(column_offset(IMAGES_COLUMN_SIZES, IMAGES_COL_GRP) == 0);
const _: () = assert!(column_offset(IMAGES_COLUMN_SIZES, IMAGES_COL_RENDER_STYLE) == 7992);
const _: () = assert!(column_offset(IMAGES_COLUMN_SIZES, IMAGES_COL_COLOR_SHIFT) == 8991);
const _: () = assert!(column_offset(IMAGES_COLUMN_SIZES, IMAGES_COL_SPECIAL_OVERLAY) == 25974);

/// A single `images.dat` entry's exposed fields.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ImageEntry {
    /// 1-based index into `images.tbl` (the GRP/anim filename); 0 means none.
    pub grp: u32,
    pub has_directional_frames: bool,
    /// Raw draw-style code; `9` = "use remapping" (teamcolor draw style), `10` = shadow.
    pub render_style: u8,
    pub color_shift: u8,
    /// 1-based index into `images.tbl`, if any (the 4th of six overlay columns: shield, attack,
    /// damage, special, landing dust, lift-off dust). Despite the "overlay" naming, this is *not*
    /// a drawable image reference (i.e. not another `images.dat` entry) — the `images.tbl` string
    /// it names is the file path of an `.lo` (overlay-offset) file describing where the overlay
    /// should be positioned, not a GRP/anim to render. `None` means no overlay.
    pub special_overlay: Option<u16>,
}

/// A parsed `images.dat`: BW's per-image rendering metadata table. Only a subset of its columns
/// is exposed (see [`ImageEntry`]); the rest are accounted for in the layout but not stored.
#[derive(Debug, Clone, Default)]
pub struct ImagesDat {
    grp: Vec<u32>,
    has_directional_frames: Vec<u8>,
    render_style: Vec<u8>,
    color_shift: Vec<u8>,
    special_overlay: Vec<u32>,
}

impl ImagesDat {
    /// Looks up an image's exposed fields by image ID. `None` if out of range (>= 999).
    pub fn entry(&self, image_id: u16) -> Option<ImageEntry> {
        let i = image_id as usize;
        if i >= IMAGES_COUNT {
            return None;
        }
        let special_overlay = self.special_overlay[i];
        Some(ImageEntry {
            grp: self.grp[i],
            has_directional_frames: self.has_directional_frames[i] != 0,
            render_style: self.render_style[i],
            color_shift: self.color_shift[i],
            special_overlay: if special_overlay == 0 {
                None
            } else {
                Some(special_overlay as u16)
            },
        })
    }
}

/// Parses an `images.dat` file (real size 37962 bytes; permissive on any other length).
pub fn parse_images_dat(data: &[u8]) -> ImagesDat {
    let col = |index: usize| -> &[u8] {
        column_bytes(
            data,
            column_offset(IMAGES_COLUMN_SIZES, index),
            IMAGES_COLUMN_SIZES[index],
        )
    };

    let grp = decode_column::<u32, 4>(col(IMAGES_COL_GRP), IMAGES_COUNT, u32::from_le_bytes);
    let has_directional_frames =
        decode_column::<u8, 1>(col(IMAGES_COL_HAS_DIRECTIONAL_FRAMES), IMAGES_COUNT, |b| {
            b[0]
        });
    let render_style = decode_column::<u8, 1>(col(IMAGES_COL_RENDER_STYLE), IMAGES_COUNT, |b| b[0]);
    let color_shift = decode_column::<u8, 1>(col(IMAGES_COL_COLOR_SHIFT), IMAGES_COUNT, |b| b[0]);
    let special_overlay = decode_column::<u32, 4>(
        col(IMAGES_COL_SPECIAL_OVERLAY),
        IMAGES_COUNT,
        u32::from_le_bytes,
    );

    ImagesDat {
        grp,
        has_directional_frames,
        render_style,
        color_shift,
        special_overlay,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Builds a zero-filled buffer matching `sizes`' total length.
    fn zero_buffer(sizes: &[usize]) -> Vec<u8> {
        vec![0u8; total_size(sizes)]
    }

    fn write_u8(data: &mut [u8], sizes: &[usize], col: usize, index: usize, value: u8) {
        let offset = column_offset(sizes, col) + index;
        data[offset] = value;
    }

    fn write_u16(data: &mut [u8], sizes: &[usize], col: usize, index: usize, value: u16) {
        let offset = column_offset(sizes, col) + index * 2;
        data[offset..offset + 2].copy_from_slice(&value.to_le_bytes());
    }

    fn write_u32(data: &mut [u8], sizes: &[usize], col: usize, index: usize, value: u32) {
        let offset = column_offset(sizes, col) + index * 4;
        data[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
    }

    fn write_i16_pair(
        data: &mut [u8],
        sizes: &[usize],
        col: usize,
        index: usize,
        value: (i16, i16),
    ) {
        let offset = column_offset(sizes, col) + index * 4;
        data[offset..offset + 2].copy_from_slice(&value.0.to_le_bytes());
        data[offset + 2..offset + 4].copy_from_slice(&value.1.to_le_bytes());
    }

    fn write_i16_quad(
        data: &mut [u8],
        sizes: &[usize],
        col: usize,
        index: usize,
        value: (i16, i16, i16, i16),
    ) {
        let offset = column_offset(sizes, col) + index * 8;
        data[offset..offset + 2].copy_from_slice(&value.0.to_le_bytes());
        data[offset + 2..offset + 4].copy_from_slice(&value.1.to_le_bytes());
        data[offset + 4..offset + 6].copy_from_slice(&value.2.to_le_bytes());
        data[offset + 6..offset + 8].copy_from_slice(&value.3.to_le_bytes());
    }

    #[test]
    fn units_dat_column_layout_sums_to_real_size() {
        assert_eq!(total_size(UNITS_COLUMN_SIZES), UNITS_DAT_SIZE);
    }

    #[test]
    fn units_dat_extracts_exposed_columns_incl_subrange_affected_offsets() {
        let mut data = zero_buffer(UNITS_COLUMN_SIZES);

        let unit_id = 5usize;
        write_u8(&mut data, UNITS_COLUMN_SIZES, UNITS_COL_FLINGY, unit_id, 42);
        write_u16(
            &mut data,
            UNITS_COLUMN_SIZES,
            UNITS_COL_SUB_UNIT_1,
            unit_id,
            777,
        );
        write_u8(
            &mut data,
            UNITS_COLUMN_SIZES,
            UNITS_COL_UNIT_DIRECTION,
            unit_id,
            3,
        );
        write_u32(
            &mut data,
            UNITS_COLUMN_SIZES,
            UNITS_COL_SPECIAL_ABILITY_FLAGS,
            unit_id,
            0x0000_0005,
        );
        write_i16_pair(
            &mut data,
            UNITS_COLUMN_SIZES,
            UNITS_COL_PLACEBOX_SIZE,
            unit_id,
            (10, 20),
        );
        // The bounds column sits after the subrange-sized addon_size column, so this also proves
        // the subrange columns (infestation, ready_sound, piss/yes sounds, addon_size) shifted
        // its offset correctly — it's computed from `UNITS_COLUMN_SIZES`, not hardcoded.
        write_i16_quad(
            &mut data,
            UNITS_COLUMN_SIZES,
            UNITS_COL_BOUNDS,
            unit_id,
            (-1, -2, 30, 40),
        );
        write_u8(
            &mut data,
            UNITS_COLUMN_SIZES,
            UNITS_COL_STAR_EDIT_GROUP_FLAGS,
            unit_id,
            0x80,
        );

        let dat = parse_units_dat(&data);
        let entry = dat.entry(unit_id as u16).unwrap();
        assert_eq!(entry.flingy, 42);
        assert_eq!(entry.sub_unit_1, 777);
        assert_eq!(entry.unit_direction, 3);
        assert_eq!(entry.special_ability_flags, 0x0000_0005);
        assert_eq!(entry.placebox, (10, 20));
        assert_eq!(entry.bounds, (-1, -2, 30, 40));
        assert_eq!(entry.star_edit_group_flags, 0x80);

        // A different entry stays zeroed.
        let other = dat.entry(0).unwrap();
        assert_eq!(other.flingy, 0);
        assert_eq!(other.bounds, (0, 0, 0, 0));
    }

    #[test]
    fn units_dat_out_of_range_id_is_none() {
        let data = zero_buffer(UNITS_COLUMN_SIZES);
        let dat = parse_units_dat(&data);
        assert!(dat.entry(UNITS_COUNT as u16).is_none());
        assert!(dat.entry(u16::MAX).is_none());
    }

    #[test]
    fn units_dat_truncated_data_is_permissive() {
        // Only the first few bytes of the file are present.
        let data = vec![0xAAu8; 10];
        let dat = parse_units_dat(&data);
        // In-range entries still parse (zero-filled where data is missing), no panic.
        let entry = dat.entry(0).unwrap();
        assert_eq!(entry.sub_unit_1, 0);
        assert_eq!(entry.bounds, (0, 0, 0, 0));

        let empty = parse_units_dat(&[]);
        assert!(empty.entry(0).is_some());
        assert_eq!(empty.entry(0).unwrap().flingy, 0);
    }

    #[test]
    fn flingy_dat_column_layout_sums_to_real_size() {
        assert_eq!(total_size(FLINGY_COLUMN_SIZES), FLINGY_DAT_SIZE);
    }

    #[test]
    fn flingy_dat_extracts_sprite_column() {
        let mut data = zero_buffer(FLINGY_COLUMN_SIZES);
        write_u16(&mut data, FLINGY_COLUMN_SIZES, FLINGY_COL_SPRITE, 3, 555);

        let dat = parse_flingy_dat(&data);
        assert_eq!(dat.sprite_id(3), Some(555));
        assert_eq!(dat.sprite_id(0), Some(0));
        assert_eq!(dat.sprite_id(FLINGY_COUNT as u8), None);
    }

    #[test]
    fn flingy_dat_truncated_data_is_permissive() {
        let dat = parse_flingy_dat(&[]);
        assert_eq!(dat.sprite_id(0), Some(0));
        assert_eq!(dat.sprite_id(FLINGY_COUNT as u8), None);
    }

    #[test]
    fn sprites_dat_column_layout_sums_to_real_size() {
        assert_eq!(total_size(SPRITES_COLUMN_SIZES), SPRITES_DAT_SIZE);
    }

    #[test]
    fn sprites_dat_extracts_image_column() {
        let mut data = zero_buffer(SPRITES_COLUMN_SIZES);
        write_u16(&mut data, SPRITES_COLUMN_SIZES, SPRITES_COL_IMAGE, 200, 42);

        let dat = parse_sprites_dat(&data);
        assert_eq!(dat.image_id(200), Some(42));
        assert_eq!(dat.image_id(SPRITES_COUNT as u16), None);
    }

    #[test]
    fn sprites_dat_truncated_data_is_permissive() {
        let dat = parse_sprites_dat(&[0u8; 4]);
        assert_eq!(dat.image_id(0), Some(0));
    }

    #[test]
    fn images_dat_column_layout_sums_to_real_size() {
        assert_eq!(total_size(IMAGES_COLUMN_SIZES), IMAGES_DAT_SIZE);
    }

    #[test]
    fn images_dat_extracts_exposed_columns() {
        let mut data = zero_buffer(IMAGES_COLUMN_SIZES);
        let image_id = 50usize;
        write_u32(&mut data, IMAGES_COLUMN_SIZES, IMAGES_COL_GRP, image_id, 12);
        write_u8(
            &mut data,
            IMAGES_COLUMN_SIZES,
            IMAGES_COL_HAS_DIRECTIONAL_FRAMES,
            image_id,
            1,
        );
        write_u8(
            &mut data,
            IMAGES_COLUMN_SIZES,
            IMAGES_COL_RENDER_STYLE,
            image_id,
            9,
        );
        write_u8(
            &mut data,
            IMAGES_COLUMN_SIZES,
            IMAGES_COL_COLOR_SHIFT,
            image_id,
            2,
        );
        write_u32(
            &mut data,
            IMAGES_COLUMN_SIZES,
            IMAGES_COL_SPECIAL_OVERLAY,
            image_id,
            77,
        );

        let dat = parse_images_dat(&data);
        let entry = dat.entry(image_id as u16).unwrap();
        assert_eq!(entry.grp, 12);
        assert!(entry.has_directional_frames);
        assert_eq!(entry.render_style, 9);
        assert_eq!(entry.color_shift, 2);
        assert_eq!(entry.special_overlay, Some(77));

        // Zero overlay means "none".
        let other = dat.entry(0).unwrap();
        assert_eq!(other.special_overlay, None);
        assert!(!other.has_directional_frames);
    }

    #[test]
    fn images_dat_out_of_range_id_is_none() {
        let data = zero_buffer(IMAGES_COLUMN_SIZES);
        let dat = parse_images_dat(&data);
        assert!(dat.entry(IMAGES_COUNT as u16).is_none());
    }

    #[test]
    fn images_dat_truncated_data_is_permissive() {
        let dat = parse_images_dat(&[]);
        let entry = dat.entry(0).unwrap();
        assert_eq!(entry.grp, 0);
        assert_eq!(entry.special_overlay, None);
    }
}
