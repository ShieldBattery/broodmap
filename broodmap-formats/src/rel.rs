//! Parser for `images.rel`, SC:R's per-image art redirect table.
//!
//! The file has no header: it's a flat array of fixed-size records, one per image ID, giving each
//! image an optional redirect to another image's art. Parsing is permissive: trailing bytes that
//! don't form a full record are ignored.

/// Size in bytes of a single record: `u32` rel_type, `u32` ref_image.
const RECORD_SIZE: usize = 8;

/// Sane upper bound on how many records are parsed. Real `images.rel` files have exactly 999
/// records (one per `images.dat` entry); this leaves headroom for future growth in that count
/// while still capping a hostile file (e.g. 100 MB of repeated records) from growing the parsed
/// `Vec` anywhere near that size.
const MAX_RECORDS: usize = 1024;

/// Bit in `rel_type` that, when set (and `ref_image` is present), means this image's art (its
/// `.anim` file) should be loaded from `ref_image` instead of the image's own ID.
const REDIRECT_FLAG: u32 = 0x200;

/// Sentinel `ref_image` value meaning "no redirect target".
const NO_REF: u32 = 0xFFFF_FFFF;

/// `rel_type` value marking a record as a shadow-image entry: image ID `i`'s record has
/// `rel_type == SHADOW_TYPE` exactly (a plain type code, not a flag bit like [`REDIRECT_FLAG`])
/// iff `i` is a shadow image, and that record's `ref_image` names the image it's the shadow
/// *for* (the "owner"/parent image).
///
/// Verified against a real SC:R install: the set of `rel_type == SHADOW_TYPE` record indices
/// equals the set of `images.dat` `render_style == 10` ("shadow" draw style) images exactly (230
/// of each). 218 distinct parents are named across those 230 records; 12 parents have two shadow
/// records (obscure neutral pickups with `*Shad`/`*Sha2` GRP variants, plus a couple of doodads
/// whose shadow GRP is listed under two image IDs) — consumers wanting a single shadow per
/// parent should pick the lowest shadow ID among duplicates, matching what's verified live.
/// [`Self::shadow_parent`] and [`Self::shadow_pairs`] expose this table; inverting it (owner ->
/// shadow rather than shadow -> owner) is left to callers, e.g.
/// `broodmap_render::gamedata::GameData`, which builds and gates that inverse lookup.
///
/// This type code never participates in [`Self::resolve`]'s art redirect: it isn't the
/// [`REDIRECT_FLAG`] bit, so a shadow record simply resolves to itself, same as any other
/// non-redirecting record.
const SHADOW_TYPE: u32 = 8;

/// A single `images.rel` record.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct Record {
    rel_type: u32,
    ref_image: Option<u32>,
}

/// A parsed `images.rel`: an art-redirect table indexed by image ID.
#[derive(Debug, Clone, Default)]
pub struct ImagesRel {
    records: Vec<Record>,
}

/// Parses an `images.rel` file (real size 7992 bytes for 999 records; permissive on any other
/// length — trailing bytes that don't form a full 8-byte record are ignored). Parsing never
/// produces more than [`MAX_RECORDS`] records, regardless of how much data is passed in.
pub fn parse_images_rel(data: &[u8]) -> ImagesRel {
    let records = data
        .as_chunks::<RECORD_SIZE>()
        .0
        .iter()
        .take(MAX_RECORDS)
        .map(|rec| {
            let rel_type = u32::from_le_bytes(rec[0..4].try_into().unwrap());
            let ref_raw = u32::from_le_bytes(rec[4..8].try_into().unwrap());
            let ref_image = if ref_raw == NO_REF {
                None
            } else {
                Some(ref_raw)
            };
            Record {
                rel_type,
                ref_image,
            }
        })
        .collect();

    ImagesRel { records }
}

impl ImagesRel {
    /// Resolves an image ID to the image whose art should actually be loaded for it: if the
    /// image's record has the redirect flag set and a real `ref_image` that fits in a `u16`
    /// (a valid image ID), that target is returned; otherwise (no record, flag unset, sentinel
    /// ref, or a `ref_image` too large to be a real image ID) `image_id` itself is returned
    /// unchanged. A too-large `ref_image` is never truncated (e.g. `0x10000` must not silently
    /// become image 0).
    pub fn resolve(&self, image_id: u16) -> u16 {
        match self.records.get(image_id as usize) {
            Some(&Record {
                rel_type,
                ref_image: Some(ref_image),
            }) if rel_type & REDIRECT_FLAG != 0 => u16::try_from(ref_image).unwrap_or(image_id),
            _ => image_id,
        }
    }

    /// If `image_id`'s record marks it as a shadow image (`rel_type == `[`SHADOW_TYPE`] exactly),
    /// returns the ID of the image it's a shadow *for* (its `ref_image`) -- `None` if the record
    /// isn't a shadow record, there's no record at all, or `ref_image` doesn't fit in a `u16`
    /// (mirrors [`Self::resolve`]'s handling of an oversized `ref_image`: never truncated,
    /// treated as absent instead).
    pub fn shadow_parent(&self, image_id: u16) -> Option<u16> {
        match self.records.get(image_id as usize) {
            Some(&Record {
                rel_type: SHADOW_TYPE,
                ref_image: Some(ref_image),
            }) => u16::try_from(ref_image).ok(),
            _ => None,
        }
    }

    /// Every `(shadow_id, parent_id)` pair the table encodes -- i.e. every image ID for which
    /// [`Self::shadow_parent`] returns `Some`, paired with that return value. Callers building a
    /// parent -> shadow lookup (there is no single "the" shadow per parent in the raw data --
    /// see [`SHADOW_TYPE`]'s docs on the 12 parents with two records) iterate this and pick
    /// per-parent, e.g. the lowest `shadow_id`.
    pub fn shadow_pairs(&self) -> impl Iterator<Item = (u16, u16)> + '_ {
        (0..self.records.len())
            .filter_map(|i| u16::try_from(i).ok())
            .filter_map(|shadow_id| {
                self.shadow_parent(shadow_id)
                    .map(|parent_id| (shadow_id, parent_id))
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn record(rel_type: u32, ref_image: u32) -> [u8; RECORD_SIZE] {
        let mut rec = [0u8; RECORD_SIZE];
        rec[0..4].copy_from_slice(&rel_type.to_le_bytes());
        rec[4..8].copy_from_slice(&ref_image.to_le_bytes());
        rec
    }

    #[test]
    fn redirect_flag_set_with_ref_redirects() {
        let mut data = Vec::new();
        data.extend(record(0, 0)); // image 0: no redirect
        data.extend(record(REDIRECT_FLAG, 42)); // image 1: redirect to 42

        let rel = parse_images_rel(&data);
        assert_eq!(rel.resolve(0), 0);
        assert_eq!(rel.resolve(1), 42);
    }

    #[test]
    fn redirect_flag_unset_does_not_redirect_even_with_ref() {
        let data = record(0, 42).to_vec();
        let rel = parse_images_rel(&data);
        assert_eq!(rel.resolve(0), 0);
    }

    #[test]
    fn sentinel_ref_image_does_not_redirect_even_with_flag() {
        let data = record(REDIRECT_FLAG, NO_REF).to_vec();
        let rel = parse_images_rel(&data);
        assert_eq!(rel.resolve(0), 0);
    }

    #[test]
    fn other_flag_bits_set_alongside_redirect_still_redirect() {
        let data = record(REDIRECT_FLAG | 0x1, 7).to_vec();
        let rel = parse_images_rel(&data);
        assert_eq!(rel.resolve(0), 7);
    }

    #[test]
    fn out_of_range_id_returns_itself() {
        let data = record(REDIRECT_FLAG, 7).to_vec();
        let rel = parse_images_rel(&data);
        assert_eq!(rel.resolve(1), 1);
        assert_eq!(rel.resolve(u16::MAX), u16::MAX);
    }

    #[test]
    fn trailing_partial_record_is_ignored() {
        let mut data = record(REDIRECT_FLAG, 1).to_vec();
        data.extend_from_slice(&[0xAB; 3]); // partial trailing record
        let rel = parse_images_rel(&data);
        assert_eq!(rel.records.len(), 1);
    }

    #[test]
    fn empty_input_yields_no_records() {
        let rel = parse_images_rel(&[]);
        assert_eq!(rel.resolve(0), 0);
    }

    #[test]
    fn parsing_caps_records_at_max_records() {
        // Far more records than any real file (or even MAX_RECORDS itself) would have, to prove
        // a hostile/huge file doesn't grow the parsed `Vec` past the cap.
        let extra = 500;
        let mut data = Vec::with_capacity((MAX_RECORDS + extra) * RECORD_SIZE);
        for _ in 0..MAX_RECORDS + extra {
            data.extend(record(REDIRECT_FLAG, 1));
        }

        let rel = parse_images_rel(&data);
        assert_eq!(rel.records.len(), MAX_RECORDS);

        // A record just past the cap is simply never parsed, so resolving its ID returns the ID
        // unchanged rather than following the redirect that record would have specified.
        assert_eq!(rel.resolve(MAX_RECORDS as u16), MAX_RECORDS as u16);
        // The last record within the cap is parsed and does redirect.
        assert_eq!(rel.resolve((MAX_RECORDS - 1) as u16), 1);
    }

    #[test]
    fn ref_image_that_does_not_fit_in_u16_does_not_redirect() {
        // 0x10000 would truncate (`as u16`) to 0, which must not happen: it should instead be
        // treated the same as "no redirect".
        let data = record(REDIRECT_FLAG, 0x1_0000).to_vec();
        let rel = parse_images_rel(&data);
        assert_eq!(rel.resolve(0), 0);
    }

    #[test]
    fn ref_image_at_u16_max_boundary_still_redirects() {
        // u16::MAX itself is a valid (if unusual) image ID and must round-trip, proving the fix
        // only rejects values that genuinely don't fit in a u16.
        let data = record(REDIRECT_FLAG, u16::MAX as u32).to_vec();
        let rel = parse_images_rel(&data);
        assert_eq!(rel.resolve(0), u16::MAX);
    }

    #[test]
    fn shadow_type_record_reports_its_parent() {
        let mut data = Vec::new();
        data.extend(record(0, 0)); // image 0: not a shadow
        data.extend(record(SHADOW_TYPE, 0)); // image 1: shadow of image 0

        let rel = parse_images_rel(&data);
        assert_eq!(rel.shadow_parent(0), None);
        assert_eq!(rel.shadow_parent(1), Some(0));
    }

    #[test]
    fn shadow_type_record_does_not_redirect_via_resolve() {
        // SHADOW_TYPE (8) doesn't have the REDIRECT_FLAG (0x200) bit set, so `resolve` must treat
        // a shadow record the same as any other non-redirecting record: unchanged.
        let data = record(SHADOW_TYPE, 0).to_vec();
        let rel = parse_images_rel(&data);
        assert_eq!(rel.resolve(0), 0);
    }

    #[test]
    fn non_shadow_rel_type_is_not_a_shadow_parent_even_with_a_ref_image() {
        let data = record(REDIRECT_FLAG, 5).to_vec();
        let rel = parse_images_rel(&data);
        assert_eq!(rel.shadow_parent(0), None);
    }

    #[test]
    fn shadow_parent_is_none_when_there_is_no_record() {
        let rel = parse_images_rel(&[]);
        assert_eq!(rel.shadow_parent(0), None);
    }

    #[test]
    fn shadow_parent_ref_image_that_does_not_fit_in_u16_is_none() {
        let data = record(SHADOW_TYPE, 0x1_0000).to_vec();
        let rel = parse_images_rel(&data);
        assert_eq!(rel.shadow_parent(0), None);
    }

    #[test]
    fn shadow_parent_sentinel_ref_image_is_none() {
        let data = record(SHADOW_TYPE, NO_REF).to_vec();
        let rel = parse_images_rel(&data);
        assert_eq!(rel.shadow_parent(0), None);
    }

    #[test]
    fn shadow_pairs_yields_every_shadow_record_and_only_those() {
        let mut data = Vec::new();
        data.extend(record(0, 0)); // image 0: not a shadow
        data.extend(record(SHADOW_TYPE, 0)); // image 1: shadow of image 0
        data.extend(record(REDIRECT_FLAG, 3)); // image 2: a redirect, not a shadow
        data.extend(record(SHADOW_TYPE, 0)); // image 3: also a shadow of image 0

        let rel = parse_images_rel(&data);
        let pairs: Vec<_> = rel.shadow_pairs().collect();
        assert_eq!(pairs, vec![(1, 0), (3, 0)]);
    }

    #[test]
    fn shadow_pairs_is_empty_for_a_table_with_no_shadow_records() {
        let data = record(REDIRECT_FLAG, 1).to_vec();
        let rel = parse_images_rel(&data);
        assert_eq!(rel.shadow_pairs().count(), 0);
    }
}
