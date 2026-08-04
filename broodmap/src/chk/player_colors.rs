//! Parsing for the `COLR` ("Player colors") and `CRGB` ("Player colors [SC:R]") chunks, plus a
//! resolved, chunk-agnostic view of the 8 players' colors.
//!
//! ## Researched layout
//!
//! The layouts below were cross-checked against two independent sources that agree byte-for-byte:
//!
//! - The StarEdit Network CHK format wiki
//!   (<http://www.staredit.net/wiki/index.php?title=Scenario.chk>), fetched via `action=raw` to
//!   read the "COLR" and "CRGB" section wikitext directly.
//! - The [Chkdraft](https://github.com/TheNitesWhoSay/Chkdraft) map editor's C++ source
//!   (`src/mapping_core/chk.h` for the `Chk::COLR`/`Chk::CRGB`/`Chk::PlayerColor`/
//!   `Chk::PlayerColorSetting` definitions, and `src/mapping_core/render/sc_gl_graphics.cpp`'s
//!   `MapGraphics::getPlayerColor` for the rendering/precedence logic and the SC:R-only color
//!   RGB table). Chkdraft's hardcoded RGB values were decoded from its packed `0xAABBGGRR` u32s
//!   and spot-checked against the wiki's hex codes for all 22 documented color indices - they
//!   match exactly (e.g. index 12 "Pale Green": wiki `#74A47C`, Chkdraft `0xFF7CA474` ->
//!   R=0x74,G=0xA4,B=0x7C).
//!
//! (`bw-chk` (<https://github.com/ShieldBattery/bw-chk>) was also checked, but it does not
//! implement `COLR`/`CRGB` parsing at all, so it wasn't usable as a cross-check for the layout.)
//!
//! ### `COLR` -- "Player colors" (8 bytes)
//!
//! Documented as required for Brood War onward (all game types); validation requires the section
//! be exactly 8 bytes.
//!
//! - `u8[8]`: one byte per player slot (0-7), each an index into the
//!   [color-choice table](DEFAULT_COLOR_TABLE) below. Values above 11 are "overflow" - as of patch
//!   1.18.6 they resolve to the slot's default color.
//! - The wiki notes that when `CRGB` is also present, `COLR` still defines the *leaderboard* text
//!   color independently of `CRGB`'s in-game color. This module does not model that distinction;
//!   [`PlayerColors`] represents the primary (in-game) color, matching how Chkdraft's renderer
//!   resolves colors for drawing (see precedence below).
//!
//! ### `CRGB` -- "Player colors [SC:R]" (32 bytes)
//!
//! Requirement is undocumented (can appear in any map type, including non-remaster maps);
//! validation requires the section be exactly 32 bytes.
//!
//! - `u8[8][3]`: an RGB triple per player (R, G, B in that byte order).
//! - `u8[8]`: a color-selection mode byte per player:
//!   - `0` = Random predefined color.
//!   - `1` = Player's choice (no fixed color is stored; the lobby/player picks one).
//!   - `2` = Custom RGB color (use that player's RGB triple as-is).
//!   - `3` = Use a `COLR`-style color index, stored in the *blue* byte of that player's RGB
//!     triple (the R and G bytes are documented/observed to contain junk data in this mode).
//!
//! SC:R extends the color-choice table with indices 12-21 (only selectable via `CRGB`'s index
//! mode, not via `COLR` itself); index 22+ means "random" (may repeat a color already in use).
//!
//! ### Precedence between `COLR` and `CRGB`
//!
//! SC:R writes both chunks for backwards compatibility. Chkdraft's rendering code
//! (`MapGraphics::getPlayerColor`) uses `CRGB` (interpreted per the selection-mode byte above)
//! whenever it is present, and only falls back to `COLR`'s index when `CRGB` is absent. This
//! module mirrors that: [`resolve_player_colors`] uses `CRGB` for all 8 players whenever the
//! chunk is present at all, regardless of whether `COLR` is also present.
use thiserror::Error;

/// RGB values for the documented player color choices, indexed by their `COLR`/`CRGB`-index
/// value. Indices 0-11 are the original/Brood War color choices (selectable via both `COLR` and
/// `CRGB`); indices 12-21 are SC:R additions (only selectable via `CRGB`'s index-selection mode).
///
/// Values 22 and above indicate "random" and have no fixed RGB value, so they are not present in
/// this table.
pub const DEFAULT_COLOR_TABLE: [[u8; 3]; 22] = [
    [244, 4, 4],     // 0: Red
    [12, 72, 204],   // 1: Blue
    [44, 180, 148],  // 2: Teal
    [136, 64, 156],  // 3: Purple
    [248, 140, 20],  // 4: Orange
    [112, 48, 20],   // 5: Brown
    [204, 224, 208], // 6: White
    [252, 252, 56],  // 7: Yellow
    [8, 128, 8],     // 8: Green
    [252, 252, 124], // 9: Pale yellow
    [236, 196, 176], // 10: Tan
    [64, 104, 212],  // 11: Azure (neutral color)
    [116, 164, 124], // 12: Pale green (SC:R)
    [114, 144, 184], // 13: Blueish grey (SC:R)
    [0, 228, 252],   // 14: Cyan (SC:R)
    [255, 196, 228], // 15: Pink (SC:R)
    [128, 128, 0],   // 16: Olive (SC:R)
    [210, 245, 60],  // 17: Lime (SC:R)
    [0, 0, 128],     // 18: Navy (SC:R)
    [240, 50, 230],  // 19: Magenta (SC:R)
    [128, 128, 128], // 20: Grey (SC:R)
    [60, 60, 60],    // 21: Black (SC:R)
];

/// The resolved color setting for a single player, combining `COLR` and `CRGB` data (if present).
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq)]
pub enum PlayerColor {
    /// No fixed color is specified; consumers should fall back to the slot's default color (or,
    /// for an interactive lobby, let the player choose their own).
    #[default]
    Default,
    /// The color should be randomly chosen from the predefined color table (potentially
    /// repeating a color already in use by another player). Deterministic consumers should treat
    /// this the same as [`PlayerColor::Default`] (i.e. use the slot's default color).
    Random,
    /// An index into [`DEFAULT_COLOR_TABLE`]. Indices outside the table's bounds should resolve
    /// to the slot's default color.
    Indexed(u8),
    /// An explicit RGB color value.
    Rgb([u8; 3]),
}

/// The resolved player colors for all 8 player slots in a scenario, combining the `COLR` and
/// `CRGB` chunks (if present).
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq)]
pub struct PlayerColors {
    /// The color setting for each of the 8 player slots.
    pub colors: [PlayerColor; 8],
}

/// Resolves the RGB color that should actually be used for a given [`PlayerColor`], given the
/// player's slot index (0-7). Out-of-range indices (for [`PlayerColor::Indexed`]) and the
/// non-deterministic variants ([`PlayerColor::Default`], [`PlayerColor::Random`]) fall back to
/// the slot's default color (i.e. `DEFAULT_COLOR_TABLE[slot]` for `slot < 8`).
pub fn resolve_color(color: &PlayerColor, slot: u8) -> [u8; 3] {
    match color {
        PlayerColor::Indexed(i) => {
            if let Some(rgb) = DEFAULT_COLOR_TABLE.get(*i as usize) {
                return *rgb;
            }
            slot_default_color(slot)
        }
        PlayerColor::Rgb(rgb) => *rgb,
        PlayerColor::Default | PlayerColor::Random => slot_default_color(slot),
    }
}

/// Returns the default color for a given player slot (0-7 map directly to
/// [`DEFAULT_COLOR_TABLE`]'s first 8 entries, matching the original 8 Brood War player colors).
/// Slots outside that range wrap around the table so that this never panics.
fn slot_default_color(slot: u8) -> [u8; 3] {
    DEFAULT_COLOR_TABLE[slot as usize % DEFAULT_COLOR_TABLE.len()]
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum ColrError {
    #[error("Invalid data length")]
    InvalidDataLength,
}

/// Reads the raw contents of a `COLR` chunk: one color-table index per player slot (0-7).
pub fn read_colr(data: &[u8]) -> Result<[u8; 8], ColrError> {
    data.try_into().map_err(|_| ColrError::InvalidDataLength)
}

/// The raw contents of a `CRGB` chunk, with strings/colors not yet resolved into a
/// [`PlayerColor`].
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct RawCrgb {
    /// The RGB triple for each player slot (meaning depends on `setting`, see [module
    /// docs](self)).
    pub rgb: [[u8; 3]; 8],
    /// The color-selection mode for each player slot (see [module docs](self)).
    pub setting: [u8; 8],
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq)]
pub enum CrgbError {
    #[error("Invalid data length")]
    InvalidDataLength,
}

/// Reads the raw contents of a `CRGB` chunk.
pub fn read_crgb(data: &[u8]) -> Result<RawCrgb, CrgbError> {
    if data.len() != 32 {
        return Err(CrgbError::InvalidDataLength);
    }

    let mut rgb = [[0u8; 3]; 8];
    for (i, slot) in rgb.iter_mut().enumerate() {
        *slot = [data[i * 3], data[i * 3 + 1], data[i * 3 + 2]];
    }
    let setting: [u8; 8] = data[24..32].try_into().unwrap();

    Ok(RawCrgb { rgb, setting })
}

/// Resolves [`PlayerColors`] from the raw `COLR` and/or `CRGB` chunk contents. Either (or both)
/// may be absent, in which case those players default to [`PlayerColor::Default`].
///
/// When `CRGB` is present, it is used for all 8 players (per its own per-player selection mode),
/// taking precedence over `COLR` entirely - this matches how SC:R itself resolves in-game player
/// colors when both chunks are present (see [module docs](self) for the source backing this).
pub fn resolve_player_colors(colr: Option<&[u8; 8]>, crgb: Option<&RawCrgb>) -> PlayerColors {
    let mut colors = [PlayerColor::default(); 8];

    if let Some(colr) = colr {
        for (i, &index) in colr.iter().enumerate() {
            // Per SC:R 1.18.6 behavior (see module docs): a COLR value above 11 (the highest
            // index selectable via COLR itself) is "overflow" and falls back to the slot's
            // default color, rather than being treated as a (possibly SC:R-extended, CRGB-only)
            // table index.
            colors[i] = if index <= 11 {
                PlayerColor::Indexed(index)
            } else {
                PlayerColor::Default
            };
        }
    }

    if let Some(crgb) = crgb {
        for (color, (setting, rgb)) in colors.iter_mut().zip(crgb.setting.iter().zip(&crgb.rgb)) {
            *color = match setting {
                0 => PlayerColor::Random,
                2 => PlayerColor::Rgb(*rgb),
                3 => PlayerColor::Indexed(rgb[2]),
                // 1 ("player's choice") and any other/unrecognized value: no fixed color.
                _ => PlayerColor::Default,
            };
        }
    }

    PlayerColors { colors }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_colr_valid() {
        let data = [0u8, 1, 2, 3, 4, 5, 6, 7];
        assert_eq!(read_colr(&data), Ok(data));
    }

    #[test]
    fn read_colr_wrong_size() {
        assert_eq!(read_colr(&[0u8; 7]), Err(ColrError::InvalidDataLength));
        assert_eq!(read_colr(&[0u8; 9]), Err(ColrError::InvalidDataLength));
        assert_eq!(read_colr(&[]), Err(ColrError::InvalidDataLength));
    }

    #[test]
    fn read_crgb_valid() {
        let mut data = Vec::with_capacity(32);
        // RGB triples: player 0 = (10, 20, 30), player 1 = (40, 50, 60), rest zero.
        data.extend_from_slice(&[10, 20, 30]);
        data.extend_from_slice(&[40, 50, 60]);
        data.extend_from_slice(&[0; 3 * 6]);
        // Selection bytes.
        data.extend_from_slice(&[2, 3, 0, 1, 0, 0, 0, 0]);

        let raw = read_crgb(&data).unwrap();
        assert_eq!(raw.rgb[0], [10, 20, 30]);
        assert_eq!(raw.rgb[1], [40, 50, 60]);
        assert_eq!(raw.setting, [2, 3, 0, 1, 0, 0, 0, 0]);
    }

    #[test]
    fn read_crgb_wrong_size() {
        assert_eq!(read_crgb(&[0u8; 31]), Err(CrgbError::InvalidDataLength));
        assert_eq!(read_crgb(&[0u8; 33]), Err(CrgbError::InvalidDataLength));
        assert_eq!(read_crgb(&[]), Err(CrgbError::InvalidDataLength));
    }

    #[test]
    fn resolve_neither_chunk_is_all_default() {
        let colors = resolve_player_colors(None, None);
        assert_eq!(colors.colors, [PlayerColor::Default; 8]);
    }

    #[test]
    fn resolve_colr_only_is_indexed() {
        let colr = [0u8, 1, 2, 3, 4, 5, 6, 7];
        let colors = resolve_player_colors(Some(&colr), None);
        for (i, color) in colors.colors.iter().enumerate() {
            assert_eq!(*color, PlayerColor::Indexed(i as u8));
        }
    }

    #[test]
    fn resolve_crgb_only_covers_all_modes() {
        let raw = RawCrgb {
            rgb: [
                [0, 0, 0],
                [0, 0, 0],
                [11, 22, 33],
                [0xAA, 0xBB, 5],
                [0, 0, 0],
                [0, 0, 0],
                [0, 0, 0],
                [0, 0, 0],
            ],
            setting: [0, 1, 2, 3, 4, 255, 0, 0],
        };
        let colors = resolve_player_colors(None, Some(&raw));

        assert_eq!(colors.colors[0], PlayerColor::Random);
        assert_eq!(colors.colors[1], PlayerColor::Default);
        assert_eq!(colors.colors[2], PlayerColor::Rgb([11, 22, 33]));
        assert_eq!(colors.colors[3], PlayerColor::Indexed(5));
        // Unrecognized selection mode values fall back to Default.
        assert_eq!(colors.colors[4], PlayerColor::Default);
        assert_eq!(colors.colors[5], PlayerColor::Default);
    }

    #[test]
    fn resolve_colr_value_above_11_falls_back_to_slot_default() {
        // COLR=12 is "overflow" per SC:R 1.18.6 behavior: it must not be treated as SC:R's
        // extended (CRGB-only) color table index 12 ("Pale green"), even though the raw byte
        // value happens to be in-bounds for `DEFAULT_COLOR_TABLE`.
        let mut colr = [0u8; 8];
        colr[3] = 12;
        let colors = resolve_player_colors(Some(&colr), None);

        assert_eq!(colors.colors[3], PlayerColor::Default);
        assert_eq!(resolve_color(&colors.colors[3], 3), DEFAULT_COLOR_TABLE[3]);
    }

    #[test]
    fn resolve_colr_value_11_and_below_stays_indexed() {
        let mut colr = [0u8; 8];
        colr[3] = 11;
        let colors = resolve_player_colors(Some(&colr), None);
        assert_eq!(colors.colors[3], PlayerColor::Indexed(11));
    }

    #[test]
    fn resolve_crgb_mode3_index_12_uses_extended_table_entry() {
        // CRGB's mode-3 (index-in-blue-byte) may use the SC:R-extended range 0..=21, unlike
        // COLR. Index 12 ("Pale green") should resolve to the table entry, not fall back to
        // Default.
        let mut raw = RawCrgb {
            rgb: [[0, 0, 0]; 8],
            setting: [1; 8],
        };
        raw.rgb[3] = [0xAA, 0xBB, 12]; // R/G are junk in mode 3; only the blue byte matters.
        raw.setting[3] = 3;

        let colors = resolve_player_colors(None, Some(&raw));
        assert_eq!(colors.colors[3], PlayerColor::Indexed(12));
        assert_eq!(resolve_color(&colors.colors[3], 3), DEFAULT_COLOR_TABLE[12]);
    }

    #[test]
    fn resolve_crgb_takes_precedence_over_colr() {
        let colr = [7u8; 8];
        let raw = RawCrgb {
            rgb: [[1, 2, 3]; 8],
            setting: [2; 8],
        };

        let colors = resolve_player_colors(Some(&colr), Some(&raw));
        for color in colors.colors {
            assert_eq!(color, PlayerColor::Rgb([1, 2, 3]));
        }
    }

    #[test]
    fn resolve_color_indexed_in_range() {
        assert_eq!(resolve_color(&PlayerColor::Indexed(0), 5), [244, 4, 4]);
        assert_eq!(
            resolve_color(&PlayerColor::Indexed(21), 5),
            [60, 60, 60] // Black
        );
    }

    #[test]
    fn resolve_color_indexed_out_of_range_falls_back_to_slot_default() {
        assert_eq!(
            resolve_color(&PlayerColor::Indexed(255), 1),
            DEFAULT_COLOR_TABLE[1]
        );
    }

    #[test]
    fn resolve_color_default_and_random_use_slot_default() {
        for slot in 0..8u8 {
            assert_eq!(
                resolve_color(&PlayerColor::Default, slot),
                DEFAULT_COLOR_TABLE[slot as usize]
            );
            assert_eq!(
                resolve_color(&PlayerColor::Random, slot),
                DEFAULT_COLOR_TABLE[slot as usize]
            );
        }
    }

    #[test]
    fn resolve_color_rgb_is_passthrough() {
        assert_eq!(resolve_color(&PlayerColor::Rgb([1, 2, 3]), 0), [1, 2, 3]);
    }

    #[test]
    fn default_color_table_has_documented_values() {
        // Spot-check a few entries against the cross-checked wiki/Chkdraft values documented in
        // this module's docs.
        assert_eq!(DEFAULT_COLOR_TABLE[0], [244, 4, 4]); // Red
        assert_eq!(DEFAULT_COLOR_TABLE[7], [252, 252, 56]); // Yellow
        assert_eq!(DEFAULT_COLOR_TABLE[11], [64, 104, 212]); // Azure (neutral)
        assert_eq!(DEFAULT_COLOR_TABLE[12], [116, 164, 124]); // Pale green (SC:R)
        assert_eq!(DEFAULT_COLOR_TABLE[21], [60, 60, 60]); // Black (SC:R)
    }
}
