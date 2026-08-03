pub mod chk;
pub mod limits;
pub mod mpq;

pub use chk::Chk;
pub use chk::strings::StringEncoding;
pub use limits::{Resource, ResourceLimitError, ResourceLimits};
pub use mpq::Mpq;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ChkExtractionError {
    #[error("Failed to read MPQ")]
    MpqError(#[from] mpq::MpqError),
    #[error("Failed to read CHK")]
    ChkError(#[from] chk::ChkError),
    #[error("Failed to find CHK in MPQ")]
    ChkNotFound,
}

pub const CHK_PATH: &str = "staredit\\scenario.chk";

/// Extracts a CHK from an MPQ file. This does all the necessary steps to get map information given
/// a map file (excluding getting associated files, such as sounds. These can be retrieved through
/// the resulting [Mpq] object).
///
/// This function behaves similarly to SC:R's map loading process as far as locale goes. If a locale
/// is specified, it will be searched first, followed by the neutral locale (0).
///
/// If a `str_encoding` is specified, any strings in the CHK will be decoded using it. If not, the
/// string encoding will be automatically determined based on the contents of the file.
pub fn extract_chk_from_map(
    map_bytes: &[u8],
    locale: Option<u16>,
    str_encoding: Option<StringEncoding>,
) -> Result<(Chk, Mpq<'_>), ChkExtractionError> {
    extract_chk_from_map_with_limits(map_bytes, locale, str_encoding, &ResourceLimits::default())
}

/// Extracts and parses a CHK using explicit resource limits.
pub fn extract_chk_from_map_with_limits<'a>(
    map_bytes: &'a [u8],
    locale: Option<u16>,
    str_encoding: Option<StringEncoding>,
    limits: &ResourceLimits,
) -> Result<(Chk, Mpq<'a>), ChkExtractionError> {
    let mpq =
        Mpq::from_bytes_with_limits(map_bytes, limits).map_err(ChkExtractionError::MpqError)?;
    let chk_data = mpq.read_file(CHK_PATH, locale).map_err(|e| match e {
        mpq::MpqError::FileNotFound => ChkExtractionError::ChkNotFound,
        e => ChkExtractionError::MpqError(e),
    })?;

    let chk = Chk::from_bytes_with_limits(chk_data, str_encoding, limits)
        .map_err(ChkExtractionError::ChkError)?;

    Ok((chk, mpq))
}
