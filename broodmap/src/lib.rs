pub mod chk;
pub mod mpq;

pub use chk::strings::StringEncoding;
pub use chk::Chk;
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
) -> Result<(Chk<'_>, Mpq<'_>), ChkExtractionError> {
    let locale = match locale {
        Some(0) => None,
        l => l,
    };

    let mpq = Mpq::from_bytes(map_bytes).map_err(ChkExtractionError::MpqError)?;
    let chk_data = mpq.read_file(CHK_PATH, locale);
    let chk_data = match chk_data {
        Ok(chk_data) => chk_data,
        Err(mpq::MpqError::FileNotFound) => {
            if locale.is_none() {
                return Err(ChkExtractionError::ChkNotFound);
            };

            mpq.read_file(CHK_PATH, None).map_err(|e| match e {
                mpq::MpqError::FileNotFound => ChkExtractionError::ChkNotFound,
                e => ChkExtractionError::MpqError(e),
            })?
        }
        Err(e) => return Err(ChkExtractionError::MpqError(e)),
    };

    let chk = Chk::from_bytes(chk_data, str_encoding).map_err(ChkExtractionError::ChkError)?;

    Ok((chk, mpq))
}
