//! Reading render assets straight from a StarCraft: Remastered install's CASC storage, through
//! file-access callbacks the host provides (sync `node:fs` in Node or Electron, for example).
//!
//! broodcasc does the CASC work (indices, encoding table, root catalog, BLTE decoding) in WASM;
//! JS only opens files and reads byte ranges. Decoded assets are kept in a size-bounded cache
//! shared by every `MapRenderer` loading from the same `CascStorage`, since a few large assets
//! (`SD/mainSD.anim` alone is ~38 MB) are needed by every render.

use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::Arc;

use broodcasc::io::{ReadAt, StorageProvider};
use broodcasc::{CascError, Storage};
use broodmap_render::AssetRequest;
use js_sys::{Array, Function, Reflect, Uint8Array};
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;

// The JS handles below are marked Send + Sync because broodcasc's provider traits require it.
// That only holds while the module has a single thread.
#[cfg(target_feature = "atomics")]
compile_error!("broodmap-wasm's CASC file access assumes a single-threaded wasm target");

/// The default for `cacheBytes`: roughly the whole asset set for the `original` style (the one
/// `mainSD.anim`, all 8 SD tilesets and the GRPs), so repeated renders never decode twice.
const DEFAULT_CACHE_BYTES: usize = 128 * 1024 * 1024;

/// The largest file position a JS number represents exactly (`Number.MAX_SAFE_INTEGER`).
const MAX_JS_POSITION: u64 = (1 << 53) - 1;

/// Why an asset couldn't be read.
#[derive(Debug)]
pub(crate) enum ReadFailure {
    /// The storage doesn't have it: not in the catalog, or cataloged but not installed.
    Missing,
    /// Reading it failed (I/O, corrupt data, ...).
    Error(String),
}

impl From<CascError> for ReadFailure {
    fn from(e: CascError) -> Self {
        match e {
            CascError::NotFound(_) | CascError::NotInstalled(_) => ReadFailure::Missing,
            e => ReadFailure::Error(e.to_string()),
        }
    }
}

/// A SC:R install's CASC storage, read through host-provided file callbacks. Pass it to
/// `MapRenderer.loadAssets` in place of the `requiredAssets`/`addAsset` prefetch.
///
/// `files` provides sync access to the install directory (the one containing `.build.info`).
/// Paths are relative to it and use `/` separators:
///
/// - `open(path)`: opens a file for reading and returns any handle value; throws if it can't.
/// - `size(handle)`: the file's length in bytes.
/// - `read(handle, buffer, position)`: reads up to `buffer.length` bytes at `position` into the
///   `Uint8Array` `buffer`, returning how many were read (0 only at end of file).
/// - `close(handle)`: closes a handle `open` returned.
/// - `listDir(path)`: the names of the entries in a directory, or `[]` if it doesn't exist.
///
/// In Node these map one to one onto `fs.openSync`, `fs.fstatSync(fd).size`, `fs.readSync`,
/// `fs.closeSync` and `fs.readdirSync`. Every callback is called with `files` as `this`.
///
/// `options` is an optional JSON object: `{ "cacheBytes": 134217728 }` bounds the shared cache
/// of decoded assets (least recently used are evicted first; 0 disables caching).
///
/// Data archives stay open until the storage is freed (`free()`).
#[wasm_bindgen]
pub struct CascStorage {
    storage: Storage<JsProvider>,
    cache: RefCell<AssetCache>,
}

#[derive(Debug, Default, serde::Deserialize)]
#[serde(rename_all = "camelCase", default, deny_unknown_fields)]
struct CascOptions {
    cache_bytes: Option<usize>,
}

#[wasm_bindgen]
impl CascStorage {
    /// Opens the storage, reading the install's build info, indices, encoding table and root
    /// catalog (a few MB). Throws if `files` lacks a callback or the directory isn't a readable
    /// SC:R install.
    #[wasm_bindgen(constructor)]
    pub fn new(files: JsValue, options: Option<String>) -> Result<CascStorage, String> {
        let options = match options.as_deref().map(str::trim) {
            None | Some("") => CascOptions::default(),
            Some(json) => {
                serde_json::from_str(json).map_err(|e| format!("invalid options: {e}"))?
            }
        };
        let provider = JsProvider {
            files: Arc::new(JsFiles::from_object(&files)?),
        };
        let storage = Storage::open_with_provider(provider)
            .map_err(|e| format!("failed to open CASC storage: {e}"))?;
        Ok(CascStorage {
            storage,
            cache: RefCell::new(AssetCache::new(
                options.cache_bytes.unwrap_or(DEFAULT_CACHE_BYTES),
            )),
        })
    }

    /// The install's game version from its build info (e.g. `"1.23.10.13515"`), if recorded.
    /// Useful in cache keys for rendered images, since a game patch can change the assets.
    #[wasm_bindgen(getter)]
    pub fn version(&self) -> Option<String> {
        self.storage
            .build_info()
            .active_record()
            .and_then(|r| r.version())
            .map(str::to_owned)
    }

    /// The total size of the decoded assets currently cached.
    #[wasm_bindgen(getter, js_name = cachedBytes)]
    pub fn cached_bytes(&self) -> usize {
        self.cache.borrow().used
    }
}

impl CascStorage {
    /// Reads one asset, from the cache when it's there.
    pub(crate) fn read(&self, req: &AssetRequest) -> Result<Arc<[u8]>, ReadFailure> {
        let path = req.casc_path();
        if let Some(bytes) = self.cache.borrow_mut().get(&path) {
            return Ok(bytes);
        }
        let bytes: Arc<[u8]> = self.storage.read_file(&path)?.into();
        self.cache.borrow_mut().insert(path, bytes.clone());
        Ok(bytes)
    }
}

/// Decoded assets keyed by CASC path, bounded by total size, evicting the least recently used.
/// An evicted asset stays alive for as long as a `MapRenderer` still holds it.
pub(crate) struct AssetCache {
    budget: usize,
    used: usize,
    clock: u64,
    entries: HashMap<String, CacheEntry>,
}

struct CacheEntry {
    bytes: Arc<[u8]>,
    last_used: u64,
}

impl AssetCache {
    pub(crate) fn new(budget: usize) -> Self {
        AssetCache {
            budget,
            used: 0,
            clock: 0,
            entries: HashMap::new(),
        }
    }

    pub(crate) fn get(&mut self, path: &str) -> Option<Arc<[u8]>> {
        self.clock += 1;
        let entry = self.entries.get_mut(path)?;
        entry.last_used = self.clock;
        Some(entry.bytes.clone())
    }

    pub(crate) fn insert(&mut self, path: String, bytes: Arc<[u8]>) {
        if bytes.len() > self.budget {
            return;
        }
        self.clock += 1;
        let entry = CacheEntry {
            bytes,
            last_used: self.clock,
        };
        self.used += entry.bytes.len();
        if let Some(old) = self.entries.insert(path, entry) {
            self.used -= old.bytes.len();
        }
        while self.used > self.budget {
            let Some(oldest) = self
                .entries
                .iter()
                .min_by_key(|(_, e)| e.last_used)
                .map(|(path, _)| path.clone())
            else {
                break;
            };
            if let Some(evicted) = self.entries.remove(&oldest) {
                self.used -= evicted.bytes.len();
            }
        }
    }
}

/// The host's file callbacks (see [`CascStorage`]), called with `target` as `this`.
struct JsFiles {
    target: JsValue,
    open: Function,
    size: Function,
    read: Function,
    close: Function,
    list_dir: Function,
}

impl JsFiles {
    fn from_object(files: &JsValue) -> Result<Self, String> {
        let method = |name: &str| {
            Reflect::get(files, &JsValue::from_str(name))
                .ok()
                .and_then(|value| value.dyn_into::<Function>().ok())
                .ok_or_else(|| format!("CascStorage files object has no `{name}` function"))
        };
        Ok(JsFiles {
            target: files.clone(),
            open: method("open")?,
            size: method("size")?,
            read: method("read")?,
            close: method("close")?,
            list_dir: method("listDir")?,
        })
    }
}

// SAFETY: see the `atomics` guard at the top of the module: a wasm32 target without atomics has
// exactly one thread, so these JS handles are never reached from another thread. On non-wasm
// targets every wasm-bindgen import panics, so `JsFiles::from_object` can't produce a value.
unsafe impl Send for JsFiles {}
unsafe impl Sync for JsFiles {}

struct JsProvider {
    files: Arc<JsFiles>,
}

impl StorageProvider for JsProvider {
    type File = JsFile;

    fn open(&self, path: &str) -> broodcasc::error::Result<JsFile> {
        let files = &self.files;
        let handle = files
            .open
            .call1(&files.target, &JsValue::from_str(path))
            .map_err(js_error)?;
        let mut file = JsFile {
            handle,
            files: files.clone(),
            len: 0,
        };
        // On failure here `file` is dropped, which closes the handle.
        let size = files
            .size
            .call1(&files.target, &file.handle)
            .map_err(js_error)?;
        file.len = js_count(&size).ok_or_else(|| {
            malformed("file size", format!("`size` returned {size:?} for {path}"))
        })?;
        Ok(file)
    }

    fn list_dir(&self, path: &str) -> broodcasc::error::Result<Vec<String>> {
        let files = &self.files;
        let names = files
            .list_dir
            .call1(&files.target, &JsValue::from_str(path))
            .map_err(js_error)?;
        if !Array::is_array(&names) {
            return Err(malformed(
                "directory listing",
                format!("`listDir` returned a non-array for {path}"),
            ));
        }
        Array::from(&names)
            .iter()
            .map(|name| {
                name.as_string().ok_or_else(|| {
                    malformed(
                        "directory listing",
                        format!("`listDir` returned a non-string name in {path}"),
                    )
                })
            })
            .collect()
    }
}

struct JsFile {
    handle: JsValue,
    files: Arc<JsFiles>,
    len: u64,
}

// SAFETY: as for `JsFiles`.
unsafe impl Send for JsFile {}
unsafe impl Sync for JsFile {}

impl ReadAt for JsFile {
    fn read_exact_at(&self, offset: u64, buf: &mut [u8]) -> broodcasc::error::Result<()> {
        if buf.is_empty() {
            return Ok(());
        }
        let total = u32::try_from(buf.len())
            .map_err(|_| malformed("positioned read", "read is too large"))?;
        let chunk = Uint8Array::new_with_length(total);
        let mut filled = 0u32;
        while filled < total {
            let position = offset
                .checked_add(u64::from(filled))
                .filter(|&p| p <= MAX_JS_POSITION)
                .ok_or_else(|| malformed("positioned read", "position is past 2^53 bytes"))?;
            let read = self
                .files
                .read
                .call3(
                    &self.files.target,
                    &self.handle,
                    &chunk.subarray(filled, total),
                    &JsValue::from_f64(position as f64),
                )
                .map_err(js_error)?;
            let n = js_count(&read)
                .filter(|&n| n <= u64::from(total - filled))
                .ok_or_else(|| malformed("positioned read", format!("`read` returned {read:?}")))?;
            if n == 0 {
                return Err(std::io::Error::from(std::io::ErrorKind::UnexpectedEof).into());
            }
            filled += n as u32;
        }
        chunk.copy_to(buf);
        Ok(())
    }

    fn len(&self) -> broodcasc::error::Result<u64> {
        Ok(self.len)
    }
}

impl Drop for JsFile {
    fn drop(&mut self) {
        // Nothing useful can be done about a failed close.
        let _ = self.files.close.call1(&self.files.target, &self.handle);
    }
}

/// A non-negative integer JS number (a size or byte count), if `value` is one.
fn js_count(value: &JsValue) -> Option<u64> {
    let n = value.as_f64()?;
    (n >= 0.0 && n.fract() == 0.0 && n <= MAX_JS_POSITION as f64).then_some(n as u64)
}

fn malformed(what: &'static str, reason: impl Into<String>) -> CascError {
    CascError::Malformed {
        what,
        reason: reason.into(),
    }
}

/// Converts an exception thrown by a file callback, keeping Node's `ENOENT` as `NotFound`.
fn js_error(error: JsValue) -> CascError {
    let message = error
        .dyn_ref::<js_sys::Error>()
        .map(|e| String::from(e.message()))
        .or_else(|| error.as_string())
        .unwrap_or_else(|| format!("{error:?}"));
    let code = Reflect::get(&error, &JsValue::from_str("code"))
        .ok()
        .and_then(|code| code.as_string());
    let kind = match code.as_deref() {
        Some("ENOENT") => std::io::ErrorKind::NotFound,
        _ => std::io::ErrorKind::Other,
    };
    std::io::Error::new(kind, message).into()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn bytes(len: usize) -> Arc<[u8]> {
        vec![0u8; len].into()
    }

    #[test]
    fn cache_evicts_least_recently_used_to_stay_in_budget() {
        let mut cache = AssetCache::new(10);
        cache.insert("a".into(), bytes(4));
        cache.insert("b".into(), bytes(4));
        assert!(cache.get("a").is_some());
        cache.insert("c".into(), bytes(4));
        assert_eq!(cache.used, 8);
        assert!(cache.get("b").is_none(), "b was used least recently");
        assert!(cache.get("a").is_some());
        assert!(cache.get("c").is_some());
    }

    #[test]
    fn cache_skips_assets_larger_than_the_budget_and_replaces_in_place() {
        let mut cache = AssetCache::new(10);
        cache.insert("a".into(), bytes(4));
        cache.insert("huge".into(), bytes(11));
        assert!(cache.get("huge").is_none());
        assert!(cache.get("a").is_some());

        cache.insert("a".into(), bytes(6));
        assert_eq!(cache.used, 6);

        let mut disabled = AssetCache::new(0);
        disabled.insert("a".into(), bytes(1));
        assert_eq!(disabled.used, 0);
        assert!(disabled.get("a").is_none());
    }

    #[test]
    fn missing_and_failed_reads_are_told_apart() {
        assert!(matches!(
            ReadFailure::from(CascError::NotFound("x".into())),
            ReadFailure::Missing
        ));
        assert!(matches!(
            ReadFailure::from(CascError::NotInstalled("x".into())),
            ReadFailure::Missing
        ));
        assert!(matches!(
            ReadFailure::from(CascError::ChecksumMismatch("blte")),
            ReadFailure::Error(_)
        ));
    }
}
