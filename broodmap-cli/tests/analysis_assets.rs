use std::{
    fs,
    path::PathBuf,
    process::Command,
    time::{SystemTime, UNIX_EPOCH},
};

struct TestDirectory(PathBuf);

impl Drop for TestDirectory {
    fn drop(&mut self) {
        // Also clean up during assertion unwinding; preserve the original failure if this fails.
        let _ = fs::remove_dir_all(&self.0);
    }
}

#[test]
fn analysis_only_export_needs_only_terrain_and_units_tables() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let root = std::env::temp_dir().join(format!(
        "broodmap-analysis-export-{}-{unique}",
        std::process::id()
    ));
    let _cleanup = TestDirectory(root.clone());
    let source = root.join("source");
    let output = root.join("output");
    fs::create_dir_all(source.join("TileSet")).unwrap();
    fs::create_dir_all(source.join("arr")).unwrap();
    let units = vec![0u8; 19876];
    fs::write(source.join("arr/units.dat"), &units).unwrap();
    let cv5 = vec![0u8; 52];
    let vf4 = vec![1u8; 32];
    fs::write(source.join("TileSet/jungle.cv5"), &cv5).unwrap();
    fs::write(source.join("TileSet/jungle.vf4"), &vf4).unwrap();
    let map = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../broodmap/assets/lt.scm");
    let result = Command::new(env!("CARGO_BIN_EXE_broodmap-cli"))
        .args(["fetch-assets", "--analysis-only", "--assets-dir"])
        .arg(&source)
        .arg("--out")
        .arg(&output)
        // Duplicate map requests must still produce exactly three asset files.
        .arg(&map)
        .arg(&map)
        .output()
        .unwrap();
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    assert_eq!(fs::read(output.join("TileSet/jungle.cv5")).unwrap(), cv5);
    assert_eq!(fs::read(output.join("TileSet/jungle.vf4")).unwrap(), vf4);
    assert_eq!(fs::read_dir(output.join("TileSet")).unwrap().count(), 2);
    assert_eq!(fs::read(output.join("arr/units.dat")).unwrap(), units);
    assert_eq!(fs::read_dir(output.join("arr")).unwrap().count(), 1);
    assert_eq!(fs::read_dir(&output).unwrap().count(), 2);
    assert!(String::from_utf8_lossy(&result.stdout).contains("Fetched 3 files"));
    fs::remove_dir_all(&root).unwrap();
}
