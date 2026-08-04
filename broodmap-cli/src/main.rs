use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use clap::{Parser, Subcommand, ValueEnum};

use broodmap::extract_chk_from_map;
use broodmap_render::{ArtStyle, CascSource, DirSource, RenderOptions, render_terrain};

/// Default SC:R install directory used when neither `--assets-dir` nor a custom install path is
/// given.
const DEFAULT_INSTALL_DIR: &str = r"C:\Program Files (x86)\StarCraft";

#[derive(Parser)]
#[command(
    name = "broodmap-cli",
    about = "Tools for working with Brood War map files"
)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Renders a map's terrain to a PNG image using StarCraft: Remastered assets.
    Render(RenderArgs),
}

#[derive(clap::Args)]
struct RenderArgs {
    /// Path to a .scm/.scx map file.
    map: PathBuf,

    /// Output PNG path. Defaults to the map's filename with a .png extension, in the current
    /// directory.
    #[arg(long)]
    out: Option<PathBuf>,

    /// StarCraft: Remastered install directory (containing `.build.info`), used to read assets
    /// via CASC unless `--assets-dir` is given.
    #[arg(long, default_value = DEFAULT_INSTALL_DIR)]
    install: PathBuf,

    /// Read assets from a plain directory of extracted files instead of a CASC install.
    #[arg(long)]
    assets_dir: Option<PathBuf>,

    /// Maximum output dimension, in pixels.
    #[arg(long, default_value_t = 1024)]
    size: u32,

    /// Art style to render. The asset resolution (HD vs. HD2 for the Remastered-family styles)
    /// is picked automatically from `--size`.
    #[arg(long, value_enum, default_value_t = ArtStyleArg::Remastered)]
    style: ArtStyleArg,
}

#[derive(Copy, Clone, ValueEnum)]
enum ArtStyleArg {
    /// The original 1.16.1-style art.
    Original,
    /// The Remastered art.
    Remastered,
    /// The StarCraft: Cartooned art.
    #[value(alias = "carbot")]
    Cartooned,
}

impl From<ArtStyleArg> for ArtStyle {
    fn from(value: ArtStyleArg) -> Self {
        match value {
            ArtStyleArg::Original => ArtStyle::Original,
            ArtStyleArg::Remastered => ArtStyle::Remastered,
            ArtStyleArg::Cartooned => ArtStyle::Cartooned,
        }
    }
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Command::Render(args) => render(args),
    }
}

fn render(args: RenderArgs) -> Result<()> {
    let map_bytes = std::fs::read(&args.map)
        .with_context(|| format!("failed to read map file {}", args.map.display()))?;

    let (chk, _mpq) = extract_chk_from_map(&map_bytes, None, None)
        .with_context(|| format!("failed to parse map file {}", args.map.display()))?;

    let terrain = chk
        .terrain()
        .map_err(|e| anyhow::anyhow!("failed to read map terrain: {e}"))?;
    let tileset = chk.tileset();

    let options = RenderOptions {
        art_style: args.style.into(),
        max_dimension: Some(args.size),
        ..Default::default()
    };

    let out_path = args
        .out
        .clone()
        .unwrap_or_else(|| default_out_path(&args.map));

    let image = if let Some(assets_dir) = &args.assets_dir {
        let source = DirSource::new(assets_dir);
        render_terrain(terrain, tileset, &source, &options)
            .context("failed to render map terrain")?
    } else {
        let source = CascSource::open(&args.install).with_context(|| {
            format!(
                "failed to open StarCraft: Remastered install at {}",
                args.install.display()
            )
        })?;
        render_terrain(terrain, tileset, &source, &options)
            .context("failed to render map terrain")?
    };

    let png_bytes = image
        .encode_png()
        .context("failed to encode rendered map as PNG")?;
    std::fs::write(&out_path, png_bytes)
        .with_context(|| format!("failed to write output file {}", out_path.display()))?;

    println!(
        "Wrote {} ({}x{})",
        out_path.display(),
        image.width,
        image.height
    );

    Ok(())
}

/// Defaults the output path to the map's filename with a `.png` extension, in the current
/// directory.
fn default_out_path(map_path: &Path) -> PathBuf {
    let stem = map_path
        .file_stem()
        .map(|s| s.to_string_lossy().into_owned())
        .unwrap_or_else(|| "map".to_string());
    PathBuf::from(format!("{stem}.png"))
}
