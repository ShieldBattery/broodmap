use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use clap::{Parser, Subcommand, ValueEnum};

use broodmap::extract_chk_from_map;
use broodmap_render::{
    ArtStyle, CascSource, DirSource, GameData, MinimapOptions, Preview, RenderOptions, RgbaImage,
    StartLocations, UnitFilter, build_minimap_table, render_chk_minimap, render_chk_preview,
    render_terrain,
};

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
    /// Renders a map preview (terrain plus units, resources and doodads) to a PNG image using
    /// StarCraft: Remastered assets.
    Render(RenderArgs),
    /// Renders a zero-asset minimap (a preset color per terrain tile, baked from real SC:R
    /// assets at build time, plus unit/resource/start-location dots) to a PNG image. Needs no
    /// SC:R assets by default; pass `--install`/`--assets-dir` only to size unit dots from
    /// `units.dat` and refine melee filtering (see `--help`).
    Minimap(MinimapArgs),
    /// Regenerates the 8 committed per-tileset minimap color tables
    /// (`broodmap-render/src/minimap/tables/*.bin`) from a real StarCraft: Remastered install.
    /// Dev-time only -- the committed output is what `minimap`/the library's zero-asset renderer
    /// actually ships with.
    #[command(hide = true)]
    GenMinimapTables(GenMinimapTablesArgs),
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
    #[arg(long, value_enum, default_value_t = ArtStyleArg::Original)]
    style: ArtStyleArg,

    /// Art style for the unit/sprite layer, if it should differ from `--style` (e.g. Cartooned
    /// terrain with Remastered units). Defaults to `--style`.
    #[arg(long, value_enum)]
    unit_style: Option<ArtStyleArg>,

    /// How start locations are drawn.
    #[arg(long, value_enum, default_value_t = StartLocationsArg::Block)]
    start_locations: StartLocationsArg,

    /// Show everything the map placed, as placed (UMS view). By default the preview applies
    /// melee rules instead: preplaced player-owned units are dropped (the game replaces them
    /// with starting workers) and units overlapping a start location's spawn area are cleared,
    /// keeping neutral units, resources and start locations.
    #[arg(long)]
    as_placed: bool,

    /// Don't draw critters.
    #[arg(long)]
    no_critters: bool,

    /// Don't draw mineral fields or vespene geysers.
    #[arg(long)]
    no_resources: bool,

    /// Don't draw neutral-owned buildings (e.g. an unclaimed Zerg Extractor). Player-owned
    /// buildings, resources and start locations are unaffected.
    #[arg(long)]
    no_neutral_buildings: bool,

    /// Don't draw THG2 doodad sprites.
    #[arg(long)]
    no_doodads: bool,

    /// Don't draw drop shadows under units and sprites.
    #[arg(long)]
    no_shadows: bool,

    /// Render terrain only, skipping the unit/sprite overlay entirely.
    #[arg(long)]
    terrain_only: bool,
}

#[derive(clap::Args)]
struct MinimapArgs {
    /// Path to a .scm/.scx map file.
    map: PathBuf,

    /// Output PNG path.
    #[arg(long, default_value = "minimap.png")]
    out: PathBuf,

    /// Integer upscale of the native minimap image (`MinimapOptions::scale`). BW's own minimap
    /// is a small native texture (<= 128px per side) that the UI magnifies; the library default
    /// is 1 (no upscale, the native size), but that's too small to eyeball on a modern display,
    /// so the CLI defaults higher.
    #[arg(long, default_value_t = 4)]
    scale: u32,

    /// Show everything the map placed, as placed, instead of applying melee rules (dropping
    /// preplaced player-owned units and clearing start-area spawns).
    #[arg(long)]
    as_placed: bool,

    /// How start locations are drawn. `sprite` behaves exactly like `block` here -- there's no
    /// art in this zero-asset render path.
    #[arg(long, value_enum, default_value_t = StartLocationsArg::Block)]
    start_locations: StartLocationsArg,

    /// StarCraft: Remastered install directory, read ONLY to load `units.dat` (for dot sizing
    /// and melee's start-area clearing) -- unlike `render`, this is optional. With neither this
    /// nor `--assets-dir`, the minimap still renders in full, just with 1x1-tile dots and no
    /// start-area clearing (the zero-asset promise).
    #[arg(long)]
    install: Option<PathBuf>,

    /// Read the `.dat` tables from a plain directory of extracted files instead of a CASC
    /// install.
    #[arg(long)]
    assets_dir: Option<PathBuf>,
}

#[derive(clap::Args)]
struct GenMinimapTablesArgs {
    /// StarCraft: Remastered install directory (containing `.build.info`).
    #[arg(long, default_value = DEFAULT_INSTALL_DIR)]
    install: PathBuf,

    /// Output directory for the 8 generated `<tileset>.bin` blobs.
    #[arg(long, default_value = "broodmap-render/src/minimap/tables")]
    out: PathBuf,
}

/// The filename stems of the 8 tilesets, matching `AssetRequest`'s tileset stems
/// (`broodmap-render/src/source.rs`) and the classic asset files' own naming
/// (`TileSet/<stem>.cv5`/`.vx4ex`/`.vr4`/`.wpe`).
const TILESET_STEMS: [&str; 8] = [
    "badlands", "platform", "install", "ashworld", "jungle", "desert", "ice", "twilight",
];

#[derive(Copy, Clone, ValueEnum)]
enum StartLocationsArg {
    /// A solid block in the owning player's color (the map-preview convention).
    Block,
    /// The in-game start-location graphic.
    Sprite,
    /// Not drawn.
    Hidden,
}

impl From<StartLocationsArg> for StartLocations {
    fn from(value: StartLocationsArg) -> Self {
        match value {
            StartLocationsArg::Block => StartLocations::ColorBlock,
            StartLocationsArg::Sprite => StartLocations::Sprite,
            StartLocationsArg::Hidden => StartLocations::Hidden,
        }
    }
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
        Command::Minimap(args) => minimap(args),
        Command::GenMinimapTables(args) => gen_minimap_tables(args),
    }
}

fn render(args: RenderArgs) -> Result<()> {
    let map_bytes = std::fs::read(&args.map)
        .with_context(|| format!("failed to read map file {}", args.map.display()))?;

    let (chk, _mpq) = extract_chk_from_map(&map_bytes, None, None)
        .with_context(|| format!("failed to parse map file {}", args.map.display()))?;

    let options = RenderOptions {
        art_style: args.style.into(),
        unit_style: args.unit_style.map(Into::into),
        max_dimension: Some(args.size),
        start_locations: args.start_locations.into(),
        unit_filter: if args.as_placed {
            UnitFilter::AsPlaced
        } else {
            UnitFilter::Melee
        },
        show_critters: !args.no_critters,
        show_resources: !args.no_resources,
        show_doodad_sprites: !args.no_doodads,
        show_neutral_buildings: !args.no_neutral_buildings,
        show_shadows: !args.no_shadows,
        ..Default::default()
    };

    let out_path = args
        .out
        .clone()
        .unwrap_or_else(|| default_out_path(&args.map));

    let render = |source: &dyn broodmap_render::TilesetDataSource| -> Result<Preview> {
        if args.terrain_only {
            let terrain = chk
                .terrain()
                .map_err(|e| anyhow::anyhow!("failed to read map terrain: {e}"))?;
            let image: RgbaImage = render_terrain(terrain, chk.tileset(), source, &options)
                .context("failed to render map terrain")?;
            Ok(Preview {
                image,
                warnings: Vec::new(),
            })
        } else {
            render_chk_preview(&chk, source, &options).context("failed to render map preview")
        }
    };

    let preview = if let Some(assets_dir) = &args.assets_dir {
        render(&DirSource::new(assets_dir))?
    } else {
        let source = CascSource::open(&args.install).with_context(|| {
            format!(
                "failed to open StarCraft: Remastered install at {}",
                args.install.display()
            )
        })?;
        render(&source)?
    };

    for warning in &preview.warnings {
        eprintln!("warning: {warning}");
    }

    let png_bytes = preview
        .image
        .encode_png()
        .context("failed to encode rendered map as PNG")?;
    std::fs::write(&out_path, png_bytes)
        .with_context(|| format!("failed to write output file {}", out_path.display()))?;

    println!(
        "Wrote {} ({}x{})",
        out_path.display(),
        preview.image.width,
        preview.image.height
    );

    Ok(())
}

/// Renders a zero-asset minimap. Unlike `render`, the source (`--install`/`--assets-dir`) is
/// entirely optional: with neither given, this loads no `GameData` at all and still produces a
/// complete minimap (see `MinimapOptions`'s docs on what's lost without it -- dot sizing and
/// melee's start-area clearing, nothing else).
fn minimap(args: MinimapArgs) -> Result<()> {
    let map_bytes = std::fs::read(&args.map)
        .with_context(|| format!("failed to read map file {}", args.map.display()))?;

    let (chk, _mpq) = extract_chk_from_map(&map_bytes, None, None)
        .with_context(|| format!("failed to parse map file {}", args.map.display()))?;

    let options = MinimapOptions {
        scale: args.scale,
        start_locations: args.start_locations.into(),
        unit_filter: if args.as_placed {
            UnitFilter::AsPlaced
        } else {
            UnitFilter::Melee
        },
        ..Default::default()
    };

    let data = if let Some(assets_dir) = &args.assets_dir {
        let source = DirSource::new(assets_dir);
        Some(GameData::load(&source).context("failed to load game data tables")?)
    } else if let Some(install) = &args.install {
        let source = CascSource::open(install).with_context(|| {
            format!(
                "failed to open StarCraft: Remastered install at {}",
                install.display()
            )
        })?;
        Some(GameData::load(&source).context("failed to load game data tables")?)
    } else {
        None
    };

    let preview = render_chk_minimap(&chk, data.as_ref(), &options);

    for warning in &preview.warnings {
        eprintln!("warning: {warning}");
    }

    let png_bytes = preview
        .image
        .encode_png()
        .context("failed to encode minimap as PNG")?;
    std::fs::write(&args.out, png_bytes)
        .with_context(|| format!("failed to write output file {}", args.out.display()))?;

    println!(
        "Wrote {} ({}x{})",
        args.out.display(),
        preview.image.width,
        preview.image.height
    );

    Ok(())
}

/// Regenerates the 8 committed per-tileset minimap color tables from a real SC:R install (see
/// `broodmap_render::build_minimap_table`'s docs for the algorithm/blob layout). Reads the
/// classic `TileSet/<stem>.cv5`/`.vx4ex`/`.vr4`/`.wpe` files directly via `broodcasc::Storage`
/// (rather than through `broodmap_render::AssetRequest`, which has no variant for these --
/// they're dev-time-only inputs, never read at render time).
fn gen_minimap_tables(args: GenMinimapTablesArgs) -> Result<()> {
    let storage = broodcasc::Storage::open(&args.install).with_context(|| {
        format!(
            "failed to open StarCraft: Remastered install at {}",
            args.install.display()
        )
    })?;

    std::fs::create_dir_all(&args.out)
        .with_context(|| format!("failed to create output directory {}", args.out.display()))?;

    for stem in TILESET_STEMS {
        let cv5 = storage
            .read_file(&format!("TileSet/{stem}.cv5"))
            .with_context(|| format!("failed to read TileSet/{stem}.cv5"))?;
        let vx4ex = storage
            .read_file(&format!("TileSet/{stem}.vx4ex"))
            .with_context(|| format!("failed to read TileSet/{stem}.vx4ex"))?;
        let vr4 = storage
            .read_file(&format!("TileSet/{stem}.vr4"))
            .with_context(|| format!("failed to read TileSet/{stem}.vr4"))?;
        let wpe = storage
            .read_file(&format!("TileSet/{stem}.wpe"))
            .with_context(|| format!("failed to read TileSet/{stem}.wpe"))?;

        let table = build_minimap_table(&cv5, &vx4ex, &vr4, &wpe)
            .with_context(|| format!("failed to build the minimap table for {stem}"))?;

        let out_path = args.out.join(format!("{stem}.bin"));
        std::fs::write(&out_path, &table)
            .with_context(|| format!("failed to write {}", out_path.display()))?;

        println!("{stem}: {} bytes -> {}", table.len(), out_path.display());
    }

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
