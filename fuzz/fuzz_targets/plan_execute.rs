#![no_main]

use broodmap::chk::tileset::Tileset;
use broodmap_render::{
    ArtPack, AssetRequest, AssetTier, MemorySource, PlannedBlock, PlannedSprite, RenderPlan,
    SdCanvasSource, TerrainPlan, execute_plan,
};
use libfuzzer_sys::fuzz_target;

/// A `RenderPlan` is plain data — a deserialized (possibly hostile) plan can claim anything, and
/// `execute_plan` must clamp rather than panic or balloon. This target hand-carves a plan from
/// fuzzer bytes (hostile dimensions, `px_per_tile`, sprite/block coordinates, mismatched
/// megatile grids) and executes it against fuzzer-controlled asset bytes, covering the executor
/// end to end: terrain rasterize, the anim/mainSD sprite paths, SD GRP canvas resolution, and
/// block drawing.
///
/// Work stays bounded by construction, alternating between the two clamp regimes: small tile
/// dims with an arbitrary (clamped-to-128) `px_per_tile`, or arbitrary (clamped-to-256) tile
/// dims with a tiny `px_per_tile` — each at most a few MB of output.
const MAX_SMALL_DIM: u16 = 8;
const MAX_MEGATILES: usize = 64;
const MAX_SPRITES: usize = 8;
const MAX_BLOCKS: usize = 4;

/// Reads little-endian scalars off the front of `data`, defaulting to 0 past the end.
struct Carver<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> Carver<'a> {
    fn u8(&mut self) -> u8 {
        let value = self.data.get(self.pos).copied().unwrap_or(0);
        self.pos += 1;
        value
    }

    fn u16(&mut self) -> u16 {
        u16::from_le_bytes([self.u8(), self.u8()])
    }

    fn u32(&mut self) -> u32 {
        u32::from_le_bytes([self.u8(), self.u8(), self.u8(), self.u8()])
    }

    fn i32(&mut self) -> i32 {
        self.u32() as i32
    }

    fn rest(&self) -> &'a [u8] {
        self.data.get(self.pos..).unwrap_or(&[])
    }
}

fuzz_target!(|data: &[u8]| {
    let mut carver = Carver { data, pos: 0 };

    let flags = carver.u8();
    let small_dims_mode = flags & 1 == 0;
    let (map_w, map_h, ppt) = if small_dims_mode {
        (
            (carver.u16() % MAX_SMALL_DIM as u16 + 1) as u32,
            (carver.u16() % MAX_SMALL_DIM as u16 + 1) as u32,
            carver.u32(), // hostile: executor must clamp to 1..=128
        )
    } else {
        (
            carver.u32(), // hostile: executor must clamp to <=256
            carver.u32(),
            (carver.u8() % 4) as u32,
        )
    };

    let tier = match carver.u8() % 3 {
        0 => AssetTier::Sd,
        1 => AssetTier::Hd2,
        _ => AssetTier::Hd,
    };
    let pack = if carver.u8() % 2 == 0 {
        ArtPack::Standard
    } else {
        ArtPack::Carbot
    };

    // Deliberately allowed to disagree with map_w * map_h (shorter grids draw as megatile 0).
    let megatile_count = (carver.u8() as usize) % (MAX_MEGATILES + 1);
    let megatiles: Vec<u16> = (0..megatile_count).map(|_| carver.u16()).collect();

    let sprite_count = (carver.u8() as usize) % (MAX_SPRITES + 1);
    let sprites: Vec<PlannedSprite> = (0..sprite_count)
        .map(|i| PlannedSprite {
            // Force the first sprite onto image 0 so the decode paths (whose art the source
            // below actually carries) are reliably exercised; the rest roam free.
            image_id: if i == 0 { 0 } else { carver.u16() },
            frame: carver.u32(),
            flip: carver.u8() % 2 == 1,
            x: carver.i32(),
            y: carver.i32(),
            tint: (carver.u8() % 2 == 1).then(|| [carver.u8(), carver.u8(), carver.u8()]),
            is_shadow: carver.u8() % 2 == 1,
        })
        .collect();

    let sd_canvases = vec![
        SdCanvasSource {
            image_id: 0,
            grp_path: "a".to_string(), // present in the source below
        },
        SdCanvasSource {
            image_id: carver.u16(),
            grp_path: "missing".to_string(), // never present: the fallback path
        },
    ];

    let block_count = (carver.u8() as usize) % (MAX_BLOCKS + 1);
    let blocks: Vec<PlannedBlock> = (0..block_count)
        .map(|_| PlannedBlock {
            x: carver.i32(),
            y: carver.i32(),
            width: carver.u32(),
            height: carver.u32(),
            color: [carver.u8(), carver.u8(), carver.u8()],
        })
        .collect();

    let plan = RenderPlan {
        width: carver.u32(),  // informational; executor derives its own
        height: carver.u32(), // informational
        px_per_tile: ppt,
        terrain: TerrainPlan {
            tileset: Tileset::Jungle,
            tier,
            pack,
            width: map_w,
            height: map_h,
            megatiles,
        },
        unit_tier: tier,
        unit_pack: pack,
        sprites,
        sd_canvases,
        blocks,
        manifest: Vec::new(), // never read by the executor
    };

    // Split the remaining bytes into the asset payloads. All parsers/decoders are permissive,
    // so truncated/nonsensical bytes are exactly the interesting case.
    let rest = carver.rest();
    let quarter = rest.len() / 4;
    let (dds_bytes, rest) = rest.split_at(quarter);
    let (mainsd_bytes, rest) = rest.split_at(quarter);
    let (anim_bytes, grp_bytes) = rest.split_at(quarter);

    let mut source = MemorySource::new();
    for asset_tier in [AssetTier::Sd, AssetTier::Hd2, AssetTier::Hd] {
        for asset_pack in [ArtPack::Standard, ArtPack::Carbot] {
            source.insert(
                AssetRequest::TilesetDds(Tileset::Jungle, asset_tier, asset_pack),
                dds_bytes.to_vec(),
            );
            source.insert(
                AssetRequest::Anim {
                    image_id: 0,
                    tier: asset_tier,
                    pack: asset_pack,
                },
                anim_bytes.to_vec(),
            );
        }
    }
    source.insert(AssetRequest::MainSdAnim, mainsd_bytes.to_vec());
    source.insert(
        AssetRequest::Grp {
            path: "a".to_string(),
        },
        grp_bytes.to_vec(),
    );

    let _ = execute_plan(&plan, &source);
});
