// Type-checked by `pnpm run test:types` through the built package's exports, with only the default
// ES2022 lib, to catch declarations that need consumers to change their tsconfig.
import init, {
  CascStorage,
  MapRenderer,
  initSync,
  summarizeMapJson,
  type CascFiles,
  type CascStorageOptions,
  type MapLimitOptions,
  type MapSummary,
} from '@shieldbattery/broodmap'

export async function summarize(mapBytes: Uint8Array, limits: MapLimitOptions): Promise<string[]> {
  await init()
  const options = JSON.stringify(limits)
  const summary: MapSummary = JSON.parse(summarizeMapJson(mapBytes, options))

  const map = new MapRenderer(mapBytes, options)
  try {
    const rendered: MapSummary = JSON.parse(map.summaryJson())
    return [summary.name ?? '', rendered.stringEncoding, ...summary.triggerActionKinds]
  } finally {
    map.free()
  }
}

export function renderFromInstall(files: CascFiles, mapBytes: Uint8Array): Uint8Array {
  const options: CascStorageOptions = { cacheBytes: 0 }
  const storage = new CascStorage(files, JSON.stringify(options))
  const map = new MapRenderer(mapBytes)
  try {
    const missing: string[] = map.loadAssets(storage, '{}')
    return missing.length ? new Uint8Array() : map.renderPng('{}')
  } finally {
    map.free()
    storage.free()
  }
}

export { initSync }
