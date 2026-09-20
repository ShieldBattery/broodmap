import init, { MapRenderer } from '../pkg/broodmap_wasm.js'
import { analyzeBaseAreas } from './base-areas.js'

let map = null
let analysis = null
let ready = null
let jobs = Promise.resolve()
let baseAreaCache = null
const areaRequests = new Map()

self.onmessage = ({ data }) => {
  // Cancellation must bypass the serialized queue: the running job yields between origins.
  if (data.type === 'cancelBaseAreas') {
    const request = areaRequests.get(data.token)
    if (request) request.cancelled = true
    return
  }
  if (data.type === 'findBaseAreas') areaRequests.set(data.token, { cancelled: false })
  jobs = jobs.then(() => handle(data)).catch((error) => {
    self.postMessage({ id: data.id, ok: false, error: message(error) })
  }).finally(() => {
    if (data.type === 'findBaseAreas') areaRequests.delete(data.token)
  })
}

async function wasm() {
  if (!ready) ready = init()
  return ready
}

function message(error) {
  return error?.message || String(error)
}

function imageResult(image) {
  try {
    return {
      width: image.width,
      height: image.height,
      data: new Uint8Array(image.data).buffer,
      warnings: [...(image.warnings || [])],
    }
  } finally {
    image.free?.()
  }
}

function post(id, result, buffers = []) {
  self.postMessage({ id, ok: true, result }, buffers)
}

function assetUrl(base, path) {
  return new URL(path.replace(/^[/\\]+/, ''), base).href
}

async function fetchBytes(base, path, required, maxBytes) {
  let response
  try {
    response = await fetch(assetUrl(base, path))
  } catch (error) {
    throw new Error(`Could not fetch ${path}: ${message(error)}. Check the asset base and rerun broodmap-cli fetch-assets.`)
  }
  if (!response.ok) {
    if (!required) return null
    throw new Error(`Missing required analysis asset ${path} (${response.status}). Check the asset base and rerun broodmap-cli fetch-assets.`)
  }
  if (maxBytes && response.headers.get('Content-Type')?.includes('text/html')) {
    await response.body?.cancel()
    throw new Error(`Analysis asset ${path} returned HTML instead of analysis data. Check the asset base and rerun broodmap-cli fetch-assets --analysis-only.`)
  }
  if (!maxBytes) return new Uint8Array(await response.arrayBuffer())
  const declaredLength = Number(response.headers.get('Content-Length'))
  if (Number.isFinite(declaredLength) && declaredLength > maxBytes) {
    await response.body?.cancel()
    throw new Error(`Analysis asset ${path} exceeds its ${maxBytes}-byte limit.`)
  }
  if (!response.body) throw new Error(`Analysis asset ${path} has no readable response body.`)
  const reader = response.body.getReader()
  const chunks = []
  let length = 0
  try {
    while (true) {
      const { done, value } = await reader.read()
      if (done) break
      length += value.byteLength
      if (length > maxBytes) {
        await reader.cancel()
        throw new Error(`Analysis asset ${path} exceeds its ${maxBytes}-byte limit.`)
      }
      chunks.push(value)
    }
  } finally {
    reader.releaseLock()
  }
  const bytes = new Uint8Array(length)
  let offset = 0
  for (const chunk of chunks) {
    bytes.set(chunk, offset)
    offset += chunk.byteLength
  }
  return bytes
}

async function addAssets(paths, base, allowMissing) {
  let fetched = 0
  let skipped = 0
  for (const path of paths) {
    const bytes = await fetchBytes(base, path, !allowMissing)
    if (!bytes) {
      skipped++
      continue
    }
    map.addAsset(path, bytes)
    fetched++
  }
  return { fetched, skipped }
}

async function handle({ id, type, ...payload }) {
  await wasm()
  if (type === 'loadMap') {
    baseAreaCache = null
    analysis?.free?.()
    analysis = null
    map?.free?.()
    map = null
    map = new MapRenderer(new Uint8Array(payload.bytes))
    const minimap = imageResult(map.minimapRgba(JSON.stringify({ scale: 2 })))
    post(id, { widthTiles: map.widthTiles, heightTiles: map.heightTiles, minimap }, [minimap.data])
    return
  }
  if (!map) throw new Error('Choose a map first.')
  if (type === 'render') {
    const first = await addAssets(map.requiredAssets(payload.options), payload.assetBase, false)
    const second = await addAssets(map.requiredGraphics(payload.options), payload.assetBase, true)
    const started = performance.now()
    const image = imageResult(map.renderRgba(payload.options))
    post(id, { image, elapsedMs: Math.round(performance.now() - started), fetched: first.fetched + second.fetched, skipped: first.skipped + second.skipped }, [image.data])
    return
  }
  if (type === 'analyzeTerrain') {
    const paths = map.requiredMapAnalysisAssets()
    if (paths.length !== 3) throw new Error(`Expected CV5, VF4, and units.dat analysis assets, received ${paths.length}.`)
    const cv5 = await fetchBytes(payload.assetBase, paths[0], true, 2048 * 52)
    const vf4 = await fetchBytes(payload.assetBase, paths[1], true, 65536 * 32)
    const units = await fetchBytes(payload.assetBase, paths[2], true, 19876)
    const nextAnalysis = map.analyzeMap(cv5, vf4, units)
    let flags
    let clearance
    let minimap
    try {
      // The bindings return owned arrays, so their buffers can be transferred directly.
      flags = nextAnalysis.cellFlags().buffer
      clearance = nextAnalysis.clearancePixels().buffer
      minimap = imageResult(map.minimapRgba(JSON.stringify({ scale: 2 })))
    } catch (error) {
      nextAnalysis.free?.()
      throw error
    }
    const oldAnalysis = analysis
    analysis = nextAnalysis
    baseAreaCache = null
    oldAnalysis?.free?.()
    post(id, { widthWalkTiles: analysis.widthWalkTiles, heightWalkTiles: analysis.heightWalkTiles, obstacleCount: analysis.obstacleCount, flags, clearance, minimap }, [flags, clearance, minimap.data])
    return
  }
  if (type === 'setObstaclesEnabled') {
    if (!analysis) throw new Error('Analyze terrain before changing obstacle handling.')
    analysis.setObstaclesEnabled(Boolean(payload.enabled))
    const flags = analysis.cellFlags().buffer
    const clearance = analysis.clearancePixels().buffer
    post(id, { flags, clearance, obstacleCount: analysis.obstacleCount, enabled: Boolean(payload.enabled) }, [flags, clearance])
    return
  }
  if (type === 'findRegions') {
    if (!analysis) throw new Error('Analyze terrain before finding regions.')
    const started = performance.now()
    const snapshot = analysis.analyzeRegions(Number(payload.minProminencePixels), Number(payload.minRelativeProminencePercent))
    try {
      const labels = snapshot.labels().buffer
      const metadata = JSON.parse(snapshot.metadataJson())
      post(id, { labels, metadata, elapsedMs: Math.round(performance.now() - started) }, [labels])
    } finally {
      snapshot.free?.()
    }
    return
  }
  if (type === 'route') {
    if (!analysis) throw new Error('Analyze terrain before routing.')
    post(id, JSON.parse(analysis.routeJson(payload.startX, payload.startY, payload.endX, payload.endY)))
    return
  }
  if (type === 'findBases') {
    if (!analysis) throw new Error('Analyze terrain before finding bases.')
    const started = performance.now()
    const result = JSON.parse(analysis.findBasesJson('{}'))
    post(id, { ...result, elapsedMs: Math.round(performance.now() - started) })
    return
  }
  if (type === 'baseRoute') {
    if (!analysis) throw new Error('Analyze terrain before comparing bases.')
    post(id, JSON.parse(analysis.baseRouteJson(payload.startId, payload.endId)))
    return
  }
  if (type === 'findBaseAreas') {
    if (!analysis) throw new Error('Analyze terrain before finding base areas.')
    const started = performance.now()
    const request = areaRequests.get(payload.token)
    const checkCancelled = () => {
      if (!request || request.cancelled) throw new Error('Base-area analysis cancelled.')
    }
    const checkpoint = async () => {
      await new Promise((resolve) => setTimeout(resolve, 0))
      checkCancelled()
    }
    await checkpoint()
    const key = JSON.stringify([payload.minWideningPercent, payload.bases])
    const cached = baseAreaCache?.key === key
    let result = cached ? baseAreaCache.result : await analyzeBaseAreas(
      analysis, payload.bases, payload.minWideningPercent, {
        checkpoint,
        onProgress: (progress) => self.postMessage({ id, progress }),
      },
    )
    await checkpoint()
    // Only complete successful runs replace the cache. Transfer a copy to retain ownership.
    if (!cached) baseAreaCache = { key, result }
    result = { ...result, labels: result.labels.slice(0), cached, elapsedMs: Math.round(performance.now() - started) }
    post(id, result, [result.labels])
    return
  }
  if (type === 'findEntrances') {
    if (!analysis) throw new Error('Analyze terrain before inspecting entrances.')
    const started = performance.now()
    const start = [payload.startX, payload.startY]
    const end = [payload.endX, payload.endY]
    const surveys = JSON.parse(analysis.entrancesBatchJson(
      JSON.stringify([[start, end], [end, start]]), payload.minWideningPercent,
    )).map((survey, index) => ({ from: index ? 'B' : 'A', ...survey }))
    post(id, { surveys, elapsedMs: Math.round(performance.now() - started) })
    return
  }
  throw new Error(`Unknown worker request: ${type}`)
}
