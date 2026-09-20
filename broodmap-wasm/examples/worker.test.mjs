import assert from 'node:assert/strict'
import { readFile } from 'node:fs/promises'
import test from 'node:test'
import vm from 'node:vm'

const workerSource = (await readFile(new URL('./worker.js', import.meta.url), 'utf8')).replace(/\r\n/g, '\n')
const injectedSource = workerSource.replace(
  "import init, { MapRenderer } from '../pkg/broodmap_wasm.js'\nimport { analyzeBaseAreas } from './base-areas.js'\n",
  'const { init, MapRenderer, analyzeBaseAreas } = testDeps\n',
)
assert.notEqual(injectedSource, workerSource, 'worker import prelude changed')

async function timeout(promise, label) {
  let timer
  try {
    return await Promise.race([
      promise,
      new Promise((_, reject) => { timer = setTimeout(() => reject(new Error(`timed out waiting for ${label}`)), 5_000) }),
    ])
  } finally {
    clearTimeout(timer)
  }
}

function analysisStub() {
  return {
    widthWalkTiles: 2,
    heightWalkTiles: 2,
    obstacleCount: 0,
    cellFlags: () => new Uint8Array(4),
    clearancePixels: () => new Uint16Array(4),
    setObstaclesEnabled() {},
    free() {},
  }
}

function fetchStub() {
  return async () => ({
    ok: true,
    headers: { get: () => null },
    body: {
      getReader: () => ({
        read: async () => ({ done: true }),
        releaseLock() {},
        cancel: async () => {},
      }),
      cancel: async () => {},
    },
  })
}

function createHarness() {
  const events = []
  const waiters = new Map()
  const calls = { analyze: [], sources: [] }
  let cancelOnProgress = null
  let analysisGeneration = 0

  const self = {
    onmessage: null,
    postMessage(message, transfer = []) {
      const copied = structuredClone(message, { transfer })
      events.push(copied)
      if (copied.progress && cancelOnProgress !== null) {
        const token = cancelOnProgress
        cancelOnProgress = null
        self.onmessage({ data: { type: 'cancelBaseAreas', token } })
      }
      if (Object.hasOwn(copied, 'ok')) {
        const waiter = waiters.get(copied.id)
        if (waiter) {
          waiters.delete(copied.id)
          waiter.resolve(copied)
        }
      }
    },
  }

  class MapRenderer {
    constructor() {
      this.widthTiles = 1
      this.heightTiles = 1
    }

    minimapRgba() {
      return { width: 1, height: 1, data: new Uint8Array(4), warnings: [], free() {} }
    }

    requiredMapAnalysisAssets() {
      return ['cv5', 'vf4', 'units']
    }

    analyzeMap() {
      analysisGeneration++
      return analysisStub()
    }

    free() {}
  }

  const testDeps = {
    init: async () => {},
    MapRenderer,
    analyzeBaseAreas: async (_analysis, bases, widening, { checkpoint, onProgress }) => {
      calls.analyze.push({ bases: structuredClone(bases), widening })
      await checkpoint()
      onProgress({ completedOrigins: 1, totalOrigins: 1, surveyCount: 2 })
      await checkpoint()
      const labels = new Uint32Array([analysisGeneration, calls.analyze.length, widening, bases.length]).buffer
      calls.sources.push(labels)
      return {
        terrainOnly: true,
        labels,
        areas: [],
        boundaries: [],
        baseAreas: [],
        surveyCount: 2,
        skippedAnchorCount: 0,
        observationCount: 0,
        consolidatedCount: 0,
      }
    },
  }
  const context = vm.createContext({
    testDeps,
    self,
    performance: { now: () => 0 },
    setTimeout,
    fetch: fetchStub(),
    structuredClone,
    URL,
  })
  vm.runInContext(injectedSource, context, { filename: 'worker.js' })

  const request = (data) => {
    const completion = new Promise((resolve, reject) => {
      waiters.set(data.id, { resolve, reject })
    })
    self.onmessage({ data })
    return timeout(completion, `worker response ${data.id}`)
  }
  return {
    events,
    calls,
    request,
    cancelOnProgress: (token) => { cancelOnProgress = token },
    cancel: (token) => self.onmessage({ data: { type: 'cancelBaseAreas', token } }),
  }
}

const bases = [
  { id: 0, routeAnchor: [0, 0] },
  { id: 1, routeAnchor: [1, 0] },
]

async function setup(harness) {
  const loaded = await harness.request({ id: 1, type: 'loadMap', bytes: new Uint8Array([1]) })
  assert.equal(loaded.ok, true)
  const analyzed = await harness.request({ id: 2, type: 'analyzeTerrain', assetBase: 'https://assets.example/' })
  assert.equal(analyzed.ok, true)
}

async function findAreas(harness, id, token, widening = 25, catalog = bases) {
  return harness.request({
    id,
    type: 'findBaseAreas',
    token,
    minWideningPercent: widening,
    bases: catalog,
  })
}

test('worker retains cached source labels across transfers, keys by inputs, preserves terrain-only cache across obstacle mode, and clears it after terrain replacement', async () => {
  const harness = createHarness()
  await setup(harness)

  const first = await findAreas(harness, 3, 1)
  assert.equal(first.ok, true)
  assert.equal(first.result.cached, false)
  assert.deepEqual([...new Uint32Array(first.result.labels)], [1, 1, 25, 2])
  assert.equal(harness.calls.analyze.length, 1)
  assert.equal(harness.calls.sources[0].byteLength, 16, 'transferred result must be a copy')

  const cached = await findAreas(harness, 4, 2)
  assert.equal(cached.ok, true)
  assert.equal(cached.result.cached, true)
  assert.deepEqual([...new Uint32Array(cached.result.labels)], [1, 1, 25, 2])
  assert.equal(harness.calls.analyze.length, 1)

  const changedWidening = await findAreas(harness, 5, 3, 40)
  assert.equal(changedWidening.result.cached, false)
  assert.equal(harness.calls.analyze.length, 2)
  const changedCatalog = [...bases, { id: 2, routeAnchor: [2, 0] }]
  const changedBases = await findAreas(harness, 6, 4, 40, changedCatalog)
  assert.equal(changedBases.result.cached, false)
  assert.equal(harness.calls.analyze.length, 3)

  const obstacle = await harness.request({ id: 7, type: 'setObstaclesEnabled', enabled: false })
  assert.equal(obstacle.ok, true)
  const afterObstacle = await findAreas(harness, 8, 5, 40, changedCatalog)
  assert.equal(afterObstacle.result.cached, true, 'terrain-only areas survive active-obstacle changes')
  assert.equal(harness.calls.analyze.length, 3)

  const reanalyzed = await harness.request({ id: 9, type: 'analyzeTerrain', assetBase: 'https://assets.example/' })
  assert.equal(reanalyzed.ok, true)
  const afterTerrain = await findAreas(harness, 10, 6, 40, changedCatalog)
  assert.equal(afterTerrain.result.cached, false)
  assert.equal(harness.calls.analyze.length, 4)
  assert.deepEqual([...new Uint32Array(afterTerrain.result.labels)], [2, 4, 40, 3])
})

test('worker cancels on progress without caching a partial result, supports cancellation before queued handling, and retries cleanly', async () => {
  const harness = createHarness()
  await setup(harness)

  harness.cancelOnProgress(10)
  const cancelled = await findAreas(harness, 3, 10)
  assert.equal(cancelled.ok, false)
  assert.match(cancelled.error, /cancelled/i)
  assert.equal(harness.calls.analyze.length, 1)
  assert.equal(harness.calls.sources.length, 0, 'cancelled analysis must not produce a cacheable result')
  assert.ok(harness.events.some(event => event.id === 3 && event.progress))

  const retry = await findAreas(harness, 4, 11)
  assert.equal(retry.ok, true)
  assert.equal(retry.result.cached, false)
  assert.equal(harness.calls.analyze.length, 2)

  const queued = findAreas(harness, 5, 12)
  harness.cancel(12)
  const queuedCancelled = await queued
  assert.equal(queuedCancelled.ok, false)
  assert.match(queuedCancelled.error, /cancelled/i)
  assert.equal(harness.calls.analyze.length, 2, 'queued cancellation runs before the analyzer')

  const afterQueuedCancel = await findAreas(harness, 6, 13)
  assert.equal(afterQueuedCancel.ok, true)
  assert.equal(afterQueuedCancel.result.cached, true, 'queued cancellation must not disturb the completed cache')
  assert.equal(harness.calls.analyze.length, 2)
})


test('a reused queued token reports cancellation instead of dereferencing a missing request', async () => {
  const harness = createHarness()
  await setup(harness)
  const first = findAreas(harness, 3, 100)
  const second = findAreas(harness, 4, 100)
  assert.equal((await first).ok, true)
  const result = await second
  assert.equal(result.ok, false)
  assert.match(result.error, /cancelled/)
})
