import assert from 'node:assert/strict'
import { readFile } from 'node:fs/promises'
import test from 'node:test'
import vm from 'node:vm'
import { MessageChannel } from 'node:worker_threads'

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
    wallProfilesJson: () => JSON.stringify({ buildings: [], movers: [] }),
    wallPlacementReachJson(query) {
      const parsed = JSON.parse(query)
      if (!parsed.window || !parsed.placementSite) throw new Error('invalid wall placement reach input')
      return JSON.stringify({ origin: [1, 2], width: 3, height: 2, cells: [1, 0, 1, 0, 1, 1] })
    },
    checkWallJson(query) {
      const parsed = JSON.parse(query)
      if (!parsed.window) throw new Error('invalid wall query')
      return JSON.stringify({ model: 'pixel-rect-v1', window: parsed.window, passage: 'open', route: [parsed.start, parsed.end] })
    },
    suggestWallEndpointsJson(query, entrance) {
      const parsed = JSON.parse(query)
      const span = JSON.parse(entrance)
      if (!parsed.window || span.length !== 2) throw new Error('invalid endpoint request')
      return JSON.stringify({ start: [parsed.window[0], span[0][1]], end: [parsed.window[2] - 1, span[1][1]] })
    },
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

function createHarness({ wallSteps = 3, wallFailure = null, wallStepDuration = 0 } = {}) {
  const events = []
  const waiters = new Map()
  const calls = { analyze: [], sources: [], wallJobs: [], wallYields: [] }
  let cancelOnProgress = null
  let analysisGeneration = 0
  let cancelWallOnProgress = null
  let now = 0

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
      if (copied.progress && cancelWallOnProgress && copied.progress.exploredStates >= cancelWallOnProgress.after) {
        const { token, asynchronous } = cancelWallOnProgress
        cancelWallOnProgress = null
        if (asynchronous) {
          const channel = new MessageChannel()
          channel.port1.onmessage = () => {
            channel.port1.close()
            channel.port2.close()
            self.onmessage({ data: { type: 'cancelWallSearch', token } })
          }
          channel.port2.postMessage(null)
        } else {
          self.onmessage({ data: { type: 'cancelWallSearch', token } })
        }
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
      const analysis = analysisStub()
      analysis.startWallSearch = query => {
        const record = { query: JSON.parse(query), steps: 0, batches: [], freed: 0 }
        calls.wallJobs.push(record)
        return {
          step(count) {
            assert.ok(count > 0 && count <= 16)
            if (wallFailure === 'step') throw new Error('search step failed')
            record.batches.push(count)
            record.steps = Math.min(wallSteps, record.steps + count)
            now += wallStepDuration
            return record.steps >= wallSteps
          },
          get exploredStates() { return record.steps },
          get layoutCount() { return record.steps >= wallSteps ? 1 : 0 },
          resultJson() {
            if (wallFailure === 'result') throw new Error('search serialization failed')
            return JSON.stringify({ reason: 'resultLimit', exploredStates: record.steps, layouts: [{ buildings: [] }] })
          },
          free() { record.freed++ },
        }
      }
      return analysis
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
  function TrackingMessageChannel() {
    calls.wallYields.push(calls.wallJobs.reduce((total, job) => total + job.steps, 0))
    return new MessageChannel()
  }
  const context = vm.createContext({
    testDeps,
    self,
    performance: { now: () => now },
    MessageChannel: TrackingMessageChannel,
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
    cancelWallOnProgress: (token, after = 1, asynchronous = false) => { cancelWallOnProgress = { token, after, asynchronous } },
    cancelWall: token => self.onmessage({ data: { type: 'cancelWallSearch', token } }),
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


test('wall placement reach requires analysis and transfers its local mask', async () => {
  const harness = createHarness()
  await harness.request({ id: 99, type: 'loadMap', bytes: new Uint8Array([1]) })
  const before = await harness.request({ id: 100, type: 'wallPlacementReach', query: {} })
  assert.equal(before.ok, false)
  assert.match(before.error, /Analyze terrain/)
  await setup(harness)
  const query = { window: [9, 17, 41, 33], placementSite: { entrance: [[8, 20], [8, 28]], maxDistancePixels: 64 } }
  const result = await harness.request({ id: 101, type: 'wallPlacementReach', query })
  assert.equal(result.ok, true)
  assert.deepEqual(result.result.origin, [1, 2])
  assert.equal(result.result.width, 3)
  assert.equal(result.result.height, 2)
  assert.deepEqual([...new Uint8Array(result.result.cells)], [1, 0, 1, 0, 1, 1])
})


test('wall requests require analysis, forward structured queries, and recover after validation errors', async () => {
  const harness = createHarness()
  const before = await harness.request({ id: 101, type: 'wallProfiles' })
  assert.equal(before.ok, false)
  await setup(harness)
  const profiles = await harness.request({ id: 102, type: 'wallProfiles' })
  assert.deepEqual(profiles.result, { buildings: [], movers: [] })
  const bad = await harness.request({ id: 103, type: 'checkWall', query: {} })
  assert.equal(bad.ok, false)
  assert.match(bad.error, /invalid wall query/)
  const query = { window: [0, 0, 16, 16], start: [1, 1], end: [14, 14], mover: [0, 0, 0, 0], buildings: [] }
  const checked = await harness.request({ id: 104, type: 'checkWall', query })
  assert.equal(checked.ok, true)
  assert.deepEqual(checked.result, { model: 'pixel-rect-v1', window: query.window, passage: 'open', route: [query.start, query.end], elapsedMs: 0 })
})


test('wall search advances many states per task yield, forwards the query and frees its job', async () => {
  const harness = createHarness({ wallSteps: 129 })
  await setup(harness)
  const query = { block: [8, 8, 8, 8], buildings: [{ footprint: [3, 2] }] }
  const result = await harness.request({ id: 30, type: 'findWalls', token: 30, query })
  assert.equal(result.ok, true)
  assert.equal(result.result.reason, 'resultLimit')
  assert.equal(result.result.exploredStates, 129)
  assert.deepEqual(harness.calls.wallJobs, [{ query, steps: 129, batches: [16, 16, 16, 16, 16, 16, 16, 16, 16], freed: 1 }])
  assert.deepEqual(harness.calls.wallYields, [0, 64, 128, 129])
  assert.ok(harness.events.some(event => event.id === 30 && event.progress?.exploredStates === 16))
})

test('wall search cancels before start, during search and after its final step without publishing layouts', async () => {
  const queuedHarness = createHarness()
  await setup(queuedHarness)
  const queued = queuedHarness.request({ id: 30, type: 'findWalls', token: 30, query: {} })
  queuedHarness.cancelWall(30)
  assert.match((await queued).error, /cancelled/)
  assert.equal(queuedHarness.calls.wallJobs.length, 0)

  const duringHarness = createHarness({ wallSteps: 80 })
  await setup(duringHarness)
  duringHarness.cancelWallOnProgress(31, 1)
  const during = await duringHarness.request({ id: 31, type: 'findWalls', token: 31, query: {} })
  assert.match(during.error, /cancelled/)
  assert.ok(duringHarness.calls.wallJobs[0].steps > 0)
  assert.ok(duringHarness.calls.wallJobs[0].steps < 80)
  assert.equal(duringHarness.calls.wallJobs[0].freed, 1)

  const lateHarness = createHarness()
  await setup(lateHarness)
  lateHarness.cancelWallOnProgress(32, 3, true)
  const late = await lateHarness.request({ id: 32, type: 'findWalls', token: 32, query: {} })
  assert.match(late.error, /cancelled/)
  assert.equal(lateHarness.calls.wallJobs[0].steps, 3)
  assert.equal(lateHarness.calls.wallJobs[0].freed, 1)
  assert.ok(!lateHarness.events.some(event => event.id === 32 && event.ok))

  const retry = await lateHarness.request({ id: 33, type: 'findWalls', token: 33, query: {} })
  assert.equal(retry.ok, true)
  assert.equal(lateHarness.calls.wallJobs.at(-1).freed, 1)
})

test('wall search throttles progress independently of its cooperative slice size', async () => {
  const harness = createHarness({ wallSteps: 500, wallStepDuration: 16 })
  await setup(harness)
  const result = await harness.request({ id: 30, type: 'findWalls', token: 30, query: {} })
  assert.equal(result.ok, true)
  const progress = harness.events.filter(event => event.id === 30 && event.progress).map(event => event.progress.exploredStates)
  assert.equal(progress[0], 16)
  assert.equal(progress.at(-1), 500)
  assert.ok(progress.slice(1, -1).every((states, index) => states - progress[index] >= 96))
  assert.ok(harness.calls.wallYields.length > progress.length)
})

test('wall search frees jobs after step or serialization errors and leaves the worker queue usable', async () => {
  for (const wallFailure of ['step', 'result']) {
    const harness = createHarness({ wallFailure })
    await setup(harness)
    const failed = await harness.request({ id: 30, type: 'findWalls', token: 30, query: {} })
    assert.equal(failed.ok, false)
    assert.match(failed.error, /failed/)
    assert.equal(harness.calls.wallJobs[0].freed, 1)
    assert.equal((await harness.request({ id: 31, type: 'wallProfiles' })).ok, true)
  }
})

test('reusing a wall-search token cancels the old record without deleting the new request', async () => {
  const harness = createHarness()
  await setup(harness)
  const first = harness.request({ id: 30, type: 'findWalls', token: 30, query: {} })
  const second = harness.request({ id: 31, type: 'findWalls', token: 30, query: {} })
  assert.match((await first).error, /cancelled/)
  assert.equal((await second).ok, true)
  assert.equal(harness.calls.wallJobs.length, 1)
  assert.equal(harness.calls.wallJobs[0].freed, 1)
})


test('endpoint suggestions require analysis and recover after malformed requests', async () => {
  const harness = createHarness()
  assert.equal((await harness.request({ id: 50, type: 'suggestWallEndpoints', query: {}, entrance: [] })).ok, false)
  await setup(harness)
  const bad = await harness.request({ id: 51, type: 'suggestWallEndpoints', query: {}, entrance: [] })
  assert.equal(bad.ok, false)
  assert.match(bad.error, /invalid endpoint request/)
  const good = await harness.request({ id: 52, type: 'suggestWallEndpoints', query: { window: [0, 0, 100, 100] }, entrance: [[45, 30], [45, 60]] })
  assert.equal(good.ok, true)
  assert.deepEqual(good.result, { start: [0, 30], end: [99, 60] })
})
