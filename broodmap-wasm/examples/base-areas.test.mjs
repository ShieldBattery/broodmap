import assert from 'node:assert/strict'
import test from 'node:test'
import { analyzeBaseAreas } from './base-areas.js'

const anchors = [
  { id: 0, routeAnchor: [0, 0] },
  { id: 1, routeAnchor: [1, 0] },
  { id: 2, routeAnchor: [2, 0] },
  { id: 3, routeAnchor: [8, 0] },
  { id: 4, routeAnchor: null },
]

function labels(width, bases, components = new Map()) {
  const height = Math.max(1, ...bases.flatMap(base => base.routeAnchor ? [base.routeAnchor[1] + 1] : []))
  const result = new Uint32Array(width * height)
  for (const base of bases) {
    if (base.routeAnchor) {
      result[base.routeAnchor[1] * width + base.routeAnchor[0]] = components.get(base.id) ?? 1
    }
  }
  return result
}

function fixture({
  bases = anchors,
  width = 16,
  originalLabels = labels(width, bases, new Map([[3, 2]])),
  candidateFor = () => [{ endpoints: [[100, 0], [100, 80]], widthPixels: 16 }],
} = {}) {
  const calls = { batches: [], partitions: [], freed: 0 }
  const analysis = {
    widthWalkTiles: width,
    rampsJson: () => JSON.stringify({ ramps: [] }),
    cellFlags: () => new Uint8Array(originalLabels.length).fill(1),
    entrancesJson() { throw new Error('map-wide orchestration must use batches') },
    entrancesBatchJson(json, widening) {
      const queries = JSON.parse(json)
      calls.batches.push({ queries, widening })
      return JSON.stringify(queries.map(([start, end]) => ({
        terrainOnly: true,
        candidates: candidateFor(start, end),
      })))
    },
    partitionAreas(json) {
      const spans = JSON.parse(json)
      calls.partitions.push(spans)
      return {
        labels: () => originalLabels.slice(),
        metadataJson: () => JSON.stringify({
          terrainOnly: true,
          areas: [{ id: 1, cellCount: width - 1 }, { id: 2, cellCount: 1 }],
          boundaries: spans.map(() => ({ removedEdgeCount: 1, separatedEdgeCount: 0, regionPairs: [] })),
        }),
        free: () => { calls.freed++ },
      }
    },
  }
  return { analysis, calls }
}

function source(start, end) {
  return `${start[0]}:${end[0]}`
}

function flatBases(count, positions = []) {
  return Array.from({ length: count }, (_, id) => ({ id, routeAnchor: positions[id] || [200 + id * 20, 0] }))
}

test('surveys only same-component nearest anchors, one origin batch at a time, independent of input order', async () => {
  const first = fixture()
  const progress = []
  const result = await analyzeBaseAreas(first.analysis, [...anchors].reverse(), 25, {
    onProgress: update => progress.push(update),
  })
  assert.equal(first.calls.batches.length, 3)
  assert.deepEqual(first.calls.batches.map(call => call.queries), [
    [[[0, 0], [1, 0]], [[0, 0], [2, 0]]],
    [[[1, 0], [0, 0]], [[1, 0], [2, 0]]],
    [[[2, 0], [0, 0]], [[2, 0], [1, 0]]],
  ])
  assert.deepEqual(progress, [
    { completedOrigins: 1, totalOrigins: 4, surveyCount: 2 },
    { completedOrigins: 2, totalOrigins: 4, surveyCount: 4 },
    { completedOrigins: 3, totalOrigins: 4, surveyCount: 6 },
    { completedOrigins: 4, totalOrigins: 4, surveyCount: 6 },
  ])
  assert.deepEqual(first.calls.partitions, [[], [[[100, 0], [100, 80]]]])
  assert.equal(first.calls.freed, 2)
  assert.equal(result.surveyCount, 6)
  assert.equal(result.skippedAnchorCount, 1)
  assert.equal(result.observationCount, 1)
  assert.equal(result.consolidatedCount, 1)
  assert.deepEqual(result.boundaries[0].sources, [[0, 1], [0, 2], [1, 0], [1, 2], [2, 0], [2, 1]])
  assert.deepEqual(result.baseAreas.map(area => area.baseId), [0, 1, 2, 3])
  assert.deepEqual(new Uint32Array(result.labels), labels(16, anchors, new Map([[3, 2]])))

  const second = fixture()
  const reordered = await analyzeBaseAreas(second.analysis, anchors, 25)
  assert.deepEqual(second.calls.batches, first.calls.batches)
  assert.deepEqual(reordered.boundaries, result.boundaries)
  assert.deepEqual(new Uint32Array(reordered.labels), new Uint32Array(result.labels))
})

test('uses the narrowest exact observation and deduplicated sorted sources', async () => {
  const bases = flatBases(2)
  const { analysis } = fixture({
    bases,
    originalLabels: labels(300, bases),
    width: 300,
    candidateFor: (start, end) => [{
      endpoints: start[0] < end[0] ? [[0, 0], [0, 100]] : [[0, 100], [0, 0]],
      widthPixels: start[0] < end[0] ? 20 : 8,
    }],
  })
  const result = await analyzeBaseAreas(analysis, bases, 25)
  assert.equal(result.observationCount, 1)
  assert.equal(result.consolidatedCount, 1)
  assert.equal(result.boundaries[0].widthPixels, 8)
  assert.deepEqual(result.boundaries[0].sources, [[0, 1], [1, 0]])
})

test('near consolidation keeps complete-link, nonparallel, and anchor-between gates', async () => {
  const run = async (bases, observations) => {
    const { analysis } = fixture({
      bases,
      width: 400,
      originalLabels: labels(400, bases),
      candidateFor: (start, end) => observations.get(source(start, end)) || [],
    })
    return analyzeBaseAreas(analysis, bases, 25)
  }
  const a = { endpoints: [[0, 0], [0, 100]], widthPixels: 20 }
  const b = { endpoints: [[24, 0], [24, 100]], widthPixels: 16 }
  const c = { endpoints: [[48, 0], [48, 100]], widthPixels: 12 }

  const completeLink = await run(flatBases(3), new Map([
    ['200:220', [a]], ['220:200', [b]], ['200:240', [c]],
  ]))
  assert.equal(completeLink.observationCount, 3)
  assert.equal(completeLink.consolidatedCount, 2, 'A/B may merge but C cannot transitively join')

  const nonparallel = await run(flatBases(2), new Map([
    ['200:220', [{ endpoints: [[0, 0], [20, 0]], widthPixels: 10 }]],
    ['220:200', [{ endpoints: [[0, 0], [20, 20]], widthPixels: 9 }]],
  ]))
  assert.equal(nonparallel.consolidatedCount, 2)

  const betweenBases = [
    { id: 0, routeAnchor: [200, 200] },
    { id: 1, routeAnchor: [220, 200] },
    { id: 2, routeAnchor: [1, 6] },
  ]
  const anchorBetween = await run(betweenBases, new Map([
    ['200:220', [{ endpoints: [[0, 0], [0, 100]], widthPixels: 12 }]],
    ['220:200', [{ endpoints: [[16, 0], [16, 100]], widthPixels: 10 }]],
  ]))
  assert.equal(anchorBetween.consolidatedCount, 2)
})

test('cancellation or batch errors do not run the final partition and always free the original snapshot', async () => {
  const bases = flatBases(2)
  const cancelled = fixture({ bases, width: 300, originalLabels: labels(300, bases) })
  let checkpoints = 0
  await assert.rejects(
    analyzeBaseAreas(cancelled.analysis, bases, 25, {
      checkpoint: async () => {
        checkpoints++
        if (checkpoints === 3) throw new Error('cancelled')
      },
    }),
    /cancelled/,
  )
  assert.equal(cancelled.calls.batches.length, 2)
  assert.deepEqual(cancelled.calls.partitions, [[]])
  assert.equal(cancelled.calls.freed, 1)

  const failed = fixture({ bases, width: 300, originalLabels: labels(300, bases) })
  failed.analysis.entrancesBatchJson = () => { throw new Error('batch failed') }
  await assert.rejects(analyzeBaseAreas(failed.analysis, bases, 25), /batch failed/)
  assert.deepEqual(failed.calls.partitions, [[]])
  assert.equal(failed.calls.freed, 1)
})

test('adds reverse surveys even when the distant base is not among the destination nearest three', async () => {
  const bases = flatBases(5, [[0, 0], [1, 0], [2, 0], [3, 0], [50, 0]])
  const { analysis, calls } = fixture({ bases, width: 64, originalLabels: labels(64, bases), candidateFor: () => [] })
  const result = await analyzeBaseAreas(analysis, bases, 25)
  const queries = calls.batches.flatMap(batch => batch.queries)
  const pairs = new Set(queries.map(([a, b]) => `${a[0]}:${b[0]}`))
  assert(pairs.has('50:3'))
  assert(pairs.has('3:50'), 'reverse survey must survive nearest-three filtering')
  assert(!pairs.has('0:50'), 'unrelated distant pairs are not added exhaustively')
  for (const [a, b] of queries) assert(pairs.has(`${b[0]}:${a[0]}`))
  assert.equal(result.surveyCount, pairs.size)
  assert(calls.batches.every(batch => batch.queries.every(([start]) => start[0] === batch.queries[0][0][0])))
})

test('too many distinct boundaries fail before applying any cuts', async () => {
  const bases = flatBases(2)
  const observations = Array.from({ length: 257 }, (_, index) => {
    const x = (index % 16) * 128
    const y = Math.floor(index / 16) * 128
    return { endpoints: [[x, y], [x, y + 80]], widthPixels: 80 }
  })
  const { analysis, calls } = fixture({ bases, width: 300, originalLabels: labels(300, bases), candidateFor: () => observations })
  await assert.rejects(analyzeBaseAreas(analysis, bases, 25), /257 boundaries/)
  assert.deepEqual(calls.partitions, [[]])
  assert.equal(calls.freed, 1)
})


test('serial cuts are repartitioned using only the chosen boundary before publishing labels and assessments', async () => {
  const bases = [{ id: 0, routeAnchor: [0, 0] }, { id: 1, routeAnchor: [40, 0] }]
  const spans = [72, 136, 200].map(x => ({ endpoints: [[x, 0], [x, 80]], widthPixels: 80 }))
  const calls = []
  let freed = 0
  const assessment = pair => ({ removedEdgeCount: 1, separatedEdgeCount: 1, regionPairs: [pair] })
  const analysis = {
    widthWalkTiles: 45,
    rampsJson: () => JSON.stringify({ ramps: [] }),
    cellFlags: () => new Uint8Array(45).fill(1),
    entrancesBatchJson: json => JSON.stringify(JSON.parse(json).map(() => ({ candidates: spans }))),
    partitionAreas(json) {
      const cuts = JSON.parse(json)
      calls.push(cuts)
      const labels = Uint32Array.from({ length: 45 }, (_, x) => {
        if (!cuts.length) return 1
        if (cuts.length === 1) return x < 17 ? 1 : 2
        return x < 9 ? 1 : x < 17 ? 2 : x < 25 ? 3 : 4
      })
      const counts = new Map()
      for (const id of labels) counts.set(id, (counts.get(id) || 0) + 1)
      const boundaries = cuts.length === 3
        ? [[1, 2], [2, 3], [3, 4]].map(assessment)
        : cuts.map(() => assessment([1, 2]))
      return {
        labels: () => labels,
        metadataJson: () => JSON.stringify({ areas: [...counts].map(([id, cellCount]) => ({ id, cellCount })), boundaries }),
        free: () => { freed++ },
      }
    },
  }
  const result = await analyzeBaseAreas(analysis, bases, 25)
  assert.deepEqual(calls, [[], spans.map(span => span.endpoints), [spans[1].endpoints]])
  assert.equal(freed, 3)
  assert.equal(result.serialCutsRemoved, 2)
  assert.equal(result.boundaries.length, 1)
  assert.equal(result.boundaries[0].observations.length, 3)
  assert.deepEqual(result.boundaries[0].regionPairs, [[1, 2]])
  assert.deepEqual(result.baseAreas.map(area => area.cellCount), [17, 28])
  assert.deepEqual([...new Uint32Array(result.labels)], [...Array(17).fill(1), ...Array(28).fill(2)])
})

test('crossing conflicts are removed before partitioning and counted separately from duplicates', async () => {
  const bases = flatBases(2)
  const broad = { endpoints: [[608, 1036], [1176, 1036]], widthPixels: 568 }
  const narrow = [
    { endpoints: [[828, 928], [828, 1280]], widthPixels: 352 },
    { endpoints: [[928, 832], [1192, 1096]], widthPixels: 373.296 },
  ]
  const { analysis, calls } = fixture({
    bases, width: 300, originalLabels: labels(300, bases),
    candidateFor: () => [broad, ...narrow],
  })
  const result = await analyzeBaseAreas(analysis, bases, 25)
  assert.deepEqual(calls.partitions, [[], narrow.map(candidate => candidate.endpoints)])
  assert.equal(result.observationCount, 3)
  assert.equal(result.crossingCutsRemoved, 1)
  assert.equal(result.serialCutsRemoved, 0)
  assert.equal(result.boundaries.length, 2)
})

test('structural ramp ends stay distinct through nearby and serial consolidation', async () => {
  const bases = flatBases(2)
  const { analysis, calls } = fixture({
    bases, width: 300, originalLabels: labels(300, bases), candidateFor: () => [],
  })
  const lower = { position: [12, 2], endpoints: [[80, 16], [120, 16]], widthPixels: 40 }
  const upper = { position: [12, 5], endpoints: [[80, 40], [120, 40]], widthPixels: 40 }
  analysis.rampsJson = () => JSON.stringify({
    ramps: [{ id: 1, lowerElevation: 0, upperElevation: 1, lower, upper }],
  })
  const result = await analyzeBaseAreas(analysis, bases, 25)
  assert.equal(result.rampCount, 1)
  assert.deepEqual(calls.partitions, [[], [lower.endpoints, upper.endpoints]])
  assert.deepEqual(result.boundaries.map(b => [b.kind, b.rampId, b.rampEnd]), [
    ['ramp', 1, 'lower'], ['ramp', 1, 'upper'],
  ])
  assert.equal(result.rampMouthsRemoved, 0)
  assert.equal(result.junctionCutsRemoved, 0)
})

test('the total boundary cap includes both ends of each ramp before any cuts are applied', async () => {
  const bases = flatBases(2)
  const { analysis, calls } = fixture({
    bases, width: 300, originalLabels: labels(300, bases), candidateFor: () => [],
  })
  analysis.rampsJson = () => JSON.stringify({
    ramps: Array.from({ length: 129 }, (_, i) => ({
      id: i + 1, lowerElevation: 0, upperElevation: 1,
      lower: { endpoints: [[i * 8, 0], [i * 8, 80]], widthPixels: 80 },
      upper: { endpoints: [[i * 8, 96], [i * 8, 176]], widthPixels: 80 },
    })),
  })
  await assert.rejects(analyzeBaseAreas(analysis, bases, 25), /258 entrance and ramp boundaries/)
  assert.deepEqual(calls.partitions, [[]])
})


test('repartitions once between two bounded ramp-mouth passes and checkpoints before each changed partition', async () => {
  const bases = [{ id: 0, routeAnchor: [0, 0] }, { id: 1, routeAnchor: [10, 0] }]
  const firstMouth = { endpoints: [[40, 0], [120, 80]], widthPixels: 200 }
  const ramp = {
    position: [12, 2], endpoints: [[48, 8], [128, 88]], widthPixels: 240,
  }
  const secondMouth = { endpoints: [[240, 0], [240, 200]], widthPixels: 528 }
  const upper = { position: [52, 2], endpoints: [[416, 8], [496, 88]], widthPixels: 240 }
  const first = firstMouth.endpoints
  const structural = ramp.endpoints
  const second = secondMouth.endpoints
  const upperSpan = upper.endpoints
  const assessment = pair => ({ removedEdgeCount: 1, separatedEdgeCount: 1, regionPairs: [pair] })

  function mock() {
    const calls = { partitions: [], freed: 0 }
    const analysis = {
      widthWalkTiles: 64,
      rampsJson: () => JSON.stringify({ ramps: [{
        id: 1, lowerElevation: 0, upperElevation: 1, lower: ramp, upper,
      }] }),
      cellFlags: () => new Uint8Array(64).fill(1),
      entrancesBatchJson: json => JSON.stringify(JSON.parse(json).map(() => ({
        candidates: [firstMouth, secondMouth],
      }))),
      partitionAreas(json) {
        const cuts = JSON.parse(json)
        calls.partitions.push(cuts)
        const has = span => cuts.some(candidate => JSON.stringify(candidate) === JSON.stringify(span))
        const values = new Uint32Array(64).fill(1)
        values[0] = values[10] = 3
        let boundaries
        if (!cuts.length) {
          boundaries = []
        } else if (has(first)) {
          values[20] = 2
          values[30] = 1
          boundaries = cuts.map(span => JSON.stringify(span) === JSON.stringify(first) ? assessment([2, 3])
            : JSON.stringify(span) === JSON.stringify(structural) ? assessment([1, 2])
              : JSON.stringify(span) === JSON.stringify(upperSpan) ? assessment([9, 10])
                : { removedEdgeCount: 1, separatedEdgeCount: 0, regionPairs: [] })
        } else if (has(second)) {
          values[20] = 4
          values[30] = 1
          boundaries = cuts.map(span => JSON.stringify(span) === JSON.stringify(second) ? assessment([4, 3])
            : JSON.stringify(span) === JSON.stringify(upperSpan) ? assessment([9, 10])
              : assessment([1, 4]))
        } else {
          values[20] = 4
          values[30] = 1
          boundaries = cuts.map(span => JSON.stringify(span) === JSON.stringify(upperSpan) ? assessment([9, 10]) : assessment([1, 4]))
        }
        return {
          labels: () => values,
          metadataJson: () => JSON.stringify({
            areas: [{ id: 1, cellCount: 61 }, { id: 3, cellCount: 2 }, { id: 4, cellCount: 1 }],
            boundaries,
          }),
          free: () => { calls.freed++ },
        }
      },
    }
    return { analysis, calls }
  }

  const complete = mock()
  const checkpoints = []
  const result = await analyzeBaseAreas(complete.analysis, bases, 25, {
    checkpoint: async () => { checkpoints.push(complete.calls.partitions.length) },
  })
  assert.deepEqual(complete.calls.partitions, [[], [first, structural, second, upperSpan], [structural, second, upperSpan], [structural, upperSpan]])
  assert.equal(result.rampMouthsRemoved, 2)
  assert.deepEqual(result.boundaries.map(boundary => boundary.endpoints), [structural, upperSpan])
  assert.equal(complete.calls.freed, 4)
  assert(checkpoints.filter(count => count === 2).length >= 2, 'first removal is checkpointed before refresh')
  assert(checkpoints.includes(3), 'second removal is checkpointed before its bounded refresh')

  const cancelled = mock()
  let checkpointsAtInitialPartition = 0
  await assert.rejects(analyzeBaseAreas(cancelled.analysis, bases, 25, {
    checkpoint: async () => {
      if (cancelled.calls.partitions.length === 2 && ++checkpointsAtInitialPartition === 2) throw new Error('cancelled')
    },
  }), /cancelled/)
  assert.deepEqual(cancelled.calls.partitions, [[], [first, structural, second, upperSpan]])
})
