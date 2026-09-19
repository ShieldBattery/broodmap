import assert from 'node:assert/strict'
import test from 'node:test'
import { inspectBaseAreas } from './base-areas.js'

const bases = [
  { id: 0, routeAnchor: [0, 0] },
  { id: 1, routeAnchor: [1, 0] },
  { id: 2, routeAnchor: [2, 0] },
  { id: 3, routeAnchor: [3, 0] },
  { id: 4, routeAnchor: null },
]

function fixture() {
  const calls = { batches: [], partitions: [], freed: 0 }
  const analysis = {
    widthWalkTiles: 4,
    entrancesJson() { throw new Error('The area experiment must not issue individual surveys') },
    entrancesBatchJson(json, widening) {
      const queries = JSON.parse(json)
      calls.batches.push({ queries, widening })
      return JSON.stringify(queries.map(([start, end]) => ({
        terrainOnly: true,
        route: { points: [start, end], groundDistancePixels: Math.abs(end[0] - start[0]) * 8 },
        candidates: [{ endpoints: start[0] < end[0] ? [[8, 0], [8, 8]] : [[8, 8], [8, 0]], widthPixels: 8 }],
      })))
    },
    partitionAreas(json) {
      const spans = JSON.parse(json)
      calls.partitions.push(spans)
      return {
        labels: () => new Uint32Array([1, 1, 1, 1]),
        metadataJson: () => JSON.stringify({
          terrainOnly: true, areas: [{ id: 1, cellCount: 4 }],
          boundaries: spans.map(() => ({ removedEdgeCount: 1, separatedEdgeCount: 0, regionPairs: [] })),
        }),
        free: () => { calls.freed++ },
      }
    },
  }
  return { analysis, calls }
}

test('submits one complete batch, preserves selected directions, and consolidates exact spans', () => {
  const { analysis, calls } = fixture()
  const result = inspectBaseAreas(analysis, bases, [0, 1], 25)
  assert.equal(calls.batches.length, 1)
  const { queries, widening } = calls.batches[0]
  assert.equal(widening, 25)
  assert.equal(queries.length, 10)
  assert.equal(new Set(queries.map(query => JSON.stringify(query))).size, 10)
  assert.deepEqual(result.surveys.map(s => [s.from, ...s.route.points]), [
    ['A', [0, 0], [1, 0]], ['B', [1, 0], [0, 0]],
  ])
  assert.deepEqual(calls.partitions, [[], [[[8, 0], [8, 8]]]])
  assert.equal(calls.freed, 2)
  assert.equal(result.areaExperiment.skippedAnchorCount, 1)
  assert.equal(result.areaExperiment.boundaries[0].sources.length, 10)
  assert.deepEqual(result.areaExperiment.selected.map(a => a.baseIds), [[0, 1, 2, 3], [0, 1, 2, 3]])
  assert.deepEqual(new Uint32Array(result.areaExperiment.labels), new Uint32Array([1, 1, 1, 1]))
})

test('reversing the selection preserves the partition while swapping selected survey results', () => {
  const forward = inspectBaseAreas(fixture().analysis, bases, [0, 1], 15)
  const reverse = inspectBaseAreas(fixture().analysis, bases, [1, 0], 15)
  assert.deepEqual(reverse.areaExperiment.boundaries, forward.areaExperiment.boundaries)
  assert.deepEqual(reverse.areaExperiment.labels, forward.areaExperiment.labels)
  assert.deepEqual(reverse.areaExperiment.selected, forward.areaExperiment.selected.slice().reverse())
  assert.deepEqual(reverse.surveys[0].route, forward.surveys[1].route)
})

test('a failed batch cannot produce a partial partition', () => {
  const { analysis, calls } = fixture()
  analysis.entrancesBatchJson = () => { throw new Error('batch failed') }
  assert.throws(() => inspectBaseAreas(analysis, bases, [0, 1], 25), /batch failed/)
  assert.deepEqual(calls.partitions, [])
})
