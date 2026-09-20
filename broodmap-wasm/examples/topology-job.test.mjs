import assert from 'node:assert/strict'
import test from 'node:test'
import { analyzeBaseAreas } from './base-areas.js'

function fixture({ failStep = false, failMetadata = false } = {}) {
  const calls = { begun: 0, freed: 0, steps: 0, metadata: 0 }
  const analysis = {
    widthWalkTiles: 4,
    cellFlags: () => new Uint8Array(16).fill(3),
    beginBaseTopology(json, widening) {
      calls.begun++
      calls.anchors = JSON.parse(json)
      calls.widening = widening
      return {
        advance() {
          calls.steps++
          if (failStep) throw new Error('step failed')
          return calls.steps === 3
        },
        progressJson: () => JSON.stringify({ completedOrigins: Math.min(calls.steps, 2), totalOrigins: 2, surveyCount: 2 }),
        metadataJson() {
          calls.metadata++
          assert.equal(calls.steps, 3)
          if (failMetadata) throw new Error('metadata failed')
          return JSON.stringify({ boundaries: [{ endpoints: [[8, 0], [8, 32]] }], baseAreas: [] })
        },
        labels: () => new Uint32Array(16).fill(1),
        free() { calls.freed++ },
      }
    },
  }
  return { analysis, calls }
}
const bases = [{ id: 1, routeAnchor: [1, 1], resources: ['not sent'] }, { id: 2, routeAnchor: [2, 2] }]

test('thin wrapper delegates topology, reports origin progress, and frees the completed job', async () => {
  const { analysis, calls } = fixture()
  const progress = []
  const result = await analyzeBaseAreas(analysis, bases, 25, { onProgress: p => progress.push(p) })
  assert.deepEqual(calls.anchors, bases.map(({ id, routeAnchor }) => ({ id, routeAnchor })))
  assert.equal(calls.widening, 25)
  assert.equal(calls.freed, 1)
  assert.deepEqual(progress.map(p => p.completedOrigins), [1, 2])
  assert.equal(result.labels.byteLength, 64)
  assert.equal(result.boundaries[0].buildableNearMidpoint, true)
})

test('cancellation before, during, and immediately after completion never returns partial metadata', async () => {
  for (const stop of [1, 2, 3, 5]) {
    const { analysis, calls } = fixture()
    let checkpoints = 0
    await assert.rejects(analyzeBaseAreas(analysis, bases, 25, {
      checkpoint: async () => { if (++checkpoints === stop) throw new Error('cancelled') },
    }), /cancelled/)
    assert.equal(calls.metadata, 0)
    assert.equal(calls.freed, stop === 1 ? 0 : 1)
  }
})

test('step, serialization, and progress callback errors release the owned job', async () => {
  for (const options of [{ failStep: true }, { failMetadata: true }, { callback: true }]) {
    const { analysis, calls } = fixture(options)
    await assert.rejects(analyzeBaseAreas(analysis, bases, 25, {
      onProgress: () => { if (options.callback) throw new Error('callback failed') },
    }), /failed/)
    assert.equal(calls.freed, 1)
  }
})
