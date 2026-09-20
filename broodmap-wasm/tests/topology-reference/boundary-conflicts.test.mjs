import assert from 'node:assert/strict'
import test from 'node:test'
import { selectNonCrossingBoundaries } from './boundary-conflicts.js'

function boundary(endpoints, widthPixels, extra = {}) {
  return { endpoints, widthPixels, ...extra }
}

function endpoints(result) {
  return result.boundaries.map(boundary => boundary.endpoints)
}

test('drops the Luna broad span crossing two narrower spans without merging its metadata', () => {
  const narrowVertical = boundary([[828, 928], [828, 1280]], 352, { sources: [[0, 2]] })
  const narrowDiagonal = boundary([[928, 832], [1192, 1096]], 373.296, { sources: [[2, 0]] })
  const broad = boundary([[608, 1036], [1176, 1036]], 568, { sources: [[0, 4]], note: 'raw' })
  const result = selectNonCrossingBoundaries([broad, narrowDiagonal, narrowVertical])
  assert.equal(result.removedCount, 1)
  assert.deepEqual(endpoints(result), [narrowVertical.endpoints, narrowDiagonal.endpoints])
  assert.deepEqual(result.boundaries[0].sources, [[0, 2]])
  assert.equal(result.boundaries[0].note, undefined)
})

test('endpoint touches, collinear overlap, and non-crossing spans all remain', () => {
  const touching = selectNonCrossingBoundaries([
    boundary([[0, 0], [8, 8]], 20),
    boundary([[8, 8], [16, 0]], 30),
  ])
  assert.equal(touching.removedCount, 0)

  const collinear = selectNonCrossingBoundaries([
    boundary([[0, 0], [16, 0]], 20),
    boundary([[8, 0], [24, 0]], 30),
  ])
  assert.equal(collinear.removedCount, 0)

  const separate = selectNonCrossingBoundaries([
    boundary([[0, 0], [0, 16]], 20),
    boundary([[8, 0], [8, 16]], 30),
  ])
  assert.equal(separate.removedCount, 0)
})

test('equal-width ties are canonical under shuffled and reversed endpoint input', () => {
  const first = boundary([[0, 16], [16, 0]], 100, { tag: 'first' })
  const second = boundary([[0, 0], [16, 16]], 100, { tag: 'second' })
  const expected = selectNonCrossingBoundaries([first, second])
  const shuffled = selectNonCrossingBoundaries([
    boundary([[16, 16], [0, 0]], 100, { tag: 'second' }),
    boundary([[16, 0], [0, 16]], 100, { tag: 'first' }),
  ])
  assert.deepEqual(shuffled, expected)
  assert.deepEqual(endpoints(expected), [[[0, 0], [16, 16]]])
})

test('does not mutate input boundaries or their endpoint direction', () => {
  const input = [
    boundary([[16, 0], [0, 16]], 20, { sources: [[1, 2]] }),
    boundary([[0, 0], [16, 16]], 30),
  ]
  const before = structuredClone(input)
  const result = selectNonCrossingBoundaries(input)
  assert.deepEqual(input, before)
  assert.notEqual(result.boundaries[0], input[0])
  assert.deepEqual(result.boundaries[0].endpoints, [[0, 16], [16, 0]])
})


test('coincident ramp and widening spans keep only structural evidence regardless of input order', () => {
  const ramp = boundary([[888, 3520], [1064, 3344]], 248.864,
    { kind: 'ramp', rampId: 4, rampEnd: 'lower' })
  const generic = boundary([[1064, 3344], [888, 3520]], 248.864,
    { sources: [[8, 11]], observations: ['widening'] })
  for (const input of [[ramp, generic], [generic, ramp]]) {
    const result = selectNonCrossingBoundaries(input)
    assert.equal(result.removedCount, 1)
    assert.deepEqual(result.boundaries, [ramp])
  }
})
