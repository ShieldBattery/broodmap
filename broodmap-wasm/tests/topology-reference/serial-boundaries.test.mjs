import assert from 'node:assert/strict'
import test from 'node:test'
import { collapseSerialBoundaries } from './serial-boundaries.js'

const WIDTH = 100
const HEIGHT = 70

function raster() {
  return new Uint32Array(WIDTH * HEIGHT).fill(1)
}

function fill(labels, id, left, top, right, bottom) {
  for (let y = top; y <= bottom; y++) {
    for (let x = left; x <= right; x++) labels[y * WIDTH + x] = id
  }
}

function span(y, widthPixels, sources = []) {
  return { endpoints: [[0, y], [80, y]], widthPixels, sources }
}

function chainMetadata(length, options = {}) {
  const regions = options.regions || [1, 59, 63, 66]
  return Array.from({ length }, (_, index) => ({
    removedEdgeCount: 10,
    separatedEdgeCount: 10,
    regionPairs: [[regions[index], regions[index + 1]]],
  }))
}

function chainPartition(boundaryMetadata, labels = raster()) {
  return { labels, areas: [], boundaries: boundaryMetadata }
}

function pythonLike() {
  const labels = raster()
  fill(labels, 59, 4, 7, 6, 15)
  fill(labels, 63, 4, 22, 6, 33)
  labels[50 * WIDTH + 70] = 66
  return {
    boundaries: [
      span(40, 544, [[2, 1], [0, 1]]),
      span(160, 368, [[2, 1], [1, 2]]),
      span(300, 361.984, [[0, 1], [3, 0]]),
    ],
    partition: chainPartition(chainMetadata(3), labels),
    bases: [{ routeAnchor: [0, 0] }, { routeAnchor: [70, 50] }],
  }
}

function endpointList(boundaries) {
  return boundaries.map(boundary => boundary.endpoints)
}

test('collapses the Python-like three-cut chain to its central near-minimum span', () => {
  const fixture = pythonLike()
  const result = collapseSerialBoundaries(fixture.boundaries, fixture.partition, fixture.bases, WIDTH)
  assert.deepEqual(endpointList(result), [[[0, 160], [80, 160]]])
  assert.equal(result[0].widthPixels, 368)
  assert.deepEqual(result[0].sources, [[0, 1], [1, 2], [2, 1], [3, 0]])
  assert.equal(result[0].observations.length, 3)
  assert.equal(fixture.boundaries[0].observations, undefined, 'inputs stay untouched')
})

test('an anchor in an intermediate component blocks collapse, while a missing anchor does not', () => {
  const anchored = pythonLike()
  anchored.bases.push({ routeAnchor: [5, 10] })
  assert.equal(collapseSerialBoundaries(anchored.boundaries, anchored.partition, anchored.bases, WIDTH).length, 3)

  const missing = pythonLike()
  missing.bases.push({ routeAnchor: null })
  assert.equal(collapseSerialBoundaries(missing.boundaries, missing.partition, missing.bases, WIDTH).length, 1)
})

test('a branch, partial bypass, or multi-pair boundary cannot form a serial chain', () => {
  const branch = pythonLike()
  branch.boundaries.push(span(100, 30))
  branch.partition.boundaries.push({
    removedEdgeCount: 10,
    separatedEdgeCount: 10,
    regionPairs: [[59, 67]],
  })
  assert.equal(collapseSerialBoundaries(branch.boundaries, branch.partition, branch.bases, WIDTH).length, 4)

  const bypass = pythonLike()
  bypass.partition.boundaries[1] = {
    ...bypass.partition.boundaries[1], removedEdgeCount: 11,
  }
  assert.equal(collapseSerialBoundaries(bypass.boundaries, bypass.partition, bypass.bases, WIDTH).length, 3)

  const multiplePairs = pythonLike()
  multiplePairs.partition.boundaries[1] = {
    ...multiplePairs.partition.boundaries[1], regionPairs: [[59, 63], [59, 68]],
  }
  assert.equal(collapseSerialBoundaries(multiplePairs.boundaries, multiplePairs.partition, multiplePairs.bases, WIDTH).length, 3)
})

test('transverse and longitudinal room bulges fail the raster geometry guards', () => {
  const transverseLabels = raster()
  fill(transverseLabels, 59, 0, 6, 10, 11)
  fill(transverseLabels, 63, 4, 13, 6, 18)
  const transverse = {
    boundaries: [span(40, 40), span(100, 40), span(160, 40)],
    partition: chainPartition(chainMetadata(3), transverseLabels),
  }
  assert.equal(collapseSerialBoundaries(transverse.boundaries, transverse.partition, [], WIDTH).length, 3)

  const longitudinalLabels = raster()
  fill(longitudinalLabels, 59, 4, 0, 6, 11)
  fill(longitudinalLabels, 63, 4, 13, 6, 18)
  const longitudinal = {
    boundaries: [span(40, 40), span(80, 40), span(120, 40)],
    partition: chainPartition(chainMetadata(3), longitudinalLabels),
  }
  assert.equal(collapseSerialBoundaries(longitudinal.boundaries, longitudinal.partition, [], WIDTH).length, 3)
})

test('long groups, loops, and two-node links remain unchanged', () => {
  const longLabels = raster()
  fill(longLabels, 59, 4, 12, 6, 14)
  fill(longLabels, 63, 4, 29, 6, 31)
  fill(longLabels, 64, 4, 47, 6, 49)
  const long = {
    boundaries: [span(40, 40), span(180, 40), span(320, 40), span(460, 40)],
    partition: chainPartition(chainMetadata(4, { regions: [1, 59, 63, 64, 66] }), longLabels),
  }
  assert.equal(collapseSerialBoundaries(long.boundaries, long.partition, [], WIDTH).length, 4)

  const circleLabels = raster()
  fill(circleLabels, 10, 3, 3, 4, 4)
  fill(circleLabels, 11, 6, 6, 7, 7)
  fill(circleLabels, 12, 9, 9, 10, 10)
  const circle = {
    boundaries: [span(40, 40), span(80, 40), span(120, 40)],
    partition: chainPartition([
      { removedEdgeCount: 1, separatedEdgeCount: 1, regionPairs: [[10, 12]] },
      { removedEdgeCount: 1, separatedEdgeCount: 1, regionPairs: [[10, 11]] },
      { removedEdgeCount: 1, separatedEdgeCount: 1, regionPairs: [[11, 12]] },
    ], circleLabels),
  }
  assert.equal(collapseSerialBoundaries(circle.boundaries, circle.partition, [], WIDTH).length, 3)

  const two = pythonLike()
  two.boundaries.pop()
  two.partition.boundaries.pop()
  assert.equal(collapseSerialBoundaries(two.boundaries, two.partition, two.bases, WIDTH).length, 2)
})

test('result is invariant to boundary input order when metadata is remapped with it', () => {
  const fixture = pythonLike()
  const original = collapseSerialBoundaries(fixture.boundaries, fixture.partition, fixture.bases, WIDTH)
  const order = [2, 0, 1]
  const reordered = collapseSerialBoundaries(
    order.map(index => fixture.boundaries[index]),
    { ...fixture.partition, boundaries: order.map(index => fixture.partition.boundaries[index]) },
    fixture.bases,
    WIDTH,
  )
  assert.deepEqual(reordered, original)
})
test('iterative bridge traversal handles deeply nested partition metadata without stack growth', () => {
  const count = 4096
  const boundaries = Array.from({ length: count }, (_, index) => ({
    endpoints: [[0, index * 8], [8, index * 8]], widthPixels: 16, sources: [],
  }))
  const partition = {
    labels: new Uint32Array(1),
    areas: [],
    boundaries: Array.from({ length: count }, (_, index) => ({
      removedEdgeCount: 1,
      separatedEdgeCount: 1,
      regionPairs: [[index + 1, index + 2]],
    })),
  }
  assert.equal(collapseSerialBoundaries(boundaries, partition, [], 1).length, count)
})

function flatFlags() {
  return new Uint8Array(WIDTH * HEIGHT).fill(1)
}

function flatPairFixture(options = {}) {
  const labels = raster()
  fill(labels, 2, 2, 14, 10, 19)
  if (options.nook) fill(labels, 2, 11, 16, 14, 17)
  if (options.bulge) fill(labels, 2, 11, 16, 25, 17)
  const boundaries = [
    { endpoints: [[0, 100], [80, 100]], widthPixels: 104, sources: [[2, 1]] },
    { endpoints: [[16, 156], [96, 156]], widthPixels: 96, sources: [[1, 4]] },
  ]
  const metadata = [
    { removedEdgeCount: 10, separatedEdgeCount: 10, regionPairs: [[1, 2]] },
    { removedEdgeCount: 10, separatedEdgeCount: 10, regionPairs: [[2, 3]] },
  ]
  if (options.partial) metadata[1].removedEdgeCount = 11
  if (options.branch) {
    boundaries.push({ endpoints: [[0, 220], [80, 220]], widthPixels: 96, sources: [[2, 5]] })
    metadata.push({ removedEdgeCount: 10, separatedEdgeCount: 10, regionPairs: [[2, 4]] })
  }
  return {
    boundaries,
    partition: chainPartition(metadata, labels),
    flags: flatFlags(),
    bases: options.base ? [{ routeAnchor: [4, 16] }] : [],
  }
}

function flatResult(fixture) {
  return collapseSerialBoundaries(
    fixture.boundaries, fixture.partition, fixture.bases, WIDTH, fixture.flags,
  )
}

test('flat nearby duplicate pair accepts sideways shift and bounded wall nooks', () => {
  const fixture = flatPairFixture({ nook: true })
  const result = flatResult(fixture)
  assert.equal(result.length, 1)
  assert.deepEqual(endpointList(result), [[[0, 100], [80, 100]]])
  assert.deepEqual(result[0].sources, [[1, 4], [2, 1]])
  assert.equal(result[0].observations.length, 2)
})

test('flat duplicate pass requires terrain flags, flat elevation, no ramp, no base, and full cuts', () => {
  const noFlags = flatPairFixture()
  assert.equal(collapseSerialBoundaries(noFlags.boundaries, noFlags.partition, [], WIDTH).length, 2)

  const ramp = flatPairFixture()
  ramp.flags[16 * WIDTH + 4] |= 4
  assert.equal(flatResult(ramp).length, 2)

  const elevations = flatPairFixture()
  elevations.flags[16 * WIDTH + 4] |= 8
  assert.equal(flatResult(elevations).length, 2)

  const anchored = flatPairFixture({ base: true })
  assert.equal(flatResult(anchored).length, 2)

  const partial = flatPairFixture({ partial: true })
  assert.equal(flatResult(partial).length, 2)
})

test('flat duplicate pass rejects a branch and a component escaping its cut-oriented slab', () => {
  const branch = flatPairFixture({ branch: true })
  assert.equal(flatResult(branch).length, 3)

  const bulge = flatPairFixture({ bulge: true })
  assert.equal(flatResult(bulge).length, 2)
})

test('flat duplicate pair can merge within a larger bypass cycle, but a full two-cut cycle cannot', () => {
  const bypass = flatPairFixture()
  bypass.boundaries.push(
    { endpoints: [[300, 300], [380, 300]], widthPixels: 96, sources: [] },
    { endpoints: [[300, 500], [380, 500]], widthPixels: 96, sources: [] },
  )
  bypass.partition.boundaries.push(
    { removedEdgeCount: 10, separatedEdgeCount: 10, regionPairs: [[1, 4]] },
    { removedEdgeCount: 10, separatedEdgeCount: 10, regionPairs: [[4, 3]] },
  )
  assert.equal(flatResult(bypass).length, 3, 'the two close cuts merge despite their global bypass')

  const cycle = flatPairFixture()
  cycle.partition.boundaries = [
    { removedEdgeCount: 10, separatedEdgeCount: 10, regionPairs: [[1, 2]] },
    { removedEdgeCount: 10, separatedEdgeCount: 10, regionPairs: [[1, 2]] },
  ]
  assert.equal(flatResult(cycle).length, 2, 'parallel graph edges form a full cycle, not a path')
})

test('flat complete-link proximity prevents transitive three-cut collapse and remains deterministic', () => {
  const fixture = flatPairFixture()
  fixture.boundaries.push(
    { endpoints: [[32, 236], [112, 236]], widthPixels: 96, sources: [[3, 5]] },
    { endpoints: [[300, 300], [380, 300]], widthPixels: 96, sources: [] },
    { endpoints: [[300, 500], [380, 500]], widthPixels: 96, sources: [] },
  )
  fixture.partition.boundaries.push(
    { removedEdgeCount: 10, separatedEdgeCount: 10, regionPairs: [[3, 4]] },
    { removedEdgeCount: 10, separatedEdgeCount: 10, regionPairs: [[1, 5]] },
    { removedEdgeCount: 10, separatedEdgeCount: 10, regionPairs: [[5, 4]] },
  )
  fill(fixture.partition.labels, 3, 2, 21, 10, 27)
  assert.equal(flatResult(fixture).length, 5)

  const original = flatResult(flatPairFixture({ nook: true }))
  const order = [1, 0]
  const reversed = flatPairFixture({ nook: true })
  const reordered = collapseSerialBoundaries(
    order.map(index => ({
      ...reversed.boundaries[index], endpoints: [...reversed.boundaries[index].endpoints].reverse(),
    })),
    { ...reversed.partition, boundaries: order.map(index => reversed.partition.boundaries[index]) },
    reversed.bases,
    WIDTH,
    reversed.flags,
  )
  assert.deepEqual(reordered, original)
})
