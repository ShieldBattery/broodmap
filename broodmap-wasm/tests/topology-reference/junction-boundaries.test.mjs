import assert from 'node:assert/strict'
import test from 'node:test'
import { removeJunctionBoundaries } from './junction-boundaries.js'

const WIDTH = 160
const HEIGHT = 160

function labels() {
  return new Uint32Array(WIDTH * HEIGHT)
}

function fill(grid, id, left, top, right, bottom) {
  for (let y = top; y <= bottom; y++) for (let x = left; x <= right; x++) grid[y * WIDTH + x] = id
}

function flags() {
  return new Uint8Array(WIDTH * HEIGHT).fill(1)
}

function boundary(endpoints, widthPixels, extra = {}) {
  return { endpoints, widthPixels, ...extra }
}

function full(pair, extra = {}) {
  return { removedEdgeCount: 10, separatedEdgeCount: 10, regionPairs: [pair], ...extra }
}

function fixture(options = {}) {
  const grid = labels()
  fill(grid, 1, 4, 4, 12, 12)
  fill(grid, 2, 13, 4, 21, 12)
  const boundaries = [
    boundary([[64, 64], [144, 64]], 400, { sources: [[7, 9]] }), // internal candidate 1-2
    boundary([[32, 32], [64, 96]], 280, { sources: [[7, 3]] }), // 1-3
    boundary([[32, 112], [96, 112]], 300, { sources: [[7, 5]] }), // 1-4
    boundary([[152, 32], [176, 112]], 304, { sources: [[9, 10]] }), // 2-5
  ]
  const metadata = [full([1, 2]), full([1, 3]), full([1, 4]), full([2, 5])]
  if (options.partial) metadata[0] = full([1, 2], { removedEdgeCount: 9 })
  if (options.multiPair) metadata[0] = { removedEdgeCount: 10, separatedEdgeCount: 10, regionPairs: [[1, 2], [1, 3]] }
  if (options.tooFew) metadata.pop(), boundaries.pop()
  if (options.repeatedExterior) metadata[2] = full([1, 3]), metadata[3] = full([2, 3])
  if (options.farPort) boundaries[3] = boundary([[900, 32], [900, 112]], 304)
  if (options.rampKind) boundaries[0].kind = 'ramp'
  if (options.large) fill(grid, 1, 130, 130, 132, 132)
  const terrainFlags = flags()
  if (options.ramp) terrainFlags[6 * WIDTH + 6] |= 4
  if (options.elevation) terrainFlags[6 * WIDTH + 6] |= 8
  if (options.matchingElevationContact) {
    terrainFlags[6 * WIDTH + 6] |= 8
    terrainFlags[6 * WIDTH + 16] |= 8
  }
  if (options.anchoredElevationMismatch) for (let y = 4; y <= 12; y++) for (let x = 4; x <= 12; x++) terrainFlags[y * WIDTH + x] |= 8
  return { boundaries, partition: { labels: grid, areas: [], boundaries: metadata }, terrainFlags }
}

function run(value, bases = []) {
  return removeJunctionBoundaries(value.boundaries, value.partition, bases, WIDTH, value.terrainFlags)
}

function endpointList(result) {
  return result.boundaries.map(boundary => boundary.endpoints)
}

test('removes an E17-like flat, unanchored internal junction edge while retaining all exterior ports', () => {
  const value = fixture()
  const result = run(value)
  assert.equal(result.removedCount, 1)
  assert.deepEqual(endpointList(result), [
    [[32, 32], [64, 96]],
    [[32, 112], [96, 112]],
    [[152, 32], [176, 112]],
  ])
  assert.deepEqual(result.boundaries[0].sources, [[7, 3]])
  assert.deepEqual(value.boundaries[0].sources, [[7, 9]], 'input sources remain raw and unmerged')
})

test('removes a wide flat natural split with exactly one anchored region', () => {
  const anchored = fixture()
  assert.equal(run(anchored, [{ routeAnchor: [6, 6] }]).removedCount, 1)
})

test('removes an anchored natural split with matching near-flat elevation contact', () => {
  const contact = fixture({ matchingElevationContact: true })
  fill(contact.partition.labels, 1, 4, 13, 12, 17)
  fill(contact.partition.labels, 2, 13, 13, 21, 17)
  assert.equal(run(contact, [{ routeAnchor: [6, 6] }]).removedCount, 1)
})

test('rejects two anchored regions, an anchored narrow neck, ramps, mixed elevation, and kind-ramp candidates', () => {
  const twoAnchors = fixture()
  assert.equal(run(twoAnchors, [{ routeAnchor: [6, 6] }, { routeAnchor: [16, 6] }]).removedCount, 0)
  const narrowAnchored = fixture()
  narrowAnchored.boundaries[0].widthPixels = 303
  assert.equal(run(narrowAnchored, [{ routeAnchor: [6, 6] }]).removedCount, 0)
  assert.equal(run(fixture({ anchoredElevationMismatch: true }), [{ routeAnchor: [6, 6] }]).removedCount, 0)
  assert.equal(run(fixture({ ramp: true })).removedCount, 0)
  assert.equal(run(fixture({ elevation: true })).removedCount, 0)
  assert.equal(run(fixture({ rampKind: true })).removedCount, 0)
})

test('rejects partial or multi-pair candidates, too few ports, repeated exteriors, distant ports, and large unions', () => {
  for (const options of [
    { partial: true }, { multiPair: true }, { tooFew: true }, { repeatedExterior: true },
    { farPort: true }, { large: true },
  ]) {
    assert.equal(run(fixture(options)).removedCount, 0, JSON.stringify(options))
  }
})

test('the simulated new-ramp top-left case remains protected', () => {
  const value = fixture({ ramp: true })
  assert.equal(run(value).boundaries.length, value.boundaries.length)
})

test('metadata ordering is deterministic under shuffled boundaries and reversed endpoints', () => {
  const value = fixture()
  const original = run(value)
  const order = [3, 1, 0, 2]
  const reordered = {
    boundaries: order.map(index => ({
      ...value.boundaries[index], endpoints: [...value.boundaries[index].endpoints].reverse(),
    })),
    partition: { ...value.partition, boundaries: order.map(index => value.partition.boundaries[index]) },
    terrainFlags: value.terrainFlags,
  }
  assert.deepEqual(run(reordered), original)
})

test('overlapping junction proposals are selected disjointly and reserve each others exterior ports', () => {
  const value = fixture()
  fill(value.partition.labels, 6, 22, 4, 30, 12)
  value.boundaries.push(
    boundary([[144, 64], [224, 64]], 300), // candidate 2-6, an exterior port of candidate 1-2
    boundary([[176, 120], [240, 120]], 260), // 2-7
    boundary([[232, 32], [256, 96]], 260), // 6-8
  )
  value.partition.boundaries.push(full([2, 6]), full([2, 7]), full([6, 8]))
  const result = run(value)
  assert.equal(result.removedCount, 1)
  assert(!endpointList(result).some(endpoints => JSON.stringify(endpoints) === JSON.stringify([[64, 64], [144, 64]])))
  assert(endpointList(result).some(endpoints => JSON.stringify(endpoints) === JSON.stringify([[144, 64], [224, 64]])))
})

function bridgeFixture(options = {}) {
  const grid = labels()
  fill(grid, 1, 8, 8, 16, 16) // shared central area
  fill(grid, 2, 50, 50, 62, 62) // local flat pocket
  fill(grid, 3, 70, 54, 76, 60) // distinct base-facing area
  const boundaries = [
    boundary([[400, 400], [400, 560]], 350, { tag: 'wide' }),
    boundary([[448, 400], [448, 480]], options.narrowWidth ?? 90, { tag: 'narrow' }),
    boundary([[480, 440], [560, 440]], 280, { tag: 'base-port' }),
  ]
  const metadata = [full([2, 1]), full([2, 1]), full([2, 3])]
  if (options.extraPort || options.partialPort) {
    fill(grid, 4, 64, 64, 66, 66)
    boundaries.push(boundary([[496, 496], [528, 496]], 80, { tag: 'extra' }))
    metadata.push(full([2, 4], options.partialPort ? { removedEdgeCount: 9 } : {}))
  }
  if (options.nonOrdinary) boundaries[1].kind = 'serial'
  if (options.far) fill(grid, 2, 130, 130, 132, 132)
  const terrainFlags = flags()
  if (options.ramp) terrainFlags[55 * WIDTH + 55] |= 4
  if (options.elevation) terrainFlags[55 * WIDTH + 55] |= 8
  const bases = [{ routeAnchor: [10, 10] }]
  if (!options.thirdUnanchored) bases.push({ routeAnchor: [72, 56] })
  if (options.pocketAnchored) bases.push({ routeAnchor: [55, 55] })
  return { boundaries, partition: { labels: grid, areas: [], boundaries: metadata }, terrainFlags, bases }
}

function bridgeRun(value) {
  return removeJunctionBoundaries(value.boundaries, value.partition, value.bases, WIDTH, value.terrainFlags)
}

test('removes only the broad local bridge divider while retaining the narrow bridge and anchored base port', () => {
  const value = bridgeFixture()
  const result = bridgeRun(value)
  assert.equal(result.removedCount, 1)
  assert.deepEqual(result.boundaries.map(item => item.tag), ['narrow', 'base-port'])
})

test('bridge-pocket rule rejects protected, nonlocal, nonordinary, and nonredundant shapes', () => {
  for (const options of [
    { pocketAnchored: true }, { extraPort: true }, { partialPort: true }, { narrowWidth: 180 },
    { elevation: true }, { ramp: true }, { far: true }, { thirdUnanchored: true }, { nonOrdinary: true },
  ]) {
    assert.equal(bridgeRun(bridgeFixture(options)).removedCount, 0, JSON.stringify(options))
  }
})

test('bridge pockets may share a central exterior but cannot absorb another proposal pocket', () => {
  const shared = bridgeFixture()
  fill(shared.partition.labels, 4, 90, 50, 102, 62)
  fill(shared.partition.labels, 5, 108, 54, 114, 60)
  shared.bases.push({ routeAnchor: [110, 56] })
  shared.boundaries.push(
    boundary([[720, 400], [720, 560]], 340, { tag: 'wide-shared' }),
    boundary([[768, 400], [768, 480]], 90, { tag: 'narrow-shared' }),
    boundary([[800, 440], [880, 440]], 280, { tag: 'base-port-shared' }),
  )
  shared.partition.boundaries.push(full([4, 1]), full([4, 1]), full([4, 5]))
  const independent = bridgeRun(shared)
  assert.equal(independent.removedCount, 2)
  assert.deepEqual(independent.boundaries.map(item => item.tag), [
    'narrow', 'base-port', 'narrow-shared', 'base-port-shared',
  ])

  const conflict = bridgeFixture()
  fill(conflict.partition.labels, 4, 90, 50, 102, 62)
  fill(conflict.partition.labels, 5, 108, 54, 114, 60)
  conflict.bases.push({ routeAnchor: [110, 56] })
  conflict.boundaries.push(
    boundary([[720, 400], [720, 560]], 340, { tag: 'wide-conflict' }),
    boundary([[768, 400], [768, 480]], 90, { tag: 'narrow-conflict' }),
    boundary([[800, 440], [880, 440]], 280, { tag: 'base-port-conflict' }),
  )
  // The second proposal would merge its pocket into the first pocket rather than shared central area.
  conflict.partition.boundaries.push(full([4, 2]), full([4, 2]), full([4, 5]))
  const result = bridgeRun(conflict)
  assert.equal(result.removedCount, 1)
})


test('anchored locality follows a finite span, with bounded perpendicular and endpoint distance', () => {
  const along = fixture()
  along.boundaries[0] = boundary([[64, 64], [64, 864]], 800)
  along.boundaries[1] = boundary([[32, 320], [96, 320]], 200)
  along.boundaries[2] = boundary([[32, 480], [96, 480]], 200)
  along.boundaries[3] = boundary([[32, 600], [96, 600]], 200)
  fill(along.partition.labels, 1, 70, 100, 70, 100)
  const bases = [{ routeAnchor: [6, 6] }]
  assert.equal(run(along, bases).removedCount, 1, 'far from midpoint but within 512px of segment')
  assert.equal(run(along).removedCount, 0, 'unanchored path retains midpoint bound')

  const outside = structuredClone(along)
  fill(outside.partition.labels, 1, 72, 100, 72, 100)
  assert.equal(run(outside, bases).removedCount, 0, 'bbox exceeds perpendicular radius')

  const beyondEnd = fixture()
  beyondEnd.boundaries[0] = boundary([[64, 64], [64, 464]], 400)
  fill(beyondEnd.partition.labels, 1, 8, 125, 8, 125)
  assert.equal(run(beyondEnd, bases).removedCount, 0, 'projection is clamped at the endpoint')

  const diagonal = fixture()
  diagonal.boundaries[0] = boundary([[64, 64], [864, 864]], Math.hypot(800, 800))
  diagonal.boundaries[1] = boundary([[320, 320], [400, 320]], 200)
  diagonal.boundaries[2] = boundary([[400, 480], [480, 480]], 200)
  diagonal.boundaries[3] = boundary([[600, 600], [680, 600]], 200)
  assert.equal(run(diagonal, bases).removedCount, 1)
  fill(diagonal.partition.labels, 1, 150, 4, 150, 4)
  assert.equal(run(diagonal, bases).removedCount, 0)
})
