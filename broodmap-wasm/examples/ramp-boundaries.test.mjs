import assert from 'node:assert/strict'
import test from 'node:test'
import { collapseRampInteriors, replaceRampMouths } from './ramp-boundaries.js'

const FULL = (regionPairs) => ({
  removedEdgeCount: 8,
  separatedEdgeCount: 8,
  regionPairs,
})

function cut(endpoints, widthPixels, extra = {}) {
  return { endpoints, widthPixels, ...extra }
}

function partition(width, height, cells, metadata) {
  const labels = new Uint32Array(width * height).fill(99)
  for (const [region, positions] of Object.entries(cells)) {
    for (const [x, y] of positions) labels[y * width + x] = Number(region)
  }
  return { labels, boundaries: metadata }
}

function baseScenario({ mouth = [[40, 112], [216, 288]], ramp = [[80, 72], [136, 128]],
  mouthWidth = 352, rampWidth = 96, sharedCells = [[11, 12], [12, 12]], extra = [], gridWidth = 32, gridHeight = 32 } = {}) {
  const boundaries = [
    cut(mouth, mouthWidth, { tag: 'mouth', sources: [[4, 0]] }),
    cut(ramp, rampWidth, { kind: 'ramp', rampId: 7, rampEnd: 'lower', tag: 'ramp' }),
    cut([[208, 96], [208, 160]], 80, { tag: 'other-natural-port' }),
    ...extra,
  ]
  const metadata = [FULL([[2, 3]]), FULL([[1, 2]]), FULL([[2, 4]]), ...extra.map(() => FULL([[9, 10]]))]
  return {
    boundaries,
    partition: partition(gridWidth, gridHeight, {
      1: [[8, 8]],
      2: sharedCells,
      3: [[0, 0]],
      4: [[26, 16]],
    }, metadata),
    bases: [{ routeAnchor: [0, 0] }],
    width: gridWidth,
  }
}

function run(scenario) {
  return replaceRampMouths(scenario.boundaries, scenario.partition, scenario.bases, scenario.width)
}

function tags(result) { return result.boundaries.map(b => b.tag) }

function reverse(boundary) {
  return { ...boundary, endpoints: [boundary.endpoints[1], boundary.endpoints[0]] }
}

test('replaces an E1-shaped bent widening with its nearby ramp end and keeps another natural port', () => {
  const scenario = baseScenario()
  const before = structuredClone(scenario.boundaries)
  const result = run(scenario)
  assert.equal(result.removedCount, 1)
  assert.deepEqual(tags(result), ['ramp', 'other-natural-port'])
  assert.equal(result.boundaries.find(b => b.tag === 'ramp').rampId, 7)
  assert.deepEqual(result.boundaries.find(b => b.tag === 'other-natural-port').sources, undefined)
  assert.deepEqual(scenario.boundaries, before)
})

test('uses topology and geometry only: a flat bounded shared approach is eligible', () => {
  const result = run(baseScenario({
    sharedCells: [[7, 8], [8, 8], [8, 9]],
  }))
  assert.equal(result.removedCount, 1)
  assert.ok(!tags(result).includes('mouth'))
})

test('rejects anchored or multipart approaches, inadequate width, distant mouths, and an unbounded approach', () => {
  const anchored = baseScenario()
  anchored.bases.push({ routeAnchor: [11, 12] })
  assert.equal(run(anchored).removedCount, 0)

  const multipart = baseScenario()
  multipart.partition.boundaries[0] = FULL([[2, 3], [2, 5]])
  assert.equal(run(multipart).removedCount, 0)

  assert.equal(run(baseScenario({ mouthWidth: 191 })).removedCount, 0)
  assert.equal(run(baseScenario({ mouth: [[600, 112], [776, 288]] })).removedCount, 0)
  assert.equal(run(baseScenario({ sharedCells: [[11, 12], [12, 12], [0, 79]], gridHeight: 80 })).removedCount, 0)
})

test('does not replace a span between the two ends of one ramp or another structural cut', () => {
  const interior = baseScenario({
    extra: [cut([[96, 160], [152, 216]], 96, {
      kind: 'ramp', rampId: 7, rampEnd: 'upper', tag: 'other-ramp-end',
    })],
  })
  interior.partition.boundaries[3] = FULL([[2, 5]])
  assert.equal(run(interior).removedCount, 0)

  const structural = baseScenario()
  structural.boundaries[0].kind = 'serial'
  assert.equal(run(structural).removedCount, 0)
})

test('chooses one closest mouth per ramp deterministically under shuffled and reversed input', () => {
  const scenario = baseScenario({
    extra: [cut([[240, 300], [392, 452]], 320, { tag: 'farther-mouth', sources: [[8, 2]] })],
  })
  scenario.partition.boundaries[3] = FULL([[2, 6]])
  scenario.partition.labels[20 * scenario.width + 22] = 6
  const expected = run(scenario)
  assert.equal(expected.removedCount, 1)
  assert.ok(tags(expected).includes('farther-mouth'))
  assert.deepEqual(expected.boundaries.find(b => b.tag === 'farther-mouth').sources, [[8, 2]])

  const shuffled = {
    ...scenario,
    boundaries: scenario.boundaries.map(reverse).reverse(),
    partition: { ...scenario.partition, boundaries: [...scenario.partition.boundaries].reverse() },
  }
  assert.deepEqual(run(shuffled), expected)
})

test('simultaneous disjoint replacements cannot join two formerly separate base groups', () => {
  const boundaries = [
    cut([[48, 112], [200, 264]], 320, { tag: 'mouth-a' }),
    cut([[80, 72], [136, 128]], 96, { kind: 'ramp', rampId: 1, rampEnd: 'lower', tag: 'ramp-a' }),
    cut([[304, 112], [456, 264]], 320, { tag: 'mouth-b' }),
    cut([[336, 72], [392, 128]], 96, { kind: 'ramp', rampId: 2, rampEnd: 'lower', tag: 'ramp-b' }),
  ]
  const p = partition(64, 32, {
    2: [[11, 12]], 3: [[0, 0]], 1: [[8, 8]],
    5: [[43, 12]], 6: [[60, 0]], 4: [[40, 8]],
  }, [FULL([[2, 3]]), FULL([[1, 2]]), FULL([[5, 6]]), FULL([[4, 5]])])
  const result = replaceRampMouths(boundaries, p, [{ routeAnchor: [0, 0] }, { routeAnchor: [60, 0] }], 64)
  assert.equal(result.removedCount, 2)
  assert.deepEqual(tags(result), ['ramp-a', 'ramp-b'])

  // The selected mouths become traversable; retained ramp cuts remain blocked.
  const graph = new Map([[3, new Set([2])], [2, new Set([3])], [6, new Set([5])], [5, new Set([6])]])
  const reachable = (start) => {
    const seen = new Set([start]), todo = [start]
    while (todo.length) for (const next of graph.get(todo.pop()) || []) if (!seen.has(next)) seen.add(next), todo.push(next)
    return seen
  }
  assert.ok(!reachable(3).has(6))
})


test('uses one replacement for duplicate observations of the same logical ramp end', () => {
  const boundaries = [
    cut([[48, 112], [200, 264]], 320, { tag: 'mouth-a' }),
    cut([[80, 72], [136, 128]], 96, { kind: 'ramp', rampId: 9, rampEnd: 'lower', tag: 'ramp-a' }),
    cut([[304, 112], [456, 264]], 320, { tag: 'mouth-b' }),
    cut([[336, 72], [392, 128]], 96, { kind: 'ramp', rampId: 9, rampEnd: 'lower', tag: 'ramp-b' }),
  ]
  const p = partition(64, 32, {
    2: [[11, 12]], 3: [[0, 0]], 1: [[8, 8]],
    5: [[43, 12]], 6: [[60, 0]], 4: [[40, 8]],
  }, [FULL([[2, 3]]), FULL([[1, 2]]), FULL([[5, 6]]), FULL([[4, 5]])])
  const result = replaceRampMouths(boundaries, p, [], 64)
  assert.equal(result.removedCount, 1)
  assert.ok(tags(result).includes('mouth-b'))
})


test('uses exact finite segment distance for crossings, diagonal gaps, and degenerate spans', async () => {
  const { segmentDistance } = await import('./ramp-boundaries.js')
  assert.equal(segmentDistance([[0, 0], [8, 8]], [[0, 8], [8, 0]]), 0)
  assert.equal(segmentDistance([[0, 0], [0, 0]], [[3, 4], [6, 4]]), 5)
  assert.equal(segmentDistance([[0, 0], [8, 8]], [[0, 16], [8, 24]]), Math.sqrt(128))
})

function multipartScenario() {
  const boundaries = [
    cut([[40, 112], [216, 288]], 352, { tag: 'multipart-mouth', sources: [[8, 11]] }),
    cut([[80, 72], [136, 128]], 96, { kind: 'ramp', rampId: 7, rampEnd: 'lower', tag: 'ramp' }),
  ]
  const p = partition(32, 32, {
    1: [[8, 8]], 2: [[11, 12]], 3: [[0, 0]], 4: [[12, 13]],
  }, [FULL([[2, 3], [3, 4]]), FULL([[1, 2]])])
  return { boundaries, partition: p, bases: [{ routeAnchor: [0, 0] }], width: 32 }
}

test('absorbs a bounded multipart leaf star while retaining the protected ramp end', () => {
  const scenario = multipartScenario()
  const result = run(scenario)
  assert.equal(result.removedCount, 1)
  assert.deepEqual(tags(result), ['ramp'])
})

test('multipart extension rejects a protected leaf and any additional leaf port', () => {
  const protectedLeaf = multipartScenario()
  protectedLeaf.bases.push({ routeAnchor: [12, 13] })
  assert.equal(run(protectedLeaf).removedCount, 0)

  const otherPort = multipartScenario()
  otherPort.boundaries.push(cut([[184, 104], [184, 160]], 80, { tag: 'leaf-port' }))
  otherPort.partition.boundaries.push(FULL([[4, 9]]))
  otherPort.partition.labels[14 * otherPort.width + 23] = 9
  assert.equal(run(otherPort).removedCount, 0)
})

test('reserves an outer-to-inner proposal conflict during simultaneous selection', () => {
  const boundaries = [
    cut([[48, 112], [200, 264]], 320, { tag: 'mouth-a' }),
    cut([[80, 72], [136, 128]], 96, { kind: 'ramp', rampId: 1, rampEnd: 'lower', tag: 'ramp-a' }),
    cut([[304, 112], [456, 264]], 320, { tag: 'mouth-b' }),
    cut([[336, 72], [392, 128]], 96, { kind: 'ramp', rampId: 2, rampEnd: 'lower', tag: 'ramp-b' }),
  ]
  const p = partition(64, 32, {
    1: [[8, 8]], 2: [[11, 12]], 3: [[40, 12]], 4: [[60, 0]], 5: [[40, 8]],
  }, [FULL([[2, 3]]), FULL([[1, 2]]), FULL([[3, 4]]), FULL([[5, 3]])])
  const result = replaceRampMouths(boundaries, p, [], 64)
  assert.equal(result.removedCount, 1)
  assert.ok(tags(result).includes('mouth-b'))
})


test('uses a near-identical parallel ramp representative when the widening is not twice its width', () => {
  const result = run(baseScenario({
    mouth: [[40, 112], [200, 272]],
    ramp: [[48, 120], [208, 280]],
    mouthWidth: 200,
    rampWidth: 240,
  }))
  assert.equal(result.removedCount, 1)
  assert.ok(!tags(result).includes('mouth'))
})

test('near-identical representative exception requires both endpoint proximity and parallel direction', () => {
  const angled = run(baseScenario({
    mouth: [[80, 100], [160, 100]],
    ramp: [[85, 80], [155, 120]],
    mouthWidth: 200,
    rampWidth: 240,
  }))
  assert.equal(angled.removedCount, 0)

  const displaced = run(baseScenario({
    mouth: [[40, 112], [200, 272]],
    ramp: [[88, 160], [248, 320]],
    mouthWidth: 200,
    rampWidth: 240,
  }))
  assert.equal(displaced.removedCount, 0)
})


function overlayScenario({ generic = [[120, 40], [280, 200]], genericWidth = 226,
  genericPairs = [[2, 3]], bases = [{ routeAnchor: [0, 0] }] } = {}) {
  const boundaries = [
    cut(generic, genericWidth, { tag: 'generic' }),
    cut([[80, 80], [240, 240]], 226, { kind: 'ramp', rampId: 17, rampEnd: 'lower', tag: 'ramp' }),
  ]
  const p = partition(64, 64, {
    1: [[0, 0]], 2: [[8, 8]], 3: [[9, 8]], 4: [[10, 8]], 5: [[11, 8]],
  }, [FULL(genericPairs), FULL([[3, 4]])])
  return { boundaries, partition: p, bases, width: 64 }
}

test('removes a local two-port parallel overlay while retaining its structural ramp end', () => {
  const result = run(overlayScenario())
  assert.equal(result.removedCount, 1)
  assert.deepEqual(tags(result), ['ramp'])
})

test('parallel overlay removal rejects a narrow neck, poor overlap, offset, anchors, multipart branches, and exits', () => {
  assert.equal(run(overlayScenario({ genericWidth: 80 })).removedCount, 0)
  assert.equal(run(overlayScenario({ generic: [[128, 128], [288, 288]] })).removedCount, 0)
  assert.equal(run(overlayScenario({ generic: [[128, 32], [288, 192]] })).removedCount, 0)
  assert.equal(run(overlayScenario({ bases: [{ routeAnchor: [8, 8] }] })).removedCount, 0)
  assert.equal(run(overlayScenario({ bases: [{ routeAnchor: [9, 8] }] })).removedCount, 0)
  assert.equal(run(overlayScenario({ genericPairs: [[2, 3], [3, 5]] })).removedCount, 0)

  const exited = overlayScenario()
  exited.boundaries.push(cut([[240, 200], [280, 200]], 64, { tag: 'extra-exit' }))
  exited.partition.boundaries.push(FULL([[3, 5]]))
  assert.equal(run(exited).removedCount, 0)
})

function triangleScenario() {
  const boundaries = [
    cut([[40, 112], [200, 272]], 200, { tag: 'generic' }),
    cut([[48, 120], [208, 280]], 240, { kind: 'ramp', rampId: 7, rampEnd: 'upper', tag: 'ramp' }),
  ]
  const p = partition(32, 32, {
    1: [[0, 0]], 2: [[11, 12]], 3: [[12, 13]],
  }, [FULL([[1, 3], [2, 3]]), FULL([[1, 2], [1, 3]])])
  return { boundaries, partition: p, bases: [{ routeAnchor: [0, 0] }], width: 32 }
}

test('replaces only a bounded near-identical triangular generic/ramp pair', () => {
  const result = run(triangleScenario())
  assert.equal(result.removedCount, 1)
  assert.deepEqual(tags(result), ['ramp'])
})

test('near-triangle path rejects a second anchored region or a non-triangular pair graph', () => {
  const anchored = triangleScenario()
  anchored.bases.push({ routeAnchor: [11, 12] })
  assert.equal(run(anchored).removedCount, 0)

  const nonTriangle = triangleScenario()
  nonTriangle.partition.boundaries[0] = FULL([[1, 3], [2, 4]])
  nonTriangle.partition.labels[14 * nonTriangle.width + 23] = 4
  assert.equal(run(nonTriangle).removedCount, 0)
})

test('near-triangle selection reserves its complete possible union', () => {
  const boundaries = [
    cut([[40, 112], [200, 272]], 200, { tag: 'generic-a' }),
    cut([[48, 120], [208, 280]], 240, { kind: 'ramp', rampId: 7, rampEnd: 'upper', tag: 'ramp-a' }),
    cut([[296, 112], [456, 272]], 200, { tag: 'generic-b' }),
    cut([[304, 120], [464, 280]], 240, { kind: 'ramp', rampId: 8, rampEnd: 'upper', tag: 'ramp-b' }),
  ]
  const p = partition(64, 32, {
    1: [[0, 0]], 2: [[11, 12]], 3: [[12, 13]], 4: [[40, 12]], 5: [[41, 13]],
  }, [
    FULL([[1, 3], [2, 3]]), FULL([[1, 2], [1, 3]]),
    FULL([[1, 5], [4, 5]]), FULL([[1, 4], [1, 5]]),
  ])
  const result = replaceRampMouths(boundaries, p, [{ routeAnchor: [0, 0] }], 64)
  assert.equal(result.removedCount, 1)
  assert.ok(tags(result).includes('generic-b'))
})


function twoCenterScenario(shared = [[40, 100]]) {
  const boundaries = [
    cut([[320, 0], [320, 800]], 600, { tag: 'broad-mouth' }),
    cut([[80, 72], [136, 128]], 96, { kind: 'ramp', rampId: 12, rampEnd: 'upper', tag: 'ramp' }),
  ]
  const p = partition(128, 128, {
    1: [[8, 8]], 2: shared, 3: [[0, 0]],
  }, [FULL([[2, 3]]), FULL([[1, 2]])])
  return { boundaries, partition: p, bases: [{ routeAnchor: [0, 0] }], width: 128 }
}

test('allows a bounded two-port broad approach when each corner is near the mouth or ramp center', () => {
  const scenario = twoCenterScenario()
  const result = run(scenario)
  assert.equal(result.removedCount, 1)
  assert.deepEqual(tags(result), ['ramp'])

  const shuffled = {
    ...scenario,
    boundaries: scenario.boundaries.map(reverse).reverse(),
    partition: { ...scenario.partition, boundaries: [...scenario.partition.boundaries].reverse() },
  }
  assert.deepEqual(run(shuffled), result)
})

test('two-center broad locality rejects a third port, an approach far from both cuts, or an anchor', () => {
  const ported = twoCenterScenario()
  ported.boundaries.push(cut([[360, 760], [400, 760]], 80, { tag: 'third-port' }))
  ported.partition.boundaries.push(FULL([[2, 9]]))
  ported.partition.labels[99 * ported.width + 42] = 9
  assert.equal(run(ported).removedCount, 0)

  assert.equal(run(twoCenterScenario([[100, 120]])).removedCount, 0)

  const anchored = twoCenterScenario()
  anchored.bases.push({ routeAnchor: [40, 100] })
  assert.equal(run(anchored).removedCount, 0)
})


function rampInteriorScenario({ multipart = false } = {}) {
  const boundaries = [
    cut([[80, 72], [136, 128]], 96, { kind: 'ramp', rampId: 1, rampEnd: 'upper', lowerElevation: 0, upperElevation: 1, tag: 'upper' }),
    cut([[112, 96], [168, 152]], 200, { tag: 'inside-a' }),
    cut([[136, 112], [192, 168]], 200, { tag: 'inside-b' }),
    cut([[144, 96], [200, 152]], 96, { kind: 'ramp', rampId: 1, rampEnd: 'lower', lowerElevation: 0, upperElevation: 1, tag: 'lower' }),
  ]
  const secondPairs = multipart ? [[3, 4], [4, 6]] : [[3, 4]]
  const p = partition(32, 32, {
    1: [[0, 0]], 2: [[10, 10]], 3: [[11, 10]], 4: [[12, 10]], 5: [[30, 0]], 6: [[12, 11]],
  }, [FULL([[1, 2]]), FULL([[2, 3]]), FULL(secondPairs), FULL([[5, 4]])])
  const terrainFlags = new Uint8Array(p.labels.length).fill(8)
  terrainFlags[10 * 32 + 10] = 12
  return { boundaries, partition: p, bases: [{ routeAnchor: [0, 0] }, { routeAnchor: [30, 0] }], width: 32, terrainFlags }
}

test('collapses a sealed same-ramp interior including a tiny multipart leaf while retaining both ends', () => {
  const scenario = rampInteriorScenario({ multipart: true })
  const result = collapseRampInteriors(
    scenario.boundaries, scenario.partition, scenario.bases, scenario.width, scenario.terrainFlags,
  )
  assert.equal(result.removedCount, 2)
  assert.deepEqual(tags(result), ['upper', 'lower'])

  const shuffled = {
    ...scenario,
    boundaries: scenario.boundaries.map(reverse).reverse(),
    partition: { ...scenario.partition, boundaries: [...scenario.partition.boundaries].reverse() },
  }
  assert.deepEqual(collapseRampInteriors(
    shuffled.boundaries, shuffled.partition, shuffled.bases, shuffled.width, shuffled.terrainFlags,
  ), result)
})

function rampPortLeafScenario() {
  const boundaries = [
    cut([[80, 72], [136, 128]], 96, { kind: 'ramp', rampId: 19, rampEnd: 'upper', lowerElevation: 0, upperElevation: 1, tag: 'upper' }),
    cut([[112, 96], [168, 152]], 96, { tag: 'inside' }),
    cut([[144, 96], [200, 152]], 96, { kind: 'ramp', rampId: 19, rampEnd: 'lower', lowerElevation: 0, upperElevation: 1, tag: 'lower' }),
  ]
  const p = partition(32, 32, {
    1: [[0, 0]], 2: [[10, 10]], 3: [[11, 10]], 4: [[13, 10]], 5: [[12, 10]], 6: [[14, 10]],
  }, [FULL([[1, 2]]), FULL([[2, 3]]), FULL([[4, 3], [4, 5]])])
  const terrainFlags = new Uint8Array(p.labels.length).fill(8)
  terrainFlags[10 * 32 + 10] = 12
  return { boundaries, partition: p, bases: [{ routeAnchor: [0, 0] }], width: 32, terrainFlags }
}

test('collapses a local unanchored ramp-only port leaf while retaining both structural ends', () => {
  const scenario = rampPortLeafScenario()
  const result = collapseRampInteriors(
    scenario.boundaries, scenario.partition, scenario.bases, scenario.width, scenario.terrainFlags,
  )
  assert.equal(result.removedCount, 1)
  assert.deepEqual(tags(result), ['upper', 'lower'])
})

test('ramp-only port leaves reject an anchor or an extra exit', () => {
  const anchored = rampPortLeafScenario()
  anchored.bases.push({ routeAnchor: [12, 10] })
  assert.equal(collapseRampInteriors(
    anchored.boundaries, anchored.partition, anchored.bases, anchored.width, anchored.terrainFlags,
  ).removedCount, 0)

  const exited = rampPortLeafScenario()
  exited.boundaries.push(cut([[200, 112], [240, 112]], 64, { tag: 'leaf-exit' }))
  exited.partition.boundaries.push(FULL([[5, 6]]))
  assert.equal(collapseRampInteriors(
    exited.boundaries, exited.partition, exited.bases, exited.width, exited.terrainFlags,
  ).removedCount, 0)
})

test('ramp interior collapse requires terrain flags and rejects anchors, extra ports, flat interiors, and wrong elevations', () => {
  const scenario = rampInteriorScenario()
  assert.throws(() => collapseRampInteriors(scenario.boundaries, scenario.partition, scenario.bases, scenario.width), /terrain flags/)
  assert.throws(() => collapseRampInteriors(scenario.boundaries, scenario.partition, scenario.bases, scenario.width, new Uint8Array(1)), /terrain flags/)

  const anchored = rampInteriorScenario()
  anchored.bases.push({ routeAnchor: [10, 10] })
  assert.equal(collapseRampInteriors(anchored.boundaries, anchored.partition, anchored.bases, anchored.width, anchored.terrainFlags).removedCount, 0)

  const ported = rampInteriorScenario()
  ported.boundaries.push(cut([[200, 112], [200, 168]], 80, { tag: 'extra-port' }))
  ported.partition.boundaries.push({ removedEdgeCount: 1, separatedEdgeCount: 0, regionPairs: [[4, 7]] })
  ported.partition.labels[12 * ported.width + 14] = 7
  assert.equal(collapseRampInteriors(ported.boundaries, ported.partition, ported.bases, ported.width, ported.terrainFlags).removedCount, 0)

  const flat = rampInteriorScenario()
  flat.terrainFlags.fill(8)
  assert.equal(collapseRampInteriors(flat.boundaries, flat.partition, flat.bases, flat.width, flat.terrainFlags).removedCount, 0)

  const mixed = rampInteriorScenario()
  mixed.terrainFlags[10 * mixed.width + 11] = 16
  assert.equal(collapseRampInteriors(mixed.boundaries, mixed.partition, mixed.bases, mixed.width, mixed.terrainFlags).removedCount, 0)
})


test('ramp interior collapse never removes a multipart ordinary edge or ramp port with pairs outside its component', () => {
  const ordinaryLeak = rampInteriorScenario()
  ordinaryLeak.partition.boundaries[1] = FULL([[2, 3], [10, 11]])
  ordinaryLeak.partition.labels[12 * ordinaryLeak.width + 15] = 10
  ordinaryLeak.partition.labels[12 * ordinaryLeak.width + 16] = 11
  ordinaryLeak.bases.push({ routeAnchor: [15, 12] }, { routeAnchor: [16, 12] })
  assert.equal(collapseRampInteriors(
    ordinaryLeak.boundaries, ordinaryLeak.partition, ordinaryLeak.bases, ordinaryLeak.width, ordinaryLeak.terrainFlags,
  ).removedCount, 0)

  const portLeak = rampInteriorScenario()
  portLeak.partition.boundaries[0] = FULL([[1, 2], [8, 9]])
  portLeak.partition.labels[12 * portLeak.width + 15] = 8
  portLeak.partition.labels[12 * portLeak.width + 16] = 9
  assert.equal(collapseRampInteriors(
    portLeak.boundaries, portLeak.partition, portLeak.bases, portLeak.width, portLeak.terrainFlags,
  ).removedCount, 0)
})


function turningStarScenario(leaf = [[40, 100]]) {
  const boundaries = [
    cut([[320, 0], [320, 800]], 600, { tag: 'star-mouth' }),
    cut([[80, 72], [136, 128]], 96, { kind: 'ramp', rampId: 13, rampEnd: 'upper', tag: 'ramp' }),
  ]
  const p = partition(128, 128, {
    1: [[8, 8]], 2: [[20, 16]], 3: [[0, 0]], 4: leaf,
  }, [FULL([[2, 3], [3, 4]]), FULL([[1, 2]])])
  return { boundaries, partition: p, bases: [{ routeAnchor: [0, 0] }], width: 128 }
}

test('allows a strict right-leaf star when each local interior is near the mouth or ramp', () => {
  const scenario = turningStarScenario()
  const result = run(scenario)
  assert.equal(result.removedCount, 1)
  assert.deepEqual(tags(result), ['ramp'])
})

test('two-center strict stars reject a leaf port, leaf anchor, or leaf far from both cuts', () => {
  const ported = turningStarScenario()
  ported.boundaries.push(cut([[360, 760], [400, 760]], 80, { tag: 'leaf-port' }))
  ported.partition.boundaries.push(FULL([[4, 9]]))
  ported.partition.labels[99 * ported.width + 42] = 9
  assert.equal(run(ported).removedCount, 0)

  const anchored = turningStarScenario()
  anchored.bases.push({ routeAnchor: [40, 100] })
  assert.equal(run(anchored).removedCount, 0)

  assert.equal(run(turningStarScenario([[100, 120]])).removedCount, 0)
})

test('simultaneous strict stars retain each ramp while sharing one anchored outer area', () => {
  const first = turningStarScenario()
  const secondMouth = cut([[704, 0], [704, 800]], 600, { tag: 'star-mouth-b' })
  const secondRamp = cut([[464, 72], [520, 128]], 96, { kind: 'ramp', rampId: 14, rampEnd: 'upper', tag: 'ramp-b' })
  first.boundaries.push(secondMouth, secondRamp)
  first.partition.boundaries.push(FULL([[5, 3], [3, 7]]), FULL([[6, 5]]))
  first.partition.labels[16 * first.width + 68] = 5
  first.partition.labels[8 * first.width + 58] = 6
  first.partition.labels[100 * first.width + 88] = 7
  const result = run(first)
  assert.equal(result.removedCount, 2)
  assert.deepEqual(tags(result), ['ramp', 'ramp-b'])
})
