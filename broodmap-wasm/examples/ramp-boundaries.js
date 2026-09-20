// Replace oversized nearby widening observations with structural ramp evidence.
// This selects actual observed cuts; it never clips or moves a span.
const MAX_MOUTH_DISTANCE = 224
const MAX_APPROACH_RADIUS = 512
const MAX_REPRESENTATIVE_OFFSET = 32
const MAX_OVERLAY_NORMAL_OFFSET = 64
const MIN_OVERLAY_FRACTION = 0.75
const MAX_OVERLAY_WIDTH_RATIO = 1.5
const MIN_PARALLEL_COSINE = Math.cos(Math.PI / 12)

function compareEndpoints(a, b) {
  return a[0][0] - b[0][0] || a[0][1] - b[0][1]
    || a[1][0] - b[1][0] || a[1][1] - b[1][1]
}
function endpoints(boundary) {
  const a = [...boundary.endpoints[0]], b = [...boundary.endpoints[1]]
  return a[0] < b[0] || (a[0] === b[0] && a[1] <= b[1]) ? [a, b] : [b, a]
}
function isNearbyParallelRepresentative(a, b) {
  const [a0, a1] = endpoints({ endpoints: a })
  const [b0, b1] = endpoints({ endpoints: b })
  if (distance(a0, b0) > MAX_REPRESENTATIVE_OFFSET || distance(a1, b1) > MAX_REPRESENTATIVE_OFFSET) return false
  const adx = a1[0] - a0[0], ady = a1[1] - a0[1]
  const bdx = b1[0] - b0[0], bdy = b1[1] - b0[1]
  const lengths = Math.hypot(adx, ady) * Math.hypot(bdx, bdy)
  return lengths > 0 && Math.abs(adx * bdx + ady * bdy) / lengths >= MIN_PARALLEL_COSINE
}
// An ordinary cut can be a rasterized copy of a ramp end even when their endpoints differ.
// Require substantial collinear overlap instead of extending the tighter representative envelope.
function overlappingRampRepresentative(candidate, ramp) {
  const [candidateStart, candidateEnd] = endpoints({ endpoints: candidate })
  const [rampStart, rampEnd] = endpoints({ endpoints: ramp })
  const dx = rampEnd[0] - rampStart[0], dy = rampEnd[1] - rampStart[1]
  const rampLength = Math.hypot(dx, dy)
  const candidateDx = candidateEnd[0] - candidateStart[0]
  const candidateDy = candidateEnd[1] - candidateStart[1]
  const candidateLength = Math.hypot(candidateDx, candidateDy)
  if (!rampLength || !candidateLength
    || Math.abs(dx * candidateDx + dy * candidateDy) / (rampLength * candidateLength) < MIN_PARALLEL_COSINE) {
    return null
  }
  const normalOffset = point => Math.abs((point[0] - rampStart[0]) * dy - (point[1] - rampStart[1]) * dx) / rampLength
  if (Math.max(normalOffset(candidateStart), normalOffset(candidateEnd)) > MAX_OVERLAY_NORMAL_OFFSET) return null
  const project = point => ((point[0] - rampStart[0]) * dx + (point[1] - rampStart[1]) * dy) / rampLength
  const candidateMin = Math.min(project(candidateStart), project(candidateEnd))
  const candidateMax = Math.max(project(candidateStart), project(candidateEnd))
  const overlap = Math.max(0, Math.min(rampLength, candidateMax) - Math.max(0, candidateMin))
  if (overlap < MIN_OVERLAY_FRACTION * Math.min(rampLength, candidateLength)) return null
  return Math.max(normalOffset(candidateStart), normalOffset(candidateEnd))
}
function center(boundary) {
  return boundary.endpoints[0].map((v, i) => (v + boundary.endpoints[1][i]) / 2)
}
function fullCut(entry) {
  return entry?.removedEdgeCount > 0 && entry.removedEdgeCount === entry.separatedEdgeCount
    && Array.isArray(entry.regionPairs) && entry.regionPairs.length > 0
    && entry.regionPairs.every(pair => Array.isArray(pair) && pair.length === 2 && pair[0] !== pair[1])
}
function fullPair(entry) {
  return fullCut(entry) && entry.regionPairs.length === 1
}
function distance(a, b) { return Math.hypot(a[0] - b[0], a[1] - b[1]) }
function pointSegmentDistance(point, a, b) {
  const dx = b[0] - a[0], dy = b[1] - a[1]
  const lengthSquared = dx * dx + dy * dy
  if (lengthSquared === 0) return distance(point, a)
  const t = Math.max(0, Math.min(1, ((point[0] - a[0]) * dx + (point[1] - a[1]) * dy) / lengthSquared))
  return Math.hypot(point[0] - (a[0] + t * dx), point[1] - (a[1] + t * dy))
}
function orientation(a, b, c) {
  return (b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0])
}
function onSegment(a, b, point) {
  return Math.min(a[0], b[0]) <= point[0] && point[0] <= Math.max(a[0], b[0])
    && Math.min(a[1], b[1]) <= point[1] && point[1] <= Math.max(a[1], b[1])
}
function intersects(a, b) {
  const [a0, a1] = a, [b0, b1] = b
  const ab0 = orientation(a0, a1, b0), ab1 = orientation(a0, a1, b1)
  const ba0 = orientation(b0, b1, a0), ba1 = orientation(b0, b1, a1)
  return (ab0 === 0 && onSegment(a0, a1, b0)) || (ab1 === 0 && onSegment(a0, a1, b1))
    || (ba0 === 0 && onSegment(b0, b1, a0)) || (ba1 === 0 && onSegment(b0, b1, a1))
    || (Math.sign(ab0) !== Math.sign(ab1) && Math.sign(ba0) !== Math.sign(ba1))
}

// Exact Euclidean distance for finite segments, including diagonal crossings and points.
export function segmentDistance(a, b) {
  if (intersects(a, b)) return 0
  return Math.min(
    pointSegmentDistance(a[0], b[0], b[1]), pointSegmentDistance(a[1], b[0], b[1]),
    pointSegmentDistance(b[0], a[0], a[1]), pointSegmentDistance(b[1], a[0], a[1]),
  )
}

function star(entry) {
  if (!fullCut(entry)) return null
  if (entry.regionPairs.length === 1) return { single: true, inner: entry.regionPairs[0] }
  const common = entry.regionPairs[0].filter(region => entry.regionPairs.every(pair => pair.includes(region)))
  if (common.length !== 1) return null
  const outer = common[0]
  const inner = [...new Set(entry.regionPairs.flat().filter(region => region !== outer))]
  return inner.length ? { single: false, outer, inner } : null
}

function triangleRepresentative(mouth, ramp) {
  if (!fullCut(mouth) || !fullCut(ramp) || mouth.regionPairs.length !== 2 || ramp.regionPairs.length !== 2) return null
  const regions = [...new Set(mouth.regionPairs.flat())]
  if (regions.length !== 3 || regions.some(region => !ramp.regionPairs.flat().includes(region))) return null
  const key = pair => [...pair].sort((a, b) => a - b).join(':')
  const mouthEdges = new Set(mouth.regionPairs.map(key)), rampEdges = new Set(ramp.regionPairs.map(key))
  const edges = new Set([...mouthEdges, ...rampEdges])
  if (mouthEdges.size !== 2 || rampEdges.size !== 2 || edges.size !== 3
    || [...mouthEdges].filter(edge => rampEdges.has(edge)).length !== 1) return null
  return regions.sort((a, b) => a - b)
}

/**
 * Retains ramp ends while dropping a nearby ordinary widening at least twice as wide, or an
 * ordinary span representing the same ramp end within a 32px, 15-degree envelope. A strict
 * unanchored two-port approach also admits a bounded parallel overlay with similar width.
 * A multipart mouth is eligible only as a local star: one outer area plus a ramp approach and
 * leaf pockets with no other ports. Selected approaches never overlap an earlier outer area,
 * so simultaneous replacements cannot form a base-to-base chain. The caller repartitions.
 */
export function replaceRampMouths(boundaries, partition, bases, width) {
  const labels = partition.labels
  const metadata = partition.boundaries
  if (!(labels instanceof Uint32Array) || !Number.isInteger(width) || width <= 0
    || labels.length % width || metadata.length !== boundaries.length) {
    throw new TypeError('matching partition metadata and label dimensions are required')
  }
  const anchored = new Set(bases.filter(b => b.routeAnchor).map(b =>
    labels[b.routeAnchor[1] * width + b.routeAnchor[0]]))
  const incidents = new Map()
  for (const [index, entry] of metadata.entries()) {
    for (const pair of entry?.regionPairs || []) for (const region of pair) {
      if (!incidents.has(region)) incidents.set(region, new Set())
      incidents.get(region).add(index)
    }
  }
  const proposals = []
  for (let rampIndex = 0; rampIndex < boundaries.length; rampIndex++) {
    const ramp = boundaries[rampIndex]
    if (ramp.kind !== 'ramp' || !fullCut(metadata[rampIndex])) continue
    const rampRegions = fullPair(metadata[rampIndex]) ? metadata[rampIndex].regionPairs[0] : null
    for (let mouthIndex = 0; mouthIndex < boundaries.length; mouthIndex++) {
      const mouth = boundaries[mouthIndex]
      const nearbyRepresentative = isNearbyParallelRepresentative(mouth.endpoints, ramp.endpoints)
      const overlayRepresentative = Math.max(mouth.widthPixels, ramp.widthPixels)
        <= MAX_OVERLAY_WIDTH_RATIO * Math.min(mouth.widthPixels, ramp.widthPixels)
        && overlappingRampRepresentative(mouth.endpoints, ramp.endpoints) != null
      const broadMouth = mouth.widthPixels >= 2 * ramp.widthPixels
      const mouthShape = star(metadata[mouthIndex])
      const triangle = nearbyRepresentative && triangleRepresentative(metadata[mouthIndex], metadata[rampIndex])
      if (mouth.kind != null || (!mouthShape && !triangle)
        || (!broadMouth && !nearbyRepresentative && !overlayRepresentative)) continue
      let approach, inner, outer, union, twoPortBroadApproach = false
      if (triangle) {
        if (triangle.filter(region => anchored.has(region)).length > 1) continue
        inner = triangle.filter(region => !anchored.has(region))
        outer = triangle.find(region => anchored.has(region)) ?? null
        union = triangle
      } else {
        if (!rampRegions) continue
        const shared = mouthShape.inner.filter(region => rampRegions.includes(region))
        if (shared.length !== 1) continue
        approach = shared[0]
        inner = mouthShape.single ? [approach] : mouthShape.inner
        outer = mouthShape.single
          ? mouthShape.inner.find(region => region !== approach)
          : mouthShape.outer
        union = [outer, ...inner]
        if (inner.some(region => anchored.has(region))) continue
        const approachIncidents = incidents.get(approach)
        // A looser geometric overlay still needs the same sealed two-port approach as a broad
        // mouth, and keeps both candidate sides free of base anchors.
        const overlayOnly = overlayRepresentative && !nearbyRepresentative && !broadMouth
        if (overlayOnly && (!mouthShape.single || mouthShape.inner.some(region => anchored.has(region))
          || approachIncidents?.size !== 2 || !approachIncidents.has(mouthIndex)
          || !approachIncidents.has(rampIndex))) continue
        twoPortBroadApproach = (broadMouth || overlayOnly) && mouthShape.single && approachIncidents?.size === 2
          && approachIncidents.has(mouthIndex) && approachIncidents.has(rampIndex)
        if (boundaries.some((other, index) => other.kind === 'ramp'
          && other.rampId === ramp.rampId && other.rampEnd !== ramp.rampEnd
          && metadata[index].regionPairs.some(pair => pair.includes(approach)))) continue
        if (inner.length > 1) {
          if (approachIncidents?.size !== 2 || !approachIncidents.has(mouthIndex)
            || !approachIncidents.has(rampIndex)) continue
          if (inner.some(region => region !== approach &&
            (incidents.get(region)?.size !== 1 || !incidents.get(region).has(mouthIndex)))) continue
          // A strict star has no ports beyond this mouth and ramp, so its leaves may turn locally too.
          twoPortBroadApproach = broadMouth
        }
      }
      const rampCenter = center(ramp)
      const separation = segmentDistance(mouth.endpoints, ramp.endpoints)
      if (separation > MAX_MOUTH_DISTANCE) continue
      proposals.push({
        mouthIndex, rampIndex, outer, inner, union, triangle: Boolean(triangle), separation, rampCenter,
        mouthCenter: center(mouth), twoPortBroadApproach,
      })
    }
  }
  const bounds = new Map()
  for (const proposal of proposals) for (const region of proposal.inner) {
    if (!bounds.has(region)) bounds.set(region, { minX: Infinity, minY: Infinity, maxX: -Infinity, maxY: -Infinity })
  }
  for (let i = 0; i < labels.length; i++) {
    const box = bounds.get(labels[i])
    if (!box) continue
    const x = (i % width) * 8, y = Math.floor(i / width) * 8
    box.minX = Math.min(box.minX, x)
    box.minY = Math.min(box.minY, y)
    box.maxX = Math.max(box.maxX, x + 8)
    box.maxY = Math.max(box.maxY, y + 8)
  }
  const candidates = proposals.filter(proposal => proposal.inner.every(region => {
    const box = bounds.get(region)
    // Only a strict two-port broad/overlay approach or broad mouth-only-leaf star can turn locally.
    const centers = proposal.twoPortBroadApproach ? [proposal.rampCenter, proposal.mouthCenter] : [proposal.rampCenter]
    return Number.isFinite(box.minX)
      && [[box.minX, box.minY], [box.minX, box.maxY], [box.maxX, box.minY], [box.maxX, box.maxY]]
        .every(point => centers.some(center => distance(point, center) <= MAX_APPROACH_RADIUS))
  })).sort((a, b) => a.separation - b.separation
    || compareEndpoints(endpoints(boundaries[a.mouthIndex]), endpoints(boundaries[b.mouthIndex]))
    || compareEndpoints(endpoints(boundaries[a.rampIndex]), endpoints(boundaries[b.rampIndex])))
  const removed = new Set(), absorbed = new Set(), outers = new Set(), rampEnds = new Set()
  const occupied = new Set(), triangleReserved = new Set()
  for (const proposal of candidates) {
    const ramp = boundaries[proposal.rampIndex]
    const rampKey = `${ramp.rampId}:${ramp.rampEnd}`
    if (removed.has(proposal.mouthIndex) || rampEnds.has(rampKey) || absorbed.has(proposal.outer)
      || proposal.inner.some(region => absorbed.has(region) || outers.has(region))
      || (proposal.triangle && proposal.union.some(region => occupied.has(region)))
      || (!proposal.triangle && proposal.union.some(region => triangleReserved.has(region)))) continue
    removed.add(proposal.mouthIndex)
    for (const region of proposal.inner) absorbed.add(region)
    if (proposal.outer != null) outers.add(proposal.outer)
    for (const region of proposal.union) occupied.add(region)
    if (proposal.triangle) for (const region of proposal.union) triangleReserved.add(region)
    rampEnds.add(rampKey)
  }
  return {
    boundaries: boundaries.filter((_, i) => !removed.has(i))
      .map(b => ({ ...b, endpoints: endpoints(b) }))
      .sort((a, b) => compareEndpoints(a.endpoints, b.endpoints)),
    removedCount: removed.size,
  }
}


// Collapses an ordinary-cut component only when it is the sealed interior of both ends of one ramp.
export function collapseRampInteriors(boundaries, partition, bases, width, terrainFlags) {
  const labels = partition.labels
  const metadata = partition.boundaries
  if (!(labels instanceof Uint32Array) || !(terrainFlags instanceof Uint8Array)
    || terrainFlags.length !== labels.length || !Number.isInteger(width) || width <= 0
    || labels.length % width || metadata.length !== boundaries.length) {
    throw new TypeError('matching partition metadata, labels, terrain flags, and width are required')
  }
  const anchored = new Set(bases.filter(base => base.routeAnchor).map(base =>
    labels[base.routeAnchor[1] * width + base.routeAnchor[0]]))
  const incidents = new Map(), graph = new Map()
  const addIncident = (region, index) => {
    if (!incidents.has(region)) incidents.set(region, new Set())
    incidents.get(region).add(index)
  }
  const addGraphEdge = (left, right, index) => {
    if (!graph.has(left)) graph.set(left, [])
    if (!graph.has(right)) graph.set(right, [])
    graph.get(left).push({ region: right, index })
    graph.get(right).push({ region: left, index })
  }
  for (const [index, entry] of metadata.entries()) {
    for (const pair of entry?.regionPairs || []) for (const region of pair) addIncident(region, index)
    if (boundaries[index].kind == null && fullCut(entry)) {
      for (const [left, right] of entry.regionPairs) addGraphEdge(left, right, index)
    }
  }
  const visited = new Set(), proposals = []
  for (const start of [...graph.keys()].sort((a, b) => a - b)) {
    if (visited.has(start)) continue
    const regions = new Set([start]), edges = new Set(), todo = [start]
    visited.add(start)
    while (todo.length) {
      const region = todo.pop()
      for (const edge of graph.get(region) || []) {
        edges.add(edge.index)
        if (!visited.has(edge.region)) visited.add(edge.region), regions.add(edge.region), todo.push(edge.region)
      }
    }
    if (!edges.size || edges.size > 8 || [...regions].some(region => anchored.has(region))) continue
    // A multipart boundary may contribute disconnected graph edges. Never remove it unless every
    // one of its represented pairs belongs to this sealed component.
    if ([...edges].some(index => metadata[index].regionPairs.some(pair =>
      pair.some(region => !regions.has(region))))) continue
    const ports = new Set()
    for (const region of regions) for (const index of incidents.get(region) || []) {
      if (!edges.has(index)) ports.add(index)
    }
    if (ports.size !== 2) continue
    const portIndexes = [...ports].sort((a, b) => a - b)
    const portBoundaries = portIndexes.map(index => boundaries[index])
    const ends = new Set(portBoundaries.map(boundary => boundary.rampEnd))
    if (portBoundaries.some((boundary, position) => boundary.kind !== 'ramp' || !fullCut(metadata[portIndexes[position]]))
      || portBoundaries[0].rampId !== portBoundaries[1].rampId
      || !ends.has('lower') || !ends.has('upper') || ends.size !== 2) continue
    // A multipart ramp end may have a tiny leaf behind its same external side. Treat that leaf
    // as interior only when it is unanchored and has no exit except this ramp port.
    const expandedRegions = new Set(regions)
    let validPorts = true
    for (const index of portIndexes) {
      const pairs = metadata[index].regionPairs
      const exteriors = new Set()
      for (const pair of pairs) {
        const inside = pair.filter(region => expandedRegions.has(region))
        if (inside.length > 1) validPorts = false
        if (inside.length === 1) exteriors.add(pair.find(region => region !== inside[0]))
      }
      for (const pair of pairs) {
        if (pair.some(region => expandedRegions.has(region))) continue
        const exterior = pair.filter(region => exteriors.has(region))
        if (exterior.length !== 1) { validPorts = false; continue }
        const leaf = pair.find(region => region !== exterior[0])
        const leafIncidents = incidents.get(leaf)
        if (anchored.has(leaf) || leafIncidents?.size !== 1 || !leafIncidents.has(index)) {
          validPorts = false
          continue
        }
        expandedRegions.add(leaf)
      }
      if (pairs.some(pair => pair.filter(region => expandedRegions.has(region)).length !== 1)) validPorts = false
    }
    if (!validPorts) continue
    proposals.push({
      regions: [...expandedRegions].sort((a, b) => a - b), edges: [...edges].sort((a, b) => a - b),
      ports: portIndexes, rampId: portBoundaries[0].rampId,
    })
  }
  const stats = new Map()
  for (const proposal of proposals) for (const region of proposal.regions) {
    if (!stats.has(region)) stats.set(region, { minX: Infinity, minY: Infinity, maxX: -Infinity, maxY: -Infinity, ramp: false, elevations: 0 })
  }
  for (let index = 0; index < labels.length; index++) {
    const stat = stats.get(labels[index])
    if (!stat) continue
    const x = (index % width) * 8, y = Math.floor(index / width) * 8, flags = terrainFlags[index]
    stat.minX = Math.min(stat.minX, x); stat.minY = Math.min(stat.minY, y)
    stat.maxX = Math.max(stat.maxX, x + 8); stat.maxY = Math.max(stat.maxY, y + 8)
    stat.ramp ||= (flags & 4) !== 0
    stat.elevations |= 1 << ((flags >> 3) & 3)
  }
  const candidates = proposals.filter(proposal => {
    const ports = proposal.ports.map(index => boundaries[index])
    const centers = ports.map(center)
    const allowedElevations = (1 << ports[0].lowerElevation) | (1 << ports[0].upperElevation)
    return proposal.regions.some(region => stats.get(region).ramp) && proposal.regions.every(region => {
      const stat = stats.get(region)
      return Number.isFinite(stat.minX) && (stat.elevations & ~allowedElevations) === 0
        && [[stat.minX, stat.minY], [stat.minX, stat.maxY], [stat.maxX, stat.minY], [stat.maxX, stat.maxY]]
          .every(point => centers.some(portCenter => distance(point, portCenter) <= MAX_APPROACH_RADIUS))
    })
  }).sort((left, right) => left.rampId - right.rampId
    || compareEndpoints(endpoints(boundaries[left.ports[0]]), endpoints(boundaries[right.ports[0]])))
  const removed = new Set(), reservedRegions = new Set()
  for (const candidate of candidates) {
    if (candidate.edges.some(index => removed.has(index)) || candidate.regions.some(region => reservedRegions.has(region))) continue
    for (const index of candidate.edges) removed.add(index)
    for (const region of candidate.regions) reservedRegions.add(region)
  }
  return {
    boundaries: boundaries.filter((_, index) => !removed.has(index))
      .map(boundary => ({ ...boundary, endpoints: endpoints(boundary) }))
      .sort((left, right) => compareEndpoints(left.endpoints, right.endpoints)),
    removedCount: removed.size,
  }
}
