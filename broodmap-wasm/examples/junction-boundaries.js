const MAX_PORT_DISTANCE_PIXELS = 384
const MAX_UNION_RADIUS_PIXELS = 512

function comparePoint(left, right) {
  return left[0] - right[0] || left[1] - right[1]
}

function canonicalEndpoints(endpoints) {
  const first = [endpoints[0][0], endpoints[0][1]]
  const second = [endpoints[1][0], endpoints[1][1]]
  return comparePoint(first, second) <= 0 ? [first, second] : [second, first]
}

function compareEndpoints(left, right) {
  return comparePoint(left[0], right[0]) || comparePoint(left[1], right[1])
}

function usableBoundary(boundary) {
  return Number.isFinite(boundary?.widthPixels) && boundary.widthPixels > 0
    && Array.isArray(boundary.endpoints) && boundary.endpoints.length === 2
    && boundary.endpoints.every(point => Array.isArray(point) && point.length === 2
      && Number.isFinite(point[0]) && Number.isFinite(point[1]))
}

function cloneBoundary(boundary) {
  if (!usableBoundary(boundary)) {
    throw new TypeError('each boundary must have a finite positive width and two finite endpoints')
  }
  return { ...boundary, endpoints: canonicalEndpoints(boundary.endpoints) }
}

function isFullSinglePair(metadata) {
  return metadata?.removedEdgeCount > 0
    && metadata.removedEdgeCount === metadata.separatedEdgeCount
    && Array.isArray(metadata.regionPairs)
    && metadata.regionPairs.length === 1
    && Array.isArray(metadata.regionPairs[0])
    && metadata.regionPairs[0].length === 2
    && metadata.regionPairs[0][0] !== metadata.regionPairs[0][1]
}

function midpoint(boundary) {
  return [
    (boundary.endpoints[0][0] + boundary.endpoints[1][0]) / 2,
    (boundary.endpoints[0][1] + boundary.endpoints[1][1]) / 2,
  ]
}

function withinDistance(left, right, limit) {
  const x = left[0] - right[0]
  const y = left[1] - right[1]
  return x * x + y * y <= limit * limit
}

function withinSegmentDistance(point, endpoints, limit) {
  const [start, end] = endpoints
  const x = end[0] - start[0]
  const y = end[1] - start[1]
  const lengthSquared = x * x + y * y
  const ratio = lengthSquared
    ? Math.min(1, Math.max(0, ((point[0] - start[0]) * x + (point[1] - start[1]) * y) / lengthSquared))
    : 0
  return withinDistance(point, [start[0] + x * ratio, start[1] + y * ratio], limit)
}
function baseRegions(labels, bases, width) {
  const result = new Set()
  for (const base of bases || []) {
    const anchor = base?.routeAnchor
    if (!Array.isArray(anchor) || anchor.length !== 2) continue
    const [x, y] = anchor
    if (!Number.isInteger(x) || !Number.isInteger(y) || x < 0 || y < 0 || x >= width) continue
    const label = labels[y * width + x]
    if (label !== undefined) result.add(label)
  }
  return result
}

/**
 * Removes only local, flat interior edges from a provisional partition graph. A one-anchored
 * union is allowed only when its removed span is at least as wide as every retained port.
 * Callers retain raw observations separately and re-partition once when `removedCount` is nonzero.
 * The caller's boundary cap is preserved: this selector never creates new boundaries.
 */
export function removeJunctionBoundaries(boundaries, partition, bases, width, terrainFlags) {
  if (!Array.isArray(boundaries) || !partition || !Number.isInteger(width) || width <= 0) {
    throw new TypeError('boundaries, partition, and a positive walk-grid width are required')
  }
  const labels = partition.labels
  if (!(labels instanceof Uint32Array) || labels.length % width !== 0) {
    throw new TypeError('partition.labels must be a Uint32Array whose length is divisible by width')
  }
  if (!(terrainFlags instanceof Uint8Array) || terrainFlags.length !== labels.length) {
    throw new TypeError('terrainFlags must be a Uint8Array matching partition.labels')
  }
  const metadata = Array.isArray(partition.boundaries) ? partition.boundaries : []
  const result = boundaries.map(cloneBoundary)
  if (metadata.length !== boundaries.length) {
    return { boundaries: result.sort((left, right) => compareEndpoints(left.endpoints, right.endpoints)), removedCount: 0 }
  }

  const incidents = new Map()
  const addIncident = (region, index) => {
    let values = incidents.get(region)
    if (!values) {
      values = new Set()
      incidents.set(region, values)
    }
    values.add(index)
  }
  for (const [index, entry] of metadata.entries()) {
    for (const pair of entry?.regionPairs || []) {
      if (!Array.isArray(pair) || pair.length !== 2) continue
      addIncident(pair[0], index)
      addIncident(pair[1], index)
    }
  }

  const anchored = baseRegions(labels, bases, width)
  const proposals = []
  for (const [index, entry] of metadata.entries()) {
    if (boundaries[index]?.kind === 'ramp' || !isFullSinglePair(entry)) continue
    const pair = entry.regionPairs[0]
    const anchoredRegionCount = pair.filter(region => anchored.has(region)).length
    if (anchoredRegionCount > 1) continue
    const union = new Set(pair)
    const ports = new Set()
    let widestPort = 0
    let valid = true
    for (const region of union) {
      for (const otherIndex of incidents.get(region) || []) {
        if (otherIndex === index) continue
        const otherEntry = metadata[otherIndex]
        if (!isFullSinglePair(otherEntry)) {
          valid = false
          break
        }
        const otherPair = otherEntry.regionPairs[0]
        const leftInside = union.has(otherPair[0])
        const rightInside = union.has(otherPair[1])
        if (leftInside === rightInside) {
          valid = false
          break
        }
        ports.add(otherIndex)
      }
      if (!valid) break
    }
    if (!valid || ports.size < 3) continue
    const exterior = new Set()
    const center = midpoint(boundaries[index])
    for (const port of ports) {
      const [left, right] = metadata[port].regionPairs[0]
      exterior.add(union.has(left) ? right : left)
      widestPort = Math.max(widestPort, boundaries[port].widthPixels)
      if (!withinDistance(center, midpoint(boundaries[port]), MAX_PORT_DISTANCE_PIXELS)) {
        valid = false
        break
      }
    }
    if (!valid || exterior.size < 3
      || (anchoredRegionCount === 1 && boundaries[index].widthPixels < widestPort)) continue
    proposals.push({ index, pair, anchoredRegionCount, ports, center, cells: 0, hasRamp: false, elevations: 0, withinRadius: true })
  }

  // A single flat pocket may have two nearby cuts to the same exterior plus one distinct,
  // anchored base-facing port. The wider same-exterior cut is redundant only in that exact shape.
  const bridgeProposals = []
  for (const [pocket, values] of incidents) {
    if (anchored.has(pocket) || values.size !== 3) continue
    const ports = [...values].sort((left, right) => left - right)
    if (ports.some(index => boundaries[index]?.kind != null || !isFullSinglePair(metadata[index]))) continue
    const groups = new Map()
    for (const index of ports) {
      const [left, right] = metadata[index].regionPairs[0]
      const exterior = left === pocket ? right : right === pocket ? left : null
      if (exterior == null) continue
      let indexes = groups.get(exterior)
      if (!indexes) groups.set(exterior, indexes = [])
      indexes.push(index)
    }
    if (groups.size !== 2) continue
    const shared = [...groups.entries()].find(([, indexes]) => indexes.length === 2)
    const third = [...groups.entries()].find(([, indexes]) => indexes.length === 1)
    if (!shared || !third || !anchored.has(third[0])) continue
    const [exterior, pair] = shared
    const [first, second] = pair
    const wide = boundaries[first].widthPixels >= 2 * boundaries[second].widthPixels ? first
      : boundaries[second].widthPixels >= 2 * boundaries[first].widthPixels ? second
        : null
    if (wide == null) continue
    const center = midpoint(boundaries[wide])
    if (!ports.every(index => withinDistance(center, midpoint(boundaries[index]), MAX_PORT_DISTANCE_PIXELS))) continue
    bridgeProposals.push({ type: 'bridge', index: wide, pocket, exterior, ports: new Set(ports), center })
  }
  // Summarize every participating final component once. Proposal radius checks use each
  // component bbox's four cell-corner extrema, a conservative proof that avoids per-cell work
  // per proposal and may reject a non-rectangular component that the exact check accepted.
  const regionStats = new Map()
  const addStats = region => {
    if (!regionStats.has(region)) {
      regionStats.set(region, {
        cells: 0,
        minX: Infinity,
        minY: Infinity,
        maxX: -Infinity,
        maxY: -Infinity,
        hasRamp: false,
        elevations: 0,
        elevationCounts: [0, 0, 0, 0],
      })
    }
  }
  for (const proposal of proposals) for (const region of proposal.pair) addStats(region)
  for (const proposal of bridgeProposals) addStats(proposal.pocket)
  for (let index = 0; index < labels.length; index++) {
    const stats = regionStats.get(labels[index])
    if (!stats) continue
    const x = (index % width) * 8
    const y = Math.floor(index / width) * 8
    const flags = terrainFlags[index]
    stats.cells++
    stats.minX = Math.min(stats.minX, x)
    stats.minY = Math.min(stats.minY, y)
    stats.maxX = Math.max(stats.maxX, x + 8)
    stats.maxY = Math.max(stats.maxY, y + 8)
    stats.hasRamp ||= (flags & 4) !== 0
    const elevation = (flags >> 3) & 3
    stats.elevations |= 1 << elevation
    stats.elevationCounts[elevation]++
  }
  const withinRadius = (stats, center) => stats?.cells > 0
    && withinDistance([stats.minX, stats.minY], center, MAX_UNION_RADIUS_PIXELS)
    && withinDistance([stats.maxX, stats.minY], center, MAX_UNION_RADIUS_PIXELS)
    && withinDistance([stats.minX, stats.maxY], center, MAX_UNION_RADIUS_PIXELS)
    && withinDistance([stats.maxX, stats.maxY], center, MAX_UNION_RADIUS_PIXELS)

  const withinSpanRadius = (stats, boundary) => stats?.cells > 0
    && [[stats.minX, stats.minY], [stats.maxX, stats.minY], [stats.minX, stats.maxY], [stats.maxX, stats.maxY]].every(point => withinSegmentDistance(point, boundary.endpoints, MAX_UNION_RADIUS_PIXELS))
  const dominantElevation = stats => {
    let dominant = 0
    for (let elevation = 1; elevation < stats.elevationCounts.length; elevation++) {
      if (stats.elevationCounts[elevation] > stats.elevationCounts[dominant]) dominant = elevation
    }
    return stats.elevationCounts[dominant] * 100 >= stats.cells * 99 ? dominant : null
  }
  const unionRanked = proposals.filter(proposal => {
    const first = regionStats.get(proposal.pair[0])
    const second = regionStats.get(proposal.pair[1])
    const cells = (first?.cells || 0) + (second?.cells || 0)
    const elevations = (first?.elevations || 0) | (second?.elevations || 0)
    const strictlyFlat = elevations > 0 && (elevations & (elevations - 1)) === 0
    const firstDominantElevation = dominantElevation(first)
    const secondDominantElevation = dominantElevation(second)
    const sharesNearFlatElevation = proposal.anchoredRegionCount === 1
      && firstDominantElevation != null && firstDominantElevation === secondDominantElevation
    const locallyBounded = proposal.anchoredRegionCount === 1
      ? withinSpanRadius(first, boundaries[proposal.index])
        && withinSpanRadius(second, boundaries[proposal.index])
      : withinRadius(first, proposal.center)
        && withinRadius(second, proposal.center)
    return cells > 0
      && !first?.hasRamp
      && !second?.hasRamp
      && locallyBounded
      && (strictlyFlat || sharesNearFlatElevation)
  }).map(proposal => ({ ...proposal, type: 'union' }))
  const bridgeRanked = bridgeProposals.filter(proposal => {
    const stats = regionStats.get(proposal.pocket)
    return stats?.cells > 0
      && !stats.hasRamp
      && withinRadius(stats, proposal.center)
      && stats.elevations > 0
      && (stats.elevations & (stats.elevations - 1)) === 0
  })
  const ranked = [...unionRanked, ...bridgeRanked].sort((left, right) =>
    boundaries[right.index].widthPixels - boundaries[left.index].widthPixels
    || compareEndpoints(canonicalEndpoints(boundaries[left.index].endpoints), canonicalEndpoints(boundaries[right.index].endpoints)))

  const removed = new Set()
  const reservedPorts = new Set()
  const reservedUnionRegions = new Set()
  const reservedBridgePockets = new Set()
  const reservedBridgeExteriors = new Set()
  for (const proposal of ranked) {
    if ([...proposal.ports].some(port => removed.has(port) || reservedPorts.has(port))) continue
    if (proposal.type === 'union') {
      if (reservedPorts.has(proposal.index)
        || proposal.pair.some(region => reservedUnionRegions.has(region)
          || reservedBridgePockets.has(region) || reservedBridgeExteriors.has(region))) continue
      removed.add(proposal.index)
      for (const region of proposal.pair) reservedUnionRegions.add(region)
    } else {
      if (reservedPorts.has(proposal.index)
        || reservedUnionRegions.has(proposal.pocket) || reservedUnionRegions.has(proposal.exterior)
        || reservedBridgePockets.has(proposal.pocket) || reservedBridgePockets.has(proposal.exterior)
        || reservedBridgeExteriors.has(proposal.pocket)) continue
      removed.add(proposal.index)
      reservedBridgePockets.add(proposal.pocket)
      reservedBridgeExteriors.add(proposal.exterior)
    }
    for (const port of proposal.ports) reservedPorts.add(port)
  }
  return {
    boundaries: result.filter((_, index) => !removed.has(index))
      .sort((left, right) => compareEndpoints(left.endpoints, right.endpoints)),
    removedCount: removed.size,
  }
}
