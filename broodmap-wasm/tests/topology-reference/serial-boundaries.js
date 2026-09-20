// Frozen JavaScript reference from checkpoint 0125c2b; test use only.
const MAX_GROUP_SPANS = 4
const MIN_GROUP_SPANS = 3
const MAX_CHAIN_DISTANCE_PIXELS = 384
const WIDTH_TOLERANCE_PIXELS = 8
const GEOMETRY_TOLERANCE_PIXELS = 16
const FLAT_DUPLICATE_DISTANCE_PIXELS = 128
const FLAT_ENVELOPE_TOLERANCE_PIXELS = 32
const PARALLEL_COSINE = Math.cos(15 * Math.PI / 180)

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

function compareSources(left, right) {
  const count = Math.min(left.length, right.length)
  for (let index = 0; index < count; index++) {
    const first = left[index]
    const second = right[index]
    if (first[0] !== second[0]) return first[0] - second[0]
    if (first[1] !== second[1]) return first[1] - second[1]
  }
  return left.length - right.length
}

function sortedSources(sources) {
  const result = new Map()
  for (const source of sources || []) {
    if (!Array.isArray(source) || source.length < 2) continue
    const pair = [source[0], source[1]]
    result.set(`${pair[0]}:${pair[1]}`, pair)
  }
  return [...result.values()].sort((left, right) => left[0] - right[0] || left[1] - right[1])
}

function numericWidth(boundary) {
  return Number.isFinite(boundary.widthPixels) && boundary.widthPixels > 0
    ? boundary.widthPixels
    : Infinity
}

function midpoint(boundary) {
  return [
    (boundary.endpoints[0][0] + boundary.endpoints[1][0]) / 2,
    (boundary.endpoints[0][1] + boundary.endpoints[1][1]) / 2,
  ]
}

function distance(left, right) {
  return Math.hypot(left[0] - right[0], left[1] - right[1])
}

function hasUsableSpan(boundary) {
  if (!Array.isArray(boundary?.endpoints) || boundary.endpoints.length !== 2
    || !boundary.endpoints.every(point => Array.isArray(point) && point.length === 2
      && Number.isFinite(point[0]) && Number.isFinite(point[1]))) return false
  return numericWidth(boundary) !== Infinity
    && distance(boundary.endpoints[0], boundary.endpoints[1]) > 0
}

function cloneObservation(observation) {
  const result = { ...observation }
  if (Array.isArray(observation?.endpoints) && observation.endpoints.length === 2) {
    result.endpoints = canonicalEndpoints(observation.endpoints)
  }
  if (Array.isArray(observation?.sources)) result.sources = sortedSources(observation.sources)
  return result
}

function compareObservations(left, right) {
  const leftEndpoints = left.endpoints || [[Infinity, Infinity], [Infinity, Infinity]]
  const rightEndpoints = right.endpoints || [[Infinity, Infinity], [Infinity, Infinity]]
  return compareEndpoints(leftEndpoints, rightEndpoints)
    || numericWidth(left) - numericWidth(right)
    || compareSources(left.sources || [], right.sources || [])
}

function cloneBoundary(boundary) {
  const result = { ...boundary, endpoints: canonicalEndpoints(boundary.endpoints) }
  if (Array.isArray(boundary.sources)) result.sources = sortedSources(boundary.sources)
  if (Array.isArray(boundary.observations)) {
    result.observations = boundary.observations.map(cloneObservation).sort(compareObservations)
  }
  return result
}

function isFullySeparating(metadata) {
  return metadata?.removedEdgeCount > 0
    && metadata.removedEdgeCount === metadata.separatedEdgeCount
    && Array.isArray(metadata.regionPairs)
    && metadata.regionPairs.length === 1
    && Array.isArray(metadata.regionPairs[0])
    && metadata.regionPairs[0].length === 2
    && metadata.regionPairs[0][0] !== metadata.regionPairs[0][1]
}

function addIncident(incidents, region, boundaryIndex) {
  let values = incidents.get(region)
  if (!values) {
    values = new Set()
    incidents.set(region, values)
  }
  values.add(boundaryIndex)
}

function fullGraph(boundaries, metadata) {
  const incidents = new Map()
  const adjacency = new Map()
  const addAdjacent = (region, record) => {
    let values = adjacency.get(region)
    if (!values) {
      values = []
      adjacency.set(region, values)
    }
    values.push(record)
  }
  for (let boundaryIndex = 0; boundaryIndex < boundaries.length; boundaryIndex++) {
    for (const pair of metadata[boundaryIndex]?.regionPairs || []) {
      if (!Array.isArray(pair) || pair.length !== 2) continue
      const [left, right] = pair
      addIncident(incidents, left, boundaryIndex)
      addIncident(incidents, right, boundaryIndex)
      const record = { boundaryIndex, left, right }
      addAdjacent(left, record)
      addAdjacent(right, record)
    }
  }
  return { incidents, adjacency }
}

function bridgeBoundaryIndices(graph) {
  const discovery = new Map()
  const low = new Map()
  const bridges = new Set()
  let time = 0
  for (const root of graph.adjacency.keys()) {
    if (discovery.has(root)) continue
    discovery.set(root, ++time)
    low.set(root, time)
    const stack = [{ region: root, parent: null, next: 0 }]
    while (stack.length) {
      const frame = stack[stack.length - 1]
      const adjacent = graph.adjacency.get(frame.region) || []
      if (frame.next < adjacent.length) {
        const record = adjacent[frame.next++]
        if (record === frame.parent) continue
        const other = record.left === frame.region ? record.right : record.left
        if (!discovery.has(other)) {
          discovery.set(other, ++time)
          low.set(other, time)
          stack.push({ region: other, parent: record, next: 0 })
        } else {
          low.set(frame.region, Math.min(low.get(frame.region), discovery.get(other)))
        }
        continue
      }
      stack.pop()
      if (!frame.parent) continue
      const parent = frame.parent.left === frame.region ? frame.parent.right : frame.parent.left
      low.set(parent, Math.min(low.get(parent), low.get(frame.region)))
      if (low.get(frame.region) > discovery.get(parent)) bridges.add(frame.parent.boundaryIndex)
    }
  }
  return bridges
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

function candidateLinks(boundaries, metadata, graph, anchoredRegions) {
  const links = []
  for (const [region, incident] of graph.incidents) {
    if (anchoredRegions.has(region) || incident.size !== 2) continue
    const indices = [...incident].sort((left, right) =>
      compareEndpoints(boundaries[left].endpoints, boundaries[right].endpoints))
    if (!indices.every(index => hasUsableSpan(boundaries[index])
      && boundaries[index].kind !== 'ramp'
      && isFullySeparating(metadata[index]))) continue
    const pairs = indices.map(index => metadata[index].regionPairs[0])
    if (!pairs.every(pair => pair[0] === region || pair[1] === region)) continue
    const centers = indices.map(index => midpoint(boundaries[index]))
    const dx = centers[1][0] - centers[0][0]
    const dy = centers[1][1] - centers[0][1]
    const centerDistance = Math.hypot(dx, dy)
    if (!centerDistance) continue
    const unit = [dx / centerDistance, dy / centerDistance]
    const endpoints = indices.map(index => boundaries[index].endpoints)
    const spanVector = endpoints[0][1].map((coordinate, axis) => coordinate - endpoints[0][0][axis])
    const spanLength = Math.hypot(...spanVector)
    const spanUnit = spanVector.map(coordinate => coordinate / spanLength)
    const spanNormal = [-spanUnit[1], spanUnit[0]]
    const projections = endpoints.flat().map(point => [
      point[0] * spanUnit[0] + point[1] * spanUnit[1],
      point[0] * spanNormal[0] + point[1] * spanNormal[1],
    ])
    const flatBounds = [0, 1].map(axis => [
      Math.min(...projections.map(point => point[axis])),
      Math.max(...projections.map(point => point[axis])),
    ])
    links.push({
      region,
      nodes: indices,
      spanUnit,
      spanNormal,
      flatBounds,
      insideFlatEnvelope: true,
      centers,
      centerDistance,
      unit,
      normal: [-unit[1], unit[0]],
      minWidth: Math.min(...indices.map(index => numericWidth(boundaries[index]))),
      longitudinalMin: Infinity,
      longitudinalMax: -Infinity,
      transverseMin: Infinity,
      transverseMax: -Infinity,
    })
  }
  return links
}

function measureLinks(links, labels, width, terrainFlags) {
  if (!links.length) return []
  const descriptors = new Map(links.map(link => [link.region, link]))
  for (let index = 0; index < labels.length; index++) {
    const descriptor = descriptors.get(labels[index])
    if (!descriptor) continue
    if (terrainFlags) {
      const flags = terrainFlags[index]
      descriptor.hasRamp ||= (flags & 4) !== 0
      const elevation = (flags >> 3) & 3
      descriptor.elevations = (descriptor.elevations || 0) | (1 << elevation)
    }
    const x = (index % width) * 8 + 4
    const y = Math.floor(index / width) * 8 + 4
    if (terrainFlags && descriptor.insideFlatEnvelope) {
      for (const [axis, vector] of [descriptor.spanUnit, descriptor.spanNormal].entries()) {
        const projection = x * vector[0] + y * vector[1]
        const radius = 4 * (Math.abs(vector[0]) + Math.abs(vector[1]))
        // Allow wall nooks along the span, but keep the component close to the slab between
        // the cuts. This axis is independent of any sideways shift between their midpoints.
        const tolerance = axis === 0
          ? Math.max(FLAT_ENVELOPE_TOLERANCE_PIXELS, descriptor.minWidth / 4)
          : FLAT_ENVELOPE_TOLERANCE_PIXELS
        if (projection - radius < descriptor.flatBounds[axis][0] - tolerance
          || projection + radius > descriptor.flatBounds[axis][1] + tolerance) {
          descriptor.insideFlatEnvelope = false
        }
      }
    }
    const relativeX = x - descriptor.centers[0][0]
    const relativeY = y - descriptor.centers[0][1]
    const longitudinal = relativeX * descriptor.unit[0] + relativeY * descriptor.unit[1]
    const transverse = relativeX * descriptor.normal[0] + relativeY * descriptor.normal[1]
    descriptor.longitudinalMin = Math.min(descriptor.longitudinalMin, longitudinal)
    descriptor.longitudinalMax = Math.max(descriptor.longitudinalMax, longitudinal)
    descriptor.transverseMin = Math.min(descriptor.transverseMin, transverse)
    descriptor.transverseMax = Math.max(descriptor.transverseMax, transverse)
  }
  return links.filter(link => {
    if (!Number.isFinite(link.transverseMin)) return false
    const cellExtent = 8 * (Math.abs(link.normal[0]) + Math.abs(link.normal[1]))
    const longitudinalExtent = 8 * (Math.abs(link.unit[0]) + Math.abs(link.unit[1]))
    const transverseEnvelope = link.transverseMax - link.transverseMin + cellExtent
    const longitudinalEnvelope = link.longitudinalMax - link.longitudinalMin + longitudinalExtent
    return transverseEnvelope <= 1.5 * link.minWidth + GEOMETRY_TOLERANCE_PIXELS
      && longitudinalEnvelope <= link.centerDistance + link.minWidth / 2 + GEOMETRY_TOLERANCE_PIXELS
  })
}

function connectedLinkGroups(links) {
  const byNode = new Map()
  for (const link of links) {
    for (const node of link.nodes) {
      let values = byNode.get(node)
      if (!values) {
        values = []
        byNode.set(node, values)
      }
      values.push(link)
    }
  }
  const seen = new Set()
  const groups = []
  for (const first of links) {
    if (seen.has(first)) continue
    const queue = [first]
    const groupLinks = []
    const nodes = new Set()
    seen.add(first)
    while (queue.length) {
      const link = queue.pop()
      groupLinks.push(link)
      for (const node of link.nodes) {
        nodes.add(node)
        for (const adjacent of byNode.get(node)) {
          if (!seen.has(adjacent)) {
            seen.add(adjacent)
            queue.push(adjacent)
          }
        }
      }
    }
    groups.push({ links: groupLinks, nodes })
  }
  return groups
}

function groupIsSimplePath(group, metadata, minimumSpans = MIN_GROUP_SPANS) {
  if (group.nodes.size < minimumSpans || group.nodes.size > MAX_GROUP_SPANS) return false
  if (group.links.length !== group.nodes.size - 1) return false
  const degree = new Map([...group.nodes].map(node => [node, 0]))
  for (const link of group.links) {
    degree.set(link.nodes[0], degree.get(link.nodes[0]) + 1)
    degree.set(link.nodes[1], degree.get(link.nodes[1]) + 1)
  }
  const ends = [...degree].filter(([, value]) => value === 1).map(([node]) => node)
  if (ends.length !== 2 || [...degree.values()].some(value => value > 2 || value === 0)) return false
  const exteriorRegion = (node, connectedRegion) => {
    const [left, right] = metadata[node].regionPairs[0]
    return left === connectedRegion ? right : left
  }
  const endLink = end => group.links.find(link => link.nodes.includes(end))
  const outer = ends.map(end => exteriorRegion(end, endLink(end).region))
  return outer[0] !== outer[1]
}


function nearbyParallelSpans(left, right) {
  const a = canonicalEndpoints(left.endpoints)
  const b = canonicalEndpoints(right.endpoints)
  const av = [a[1][0] - a[0][0], a[1][1] - a[0][1]]
  const bv = [b[1][0] - b[0][0], b[1][1] - b[0][1]]
  const dot = av[0] * bv[0] + av[1] * bv[1]
  if (Math.abs(dot) / (Math.hypot(...av) * Math.hypot(...bv)) < PARALLEL_COSINE) return false
  if (dot < 0) b.reverse()
  return a.every((point, index) => distance(point, b[index]) <= FLAT_DUPLICATE_DISTANCE_PIXELS)
}

function representative(group, boundaries) {
  const members = [...group.nodes]
    .sort((left, right) => compareEndpoints(boundaries[left].endpoints, boundaries[right].endpoints))
  const widths = members.map(index => numericWidth(boundaries[index]))
  const minimumWidth = Math.min(...widths)
  const eligible = members.filter(index => numericWidth(boundaries[index]) <= minimumWidth + WIDTH_TOLERANCE_PIXELS)
  const centers = new Map(members.map(index => [index, midpoint(boundaries[index])]))
  return eligible.sort((left, right) => {
    const leftDistance = members.reduce((sum, member) => sum + distance(centers.get(left), centers.get(member)), 0)
    const rightDistance = members.reduce((sum, member) => sum + distance(centers.get(right), centers.get(member)), 0)
    return leftDistance - rightDistance || compareEndpoints(boundaries[left].endpoints, boundaries[right].endpoints)
  })[0]
}

function consolidatedBoundary(representativeIndex, memberIndices, boundaries) {
  const representative = cloneBoundary(boundaries[representativeIndex])
  const members = [...memberIndices].sort((left, right) => compareEndpoints(boundaries[left].endpoints, boundaries[right].endpoints))
  representative.sources = sortedSources(members.flatMap(index => boundaries[index].sources || []))
  representative.observations = members
    .flatMap(index => {
      const boundary = boundaries[index]
      return Array.isArray(boundary.observations) && boundary.observations.length
        ? boundary.observations
        : [{ endpoints: boundary.endpoints, widthPixels: boundary.widthPixels, sources: boundary.sources || [] }]
    })
    .map(cloneObservation)
    .sort(compareObservations)
  return representative
}

/**
 * Conservatively replaces a short, bridge-only chain of serial cut spans with its central narrow
 * representative. With terrain flags, also merges close parallel cuts enclosing a flat strip.
 * `partition` must describe the supplied spans, including its label raster. Flag bits 2 and 3..4
 * describe ramps and elevation; obstacle-dependent walkability bits are deliberately unused.
 */
export function collapseSerialBoundaries(boundaries, partition, bases, width, terrainFlags = null) {
  if (!Array.isArray(boundaries) || !partition || !Number.isInteger(width) || width <= 0) {
    throw new TypeError('boundaries, partition, and a positive walk-grid width are required')
  }
  const labels = partition.labels
  if (!(labels instanceof Uint32Array) || labels.length % width !== 0) {
    throw new TypeError('partition.labels must be a Uint32Array whose length is divisible by width')
  }
  if (terrainFlags !== null
    && (!(terrainFlags instanceof Uint8Array) || terrainFlags.length !== labels.length)) {
    throw new TypeError('terrainFlags must be a Uint8Array matching partition.labels')
  }
  const metadata = Array.isArray(partition.boundaries) ? partition.boundaries : []
  const result = boundaries.map(cloneBoundary)
  if (metadata.length !== boundaries.length) return result.sort((left, right) => compareEndpoints(left.endpoints, right.endpoints))

  const graph = fullGraph(boundaries, metadata)
  const bridges = bridgeBoundaryIndices(graph)
  const links = candidateLinks(boundaries, metadata, graph, baseRegions(labels, bases, width))
  const passingLinks = new Set(measureLinks(links, labels, width, terrainFlags))
  const removed = new Set()
  const replacements = new Map()

  for (const group of connectedLinkGroups(links.filter(link => link.nodes.every(node => bridges.has(node))))) {
    if (group.links.some(link => !passingLinks.has(link))) continue
    if (!groupIsSimplePath(group, metadata)) continue
    const totalDistance = group.links.map(link => link.centerDistance)
      .sort((left, right) => left - right).reduce((sum, value) => sum + value, 0)
    const endNodes = [...group.nodes].filter(node => group.links.filter(link => link.nodes.includes(node)).length === 1)
    const endDistance = distance(midpoint(boundaries[endNodes[0]]), midpoint(boundaries[endNodes[1]]))
    if (totalDistance > MAX_CHAIN_DISTANCE_PIXELS || endDistance > MAX_CHAIN_DISTANCE_PIXELS) continue
    const retained = representative(group, boundaries)
    replacements.set(retained, consolidatedBoundary(retained, group.nodes, boundaries))
    for (const member of group.nodes) if (member !== retained) removed.add(member)
  }

  if (terrainFlags) {
    // These pairs may belong to a larger junction's serial chain; require the whole enclosed
    // component to be flat, and never absorb an anchor or a branch. Complete-link proximity
    // prevents short pairwise gaps from merging a long chain transitively.
    const flatLinks = links.filter(link =>
      link.insideFlatEnvelope
      && !link.hasRamp
      && link.elevations > 0 && (link.elevations & (link.elevations - 1)) === 0
      && link.nodes.every(node => !removed.has(node) && !replacements.has(node))
      && nearbyParallelSpans(boundaries[link.nodes[0]], boundaries[link.nodes[1]]))
    for (const group of connectedLinkGroups(flatLinks)) {
      if (!groupIsSimplePath(group, metadata, 2)) continue
      const nodes = [...group.nodes]
      if (!nodes.every(left => nodes.every(right =>
        nearbyParallelSpans(boundaries[left], boundaries[right])))) continue
      const retained = representative(group, boundaries)
      replacements.set(retained, consolidatedBoundary(retained, group.nodes, boundaries))
      for (const member of group.nodes) if (member !== retained) removed.add(member)
    }
  }

  return result
    .map((boundary, index) => replacements.get(index) || boundary)
    .filter((_, index) => !removed.has(index))
    .sort((left, right) => compareEndpoints(left.endpoints, right.endpoints))
}