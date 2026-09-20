// Frozen JavaScript reference from checkpoint 0125c2b; test use only.
import { hasBuildableTerrainNearMidpoint } from '../../examples/boundary-buildability.js'
import { collapseSerialBoundaries } from './serial-boundaries.js'
import { selectNonCrossingBoundaries } from './boundary-conflicts.js'
import { removeJunctionBoundaries } from './junction-boundaries.js'
import { collapseRampInteriors, replaceRampMouths } from './ramp-boundaries.js'

// Experimental map-wide orchestration. Rust owns route and edge-cut semantics.
const NEARBY_BASE_COUNT = 3
const MAX_BATCH_QUERIES = 256
const MAX_BOUNDARY_SPANS = 256
const NEAR_DUPLICATE_DISTANCE_PIXELS = 32
const PARALLEL_COSINE = Math.cos(15 * Math.PI / 180)

function canonicalEndpoints(endpoints) {
  const [a, b] = endpoints
  return a[0] < b[0] || (a[0] === b[0] && a[1] <= b[1]) ? [a, b] : [b, a]
}

function compareEndpoints(left, right) {
  for (let index = 0; index < 2; index++) {
    for (let coordinate = 0; coordinate < 2; coordinate++) {
      const difference = left[index][coordinate] - right[index][coordinate]
      if (difference) return difference
    }
  }
  return 0
}

function compareSpans(left, right) {
  return compareEndpoints(left.endpoints, right.endpoints)
    || numericWidth(left) - numericWidth(right)
    || compareSources(left.sources, right.sources)
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

function numericWidth(span) {
  return Number.isFinite(span.widthPixels) ? span.widthPixels : Infinity
}

function chooseRepresentative(left, right) {
  const byWidth = numericWidth(left) - numericWidth(right)
  return byWidth < 0 || (byWidth === 0 && compareEndpoints(left.endpoints, right.endpoints) <= 0)
    ? left
    : right
}

function readPartition(analysis, endpoints) {
  const snapshot = analysis.partitionAreas(JSON.stringify(endpoints))
  try {
    return { labels: snapshot.labels(), ...JSON.parse(snapshot.metadataJson()) }
  } finally {
    snapshot.free()
  }
}

function componentAt(labels, width, base) {
  return labels[base.routeAnchor[1] * width + base.routeAnchor[0]]
}

function distanceSquared(left, right) {
  return (left.routeAnchor[0] - right.routeAnchor[0]) ** 2
    + (left.routeAnchor[1] - right.routeAnchor[1]) ** 2
}

function sortedSources(sources) {
  return [...new Map(sources.map(source => [`${source[0]}:${source[1]}`, source])).values()]
    .sort((left, right) => left[0] - right[0] || left[1] - right[1])
}

function sameLineDirection(left, right) {
  const dx = left.endpoints[1][0] - left.endpoints[0][0]
  const dy = left.endpoints[1][1] - left.endpoints[0][1]
  const otherDx = right.endpoints[1][0] - right.endpoints[0][0]
  const otherDy = right.endpoints[1][1] - right.endpoints[0][1]
  const length = Math.hypot(dx, dy)
  const otherLength = Math.hypot(otherDx, otherDy)
  return length > 0 && otherLength > 0
    && Math.abs(dx * otherDx + dy * otherDy) / (length * otherLength) >= PARALLEL_COSINE
}

function correspondingEndpointsAreNear(left, right) {
  return left.endpoints.every((endpoint, index) =>
    Math.hypot(endpoint[0] - right.endpoints[index][0], endpoint[1] - right.endpoints[index][1])
      <= NEAR_DUPLICATE_DISTANCE_PIXELS)
}

function anchorLiesBetween(left, right, anchors) {
  const [leftStart, leftEnd] = left.endpoints
  let [rightStart, rightEnd] = right.endpoints
  const leftDirection = [leftEnd[0] - leftStart[0], leftEnd[1] - leftStart[1]]
  let rightDirection = [rightEnd[0] - rightStart[0], rightEnd[1] - rightStart[1]]
  if (leftDirection[0] * rightDirection[0] + leftDirection[1] * rightDirection[1] < 0) {
    [rightStart, rightEnd] = [rightEnd, rightStart]
    rightDirection = [-rightDirection[0], -rightDirection[1]]
  }
  const leftLength = Math.hypot(...leftDirection)
  const rightLength = Math.hypot(...rightDirection)
  if (!leftLength || !rightLength) return false
  const cross = (direction, origin, point) =>
    direction[0] * (point[1] - origin[1]) - direction[1] * (point[0] - origin[0])
  const withinExtent = (start, direction, length, point) => {
    const projection = ((point[0] - start[0]) * direction[0] + (point[1] - start[1]) * direction[1]) / length
    return 0 <= projection && projection <= length
  }
  return anchors.some(base => {
    const point = [base.routeAnchor[0] * 8 + 4, base.routeAnchor[1] * 8 + 4]
    const leftSide = cross(leftDirection, leftStart, point)
    const rightSide = cross(rightDirection, rightStart, point)
    return leftSide * rightSide < 0
      && withinExtent(leftStart, leftDirection, leftLength, point)
      && withinExtent(rightStart, rightDirection, rightLength, point)
  })
}

function canConsolidate(left, right, anchors) {
  return sameLineDirection(left, right)
    && correspondingEndpointsAreNear(left, right)
    && !anchorLiesBetween(left, right, anchors)
    && !anchorLiesBetween(right, left, anchors)
}

function consolidateObservations(observations, anchors) {
  const groups = []
  for (const observation of observations) {
    const group = groups.find(candidate => candidate.members.every(member =>
      canConsolidate(observation, member, anchors)))
    if (group) {
      group.members.push(observation)
      group.representative = chooseRepresentative(group.representative, observation)
    } else {
      groups.push({ members: [observation], representative: observation })
    }
  }
  return groups.map(group => ({
    endpoints: group.representative.endpoints,
    widthPixels: group.representative.widthPixels,
    sources: sortedSources(group.members.flatMap(member => member.sources)),
    observations: group.members.map(({ endpoints, widthPixels, sources }) => ({ endpoints, widthPixels, sources })),
  })).sort(compareSpans)
}

function collectObservation(observations, endpoints, widthPixels, source) {
  const canonical = canonicalEndpoints(endpoints)
  const key = JSON.stringify(canonical)
  const existing = observations.get(key)
  const candidate = { endpoints: canonical, widthPixels, sources: [source] }
  if (!existing) {
    observations.set(key, candidate)
    return
  }
  existing.sources.push(source)
  const representative = chooseRepresentative(existing, candidate)
  existing.widthPixels = representative.widthPixels
}

/**
 * Surveys a bounded, map-wide set of terrain-only base connections and partitions using the
 * resulting candidate spans. This observes three nearest anchors in each base's original positive
 * terrain component, not every exit or a walling solution. Work is cancellation-friendly only at
 * supplied checkpoints; no partial partition is returned after a batch error or cancellation.
 */
export async function analyzeBaseAreas(
  analysis,
  bases,
  minWideningPercent,
  { onProgress = () => {}, checkpoint = async () => {} } = {},
) {
  const original = readPartition(analysis, [])
  const width = analysis.widthWalkTiles
  const anchored = bases
    .filter(base => base.routeAnchor !== null)
    .sort((left, right) => left.id - right.id)
    .map(base => ({ ...base, originalAreaId: componentAt(original.labels, width, base) }))
  const directedPairs = new Map()
  const addBothDirections = (left, right) => {
    directedPairs.set(`${left.id}:${right.id}`, [left.id, right.id])
    directedPairs.set(`${right.id}:${left.id}`, [right.id, left.id])
  }
  for (const origin of anchored) {
    if (!origin.originalAreaId) continue
    const destinations = anchored
      .filter(other => other.id !== origin.id && other.originalAreaId === origin.originalAreaId)
      .sort((left, right) => distanceSquared(origin, left) - distanceSquared(origin, right) || left.id - right.id)
      .slice(0, NEARBY_BASE_COUNT)
    for (const destination of destinations) addBothDirections(origin, destination)
  }
  const byId = new Map(anchored.map(base => [base.id, base]))
  const observations = new Map()
  let surveyCount = 0

  for (let originIndex = 0; originIndex < anchored.length; originIndex++) {
    const origin = anchored[originIndex]
    const destinations = [...directedPairs.values()]
      .filter(([from]) => from === origin.id)
      .map(([, destination]) => byId.get(destination))
      .sort((left, right) => left.id - right.id)
    surveyCount += destinations.length
    for (let offset = 0; offset < destinations.length; offset += MAX_BATCH_QUERIES) {
      const chunk = destinations.slice(offset, offset + MAX_BATCH_QUERIES)
      await checkpoint()
      const queries = chunk.map(destination => [origin.routeAnchor, destination.routeAnchor])
      const results = JSON.parse(analysis.entrancesBatchJson(JSON.stringify(queries), minWideningPercent))
      if (!Array.isArray(results) || results.length !== chunk.length) {
        throw new Error(`entrance batch returned ${Array.isArray(results) ? results.length : 'a non-array'} result(s) for ${chunk.length} query(ies)`)
      }
      for (const [index, survey] of results.entries()) {
        for (const candidate of survey.candidates || []) {
          collectObservation(observations, candidate.endpoints, candidate.widthPixels, [origin.id, chunk[index].id])
        }
      }
    }
    onProgress({ completedOrigins: originIndex + 1, totalOrigins: anchored.length, surveyCount })
  }

  const raw = [...observations.values()]
    .map(observation => ({ ...observation, sources: sortedSources(observation.sources) }))
    .sort(compareSpans)
  let boundaries = consolidateObservations(raw, anchored)
  if (boundaries.length > MAX_BOUNDARY_SPANS) {
    throw new Error(`base-area consolidation produced ${boundaries.length} boundaries; at most ${MAX_BOUNDARY_SPANS} are supported`)
  }

  const ramps = JSON.parse(analysis.rampsJson()).ramps
  for (const ramp of ramps) {
    for (const end of ['lower', 'upper']) {
      const span = ramp[end]
      boundaries.push({
        kind: 'ramp',
        rampId: ramp.id,
        rampEnd: end,
        lowerElevation: ramp.lowerElevation,
        upperElevation: ramp.upperElevation,
        endpoints: canonicalEndpoints(span.endpoints),
        widthPixels: span.widthPixels,
        sources: [],
        observations: [],
      })
    }
  }
  if (boundaries.length > MAX_BOUNDARY_SPANS) {
    throw new Error(`base-area analysis produced ${boundaries.length} entrance and ramp boundaries; at most ${MAX_BOUNDARY_SPANS} are supported`)
  }
  const nonCrossing = selectNonCrossingBoundaries(boundaries)
  boundaries = nonCrossing.boundaries
  await checkpoint()
  let partition = readPartition(analysis, boundaries.map(boundary => boundary.endpoints))
  await checkpoint()
  let rampMouthsRemoved = 0
  // Removing a near-coincident mouth can expose a second redundant approach farther in the base.
  // Refresh connectivity and anchor membership between passes; bound this to two repartitions.
  for (let pass = 0; pass < 2; pass++) {
    const rampMouths = replaceRampMouths(boundaries, partition, anchored, width)
    if (!rampMouths.removedCount) break
    boundaries = rampMouths.boundaries
    rampMouthsRemoved += rampMouths.removedCount
    await checkpoint()
    partition = readPartition(analysis, boundaries.map(boundary => boundary.endpoints))
  }
  const terrainFlags = analysis.cellFlags()
  const rampInteriors = collapseRampInteriors(boundaries, partition, anchored, width, terrainFlags)
  boundaries = rampInteriors.boundaries
  if (rampInteriors.removedCount) {
    await checkpoint()
    partition = readPartition(analysis, boundaries.map(boundary => boundary.endpoints))
  }
  const nearbyBoundaryCount = boundaries.length
  boundaries = collapseSerialBoundaries(boundaries, partition, anchored, width, terrainFlags)
  const serialCutsRemoved = nearbyBoundaryCount - boundaries.length
  if (boundaries.length !== nearbyBoundaryCount) {
    // Provisional cuts establish corridor connectivity; only retained cuts define the result.
    await checkpoint()
    partition = readPartition(analysis, boundaries.map(boundary => boundary.endpoints))
  }
  await checkpoint()
  const junctions = removeJunctionBoundaries(boundaries, partition, anchored, width, terrainFlags)
  boundaries = junctions.boundaries
  if (junctions.removedCount) {
    await checkpoint()
    partition = readPartition(analysis, boundaries.map(boundary => boundary.endpoints))
  }
  const counts = new Map(partition.areas.map(area => [area.id, area.cellCount]))
  const originalCounts = new Map(original.areas.map(area => [area.id, area.cellCount]))
  const baseAreas = anchored.map(base => {
    const areaId = componentAt(partition.labels, width, base)
    return {
      baseId: base.id,
      areaId,
      cellCount: counts.get(areaId),
      originalCellCount: originalCounts.get(base.originalAreaId),
      baseIds: anchored
        .filter(other => componentAt(partition.labels, width, other) === areaId)
        .map(other => other.id),
    }
  })
  return {
    terrainOnly: true,
    labels: partition.labels.buffer,
    areas: partition.areas,
    boundaries: boundaries.map((boundary, index) => ({
      ...boundary,
      ...partition.boundaries[index],
      buildableNearMidpoint: hasBuildableTerrainNearMidpoint(boundary, width, terrainFlags),
    })),
    surveyCount,
    skippedAnchorCount: bases.length - anchored.length,
    baseAreas,
    observationCount: raw.length,
    consolidatedCount: boundaries.length,
    serialCutsRemoved,
    junctionCutsRemoved: junctions.removedCount,
    crossingCutsRemoved: nonCrossing.removedCount,
    rampCount: ramps.length,
    rampMouthsRemoved,
    rampInteriorCutsRemoved: rampInteriors.removedCount,
  }
}