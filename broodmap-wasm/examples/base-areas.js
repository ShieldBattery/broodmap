// Experimental orchestration; the topology and edge-cut semantics live in the Rust core.
const NEARBY_BASE_COUNT = 3

function canonicalEndpoints(endpoints) {
  const [a, b] = endpoints
  return a[0] < b[0] || (a[0] === b[0] && a[1] <= b[1]) ? [a, b] : [b, a]
}

function readPartition(analysis, endpoints) {
  const snapshot = analysis.partitionAreas(JSON.stringify(endpoints))
  try {
    return { labels: snapshot.labels(), ...JSON.parse(snapshot.metadataJson()) }
  } finally {
    snapshot.free()
  }
}

// A bounded set of nearby connections is useful for inspecting a proposed base area, but
// does not establish that every strategically relevant exit has been found.
export function inspectBaseAreas(analysis, bases, selectedIds, minWideningPercent) {
  const anchored = bases.filter((base) => base.routeAnchor !== null)
  const byId = new Map(anchored.map((base) => [base.id, base]))
  const pairs = new Map()
  const addPair = (a, b) => {
    pairs.set(`${a}:${b}`, [a, b])
    pairs.set(`${b}:${a}`, [b, a])
  }
  addPair(...selectedIds)
  for (const id of selectedIds) {
    const base = byId.get(id)
    if (!base) throw new Error('Selected base has no route anchor.')
    const distance = (other) => (other.routeAnchor[0] - base.routeAnchor[0]) ** 2 + (other.routeAnchor[1] - base.routeAnchor[1]) ** 2
    const nearby = anchored.filter((other) => other.id !== id)
      .sort((a, b) => distance(a) - distance(b) || a.id - b.id).slice(0, NEARBY_BASE_COUNT)
    for (const other of nearby) addPair(id, other.id)
  }
  // Submit the selected pair and all nearby connections together. The core shares one
  // search among destinations with the same origin while preserving directed results.
  const orderedPairs = [...pairs.values()].sort((a, b) => a[0] - b[0] || a[1] - b[1])
  const queries = orderedPairs.map(([a, b]) => [byId.get(a).routeAnchor, byId.get(b).routeAnchor])
  const results = JSON.parse(analysis.entrancesBatchJson(JSON.stringify(queries), minWideningPercent))
  const surveys = selectedIds.map((id, index) => ({
    from: index ? 'B' : 'A',
    ...results[orderedPairs.findIndex(([a, b]) => a === id && b === selectedIds[1 - index])],
  }))
  const boundaries = new Map()
  for (const [index, [a, b]] of orderedPairs.entries()) {
    const survey = results[index]
    for (const candidate of survey.candidates) {
      const endpoints = canonicalEndpoints(candidate.endpoints)
      const key = JSON.stringify(endpoints)
      if (!boundaries.has(key)) boundaries.set(key, { endpoints, widthPixels: candidate.widthPixels, sources: [] })
      boundaries.get(key).sources.push([a, b])
    }
  }
  const proposed = [...boundaries.values()]
  const original = readPartition(analysis, [])
  const partition = readPartition(analysis, proposed.map((span) => span.endpoints))
  const width = analysis.widthWalkTiles
  const labelAt = (labels, base) => labels[base.routeAnchor[1] * width + base.routeAnchor[0]]
  const counts = new Map(partition.areas.map((area) => [area.id, area.cellCount]))
  const originalCounts = new Map(original.areas.map((area) => [area.id, area.cellCount]))
  const areaExperiment = {
    terrainOnly: true,
    labels: partition.labels.buffer,
    areas: partition.areas,
    boundaries: proposed.map((span, index) => ({ ...span, ...partition.boundaries[index] })),
    surveyCount: pairs.size,
    skippedAnchorCount: bases.length - anchored.length,
    selected: selectedIds.map((id) => {
      const base = byId.get(id)
      const areaId = labelAt(partition.labels, base)
      return {
        baseId: id,
        areaId,
        cellCount: counts.get(areaId),
        originalCellCount: originalCounts.get(labelAt(original.labels, base)),
        baseIds: anchored.filter((other) => labelAt(partition.labels, other) === areaId).map((other) => other.id),
      }
    }),
  }
  return { surveys, areaExperiment }
}
