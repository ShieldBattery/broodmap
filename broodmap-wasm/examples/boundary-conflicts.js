/**
 * Selects a deterministic, non-crossing subset of already bounded candidate spans.
 *
 * Callers retain the original candidates for inspection and enforce their own (currently 256)
 * boundary limit before this selector. This selector intentionally neither merges observations nor
 * tries to resolve touching, partially overlapping, or parallel spans; those are separate policies.
 * Exact geometry duplicates are retained once, with structural ramp evidence taking priority.
 */
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

function copyBoundary(boundary) {
  if (!usableBoundary(boundary)) {
    throw new TypeError('each boundary must have a finite positive width and two finite endpoints')
  }
  return { ...boundary, endpoints: canonicalEndpoints(boundary.endpoints) }
}

function orientation(first, second, point) {
  return (second[0] - first[0]) * (point[1] - first[1])
    - (second[1] - first[1]) * (point[0] - first[0])
}

/** True only when two segment interiors cross. Endpoint touches and collinear spans are excluded. */
function properlyCrosses(left, right) {
  const [a, b] = left.endpoints
  const [c, d] = right.endpoints
  const leftC = orientation(a, b, c)
  const leftD = orientation(a, b, d)
  const rightA = orientation(c, d, a)
  const rightB = orientation(c, d, b)
  return ((leftC < 0 && leftD > 0) || (leftC > 0 && leftD < 0))
    && ((rightA < 0 && rightB > 0) || (rightA > 0 && rightB < 0))
}

/**
 * Greedily keeps narrow spans before broad spans. Equal-width ties use canonical numeric endpoints,
 * so endpoint direction and input order cannot alter the result for distinct spans.
 */
export function selectNonCrossingBoundaries(boundaries) {
  if (!Array.isArray(boundaries)) throw new TypeError('boundaries must be an array')
  const ordered = boundaries.map(copyBoundary).sort((left, right) =>
    Number(right.kind === 'ramp') - Number(left.kind === 'ramp')
    || left.widthPixels - right.widthPixels || compareEndpoints(left.endpoints, right.endpoints))
  const selected = []
  let removedCount = 0
  for (const boundary of ordered) {
    if (selected.some(accepted => compareEndpoints(accepted.endpoints, boundary.endpoints) === 0
      || properlyCrosses(accepted, boundary))) {
      removedCount++
    } else {
      selected.push(boundary)
    }
  }
  return { boundaries: selected.sort((left, right) => compareEndpoints(left.endpoints, right.endpoints)), removedCount }
}
