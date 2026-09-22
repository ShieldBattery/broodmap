export const MAX_WALL_BUILDINGS = 32
export const DEFAULT_WALL_WINDOW_PIXELS = 768
export const MAX_WALL_WINDOW_PIXELS = 1024

export function clampWallWindow(center, mapPixels, requested = DEFAULT_WALL_WINDOW_PIXELS) {
  const width = Math.min(Math.max(1, Math.round(requested)), MAX_WALL_WINDOW_PIXELS, mapPixels[0])
  const height = Math.min(Math.max(1, Math.round(requested)), MAX_WALL_WINDOW_PIXELS, mapPixels[1])
  const left = Math.max(0, Math.min(mapPixels[0] - width, Math.round(center[0] - width / 2)))
  const top = Math.max(0, Math.min(mapPixels[1] - height, Math.round(center[1] - height / 2)))
  return [left, top, left + width, top + height]
}

export function pointInHalfOpenRect(point, rect) {
  return point[0] >= rect[0] && point[0] < rect[2] && point[1] >= rect[1] && point[1] < rect[3]
}

export function defaultWallEndpoints(endpoints, window) {
  const [[ax, ay], [bx, by]] = endpoints
  const midX = (ax + bx) / 2
  const midY = (ay + by) / 2
  const dx = bx - ax
  const dy = by - ay
  const length = Math.hypot(dx, dy) || 1
  const offset = 96
  const normalX = -dy / length * offset
  const normalY = dx / length * offset
  const clamp = ([x, y]) => [
    Math.max(window[0], Math.min(window[2] - 1, Math.round(x))),
    Math.max(window[1], Math.min(window[3] - 1, Math.round(y))),
  ]
  return [clamp([midX - normalX, midY - normalY]), clamp([midX + normalX, midY + normalY])]
}

export function placementAtTile(building, tile) {
  return {
    id: building.id,
    name: building.name,
    tile: [tile[0], tile[1]],
    footprint: [...building.footprint],
    collision: [...building.collision],
  }
}

export function addWallBuilding(buildings, building, tile) {
  if (buildings.length >= MAX_WALL_BUILDINGS) return buildings
  return [...buildings, placementAtTile(building, tile)]
}

export function removeWallBuildingAt(buildings, tile) {
  for (let index = buildings.length - 1; index >= 0; index--) {
    const placed = buildings[index]
    if (tile[0] >= placed.tile[0] && tile[0] < placed.tile[0] + placed.footprint[0]
      && tile[1] >= placed.tile[1] && tile[1] < placed.tile[1] + placed.footprint[1]) {
      return buildings.filter((_, candidate) => candidate !== index)
    }
  }
  return buildings
}

export function wallQuery(window, endpoints, mover, buildings) {
  return {
    window: [...window],
    start: [...endpoints[0]],
    end: [...endpoints[1]],
    mover: [...mover.collision],
    buildings: buildings.map(({ tile, footprint, collision }) => ({
      tile: [...tile], footprint: [...footprint], collision: [...collision],
    })),
  }
}

// The cursor selects the nearest tile-aligned building center, including odd-sized footprints.
export function centeredBuildingTile(point, footprint) {
  return point.map((value, axis) => Math.max(0, Math.round(value / 32 - footprint[axis] / 2)))
}

// Fast visual hints only. The Rust checker owns exact static rectangle validation.
export function placementPreviewCells(tile, footprint, terrain, buildings) {
  const cells = []
  for (let y = tile[1]; y < tile[1] + footprint[1]; y++) {
    for (let x = tile[0]; x < tile[0] + footprint[0]; x++) {
      let valid = x >= 0 && y >= 0 && (x + 1) * 4 <= terrain.widthWalkTiles && (y + 1) * 4 <= terrain.heightWalkTiles
      if (valid) {
        for (let wy = y * 4; wy < (y + 1) * 4; wy++) {
          for (let wx = x * 4; wx < (x + 1) * 4; wx++) {
            const flags = terrain.flags[wy * terrain.widthWalkTiles + wx]
            if ((flags & 3) !== 3 || (terrain.obstaclesEnabled && (flags & 32))) valid = false
          }
        }
      }
      if (buildings.some(b => x >= b.tile[0] && x < b.tile[0] + b.footprint[0]
        && y >= b.tile[1] && y < b.tile[1] + b.footprint[1])) valid = false
      cells.push({ x, y, valid })
    }
  }
  return cells
}

// Selection uses distance to the finite segment, not its midpoint (important for wide mouths).
export function nearestWallSpan(point, spans) {
  let nearest = -1, best = Infinity
  spans.forEach((span, index) => {
    const distance = wallSegmentDistanceSquared(point, span.endpoints)
    if (distance < best) { best = distance; nearest = index }
  })
  return nearest
}

// Choosing a ramp side selects an existing structural end, without modifying topology.
export function wallSearchSide(span, spans, side) {
  if (span?.kind !== 'ramp' || side === 'any') return { span, elevation: null }
  const end = side === 'low' ? 'lower' : 'upper'
  return {
    span: spans.find(other => other.kind === 'ramp' && other.rampId === span.rampId && other.rampEnd === end) || span,
    elevation: side === 'low' ? span.lowerElevation : span.upperElevation,
  }
}

export function wallPlacementArea(window, endpoints, size) {
  const center = endpoints[0].map((v, i) => (v + endpoints[1][i]) / 2)
  const w = Math.min(size, window[2] - window[0]), h = Math.min(size, window[3] - window[1])
  const left = Math.max(window[0], Math.min(window[2] - w, Math.round(center[0] - w / 2)))
  const top = Math.max(window[1], Math.min(window[3] - h, Math.round(center[1] - h / 2)))
  return [left, top, left + w, top + h]
}

// Radius is measured from the finite entrance to a proposed building's center. Its full
// footprint may extend past this radius, but must still fit inside the routing window.
export function wallPlacementSite(span, size) {
  return span ? { entrance: span.endpoints, maxDistancePixels: size / 2 } : null
}

export function pointInWallPlacementSite(point, site) {
  return wallSegmentDistanceSquared(point, site.entrance) <= site.maxDistancePixels ** 2
}

function wallSegmentDistanceSquared(point, [a, b]) {
  const dx = b[0] - a[0], dy = b[1] - a[1]
  const t = Math.max(0, Math.min(1, ((point[0] - a[0]) * dx + (point[1] - a[1]) * dy) / (dx * dx + dy * dy || 1)))
  return (point[0] - a[0] - dx * t) ** 2 + (point[1] - a[1] - dy * t) ** 2
}

// The worker supplies this local walkcell mask from the core terrain-connectivity rule.
// This is only a lookup; exact radius and full-footprint legality are separate checks.
export function pointInWallPlacementReach(point, reach) {
  if (!reach) return false
  const x = Math.floor(point[0] / 8) - reach.origin[0]
  const y = Math.floor(point[1] / 8) - reach.origin[1]
  return x >= 0 && y >= 0 && x < reach.width && y < reach.height
    && reach.cells[y * reach.width + x] === 1
}
