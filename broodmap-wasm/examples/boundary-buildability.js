// Presentation hint only: this does not test building footprints, terrain elevation,
// creep, resources, or whether a complete wall can be built across the section.
export function hasBuildableTerrainNearMidpoint(boundary, width, flags) {
  const [[ax, ay], [bx, by]] = boundary.endpoints
  const x = (ax + bx) / 2
  const y = (ay + by) / 2
  const radius = 64 // Two build tiles; use cell rectangles, not just their centers.
  const height = flags.length / width
  const minX = Math.max(0, Math.ceil((x - radius) / 8) - 1)
  const maxX = Math.min(width - 1, Math.floor((x + radius) / 8))
  const minY = Math.max(0, Math.ceil((y - radius) / 8) - 1)
  const maxY = Math.min(height - 1, Math.floor((y + radius) / 8))
  for (let cy = minY; cy <= maxY; cy++) {
    for (let cx = minX; cx <= maxX; cx++) {
      if (!(flags[cy * width + cx] & 2)) continue
      const dx = Math.max(cx * 8 - x, 0, x - (cx + 1) * 8)
      const dy = Math.max(cy * 8 - y, 0, y - (cy + 1) * 8)
      if (dx * dx + dy * dy <= radius * radius) return true
    }
  }
  return false
}
