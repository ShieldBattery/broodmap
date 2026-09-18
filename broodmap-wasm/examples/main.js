// WASM state stays in worker.js: the main thread only paints transferable image buffers.
const el = (id) => document.getElementById(id)
let statusRevision = 0
const status = (message) => {
  el('status').textContent = message
  return ++statusRevision
}
const ownsStatus = (token) => token === statusRevision

let worker = null
let mapName = ''
let mapInfo = null
let previewImage = null
let nextJobId = 1
let fileNonce = 0
let renderRevision = 0
let analysisRevision = 0
let routeRevision = 0
let obstacleRevision = 0
let renderInFlight = false
let analysisInFlight = false
let routeInFlight = false
let obstacleInFlight = false
let renderStatusToken = 0
let analysisStatusToken = 0
let terrain = null
let route = null
let selected = []
const pending = new Map()

function makeWorker() {
  const nextWorker = new Worker(new URL('./worker.js', import.meta.url), { type: 'module' })
  nextWorker.onmessage = ({ data }) => {
    const job = pending.get(data.id)
    if (!job) return
    pending.delete(data.id)
    if (job.worker !== nextWorker) return
    if (data.ok) job.resolve(data.result)
    else job.reject(new Error(data.error))
  }
  nextWorker.onerror = (event) => rejectWorkerJobs(nextWorker, new Error(event.message || 'Terrain worker stopped unexpectedly.'))
  return nextWorker
}

function rejectWorkerJobs(target, reason) {
  for (const [id, job] of pending) {
    if (job.worker === target) {
      pending.delete(id)
      job.reject(reason)
    }
  }
}

function replaceWorker() {
  if (worker) {
    rejectWorkerJobs(worker, new Error('Map was replaced.'))
    worker.terminate()
  }
  worker = makeWorker()
  return worker
}

function callWorker(type, payload, transfer = []) {
  const target = worker
  if (!target) return Promise.reject(new Error('Choose a map first.'))
  const id = nextJobId++
  return new Promise((resolve, reject) => {
    pending.set(id, { worker: target, resolve, reject })
    target.postMessage({ id, type, ...payload }, transfer)
  })
}

function resolveAssetBase() {
  const raw = el('assetBase').value.trim() || './assets'
  return new URL(raw.endsWith('/') ? raw : `${raw}/`, document.baseURI).href
}

function clearCanvas(id) {
  const canvas = el(id)
  canvas.width = 0
  canvas.height = 0
}

function resetTerrain() {
  routeRevision++
  obstacleRevision++
  terrain = null
  route = null
  selected = []
  routeInFlight = false
  obstacleInFlight = false
  el('respectObstacles').checked = true
  el('respectObstacles').disabled = true
  updateObstacleMode()
  clearCanvas('terrainBackdrop')
  clearCanvas('terrainOverlay')
  el('terrainShell').style.removeProperty('aspect-ratio')
  el('terrainControls').hidden = true
  el('routeResult').textContent = 'Analyze terrain, then click two walkable cells.'
  el('hover').textContent = 'Move over the terrain view after analysis.'
}

function resetMapUi() {
  mapInfo = null
  previewImage = null
  renderRevision++
  analysisRevision++
  renderInFlight = false
  analysisInFlight = false
  clearCanvas('preview')
  clearCanvas('minimap')
  resetTerrain()
  el('render').disabled = true
  el('analyze').disabled = true
}

el('map').addEventListener('change', async (event) => {
  const file = event.target.files[0]
  if (!file) return
  const selection = ++fileNonce
  resetMapUi()
  const target = replaceWorker()
  const readStatusToken = status(`Reading ${file.name}...`)
  try {
    const bytes = await file.arrayBuffer()
    if (selection !== fileNonce || worker !== target) return
    mapName = file.name
    const result = await callWorker('loadMap', { bytes }, [bytes])
    if (worker !== target || selection !== fileNonce) return
    mapInfo = result
    drawTo(el('minimap'), result.minimap)
    el('render').disabled = false
    el('analyze').disabled = false
    if (ownsStatus(readStatusToken)) status(`${file.name}: ${result.widthTiles}x${result.heightTiles} tiles. Minimap is ready; render artwork or analyze terrain.`)
  } catch (error) {
    if (selection === fileNonce && ownsStatus(readStatusToken)) status(`Failed to parse ${file.name}: ${error.message || error}`)
  }
})

for (const id of ['style', 'size']) {
  el(id).addEventListener('input', () => {
    renderRevision++
    if (renderInFlight && ownsStatus(renderStatusToken)) {
      status('Preview settings changed. Press Render to update the preview.')
    }
  })
}
el('assetBase').addEventListener('input', () => {
  renderRevision++
  analysisRevision++
  if (analysisInFlight && ownsStatus(analysisStatusToken)) {
    status('Asset base changed. Run analysis again.')
  } else if (renderInFlight && ownsStatus(renderStatusToken)) {
    status('Asset base changed. Press Render to update the preview.')
  }
})

el('render').addEventListener('click', async () => {
  if (!mapInfo || renderInFlight) return
  const revision = renderRevision
  const target = worker
  const options = JSON.stringify({ style: el('style').value, size: Number(el('size').value) || 1024 })
  renderInFlight = true
  el('render').disabled = true
  try {
    renderStatusToken = status(`Fetching artwork for ${mapName}...`)
    const result = await callWorker('render', { options, assetBase: resolveAssetBase() })
    if (target !== worker || revision !== renderRevision) return
    previewImage = result.image
    drawTo(el('preview'), previewImage)
    if (terrain) drawTerrainBackdrop()
    const notes = [
      `${mapName}: rendered ${result.image.width}x${result.image.height} in ${result.elapsedMs} ms (${result.fetched} assets).`,
      ...(result.skipped ? [`Skipped ${result.skipped} missing graphics assets.`] : []),
      ...result.image.warnings,
    ]
    if (ownsStatus(renderStatusToken)) status(notes.join('\n'))
  } catch (error) {
    if (target === worker && revision === renderRevision && ownsStatus(renderStatusToken)) status(`Render failed: ${error.message || error}`)
  } finally {
    if (target === worker) {
      renderInFlight = false
      if (mapInfo) el('render').disabled = false
    }
  }
})

el('analyze').addEventListener('click', async () => {
  if (!mapInfo || analysisInFlight) return
  const revision = ++analysisRevision
  // Retire the displayed snapshot before a new worker snapshot can replace it.
  // An asset-source edit can discard the reply, so keeping old flags here would
  // otherwise let routes use different terrain from the visible overlay.
  resetTerrain()
  const target = worker
  analysisInFlight = true
  el('analyze').disabled = true
  analysisStatusToken = status('Fetching CV5, VF4, and units.dat analysis data...')
  try {
    const result = await callWorker('analyzeTerrain', { assetBase: resolveAssetBase() })
    if (target !== worker || revision !== analysisRevision) return
    routeRevision++
    terrain = { ...result, flags: new Uint8Array(result.flags), textures: new Map(), obstaclesEnabled: true }
    el('respectObstacles').checked = true
    el('respectObstacles').disabled = false
    updateObstacleMode()
    route = null
    selected = []
    el('terrainControls').hidden = false
    el('terrainShell').style.aspectRatio = `${terrain.widthWalkTiles} / ${terrain.heightWalkTiles}`
    drawTerrainBackdrop()
    drawOverlay()
    el('routeResult').textContent = 'Click a walkable cell for A, then another for B.'
    if (ownsStatus(analysisStatusToken)) status(`Terrain analyzed: ${terrain.widthWalkTiles}x${terrain.heightWalkTiles} walk cells; ${terrain.obstacleCount} static map blockers.`)
  } catch (error) {
    if (target === worker && revision === analysisRevision && ownsStatus(analysisStatusToken)) status(`Terrain analysis failed: ${error.message || error}`)
  } finally {
    if (target === worker) {
      analysisInFlight = false
      if (mapInfo) el('analyze').disabled = false
    }
  }
})

el('overlay').addEventListener('change', drawOverlay)
el('opacity').addEventListener('input', () => {
  el('opacityValue').value = `${el('opacity').value}%`
  drawOverlay()
})
el('respectObstacles').addEventListener('change', async () => {
  if (!terrain || obstacleInFlight) return
  const enabled = el('respectObstacles').checked
  const target = worker
  const snapshot = terrain
  const obstacleToken = ++obstacleRevision
  routeRevision++
  route = null
  selected = []
  obstacleInFlight = true
  el('respectObstacles').disabled = true
  el('analyze').disabled = true
  const obstacleStatusToken = status('Updating map obstacles...')
  el('routeResult').textContent = 'Updating map obstacles...'
  drawOverlay()
  try {
    const result = await callWorker('setObstaclesEnabled', { enabled })
    if (target !== worker || snapshot !== terrain || obstacleToken !== obstacleRevision) return
    terrain.flags = new Uint8Array(result.flags)
    terrain.obstacleCount = result.obstacleCount
    terrain.obstaclesEnabled = result.enabled
    terrain.textures = new Map()
    drawOverlay()
    updateObstacleMode()
    if (ownsStatus(obstacleStatusToken)) status(`Map obstacles ${result.enabled ? 'enabled' : 'disabled'}; ${result.obstacleCount} static map blockers identified.`)
  } catch (error) {
    if (target === worker && snapshot === terrain && obstacleToken === obstacleRevision) {
      el('respectObstacles').checked = terrain.obstaclesEnabled
      if (ownsStatus(obstacleStatusToken)) status(`Could not change map obstacles: ${error.message || error}`)
    }
  } finally {
    if (target === worker && snapshot === terrain && obstacleToken === obstacleRevision) {
      obstacleInFlight = false
      el('routeResult').textContent = 'Click a walkable cell for A, then another for B.'
      el('respectObstacles').disabled = false
      if (mapInfo && !analysisInFlight) el('analyze').disabled = false
      updateObstacleMode()
    }
  }
})

function updateObstacleMode() {
  if (!terrain) {
    el('obstacleMode').textContent = 'Map obstacle routing is available after analysis.'
  } else if (el('respectObstacles').checked) {
    el('obstacleMode').textContent = 'Map obstacles enabled: resources and grounded neutral buildings are respected.'
  } else {
    el('obstacleMode').textContent = 'Terrain-only mode: map obstacles are excluded; this does not mean every invincible object is removable.'
  }
}

el('resetRoute').addEventListener('click', () => {
  routeRevision++
  selected = []
  route = null
  drawOverlay()
  el('routeResult').textContent = 'Click a walkable cell for A, then another for B.'
})

function drawTo(canvas, image) {
  canvas.width = image.width
  canvas.height = image.height
  canvas.getContext('2d').putImageData(new ImageData(new Uint8ClampedArray(image.data), image.width, image.height), 0, 0)
}

function drawTerrainBackdrop() {
  if (terrain) drawTo(el('terrainBackdrop'), previewImage || terrain.minimap)
}

function selectedOverlayCell(event) {
  if (!terrain) return null
  const canvas = el('terrainOverlay')
  const rect = canvas.getBoundingClientRect()
  const x = Math.floor(((event.clientX - rect.left) / rect.width) * terrain.widthWalkTiles)
  const y = Math.floor(((event.clientY - rect.top) / rect.height) * terrain.heightWalkTiles)
  if (x < 0 || y < 0 || x >= terrain.widthWalkTiles || y >= terrain.heightWalkTiles) return null
  return { x, y, flags: terrain.flags[y * terrain.widthWalkTiles + x] }
}

el('terrainOverlay').addEventListener('pointermove', (event) => {
  const cell = selectedOverlayCell(event)
  if (!cell) return
  const elevation = (cell.flags >> 3) & 3
  el('hover').textContent = `Walk (${cell.x}, ${cell.y}) | pixel (${cell.x * 8 + 4}, ${cell.y * 8 + 4}) | ${cell.flags & 1 ? 'walkable' : 'blocked'}, ${cell.flags & 2 ? 'terrain-buildable' : 'terrain-unbuildable'}, ${cell.flags & 4 ? 'ramp' : 'flat'}, ${cell.flags & 32 ? 'map obstacle' : 'no map obstacle'}, elevation ${elevation}`
})

el('terrainOverlay').addEventListener('click', async (event) => {
  const cell = selectedOverlayCell(event)
  if (!cell || routeInFlight || obstacleInFlight) return
  if (!(cell.flags & 1)) {
    el('routeResult').textContent = `(${cell.x}, ${cell.y}) is blocked. Pick a resolved walkable cell.`
    return
  }
  if (selected.length >= 2) {
    routeRevision++
    selected = [cell]
    route = null
    drawOverlay()
    el('routeResult').textContent = `A = (${cell.x}, ${cell.y}). Pick B.`
    return
  }
  selected.push(cell)
  if (selected.length === 1) {
    drawOverlay()
    el('routeResult').textContent = `A = (${cell.x}, ${cell.y}). Pick B.`
    return
  }
  routeInFlight = true
  const revision = routeRevision
  const target = worker
  drawOverlay()
  try {
    const result = await callWorker('route', {
      startX: selected[0].x,
      startY: selected[0].y,
      endX: selected[1].x,
      endY: selected[1].y,
    })
    if (target !== worker || revision !== routeRevision || selected.length !== 2) return
    route = result
    const ground = result.groundDistancePixels === null
      ? 'ground: disconnected'
      : `ground: ${result.groundDistancePixels}px (${(result.groundDistancePixels / 32).toFixed(2)} tiles)`
    const routeScope = terrain.obstaclesEnabled
      ? 'Static resources and grounded neutral buildings are respected; moving units, mover size, and exact engine pathing are ignored.'
      : 'Terrain-only mode excludes map obstacles; moving units, mover size, and exact engine pathing are ignored.'
    el('routeResult').textContent = `${ground}; air: ${result.airDistancePixels.toFixed(1)}px (${(result.airDistancePixels / 32).toFixed(2)} tiles). ${routeScope}`
    drawOverlay()
  } catch (error) {
    if (target === worker && revision === routeRevision) el('routeResult').textContent = `Route failed: ${error.message || error}`
  } finally {
    if (target === worker) routeInFlight = false
  }
})

function overlayTexture(kind) {
  let texture = terrain.textures.get(kind)
  if (texture) return texture
  const width = terrain.widthWalkTiles
  const height = terrain.heightWalkTiles
  const scratch = document.createElement('canvas')
  scratch.width = width
  scratch.height = height
  const pixels = new Uint8ClampedArray(width * height * 4)
  const elevation = [[35, 114, 191], [57, 173, 119], [231, 193, 74], [120, 120, 120]]
  for (let index = 0; index < terrain.flags.length; index++) {
    const flags = terrain.flags[index]
    let color = null
    if (kind === 'walkability' && !(flags & 1)) color = flags & 32 && terrain.obstaclesEnabled ? [222, 55, 218] : [230, 60, 60]
    if (kind === 'buildability' && !(flags & 2)) color = [245, 165, 45]
    if (kind === 'ramps' && (flags & 4)) color = [175, 90, 230]
    if (kind === 'obstacles' && (flags & 32)) color = [222, 55, 218]
    if (kind === 'elevation') color = elevation[(flags >> 3) & 3]
    if (color) {
      const offset = index * 4
      pixels[offset] = color[0]
      pixels[offset + 1] = color[1]
      pixels[offset + 2] = color[2]
      pixels[offset + 3] = 255
    }
  }
  scratch.getContext('2d').putImageData(new ImageData(pixels, width, height), 0, 0)
  terrain.textures.set(kind, scratch)
  return scratch
}

function drawOverlay() {
  if (!terrain) return
  const canvas = el('terrainOverlay')
  const width = terrain.widthWalkTiles
  const height = terrain.heightWalkTiles
  canvas.width = width
  canvas.height = height
  const ctx = canvas.getContext('2d')
  const rect = canvas.getBoundingClientRect()
  const unit = Math.max(width / (rect.width || 960), height / (rect.height || 960))
  ctx.globalAlpha = Number(el('opacity').value) / 100
  ctx.drawImage(overlayTexture(el('overlay').value), 0, 0)
  ctx.globalAlpha = 1
  const point = (cell) => [cell.x + 0.5, cell.y + 0.5]
  const radius = 4 * unit
  ctx.lineWidth = 2 * unit
  ctx.strokeStyle = '#ffffff'
  ctx.fillStyle = '#101923'
  ctx.font = `bold ${12 * unit}px system-ui`
  selected.forEach((cell, index) => {
    const [x, y] = point(cell)
    ctx.beginPath()
    ctx.arc(x, y, radius, 0, Math.PI * 2)
    ctx.stroke()
    ctx.fillText(index ? 'B' : 'A', x + radius, y - radius)
  })
  if (selected.length === 2) {
    const [a, b] = selected.map(point)
    ctx.save()
    ctx.strokeStyle = '#eef7ff'
    ctx.setLineDash([6 * unit, 4 * unit])
    ctx.beginPath()
    ctx.moveTo(...a)
    ctx.lineTo(...b)
    ctx.stroke()
    ctx.restore()
  }
  if (route?.points?.length) {
    ctx.strokeStyle = '#28e7ee'
    ctx.lineWidth = 3 * unit
    ctx.beginPath()
    route.points.forEach(([x, y], index) => {
      if (index) ctx.lineTo(x + 0.5, y + 0.5)
      else ctx.moveTo(x + 0.5, y + 0.5)
    })
    ctx.stroke()
  }
}
