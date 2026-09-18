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
let baseRevision = 0
let baseRouteRevision = 0
let renderInFlight = false
let analysisInFlight = false
let routeInFlight = false
let obstacleInFlight = false
let basesInFlight = false
let baseRouteInFlight = false
let renderStatusToken = 0
let analysisStatusToken = 0
let baseStatusToken = 0
let terrain = null
let route = null
let baseDiscovery = null
let baseRoute = null
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

function resetBases() {
  baseRevision++
  baseRouteRevision++
  baseDiscovery = null
  baseRoute = null
  basesInFlight = false
  baseRouteInFlight = false
  el('findBases').disabled = true
  el('showBases').checked = true
  el('showBases').disabled = true
  el('baseControls').hidden = true
  el('baseA').replaceChildren()
  el('baseB').replaceChildren()
  el('compareBases').disabled = true
  el('baseMode').textContent = 'Find bases after terrain analysis.'
  el('baseResult').textContent = 'Find bases after terrain analysis to inspect mineral fields.'
  el('baseRouteResult').textContent = 'Choose two discovered bases to compare their anchors.'
}

function syncBaseControls() {
  const pending = basesInFlight || baseRouteInFlight || obstacleInFlight || analysisInFlight
  el('findBases').disabled = !terrain || pending
  el('showBases').disabled = !baseDiscovery || basesInFlight || obstacleInFlight
  el('baseControls').hidden = !baseDiscovery
  const comparable = hasBasePair()
  el('baseA').disabled = !baseDiscovery || pending
  el('baseB').disabled = !baseDiscovery || pending
  el('compareBases').disabled = !comparable || pending
  el('respectObstacles').disabled = !terrain || pending
  el('analyze').disabled = !mapInfo || pending
}

function resetTerrain() {
  routeRevision++
  obstacleRevision++
  resetBases()
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
    syncBaseControls()
    if (ownsStatus(analysisStatusToken)) status(`Terrain analyzed: ${terrain.widthWalkTiles}x${terrain.heightWalkTiles} walk cells; ${terrain.obstacleCount} static map blockers.`)
  } catch (error) {
    if (target === worker && revision === analysisRevision && ownsStatus(analysisStatusToken)) status(`Terrain analysis failed: ${error.message || error}`)
  } finally {
    if (target === worker) {
      analysisInFlight = false
      syncBaseControls()
    }
  }
})

el('overlay').addEventListener('change', drawOverlay)
el('opacity').addEventListener('input', () => {
  el('opacityValue').value = `${el('opacity').value}%`
  drawOverlay()
})
el('respectObstacles').addEventListener('change', async () => {
  if (!terrain || obstacleInFlight || basesInFlight || baseRouteInFlight) return
  const enabled = el('respectObstacles').checked
  const target = worker
  const snapshot = terrain
  const obstacleToken = ++obstacleRevision
  routeRevision++
  route = null
  selected = []
  obstacleInFlight = true
  routeInFlight = false
  baseRouteRevision++
  baseRoute = null
  el('baseRouteResult').textContent = 'Updating map obstacles...'
  el('respectObstacles').disabled = true
  el('analyze').disabled = true
  syncBaseControls()
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
      syncBaseControls()
      if (hasBasePair()) requestBaseRoute()
      else el('baseRouteResult').textContent = 'Choose two discovered bases to compare their anchors.'
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

function baseById(id) {
  return baseDiscovery?.bases.find((base) => base.id === id) || null
}

function selectedBaseIds() {
  if (!baseDiscovery) return []
  return [el('baseA').value, el('baseB').value]
    .map((value) => Number(value))
    .filter((id) => Number.isInteger(id) && baseById(id))
}

function hasBasePair() {
  const [a, b] = selectedBaseIds()
  return Number.isInteger(a) && Number.isInteger(b) && a !== b && baseById(a).routeAnchor !== null && baseById(b).routeAnchor !== null
}

function baseLabel(base) {
  const starts = base.startPlayers.length ? `, Start P${base.startPlayers.join(', P')}` : ''
  const clearing = [
    base.requiredMinerals.length ? `clear ${base.requiredMinerals.length} mineral patch(es)` : '',
    base.requiredObstacles.length ? `clear ${base.requiredObstacles.length} building(s)` : '',
    base.startClearedObstacles.length ? `startup clears ${base.startClearedObstacles.length} object(s)` : '',
  ].filter(Boolean).map((text) => `, ${text}`).join('')
  return `Base ${base.id + 1}: ${base.mineralCount} minerals, ${base.gasCount} gas${starts}${clearing}`
}

function populateBaseControls() {
  const priorA = el('baseA').value
  const priorB = el('baseB').value
  const options = baseDiscovery.bases.map((base) => {
    const option = document.createElement('option')
    option.value = String(base.id)
    option.textContent = baseLabel(base)
    return option
  })
  el('baseA').replaceChildren(...options.map((option) => option.cloneNode(true)))
  el('baseB').replaceChildren(...options)
  const ids = baseDiscovery.bases.map((base) => String(base.id))
  el('baseA').value = ids.includes(priorA) ? priorA : (ids[0] || '')
  el('baseB').value = ids.includes(priorB) && priorB !== el('baseA').value
    ? priorB
    : (ids.find((id) => id !== el('baseA').value) || ids[0] || '')
}

function describeBases() {
  const summary = baseDiscovery.bases.map((base) => `${baseLabel(base)}${base.routeAnchor === null ? ' (depot area currently blocked)' : ''}`)
  if (baseDiscovery.unplacedClusters) summary.push(`${baseDiscovery.unplacedClusters} qualifying cluster(s) had no suitable depot footprint.`)
  if (baseDiscovery.ignoredResources) summary.push(`${baseDiscovery.ignoredResources} unsupported, malformed, out-of-map, or duplicate resource(s) ignored.`)
  el('baseResult').textContent = summary.join('\n') || 'No qualifying mineral fields found.'
}

el('findBases').addEventListener('click', async () => {
  if (!terrain || basesInFlight || baseRouteInFlight || obstacleInFlight) return
  const revision = ++baseRevision
  baseRouteRevision++
  baseRoute = null
  const target = worker
  const snapshot = terrain
  basesInFlight = true
  el('analyze').disabled = true
  el('respectObstacles').disabled = true
  syncBaseControls()
  drawOverlay()
  baseStatusToken = status('Finding resource bases...')
  try {
    const result = await callWorker('findBases', {})
    if (target !== worker || snapshot !== terrain || revision !== baseRevision) return
    baseDiscovery = result
    populateBaseControls()
    describeBases()
    el('showBases').checked = true
    el('baseMode').textContent = `Base footprints use the stock 4x3 depot heuristic (${result.elapsedMs} ms). Marked sites require clearing minerals or buildings. Start-site cleanup is conditional on that start being occupied. Current routes retain these objects.`
    el('baseRouteResult').textContent = hasBasePair()
      ? 'Choose Compare bases to route between the selected depot anchors.'
      : 'Choose two different bases with currently accessible depot anchors to compare.'
    drawOverlay()
    if (ownsStatus(baseStatusToken)) status(`Found ${result.bases.length} base candidate(s) in ${result.elapsedMs} ms.`)
  } catch (error) {
    if (target === worker && snapshot === terrain && revision === baseRevision && ownsStatus(baseStatusToken)) {
      status(`Base discovery failed: ${error.message || error}`)
    }
  } finally {
    if (target === worker && snapshot === terrain && revision === baseRevision) {
      basesInFlight = false
      if (mapInfo && !analysisInFlight && !obstacleInFlight) el('analyze').disabled = false
      if (terrain) el('respectObstacles').disabled = false
      syncBaseControls()
    }
  }
})

async function requestBaseRoute() {
  if (!terrain || basesInFlight || baseRouteInFlight || obstacleInFlight || !hasBasePair()) return
  const [startId, endId] = selectedBaseIds()
  const revision = ++baseRouteRevision
  const target = worker
  const snapshot = terrain
  baseRouteInFlight = true
  baseRoute = null
  syncBaseControls()
  drawOverlay()
  el('baseRouteResult').textContent = `Comparing Base ${startId + 1} and Base ${endId + 1}...`
  try {
    const result = await callWorker('baseRoute', { startId, endId })
    if (target !== worker || snapshot !== terrain || revision !== baseRouteRevision) return
    baseRoute = result
    const ground = result.groundDistancePixels === null
      ? 'ground: disconnected'
      : `ground: ${result.groundDistancePixels}px (${(result.groundDistancePixels / 32).toFixed(2)} tiles)`
    el('baseRouteResult').textContent = `Base ${startId + 1} to Base ${endId + 1}: ${ground}; air: ${result.airDistancePixels.toFixed(1)}px (${(result.airDistancePixels / 32).toFixed(2)} tiles).`
    drawOverlay()
  } catch (error) {
    if (target === worker && snapshot === terrain && revision === baseRouteRevision) {
      el('baseRouteResult').textContent = `Base comparison failed: ${error.message || error}`
    }
  } finally {
    if (target === worker && snapshot === terrain && revision === baseRouteRevision) {
      baseRouteInFlight = false
      syncBaseControls()
    }
  }
}

el('compareBases').addEventListener('click', requestBaseRoute)
for (const id of ['baseA', 'baseB']) {
  el(id).addEventListener('change', () => {
    baseRouteRevision++
    baseRoute = null
    el('baseRouteResult').textContent = hasBasePair()
      ? 'Choose Compare bases to route between the selected depot anchors.'
      : 'Choose two different bases with currently accessible depot anchors to compare.'
    syncBaseControls()
    drawOverlay()
  })
}
el('showBases').addEventListener('change', drawOverlay)

el('resetRoute').addEventListener('click', () => {
  routeRevision++
  routeInFlight = false
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
  if (!cell || routeInFlight || obstacleInFlight || basesInFlight) return
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
  const revision = ++routeRevision
  const snapshot = terrain
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
    if (target === worker && snapshot === terrain && revision === routeRevision) routeInFlight = false
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

function needsClearing(base) {
  return base.requiredMinerals.length || base.requiredObstacles.length || base.startClearedObstacles.length
}

function drawBases(ctx, unit) {
  if (!baseDiscovery || !el('showBases').checked) return
  const selectedIds = new Set(selectedBaseIds())
  const width = baseDiscovery.depotWidthTiles * 4
  const height = baseDiscovery.depotHeightTiles * 4
  ctx.save()
  ctx.lineWidth = 2 * unit
  ctx.font = `bold ${12 * unit}px system-ui`
  for (const base of baseDiscovery.bases) {
    const [tileX, tileY] = base.depotTile
    const selectedBase = selectedIds.has(base.id)
    ctx.strokeStyle = selectedBase ? '#ffe36e' : needsClearing(base) ? '#ffb84d' : '#75d8ff'
    ctx.fillStyle = selectedBase ? '#fff2a0' : '#d6f4ff'
    ctx.strokeRect(tileX * 4, tileY * 4, width, height)
    ctx.fillText(`${base.id + 1}${needsClearing(base) ? '*' : ''}`, tileX * 4 + unit * 2, tileY * 4 + 12 * unit)
    if (!selectedBase) continue
    ctx.fillStyle = '#7ee6ff55'
    ctx.strokeStyle = '#7ee6ff'
    ctx.lineWidth = unit
    for (const resource of base.resources) {
      const [left, top, right, bottom] = resource.bounds
      ctx.fillRect(left / 8, top / 8, (right - left) / 8, (bottom - top) / 8)
      ctx.strokeRect(left / 8, top / 8, (right - left) / 8, (bottom - top) / 8)
    }
    ctx.strokeStyle = '#ff9d35'
    ctx.fillStyle = '#ff9d3577'
    const clearingBounds = [
      ...base.requiredMinerals.map((resource) => resource.bounds),
      ...base.requiredObstacles,
      ...base.startClearedObstacles,
    ]
    for (const [left, top, right, bottom] of clearingBounds) {
      ctx.fillRect(left / 8, top / 8, (right - left) / 8, (bottom - top) / 8)
      ctx.strokeRect(left / 8, top / 8, (right - left) / 8, (bottom - top) / 8)
    }
  }
  ctx.restore()
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
  drawBases(ctx, unit)
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
  if (baseRoute && hasBasePair()) {
    const [a, b] = selectedBaseIds().map((id) => baseById(id).routeAnchor)
    ctx.save()
    ctx.strokeStyle = '#ffe3b2'
    ctx.setLineDash([6 * unit, 4 * unit])
    ctx.lineWidth = 2 * unit
    ctx.beginPath()
    ctx.moveTo(a[0] + 0.5, a[1] + 0.5)
    ctx.lineTo(b[0] + 0.5, b[1] + 0.5)
    ctx.stroke()
    ctx.restore()
  }
  if (baseRoute?.points?.length) {
    ctx.strokeStyle = '#ffb84d'
    ctx.lineWidth = 3 * unit
    ctx.beginPath()
    baseRoute.points.forEach(([x, y], index) => {
      if (index) ctx.lineTo(x + 0.5, y + 0.5)
      else ctx.moveTo(x + 0.5, y + 0.5)
    })
    ctx.stroke()
  }
}
