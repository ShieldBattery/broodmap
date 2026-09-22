import { createMapCamera } from './map-camera.js'
import { captureRequest, createPreviewQueue } from './editor-requests.js'
import {
  DEFAULT_WALL_WINDOW_PIXELS, MAX_WALL_BUILDINGS, addWallBuilding, clampWallWindow,
  defaultWallEndpoints, pointInHalfOpenRect, removeWallBuildingAt, wallQuery,
  centeredBuildingTile, placementPreviewCells, nearestWallSpan, wallSearchSide, wallPlacementArea,
  wallPlacementSite, pointInWallPlacementSite, pointInWallPlacementReach,
} from './wall-editor.js'

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
let setupRevision = 0
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
let regionRevision = 0
let regionInFlight = false
let entranceRevision = 0
let entranceInFlight = false
let renderStatusToken = 0
let analysisStatusToken = 0
let baseStatusToken = 0
let terrain = null
let route = null
let baseDiscovery = null
let baseRoute = null
let regions = null
let entrances = null
let baseAreas = null
let areaRevision = 0
let areaInFlight = false
let selected = []
let wallProfiles = null
let wallWindow = null
let wallEndpoints = null
let wallPlacementCenter = null
let wallPlacementDirection = [1, 0]
let wallBuildings = []
let wallTool = 'place'
let wallResult = null
let wallInFlight = false
let wallPickPending = false
let wallPickHover = -1
let wallCenterPending = false
let wallRevision = 0
let wallProfileRevision = 0
let wallPointer = null
let wallSearchRevision = 0
let wallSearchInFlight = false
let wallSearchResult = null
let wallSearchPreview = null
let wallSearchArea = null
let wallReachRequest = null
const pending = new Map()
const reachQueue = createPreviewQueue(async request => {
  const result = await callWorker('wallPlacementReach', { query: request.query })
  return { ...result, cells: new Uint8Array(result.cells) }
}, () => drawAnnotations())
const camera = createMapCamera({
  viewport: el('mapViewport'), surface: el('terrainShell'),
  onChange: () => {
    wallPointer = null
    el('zoomLevel').value = `${Math.round(camera.scale * 100)}%`
    drawAnnotations()
    drawPlacementPreview()
  },
})
el('zoomIn').addEventListener('click', () => camera.zoomBy(1.5))
el('zoomOut').addEventListener('click', () => camera.zoomBy(1 / 1.5))
el('fitMap').addEventListener('click', () => camera.fit())
el('focusWall').addEventListener('click', () => { if (wallWindow) camera.focus(wallWindow) })
el('workspaceMode').addEventListener('change', () => {
  invalidateWallSearch()
  el('wallEnabled').checked = el('workspaceMode').value === 'wall'
  syncControls()
  drawOverlay()
  drawPlacementPreview()
})

function wallMapPixels() {
  return terrain ? [terrain.widthWalkTiles * 8, terrain.heightWalkTiles * 8] : null
}

function selectedWallBuilding() {
  return wallProfiles?.buildings.find((building) => building.id === Number(el('wallBuilding').value)) || null
}

function selectedWallMover() {
  return wallProfiles?.movers.find((mover) => mover.id === Number(el('wallMover').value)) || null
}

function selectedWallSpan() {
  const match = /^span:(\d+)$/.exec(el('wallSpan').value)
  return match ? baseAreas?.boundaries[Number(match[1])] || null : null
}

function wallEnabled() {
  return Boolean(terrain && wallProfiles?.buildings.length && wallProfiles?.movers.length && el('wallEnabled').checked)
}

function wallControlIds() {
  return ['wallSpanScope', 'wallSpan', 'wallWindowSize', 'wallCenter', 'wallManualCenter', 'wallPickEntrance', 'wallBuilding', 'wallMover', 'wallUndo', 'wallClear', 'checkWall', ...wallSearchControlIds()]
}

function analysisBusy() {
  return wallInFlight || wallSearchInFlight || obstacleInFlight || analysisInFlight
    || basesInFlight || baseRouteInFlight || regionInFlight || entranceInFlight || areaInFlight || routeInFlight
}

function syncWallControls() {
  const ready = Boolean(terrain && wallProfiles?.buildings.length && wallProfiles?.movers.length)
  const busy = analysisBusy()
  const enabled = ready && el('wallEnabled').checked
  el('wallEnabled').disabled = !ready || busy
  el('wallEditor').hidden = !enabled
  for (const option of el('searchSize').options) {
    const tiles = Number(option.value) / 32
    option.textContent = selectedWallSpan() ? `${tiles / 2} tiles from entrance` : `${tiles} x ${tiles} tiles (custom box)`
  }
  for (const id of wallControlIds()) el(id).disabled = !enabled || busy
  for (const button of el('wallTool').querySelectorAll('button')) button.disabled = !enabled || busy
  el('cancelWallSearch').hidden = !wallSearchInFlight
  for (const button of el('wallTool').querySelectorAll('button')) button.disabled ||= Boolean(wallSearchPreview)
  const inventoryAvailable = [109, 111].every(id => wallProfiles?.buildings.some(building => building.id === id))
  if (enabled && !inventoryAvailable) el('wallSearchStatus').textContent = 'Automatic layouts require Supply Depot and Barracks profiles, which are unavailable in this asset set. Manual wall editing is still available.'
  if (enabled && !busy) {
    el('checkWall').disabled = !wallWindow || !wallEndpoints || !selectedWallMover() || Boolean(wallSearchPreview)
    el('wallPickEntrance').disabled = !baseAreas?.boundaries.length
    el('searchGround').disabled = selectedWallSpan()?.kind !== 'ramp'
    el('findWalls').disabled = wallPickPending || wallCenterPending || !wallWindow || !wallEndpoints || (!el('searchAutoEndpoints').checked && !wallEndpoints.every(point => pointInHalfOpenRect(point, wallWindow))) || !inventoryAvailable
    el('wallUndo').disabled = !wallBuildings.length
    el('wallClear').disabled = !wallBuildings.length
  }
}

function invalidateWallResult(message) {
  invalidateWallSearch()
  wallRevision++
  wallResult = null
  if (message) el('wallResult').textContent = message
  if (wallEnabled()) drawOverlay()
}

function resetWall(message = 'Analyze terrain to load stock building and mover profiles.') {
  wallReachRequest = null
  reachQueue.select(null)
  invalidateWallSearch()
  wallRevision++
  wallProfileRevision++
  wallProfiles = null
  setWallTool('place')
  wallWindow = null
  wallEndpoints = null
  wallPlacementCenter = null
  wallBuildings = []
  wallResult = null
  wallInFlight = false
  wallCenterPending = false
  wallPickPending = false
  wallPickHover = -1
  el('searchAutoEndpoints').checked = true
  el('wallPickEntrance').setAttribute('aria-pressed', 'false')
  el('wallEnabled').checked = false
  el('wallEnabled').disabled = true
  el('wallEditor').hidden = true
  el('wallBuilding').replaceChildren()
  el('wallMover').replaceChildren()
  el('wallSpan').replaceChildren(new Option('Custom spot (click map to choose)', 'manual'))
  el('wallMode').textContent = message
  el('wallResult').textContent = 'Choose an entrance or click the terrain to center a bounded window. Place up to 32 buildings in build-tile cells, then set A/B test points.'
}

function populateWallProfiles(profiles) {
  wallProfiles = profiles
  const populate = (id, entries) => {
    el(id).replaceChildren(...entries.map(({ id: value, name, footprint }) =>
      new Option(footprint ? `${name} (${footprint[0]}x${footprint[1]} tiles)` : name, String(value))))
  }
  populate('wallBuilding', profiles.buildings)
  populate('wallMover', profiles.movers)
  populate('searchBlock', profiles.movers)
  populate('searchAllow', profiles.movers)
  el('searchAllow').prepend(new Option('No passage requirement', 'none'))
  el('searchBlock').value = profiles.movers.some(m => m.id === 65) ? '65' : String(profiles.movers[0]?.id)
  el('searchAllow').value = profiles.movers.some(m => m.id === 0) ? '0' : 'none'
  el('wallEnabled').checked = el('workspaceMode').value === 'wall'
  el('wallEnabled').disabled = !profiles.buildings.length || !profiles.movers.length
  el('wallMode').textContent = profiles.buildings.length && profiles.movers.length
    ? `${profiles.buildings.length} building and ${profiles.movers.length} mover profiles ready. Choose an entrance or click the map to start.`
    : 'The analysis data did not expose usable building and mover profiles.'
  syncControls()
}

async function loadWallProfiles(snapshot) {
  const revision = ++wallProfileRevision
  const target = worker
  try {
    const profiles = await callWorker('wallProfiles', {})
    if (target !== worker || snapshot !== terrain || revision !== wallProfileRevision) return
    populateWallProfiles(profiles)
  } catch (error) {
    if (target === worker && snapshot === terrain && revision === wallProfileRevision) el('wallMode').textContent = `Wall profiles unavailable: ${error.message || error}`
  }
}

function wallSpanLabel(span, index) {
  const areas = (span.incidentAreaIds || []).join(', ')
  return `${span.kind === 'ramp' ? `Ramp (${span.rampEnd === 'upper' ? 'top' : 'bottom'})` : 'Entrance'} ${index + 1}${areas ? ` (areas ${areas})` : ''}`
}

function populateWallSpans() {
  const oldValue = el('wallSpan').value
  const options = [new Option('Custom spot (click map to choose)', 'manual')]
  const selectedIds = new Set(selectedAreas().filter(Boolean).map((area) => area.areaId))
  const showAll = el('wallSpanScope').value === 'all' || !selectedIds.size
  baseAreas?.boundaries.forEach((span, index) => {
    if (showAll || (span.incidentAreaIds || []).some((id) => selectedIds.has(id))) options.push(new Option(wallSpanLabel(span, index), `span:${index}`))
  })
  el('wallSpan').replaceChildren(...options)
  el('wallSpan').value = [...options].some((option) => option.value === oldValue) ? oldValue : 'manual'
  if (el('wallSpan').value !== oldValue) invalidateWallSearch('Entrance selection changed. Run a new search.')
  syncControls()
}

function selectedSearchSide() {
  return wallSearchSide(selectedWallSpan(), baseAreas?.boundaries || [], el('searchGround').value)
}

function currentPlacementArea() {
  if (!wallWindow || !wallEndpoints) return null
  // Entrance-relative reach owns the shape; the rectangle is only the full-footprint bound.
  if (selectedSearchSide().span) return [...wallWindow]
  return wallPlacementArea(wallWindow, [wallPlacementCenter, wallPlacementCenter], Number(el('searchSize').value))
}

function finishWallPick() {
  wallPickPending = false
  wallPickHover = -1
  el('wallPickEntrance').setAttribute('aria-pressed', 'false')
}

function setWallWindow(center, endpoints = null) {
  const mapPixels = wallMapPixels()
  if (!mapPixels) return
  wallWindow = clampWallWindow(center, mapPixels, Number(el('wallWindowSize').value) || DEFAULT_WALL_WINDOW_PIXELS)
  wallPlacementCenter = [...center]
  wallEndpoints = endpoints || [
    [Math.max(wallWindow[0], Math.min(wallWindow[2] - 1, Math.round(center[0] - 96))), Math.round(center[1])],
    [Math.max(wallWindow[0], Math.min(wallWindow[2] - 1, Math.round(center[0] + 96))), Math.round(center[1])],
  ]
  wallPlacementDirection = wallEndpoints[0].map((v, i) => wallEndpoints[1][i] - v)
  finishWallPick()
  el('searchAutoEndpoints').checked = true
  invalidateWallResult('Window moved; set or review A/B test points, then check the wall again.')
  syncControls()
  camera.focus(wallWindow)
}

function centerWallOnSelectedSpan() {
  const span = selectedSearchSide().span
  if (!span) {
    el('wallResult').textContent = 'Choose a base-area entrance, or click the terrain to center a manual window.'
    return
  }
  const index = baseAreas.boundaries.indexOf(span)
  if (index >= 0 && el('wallSpan').value !== `span:${index}`) {
    if (![...el('wallSpan').options].some(option => option.value === `span:${index}`)) {
      el('wallSpanScope').value = 'all'
      populateWallSpans()
    }
    el('wallSpan').value = `span:${index}`
  }
  const endpoints = span.endpoints
  const center = [(endpoints[0][0] + endpoints[1][0]) / 2, (endpoints[0][1] + endpoints[1][1]) / 2]
  const window = clampWallWindow(center, wallMapPixels(), Number(el('wallWindowSize').value) || DEFAULT_WALL_WINDOW_PIXELS)
  wallWindow = window
  wallPlacementCenter = center
  wallEndpoints = defaultWallEndpoints(endpoints, window)
  wallCenterPending = false
  finishWallPick()
  el('searchAutoEndpoints').checked = true
  invalidateWallResult('Entrance selected. Search will keep A/B clear of all eligible buildings; use Set A/B to override.')
  syncControls()
  camera.focus(wallWindow)
}

function describeWallResult(result) {
  const passage = {
    open: 'OPEN: A route remains through the test area.',
    blockedWithinWindow: 'BLOCKED: No route through the test area with these buildings.',
    alreadyDisconnected: 'Already disconnected before these buildings. Try different A/B points.',
    startBlocked: 'Point A is blocked. Move it to clear ground.',
    endBlocked: 'Point B is blocked. Move it to clear ground.',
  }[result.passage] || `Modeled passage: ${result.passage}.`
  const describeIssue = (issue) => ({
    outOfMap: 'extends outside the map',
    unbuildableTerrain: 'covers unbuildable terrain',
    overlapsObstacle: `overlaps map obstacle ${issue.index + 1}`,
    overlapsBuilding: `overlaps proposed building ${issue.index + 1}`,
  }[issue.kind] || issue.kind)
  const issues = (result.placements || []).flatMap((placement, index) =>
    (placement.issues || []).map((issue) => `building ${index + 1}: ${describeIssue(issue)}`))
  const placement = result.staticPlacementValid
    ? 'Placement: no static conflicts found.'
    : `Placement: ${issues.join('; ') || 'one or more placements failed.'}`
  return `${passage}\n${placement}\n${result.elapsedMs} ms${result.baselinePassable ? '' : ' | baseline did not connect A and B'}`
}

async function checkWall() {
  if (!wallEnabled() || analysisBusy()) return
  if (!wallWindow || !wallEndpoints || !selectedWallMover()) {
    el('wallResult').textContent = 'Center a query window and choose both A/B test points before checking.'
    return
  }
  if (!wallEndpoints.every((point) => pointInHalfOpenRect(point, wallWindow))) {
    el('wallResult').textContent = 'A or B is outside the outlined query window. Recenter the window or click inside it to reposition that point.'
    return
  }
  const revision = ++wallRevision
  const target = worker
  const snapshot = terrain
  wallInFlight = true
  syncControls()
  el('wallResult').textContent = 'Checking the bounded pixel-rectangle model...'
  try {
    const result = await callWorker('checkWall', { query: wallQuery(wallWindow, wallEndpoints, selectedWallMover(), wallBuildings) })
    if (target !== worker || snapshot !== terrain || revision !== wallRevision) return
    wallResult = result
    el('wallResult').textContent = describeWallResult(result)
    drawOverlay()
  } catch (error) {
    if (target === worker && snapshot === terrain && revision === wallRevision) el('wallResult').textContent = `Wall check failed: ${error.message || error}`
  } finally {
    if (target === worker && snapshot === terrain) {
      wallInFlight = false
      syncControls()
    }
  }
}

function wallSearchControlIds() {
  return ['searchDepots', 'searchBarracks', 'searchBlock', 'searchAllow', 'searchSize', 'searchBudget', 'searchGround', 'searchAutoEndpoints',
    'findWalls', 'wallSuggestion', 'useWallSuggestion', 'dismissWallSuggestion']
}

function invalidateWallSearch(message = 'Choose requirements, then find layouts near this entrance or A/B.') {
  if (wallSearchInFlight) worker?.postMessage({ type: 'cancelWallSearch', token: wallSearchRevision })
  if (wallSearchPreview || wallSearchInFlight) {
    wallResult = null
    el('wallResult').textContent = 'Your manual layout is shown again. Check it to update passage results.'
  }
  wallSearchRevision++
  wallSearchInFlight = false
  wallSearchResult = null
  wallSearchPreview = null
  wallSearchArea = null
  el('cancelWallSearch').hidden = true
  el('wallSearchResults').hidden = true
  el('wallSearchStatus').textContent = message
}

function previewWallSuggestion() {
  wallSearchPreview = wallSearchResult?.layouts[Number(el('wallSuggestion').value)] || null
  el('wallSearchResults').hidden = !wallSearchPreview
  if (wallSearchPreview) el('wallResult').textContent = `Previewing a suggestion: blocks ${wallSearchResult.blockName}${wallSearchResult.allowName ? `; passes ${wallSearchResult.allowName}` : ''}. No static placement conflicts found. Keep it to replace your manual layout, then edit or check for another mover.`
  syncControls()
  drawOverlay()
}

async function findWalls() {
  if (!wallEnabled() || analysisBusy() || !wallWindow || !wallEndpoints) return
  if (wallPickPending || wallCenterPending || (!el('searchAutoEndpoints').checked && !wallEndpoints.every(point => pointInHalfOpenRect(point, wallWindow)))) {
    el('wallSearchStatus').textContent = 'Place both A/B points inside the outlined query window before searching.'
    return
  }
  const inventory = []
  for (const [id, control] of [[109, 'searchDepots'], [111, 'searchBarracks']]) {
    const count = Number(el(control).value)
    const profile = wallProfiles.buildings.find(building => building.id === id)
    if (!Number.isInteger(count) || count < 0 || count > 3 || !profile) {
      el('wallSearchStatus').textContent = 'Choose zero to three of each available building.'
      return
    }
    for (let i = 0; i < count; i++) inventory.push(profile)
  }
  if (!inventory.length) {
    el('wallSearchStatus').textContent = 'Include at least one building in the search.'
    return
  }
  const block = wallProfiles.movers.find(m => m.id === Number(el('searchBlock').value))
  const allow = el('searchAllow').value === 'none' ? null : wallProfiles.movers.find(m => m.id === Number(el('searchAllow').value))
  if (!block) return
  invalidateWallSearch()
  const revision = ++wallSearchRevision
  const current = captureRequest(() => [worker, terrain, wallSearchRevision, baseAreas])
  const { span, elevation } = selectedSearchSide()
  wallSearchArea = currentPlacementArea()
  const query = {
    window: [...wallWindow], placementArea: wallSearchArea, placementElevation: elevation,
    placementSite: wallPlacementSite(span, Number(el('searchSize').value)),
    start: [...wallEndpoints[0]], end: [...wallEndpoints[1]],
    block: block.collision, allow: allow?.collision || null,
    buildings: inventory.map(({ footprint, collision }) => ({ footprint, collision })),
    maxStates: Number(el('searchBudget').value), maxResults: 5,
  }
  wallSearchInFlight = true
  wallResult = null
  syncControls()
  drawOverlay()
  el('wallResult').textContent = 'Searching from an empty plan. Previewing keeps your manual layout; Keep this layout replaces it.'
  el('wallSearchStatus').textContent = 'Preparing placements and checking the baseline...'
  try {
    if (el('searchAutoEndpoints').checked) {
      // Manual sites use the normal to A/B as a virtual mouth. The core chooses clear endpoints
      // outside every possible proposed collision, for both requested movers.
      const mid = wallPlacementCenter
      const [dx, dy] = wallPlacementDirection
      const length = Math.hypot(dx, dy) || 1
      const normal = [-dy / length * 16, dx / length * 16]
      const entrance = span?.endpoints || [-1, 1].map(sign => mid.map((v, i) =>
        Math.max(0, Math.min(wallMapPixels()[i] - 1, Math.round(v + sign * normal[i])))))
      const endpoints = await callWorker('suggestWallEndpoints', { query, entrance })
      if (!current()) return
      query.start = endpoints.start
      query.end = endpoints.end
      wallEndpoints = [endpoints.start, endpoints.end]
      drawOverlay()
    }
    const result = await callWorker('findWalls', { token: revision, query }, [], progress => {
      if (current()) el('wallSearchStatus').textContent = `Searching: ${progress.exploredStates} states; ${progress.foundLayouts} suggestions so far...`
    })
    if (!current()) return
    wallSearchResult = {
      ...result, blockName: block.name, allowName: allow?.name,
      layouts: result.layouts.map(layout => ({ ...layout,
        buildings: layout.buildings.map(building => ({ ...inventory[building.specIndex], ...building })),
      })),
    }
    const completion = {
      exhausted: 'Search finished within the placement area and supplied inventory.',
      stateLimit: 'Search budget reached; other layouts may remain.',
      resultLimit: 'Suggestion limit reached; other layouts may remain.',
    }[result.reason] || result.reason
    el('wallSearchStatus').textContent = `${result.layouts.length} suggestion(s); ${result.candidatePlacements} valid single-building placements, ${result.exploredStates} explored layouts in ${(result.elapsedMs / 1000).toFixed(1)}s. ${completion} ${result.layouts.length ? `Blocks ${block.name}${allow ? `; passes ${allow.name}` : ''} in the static model.` : result.reason === 'stateLimit' ? 'Try a larger budget.' : 'No qualifying wall for this zone and inventory. Try wider reach or different buildings; a larger budget cannot change an exhausted search.'}`
    el('wallSuggestion').replaceChildren(...wallSearchResult.layouts.map((layout, index) =>
      new Option(`${index + 1}: ${layout.buildings.map(b => b.name).join(' + ')}`, String(index))))
    previewWallSuggestion()
  } catch (error) {
    if (current()) el('wallSearchStatus').textContent = `Search failed: ${error.message || error}`
  } finally {
    if (current()) {
      wallSearchInFlight = false
      if (!wallSearchPreview) el('wallResult').textContent = 'Your manual layout is unchanged. Check it to update passage results.'
      syncControls()
      drawOverlay()
    }
  }
}

el('findWalls').addEventListener('click', findWalls)
el('cancelWallSearch').addEventListener('click', () => {
  invalidateWallSearch('Search cancelled. Your manual layout was kept.')
  syncControls()
  drawOverlay()
})
el('wallSuggestion').addEventListener('change', previewWallSuggestion)
el('dismissWallSuggestion').addEventListener('click', () => {
  invalidateWallSearch('Suggestions dismissed. Your manual layout was kept.')
  syncControls()
  drawOverlay()
})
el('useWallSuggestion').addEventListener('click', () => {
  if (!wallSearchPreview) return
  const buildings = wallSearchPreview.buildings
  const mover = el('searchBlock').value
  wallBuildings = buildings
  el('wallMover').value = mover
  invalidateWallResult('Suggested layout replaced the manual layout. Edit it or check it for another mover.')
  syncControls()
})
for (const id of ['searchDepots', 'searchBarracks', 'searchBlock', 'searchAllow', 'searchSize', 'searchBudget', 'searchAutoEndpoints']) {
  el(id).addEventListener('change', () => {
    invalidateWallSearch('Requirements changed. Run a new search.')
    syncControls()
    drawOverlay()
  })
}

function makeWorker() {
  const nextWorker = new Worker(new URL('./worker.js', import.meta.url), { type: 'module' })
  nextWorker.onmessage = ({ data }) => {
    const job = pending.get(data.id)
    if (!job) return
    if (job.worker !== nextWorker) return
    if (data.progress) {
      job.onProgress?.(data.progress)
      return
    }
    pending.delete(data.id)
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

function callWorker(type, payload, transfer = [], onProgress) {
  const target = worker
  if (!target) return Promise.reject(new Error('Choose a map first.'))
  const id = nextJobId++
  return new Promise((resolve, reject) => {
    pending.set(id, { worker: target, resolve, reject, onProgress })
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

function invalidateRegions(message = 'Find regions after terrain analysis.') {
  regionRevision++
  regions = null
  regionInFlight = false
  terrain?.textures.delete('regions')
  el('regionLegend').hidden = true
  el('regionResult').textContent = message
  el('hover').textContent = 'Move over the terrain view to inspect a cell.'
  if (baseDiscovery) {
    populateBaseControls()
    describeBases()
  }
}

function invalidateEntrances(message = 'Choose two base anchors to inspect terrain entrances.', keepAreas = false) {
  if (!keepAreas) invalidateBaseAreas()
  entranceRevision++
  entrances = null
  entranceInFlight = false
  el('entranceResult').textContent = message
  el('entranceLegend').hidden = true
}

function invalidateBaseAreas() {
  invalidateWallSearch('Entrance analysis changed. Run a new wall search.')
  wallRevision++
  wallInFlight = false
  wallResult = null
  wallReachRequest = null
  reachQueue.select(null)
  if (areaInFlight) worker?.postMessage({ type: 'cancelBaseAreas', token: areaRevision })
  areaRevision++
  areaInFlight = false
  baseAreas = null
  populateWallSpans()
  el('cancelBaseAreas').hidden = true
  el('baseAreaLegend').hidden = true
  el('baseAreaResult').textContent = 'Analyze all base areas once, then select bases to highlight them.'
  terrain?.textures.delete('baseAreas')
}

function selectedAreas() {
  return selectedBaseIds().map((id) => baseAreas?.baseAreas.find((area) => area.baseId === id))
}

function resetBases() {
  baseRevision++
  baseRouteRevision++
  baseDiscovery = null
  baseRoute = null
  basesInFlight = false
  baseRouteInFlight = false
  invalidateRegions()
  invalidateEntrances()
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
  el('findRegions').disabled = true
  el('regionProminence').disabled = true
  el('regionRelativeProminence').disabled = true
  el('showPassages').checked = true
  el('showPassages').disabled = true
  el('regionResult').textContent = 'Find regions after terrain analysis.'
  el('findEntrances').disabled = true
  el('findBaseAreas').disabled = true
  el('entranceWidening').disabled = true
  el('showEntrances').checked = true
  el('showEntrances').disabled = true
}

function syncControls() {
  const pending = analysisBusy()
  el('findBases').disabled = !terrain || pending
  el('findRegions').disabled = !terrain || pending
  el('regionProminence').disabled = !terrain || pending
  el('regionRelativeProminence').disabled = !terrain || pending
  el('showPassages').disabled = !regions || pending
  el('showBases').disabled = !baseDiscovery || basesInFlight || obstacleInFlight
  el('baseControls').hidden = !baseDiscovery
  const comparable = hasBasePair()
  el('baseA').disabled = !baseDiscovery || pending
  el('baseB').disabled = !baseDiscovery || pending
  el('compareBases').disabled = !comparable || pending
  el('findEntrances').disabled = !comparable || pending
  el('findBaseAreas').disabled = !baseDiscovery || pending
  el('entranceWidening').disabled = !baseDiscovery || pending
  el('showEntrances').disabled = (!entrances && !baseAreas) || pending
  el('respectObstacles').disabled = !terrain || pending
  el('analyze').disabled = !mapInfo || pending
  syncWallControls()
}

function resetTerrain() {
  resetWall()
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
  clearCanvas('mapAnnotations')
  wallPointer = null
  drawPlacementPreview()
  el('terrainControls').hidden = true
  el('clearanceLegend').hidden = true
  el('clearanceScaleControl').hidden = true
  el('elevationLegend').hidden = true
  el('regionLegend').hidden = true
  el('regionProminence').disabled = true
  el('regionRelativeProminence').disabled = true
  el('routeResult').textContent = 'Analyze terrain, then click two walkable cells.'
  el('hover').textContent = 'Move over the terrain view after analysis.'
}

function resetMapUi() {
  setupRevision++
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
  el('mapEmpty').hidden = false
  camera.setMapSize(0, 0)
}

el('map').addEventListener('change', (event) => openMapFile(event.target.files[0]))

async function openMapFile(file) {
  if (!file) return
  if (!/\.(scm|scx)$/i.test(file.name)) {
    status('Choose a StarCraft map file (.scm or .scx).')
    return
  }
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
    el('mapEmpty').hidden = true
    camera.setMapSize(result.widthTiles * 32, result.heightTiles * 32)
    drawTo(el('terrainBackdrop'), result.minimap)
    drawTo(el('minimap'), result.minimap)
    el('render').disabled = false
    el('analyze').disabled = false
    await prepareMap(selection, target)
  } catch (error) {
    if (selection === fileNonce && ownsStatus(readStatusToken)) status(`Failed to parse ${file.name}: ${error.message || error}`)
  }
}

// Files are readable only at drop time; the drag hint uses the exposed type list.
let fileDragDepth = 0
const isFileDrag = (event) => Array.from(event.dataTransfer?.types || []).includes('Files')
function clearFileDrag() {
  fileDragDepth = 0
  el('mapDropHint').hidden = true
}
document.addEventListener('dragenter', (event) => {
  if (!isFileDrag(event)) return
  event.preventDefault()
  fileDragDepth++
  el('mapDropHint').hidden = false
})
document.addEventListener('dragover', (event) => {
  event.preventDefault()
  if (!isFileDrag(event)) return
  event.dataTransfer.dropEffect = 'copy'
})
document.addEventListener('dragleave', () => {
  if (fileDragDepth && --fileDragDepth === 0) clearFileDrag()
})
document.addEventListener('drop', (event) => {
  event.preventDefault() // Prevent navigation for links as well as unsupported files.
  clearFileDrag()
  if (!isFileDrag(event)) return
  const maps = Array.from(event.dataTransfer.files).filter(file => /\.(scm|scx)$/i.test(file.name))
  if (maps.length !== 1) {
    status(maps.length ? 'Drop one map at a time.' : 'Drop a StarCraft map file (.scm or .scx).')
    return
  }
  const selection = new DataTransfer()
  selection.items.add(maps[0])
  el('map').files = selection.files
  openMapFile(maps[0])
})
window.addEventListener('dragend', clearFileDrag)
window.addEventListener('blur', clearFileDrag)

async function prepareMap(selection, target) {
  const revision = ++setupRevision
  const current = () => revision === setupRevision && selection === fileNonce && target === worker
  await analyzeTerrain()
  if (!current() || !terrain) return
  const snapshot = terrain
  await discoverBases()
  if (!current() || terrain !== snapshot) return
  if (baseDiscovery) await analyzeAllBaseAreas()
  if (!current() || terrain !== snapshot) return
  await renderMap()
}

for (const id of ['style', 'size']) {
  el(id).addEventListener('input', () => {
    renderRevision++
    if (renderInFlight && ownsStatus(renderStatusToken)) {
      status('Preview settings changed. Press Render to update the preview.')
    }
  })
}
el('assetBase').addEventListener('input', () => {
  setupRevision++
  renderRevision++
  analysisRevision++
  resetWall('Asset base changed. Analyze terrain again to reload wall profiles.')
  if (analysisInFlight && ownsStatus(analysisStatusToken)) {
    status('Asset base changed. Run analysis again.')
  } else if (renderInFlight && ownsStatus(renderStatusToken)) {
    status('Asset base changed. Press Render to update the preview.')
  }
})

el('render').addEventListener('click', renderMap)
async function renderMap() {
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
    if (terrain) {
      drawTerrainBackdrop()
      drawOverlay()
    }
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
}

el('analyze').addEventListener('click', () => prepareMap(fileNonce, worker))
async function analyzeTerrain() {
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
    terrain = { ...result, flags: new Uint8Array(result.flags), clearance: new Uint16Array(result.clearance), textures: new Map(), obstaclesEnabled: true }
    el('respectObstacles').checked = true
    el('respectObstacles').disabled = false
    el('findRegions').disabled = false
    updateObstacleMode()
    route = null
    selected = []
    el('terrainControls').hidden = false
    camera.setMapSize(terrain.widthWalkTiles * 8, terrain.heightWalkTiles * 8)
    drawTerrainBackdrop()
    drawOverlay()
    el('routeResult').textContent = 'Click a walkable cell for A, then another for B.'
    syncControls()
    await loadWallProfiles(terrain)
    if (ownsStatus(analysisStatusToken)) status(`Terrain analyzed: ${terrain.widthWalkTiles}x${terrain.heightWalkTiles} walk cells; ${terrain.obstacleCount} static map blockers.`)
  } catch (error) {
    if (target === worker && revision === analysisRevision && ownsStatus(analysisStatusToken)) status(`Terrain analysis failed: ${error.message || error}`)
  } finally {
    if (target === worker) {
      analysisInFlight = false
      syncControls()
    }
  }
}

el('overlay').addEventListener('change', () => {
  el('hover').textContent = 'Move over the terrain view to inspect a cell.'
  if (el('overlay').value === 'regions' && !regions) {
    el('regionResult').textContent = 'Find regions first to populate this overlay.'
  }
  drawOverlay()
})
el('clearanceMax').addEventListener('change', () => {
  terrain?.textures.delete('clearance')
  drawOverlay()
})
for (const id of ['regionProminence', 'regionRelativeProminence']) {
  el(id).addEventListener('change', () => {
    invalidateRegions('Find regions again with the new region settings.')
    drawOverlay()
    syncControls()
  })
}
el('showPassages').addEventListener('change', drawOverlay)
el('findRegions').addEventListener('click', async () => {
  if (!terrain || analysisBusy()) return
  invalidateRegions('Finding terrain regions...')
  const revision = regionRevision
  const target = worker
  const snapshot = terrain
  const prominence = Number(el('regionProminence').value)
  const relativeProminence = Number(el('regionRelativeProminence').value)
  regionInFlight = true
  el('findRegions').disabled = true
  el('regionProminence').disabled = true
  el('regionRelativeProminence').disabled = true
  el('showPassages').checked = true
  syncControls()
  drawOverlay()
  try {
    const result = await callWorker('findRegions', { minProminencePixels: prominence, minRelativeProminencePercent: relativeProminence })
    if (target !== worker || snapshot !== terrain || revision !== regionRevision) return
    regions = { ...result, labels: new Uint32Array(result.labels) }
    terrain.textures.delete('regions')
    el('overlay').value = 'regions'
    el('showPassages').disabled = false
    el('regionLegend').hidden = el('overlay').value !== 'regions'
    el('regionResult').textContent = `${result.metadata.regions.length} regions, ${result.metadata.passages.length} candidate passages found in ${result.elapsedMs} ms.`
    if (baseDiscovery) {
      populateBaseControls()
      describeBases()
    }
    drawOverlay()
  } catch (error) {
    if (target === worker && snapshot === terrain && revision === regionRevision) {
      el('regionResult').textContent = `Region analysis failed: ${error.message || error}`
    }
  } finally {
    if (target === worker && snapshot === terrain && revision === regionRevision) {
      regionInFlight = false
      el('regionProminence').disabled = false
      syncControls()
    }
  }
})

el('opacity').addEventListener('input', () => {
  el('opacityValue').value = `${el('opacity').value}%`
  drawOverlay()
})
el('respectObstacles').addEventListener('change', async () => {
  if (!terrain || analysisBusy()) return
  const enabled = el('respectObstacles').checked
  const target = worker
  const snapshot = terrain
  const obstacleToken = ++obstacleRevision
  invalidateWallResult('Map obstacle handling changed. Check the wall again after it finishes updating.')
  invalidateRegions('Find regions again after changing map obstacles.')
  invalidateEntrances('Find entrances again after changing map obstacles.', true)
  el('showPassages').checked = true
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
  el('findRegions').disabled = true
  syncControls()
  const obstacleStatusToken = status('Updating map obstacles...')
  el('routeResult').textContent = 'Updating map obstacles...'
  drawOverlay()
  try {
    const result = await callWorker('setObstaclesEnabled', { enabled })
    if (target !== worker || snapshot !== terrain || obstacleToken !== obstacleRevision) return
    terrain.flags = new Uint8Array(result.flags)
    terrain.clearance = new Uint16Array(result.clearance)
    terrain.obstacleCount = result.obstacleCount
    terrain.obstaclesEnabled = result.enabled
    terrain.textures = new Map()
    el('hover').textContent = 'Move over the terrain view to inspect the updated cells.'
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
      syncControls()
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
  let regionLabel = ', no active region'
  if (regions && base.routeAnchor) {
    const regionId = regions.labels[base.routeAnchor[1] * terrain.widthWalkTiles + base.routeAnchor[0]]
    if (regionId) regionLabel = `, region ${regionId}`
  }
  return `Base ${base.id + 1}: ${base.mineralCount} minerals, ${base.gasCount} gas${starts}${regionLabel}${clearing}`
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
  el('baseB').value = ids.includes(priorB)
    ? priorB
    : (ids.find((id) => id !== el('baseA').value) || ids[0] || '')
}

function describeBases() {
  const summary = baseDiscovery.bases.map((base) => `${baseLabel(base)}${base.routeAnchor === null ? ' (depot area currently blocked)' : ''}`)
  if (baseDiscovery.unplacedClusters) summary.push(`${baseDiscovery.unplacedClusters} qualifying cluster(s) had no suitable depot footprint.`)
  if (baseDiscovery.ignoredResources) summary.push(`${baseDiscovery.ignoredResources} unsupported, malformed, out-of-map, or duplicate resource(s) ignored.`)
  el('baseResult').textContent = summary.join('\n') || 'No qualifying mineral fields found.'
}

el('findBases').addEventListener('click', discoverBases)
async function discoverBases() {
  if (!terrain || analysisBusy()) return
  invalidateEntrances('Finding bases; entrance survey will need to be run again.')
  const revision = ++baseRevision
  baseRouteRevision++
  baseRoute = null
  const target = worker
  const snapshot = terrain
  basesInFlight = true
  el('analyze').disabled = true
  el('respectObstacles').disabled = true
  syncControls()
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
      syncControls()
    }
  }
}

async function requestBaseRoute() {
  if (!terrain || analysisBusy() || !hasBasePair()) return
  const [startId, endId] = selectedBaseIds()
  const revision = ++baseRouteRevision
  const target = worker
  const snapshot = terrain
  baseRouteInFlight = true
  baseRoute = null
  syncControls()
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
      syncControls()
    }
  }
}

el('compareBases').addEventListener('click', requestBaseRoute)
for (const id of ['baseA', 'baseB']) {
  el(id).addEventListener('change', () => {
    baseRouteRevision++
    baseRoute = null
    invalidateEntrances(undefined, true)
    terrain?.textures.delete('baseAreas')
    if (baseAreas) describeBaseAreas(baseAreas)
    populateWallSpans()
    el('baseRouteResult').textContent = hasBasePair()
      ? 'Choose Compare bases to route between the selected depot anchors.'
      : 'Choose two different bases with currently accessible depot anchors to compare.'
    syncControls()
    drawOverlay()
  })
}
el('showBases').addEventListener('change', drawOverlay)
el('entranceWidening').addEventListener('change', () => {
  invalidateEntrances('Inspect entrances again with the new widening threshold.')
  syncControls()
  drawOverlay()
})
el('showEntrances').addEventListener('change', drawOverlay)
el('findEntrances').addEventListener('click', inspectEntrances)
el('findBaseAreas').addEventListener('click', analyzeAllBaseAreas)
async function inspectEntrances() {
  if (!terrain || !baseDiscovery || !hasBasePair() || analysisBusy()) return
  const [startId, endId] = selectedBaseIds()
  const start = baseById(startId).routeAnchor
  const end = baseById(endId).routeAnchor
  const revision = ++entranceRevision
  const target = worker
  const snapshot = terrain
  const selectedSnapshot = `${startId}:${endId}`
  const widening = Number(el('entranceWidening').value)
  entranceInFlight = true
  entrances = null
  el('entranceResult').textContent = 'Inspecting terrain-only width transitions...'
  el('findEntrances').disabled = true
  el('findBaseAreas').disabled = true
  el('entranceWidening').disabled = true
  el('showEntrances').checked = true
  syncControls()
  drawOverlay()
  try {
    const result = await callWorker('findEntrances', {
      startX: start[0], startY: start[1], endX: end[0], endY: end[1], minWideningPercent: widening,
    })
    if (target !== worker || snapshot !== terrain || revision !== entranceRevision || selectedSnapshot !== `${selectedBaseIds()[0]}:${selectedBaseIds()[1]}`) return
    entrances = result
    el('entranceLegend').hidden = false
    const count = result.surveys.reduce((total, survey) => total + survey.candidates.length, 0)
    const summary = result.surveys.flatMap((survey) => {
      const baseId = survey.from === 'A' ? startId : endId
      if (!survey.candidates.length) return [`${survey.from} (Base ${baseId + 1}): no candidate within the first ${survey.maxDistancePixels / 32} tiles.`]
      return survey.candidates.map((candidate, index) => {
        const lowerBound = candidate.outwardWidthIsLowerBound ? 'at least ' : ''
        const turnNote = candidate.outwardSampleCount === 2 ? ' (lookahead stops before a turn)' : ''
        return `${survey.from}${index + 1} (Base ${baseId + 1}): ${(candidate.widthPixels / 32).toFixed(1)} tiles across; approach up to ${(candidate.approachMaxWidthPixels / 32).toFixed(1)}, widening to ${lowerBound}${(candidate.outwardMinWidthPixels / 32).toFixed(1)} tiles ahead${turnNote}; ${(candidate.distanceFromStartPixels / 32).toFixed(1)} tiles from ${survey.from}`
      })
    })
    el('entranceResult').textContent = !result.surveys.some((survey) => survey.route)
      ? `Base ${startId + 1} and Base ${endId + 1} are disconnected on terrain; no entrance survey route.`
      : `${count} directional candidate${count === 1 ? '' : 's'}, surveying outward from both bases (${result.elapsedMs} ms):\n${summary.join('\n')}`
    drawOverlay()
  } catch (error) {
    if (target === worker && snapshot === terrain && revision === entranceRevision) {
      el('entranceResult').textContent = `Entrance survey failed: ${error.message || error}`
    }
  } finally {
    if (target === worker && snapshot === terrain && revision === entranceRevision) {
      entranceInFlight = false
      el('entranceWidening').disabled = false
      syncControls()
    }
  }
}

el('cancelBaseAreas').addEventListener('click', () => {
  worker?.postMessage({ type: 'cancelBaseAreas', token: areaRevision })
  el('cancelBaseAreas').disabled = true
  el('baseAreaResult').textContent = 'Cancelling after the current origin search...'
})

async function analyzeAllBaseAreas() {
  if (!terrain || !baseDiscovery || analysisBusy()) return
  if (baseAreas) {
    el('overlay').value = 'baseAreas'
    describeBaseAreas(baseAreas)
    drawOverlay()
    return
  }
  const revision = ++areaRevision
  const target = worker
  const snapshot = terrain
  const catalog = baseDiscovery
  const current = () => target === worker && snapshot === terrain && catalog === baseDiscovery && revision === areaRevision
  areaInFlight = true
  el('cancelBaseAreas').hidden = false
  el('cancelBaseAreas').disabled = false
  el('baseAreaResult').textContent = 'Preparing map-wide base-area analysis...'
  syncControls()
  try {
    const result = await callWorker('findBaseAreas', {
      token: revision,
      minWideningPercent: Number(el('entranceWidening').value),
      bases: catalog.bases.map(({ id, routeAnchor }) => ({ id, routeAnchor })),
    }, [], (progress) => {
      if (current() && !el('cancelBaseAreas').disabled) {
        el('baseAreaResult').textContent = `Surveying base origins: ${progress.completedOrigins}/${progress.totalOrigins}; ${progress.surveyCount} directed surveys completed...`
      }
    })
    if (!current()) return
    // A completed worker reply may already be in transit when Cancel is clicked.
    if (el('cancelBaseAreas').disabled) {
      el('baseAreaResult').textContent = 'Base-area analysis cancelled. No partition was applied.'
      return
    }
    baseAreas = { ...result, labels: new Uint32Array(result.labels) }
    populateWallSpans()
    terrain.textures.delete('baseAreas')
    el('overlay').value = 'baseAreas'
    el('showEntrances').checked = true
    describeBaseAreas(baseAreas)
    drawOverlay()
  } catch (error) {
    if (current()) el('baseAreaResult').textContent = `${error.message || error} No partial partition was applied.`
  } finally {
    if (current()) {
      areaInFlight = false
      el('cancelBaseAreas').hidden = true
      syncControls()
    }
  }
}

el('resetRoute').addEventListener('click', () => {
  routeRevision++
  routeInFlight = false
  syncControls()
  selected = []
  route = null
  drawOverlay()
  el('routeResult').textContent = 'Click a walkable cell for A, then another for B.'
})


el('wallEnabled').addEventListener('change', () => {
  el('workspaceMode').value = el('wallEnabled').checked ? 'wall' : 'inspect'
  populateWallSpans()
  syncControls()
  drawOverlay()
})
el('wallSpanScope').addEventListener('change', () => {
  populateWallSpans()
  invalidateWallResult('Entrance list changed. Choose an entrance or use a manual window.')
  syncControls()
})
el('wallSpan').addEventListener('change', () => {
  if (selectedWallSpan()) centerWallOnSelectedSpan()
  else {
    wallCenterPending = true
    invalidateWallResult('Manual mode is armed: click the terrain to center the outlined query window.')
    syncControls()
  }
})
el('wallWindowSize').addEventListener('change', () => {
  if (wallWindow) {
    const center = [(wallWindow[0] + wallWindow[2]) / 2, (wallWindow[1] + wallWindow[3]) / 2]
    wallWindow = clampWallWindow(center, wallMapPixels(), Number(el('wallWindowSize').value))
    invalidateWallResult('Window size changed. Reposition A/B if either point is outside the outline.')
  }
  syncControls()
})
el('wallCenter').addEventListener('click', centerWallOnSelectedSpan)
el('searchGround').addEventListener('change', () => {
  if (selectedWallSpan()) centerWallOnSelectedSpan()
  else { invalidateWallResult(); syncControls() }
})
el('wallPickEntrance').addEventListener('click', () => {
  invalidateWallResult('Click anywhere near the desired chokepoint. The closest line will be selected.')
  wallPickPending = true
  wallPickHover = -1
  wallCenterPending = false
  el('wallPickEntrance').setAttribute('aria-pressed', 'true')
  syncControls()
  drawOverlay()
})
document.addEventListener('keydown', event => {
  if (event.key === 'Escape' && wallPickPending) {
    finishWallPick()
    syncControls()
    drawOverlay()
  }
})
el('wallManualCenter').addEventListener('click', () => {
  finishWallPick()
  el('wallSpan').value = 'manual'
  wallCenterPending = true
  invalidateWallResult('Manual centering is armed: click the terrain to move the window. Buildings are preserved.')
  syncControls()
})
el('wallMover').addEventListener('change', () => {
  invalidateWallResult('Mover changed. Check the wall again for its collision shape.')
  syncControls()
})
el('wallUndo').addEventListener('click', () => {
  if (!wallBuildings.length) return
  wallBuildings = wallBuildings.slice(0, -1)
  invalidateWallResult('Last building removed. Check the wall again.')
  syncControls()
})
el('wallClear').addEventListener('click', () => {
  if (!wallBuildings.length) return
  wallBuildings = []
  invalidateWallResult('All proposed buildings removed. Check the wall again.')
  syncControls()
})
el('checkWall').addEventListener('click', checkWall)

function wallPixelForCell(cell) {
  return [cell.x * 8 + 4, cell.y * 8 + 4]
}

function handleWallCanvasClick(cell, point = wallPixelForCell(cell)) {
  if (wallSearchPreview) return
  if (!wallEnabled() || analysisBusy()) return
  if ((wallPickPending || (!wallWindow && !wallCenterPending)) && baseAreas?.boundaries.length) {
    const nearest = nearestWallSpan(point, baseAreas.boundaries)
    if (nearest >= 0) {
      el('wallSpanScope').value = 'all'
      populateWallSpans()
      el('wallSpan').value = `span:${nearest}`
      centerWallOnSelectedSpan()
      return
    }
  }
  if (wallCenterPending || !wallWindow) {
    el('wallSpan').value = 'manual'
    setWallWindow(point)
    wallCenterPending = false
    return
  }
  if (!pointInHalfOpenRect(point, wallWindow)) {
    el('wallResult').textContent = 'That click is outside the outlined query window. Center a manual window there or choose a larger window before editing.'
    return
  }
  const tool = wallTool
  if (tool === 'start' || tool === 'end') {
    wallEndpoints[tool === 'start' ? 0 : 1] = point
    wallPlacementDirection = wallEndpoints[0].map((v, i) => wallEndpoints[1][i] - v)
    el('searchAutoEndpoints').checked = false
    invalidateWallResult(`${tool === 'start' ? 'A' : 'B'} test point moved. Check the wall again.`)
    syncControls()
    return
  }
  const tile = [Math.floor(cell.x / 4), Math.floor(cell.y / 4)]
  if (tool === 'remove') {
    const next = removeWallBuildingAt(wallBuildings, tile)
    if (next === wallBuildings) {
      el('wallResult').textContent = 'No proposed building covers that build tile.'
      return
    }
    wallBuildings = next
    invalidateWallResult('Building removed. Check the wall again.')
    syncControls()
    return
  }
  const building = selectedWallBuilding()
  if (!building) return
  if (wallBuildings.length >= MAX_WALL_BUILDINGS) {
    el('wallResult').textContent = `At most ${MAX_WALL_BUILDINGS} proposed buildings are supported. Remove one before placing another.`
    return
  }
  const centeredTile = centeredBuildingTile(point, building.footprint)
  wallBuildings = addWallBuilding(wallBuildings, building, centeredTile)
  invalidateWallResult(`${building.name} placed at build tile (${centeredTile[0]}, ${centeredTile[1]}). Check the wall again.`)
  syncControls()
}
function describeBaseAreas(experiment) {
  const separated = experiment.boundaries.filter((span) => span.separatedEdgeCount > 0).length
  const bypassed = experiment.boundaries.filter((span) => span.removedEdgeCount > span.separatedEdgeCount).length
  const ineffective = experiment.boundaries.filter((span) => !span.removedEdgeCount).length
  const lines = selectedAreas().map((area, index) => {
    if (!area) return `${index ? 'B' : 'A'}: no route anchor; no base area assigned.`
    const others = area.baseIds.filter((id) => id !== area.baseId)
    const connected = others.length ? `also contains Bases ${others.map((id) => id + 1).join(', ')}` : 'contains only this known base anchor'
    const reduction = area.cellCount < area.originalCellCount ? `${(100 * area.cellCount / area.originalCellCount).toFixed(1)}% of its original terrain component` : 'unchanged from its original terrain component'
    return `${index ? 'B' : 'A'} (Base ${area.baseId + 1}, area ${area.areaId}): ${(area.cellCount / 16).toFixed(0)} build tiles of area; ${reduction}; ${connected}.`
  })
  el('baseAreaResult').textContent = `${experiment.surveyCount} directed surveys across the map (${experiment.elapsedMs} ms); ${experiment.observationCount} distinct entrance observations plus ${experiment.rampCount || 0} terrain ramps consolidated to ${experiment.boundaries.length} boundaries.${experiment.crossingCutsRemoved ? ` ${experiment.crossingCutsRemoved} crossing or coincident cuts discarded.` : ''}${experiment.rampInteriorCutsRemoved ? ` ${experiment.rampInteriorCutsRemoved} cuts inside ramps removed.` : ''}${experiment.serialCutsRemoved ? ` ${experiment.serialCutsRemoved} serial or flat duplicate cuts removed.` : ''}${experiment.junctionCutsRemoved ? ` ${experiment.junctionCutsRemoved} internal junction cuts removed.` : ''} With all cuts applied: ${separated} boundaries separate components; ${bypassed} boundaries retain bypassed crossings; ${ineffective} boundaries cross no legal edges.\n${lines.join('\n')}\nExperimental terrain-only components; nearby surveys can miss exits. Muted sections have no buildable terrain within 2 tiles of their midpoint; other sections are not verified wall locations.${experiment.skippedAnchorCount ? ` ${experiment.skippedAnchorCount} bases without route anchors were not surveyed.` : ''}`
}

function drawTo(canvas, image) {
  canvas.width = image.width
  canvas.height = image.height
  canvas.getContext('2d').putImageData(new ImageData(new Uint8ClampedArray(image.data), image.width, image.height), 0, 0)
}

function drawTerrainBackdrop() {
  if (terrain) drawTo(el('terrainBackdrop'), previewImage || terrain.minimap)
}

function mapPixelAtEvent(event) {
  const rect = el('terrainOverlay').getBoundingClientRect()
  return [
    Math.floor((event.clientX - rect.left) / rect.width * terrain.widthWalkTiles * 8),
    Math.floor((event.clientY - rect.top) / rect.height * terrain.heightWalkTiles * 8),
  ]
}

function drawPlacementPreview() {
  const canvas = el('placementPreview')
  const viewport = el('mapViewport').getBoundingClientRect()
  const dpr = Math.min(window.devicePixelRatio || 1, 2)
  const width = Math.round(viewport.width * dpr), height = Math.round(viewport.height * dpr)
  if (canvas.width !== width || canvas.height !== height) { canvas.width = width; canvas.height = height }
  const ctx = canvas.getContext('2d')
  ctx.setTransform(1, 0, 0, 1, 0, 0)
  ctx.clearRect(0, 0, canvas.width, canvas.height)
  if (!wallPointer || !wallEnabled() || wallPickPending || wallCenterPending || !wallWindow || wallInFlight || wallSearchInFlight || wallSearchPreview
    || obstacleInFlight || analysisInFlight || wallTool !== 'place') return
  const building = selectedWallBuilding()
  if (!building || !pointInHalfOpenRect(wallPointer, wallWindow)) return
  const tile = centeredBuildingTile(wallPointer, building.footprint)
  const cells = placementPreviewCells(tile, building.footprint, terrain, wallBuildings)
  const surface = el('terrainOverlay').getBoundingClientRect()
  const scale = surface.width / (terrain.widthWalkTiles * 8)
  const x = surface.left - viewport.left, y = surface.top - viewport.top
  ctx.scale(dpr, dpr)
  ctx.lineWidth = 1.5
  for (const cell of cells) {
    ctx.fillStyle = cell.valid ? '#4ae08066' : '#ff586977'
    ctx.strokeStyle = cell.valid ? '#92ffb6' : '#ffabb3'
    ctx.fillRect(x + cell.x * 32 * scale, y + cell.y * 32 * scale, 32 * scale, 32 * scale)
    ctx.strokeRect(x + cell.x * 32 * scale, y + cell.y * 32 * scale, 32 * scale, 32 * scale)
  }
  const center = [(tile[0] * 32 + building.footprint[0] * 16), (tile[1] * 32 + building.footprint[1] * 16)]
  ctx.strokeStyle = '#fff0a3'
  ctx.setLineDash([4, 3])
  ctx.strokeRect(x + (center[0] - building.collision[0]) * scale,
    y + (center[1] - building.collision[1]) * scale,
    (building.collision[0] + building.collision[2] + 1) * scale,
    (building.collision[1] + building.collision[3] + 1) * scale)
  ctx.setLineDash([])
  const label = `${building.name} | ${tile[0]}, ${tile[1]}${cells.some(c => !c.valid) ? ' | blocked terrain / overlap' : ''}`
  ctx.font = '12px system-ui'
  const labelX = Math.max(4, Math.min(viewport.width - ctx.measureText(label).width - 16, x + tile[0] * 32 * scale))
  const labelY = Math.max(20, y + tile[1] * 32 * scale - 10)
  ctx.fillStyle = '#101923ee'
  ctx.fillRect(labelX, labelY - 15, ctx.measureText(label).width + 12, 21)
  ctx.fillStyle = '#f3fff7'
  ctx.fillText(label, labelX + 6, labelY)
  ctx.setTransform(1, 0, 0, 1, 0, 0)
}

function setWallTool(tool) {
  wallTool = tool
  for (const button of el('wallTool').querySelectorAll('button')) {
    button.setAttribute('aria-pressed', String(button.dataset.wallTool === tool))
  }
  drawPlacementPreview()
}
for (const button of el('wallTool').querySelectorAll('button')) {
  button.addEventListener('click', () => setWallTool(button.dataset.wallTool))
}
el('wallBuilding').addEventListener('change', drawPlacementPreview)
el('terrainOverlay').addEventListener('pointerleave', () => { wallPointer = null; drawPlacementPreview() })

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
  if (wallPickPending && terrain) {
    const hover = nearestWallSpan(mapPixelAtEvent(event), baseAreas?.boundaries || [])
    if (hover !== wallPickHover) { wallPickHover = hover; drawOverlay() }
    return
  }
  wallPointer = terrain && !camera.isPanning ? mapPixelAtEvent(event) : null
  drawPlacementPreview()
  const cell = selectedOverlayCell(event)
  if (!cell) return
  const elevation = (cell.flags >> 3) & 3
  const clearance = ` | square clearance radius ${terrain.clearance[cell.y * terrain.widthWalkTiles + cell.x]}px`
  el('hover').textContent = `Walk (${cell.x}, ${cell.y}) | pixel (${cell.x * 8 + 4}, ${cell.y * 8 + 4}) | ${cell.flags & 1 ? 'walkable' : 'blocked'}, ${cell.flags & 2 ? 'terrain-buildable' : 'terrain-unbuildable'}, ${cell.flags & 4 ? 'ramp' : 'flat'}, ${cell.flags & 32 ? 'map obstacle' : 'no map obstacle'}, elevation ${elevation}${clearance}`
  if (regions) el('hover').textContent += ' | region ' + (regions.labels[cell.y * terrain.widthWalkTiles + cell.x] || 'unassigned')
  if (baseAreas) el('hover').textContent += ' | terrain-only area ' + (baseAreas.labels[cell.y * terrain.widthWalkTiles + cell.x] || 'blocked')
})

el('terrainOverlay').addEventListener('click', async (event) => {
  const cell = selectedOverlayCell(event)
  if (!cell) return
  if (wallEnabled()) {
    handleWallCanvasClick(cell, mapPixelAtEvent(event))
    return
  }
  if (analysisBusy()) return
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
  syncControls()
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
    if (target === worker && snapshot === terrain && revision === routeRevision) {
      routeInFlight = false
      syncControls()
    }
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
  const clearanceMax = Number(el('clearanceMax').value)
  const clearanceColors = [[238, 100, 45], [40, 205, 195], [98, 86, 232]]
  const areaExperiment = baseAreas
  const [selectedA, selectedB] = selectedAreas()
  const areaA = selectedA?.areaId
  const areaB = selectedB?.areaId
  for (let index = 0; index < terrain.flags.length; index++) {
    const flags = terrain.flags[index]
    let color = null
    if (kind === 'walkability' && !(flags & 1)) color = flags & 32 && terrain.obstaclesEnabled ? [222, 55, 218] : [230, 60, 60]
    if (kind === 'buildability' && !(flags & 2)) color = [245, 165, 45]
    if (kind === 'ramps' && (flags & 4)) color = [175, 90, 230]
    if (kind === 'obstacles' && (flags & 32)) color = [222, 55, 218]
    if (kind === 'regions' && (flags & 1) && regions) {
      const regionId = regions.labels[index]
      if (regionId) color = [80 + (regionId * 67) % 150, 90 + (regionId * 43) % 140, 120 + (regionId * 29) % 120]
    }
    if (kind === 'baseAreas' && areaExperiment) {
      const label = areaExperiment.labels[index]
      if (label && label === areaA && label === areaB) color = [243, 180, 70]
      else if (label && label === areaA) color = [68, 204, 241]
      else if (label && label === areaB) color = [196, 128, 242]
    }
    if (kind === 'clearance') {
      const radius = terrain.clearance[index]
      if (!(flags & 1)) color = [42, 47, 58]
      else {
        const scale = Math.max(0, Math.min(1, (radius - 4) / (clearanceMax - 4))) * 2
        const segment = Math.min(1, Math.floor(scale))
        const fraction = scale - segment
        color = clearanceColors[segment].map((value, channel) =>
          Math.round(value + (clearanceColors[segment + 1][channel] - value) * fraction))
      }
    }
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

// Render text on the viewport canvas, with a fixed screen-space outline for contrast.
function drawMapLabel(ctx, unit, text, x, y) {
  ctx.save()
  ctx.globalAlpha = 1
  ctx.strokeStyle = '#101923'
  ctx.lineWidth = 3 * unit
  ctx.lineJoin = 'round'
  ctx.setLineDash([])
  ctx.strokeText(text, x, y)
  ctx.fillText(text, x, y)
  ctx.restore()
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
    drawMapLabel(ctx, unit, `${base.id + 1}${needsClearing(base) ? '*' : ''}`, tileX * 4 + unit * 2, tileY * 4 + 12 * unit)
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

// A single geometry-keyed terrain mask is shared by every building preview at this site.
// The Rust search computes the same mask; stale replies never replace a newer entrance/map.
function refreshPlacementReach() {
  const site = wallEnabled() && wallWindow ? wallPlacementSite(selectedSearchSide().span, Number(el('searchSize').value)) : null
  if (!site) {
    wallReachRequest = null
    reachQueue.select(null)
    return
  }
  const query = { window: [...wallWindow], placementSite: site }
  const key = JSON.stringify(query)
  if (wallReachRequest?.key === key && wallReachRequest.terrain === terrain && wallReachRequest.worker === worker) return
  wallReachRequest = { key, query, terrain, worker, result: null, error: null }
  reachQueue.select(wallReachRequest)
}

function drawWallEditor(ctx, unit) {
  if (!wallEnabled()) return
  ctx.save()
  if (wallWindow) {
    const [left, top, right, bottom] = wallWindow.map((value) => value / 8)
    ctx.strokeStyle = '#7ff4ff'
    ctx.lineWidth = 2 * unit
    ctx.setLineDash([5 * unit, 3 * unit])
    ctx.strokeRect(left, top, right - left, bottom - top)
    ctx.setLineDash([])
  }
  const area = wallSearchArea || currentPlacementArea()
  if (area) {
    const [left, top, right, bottom] = area
    const { span, elevation } = selectedSearchSide()
    const site = wallPlacementSite(span, Number(el('searchSize').value))
    const reach = site ? wallReachRequest : null
    ctx.fillStyle = '#ffe36e38'
    for (let y = Math.ceil(top / 32); (y + 1) * 32 <= bottom; y++) {
      for (let x = Math.ceil(left / 32); (x + 1) * 32 <= right; x++) {
        const center = [x * 32 + 16, y * 32 + 16]
        let eligible = !site || (pointInWallPlacementSite(center, site) && pointInWallPlacementReach(center, reach?.result))
        for (let dy = 0; dy < 4 && eligible; dy++) {
          for (let dx = 0; dx < 4; dx++) {
            const flags = terrain.flags[(y * 4 + dy) * terrain.widthWalkTiles + x * 4 + dx]
            if ((flags & 3) !== 3 || (terrain.obstaclesEnabled && (flags & 32))
              || (elevation != null && ((flags >> 3) & 3) !== elevation)) { eligible = false; break }
          }
        }
        if (eligible) ctx.fillRect(x * 4, y * 4, 4, 4)
      }
    }
    ctx.strokeStyle = '#ffe36e88'
    ctx.lineWidth = unit
    ctx.setLineDash([8 * unit, 4 * unit])
    if (!site) ctx.strokeRect(left / 8, top / 8, (right - left) / 8, (bottom - top) / 8)
    ctx.setLineDash([])
    ctx.fillStyle = '#ffe36e'
    ctx.font = `bold ${12 * unit}px system-ui`
    const label = site
      ? `Building center reach${reach?.error ? ' (preview unavailable)' : reach?.result ? '' : ' (loading)'}`
      : 'Placement zone'
    drawMapLabel(ctx, unit, elevation == null ? label : `${label}: ${el('searchGround').value === 'low' ? 'low' : 'high'} ground`, left / 8 + 4 * unit, top / 8 + 15 * unit)
  }
  const displayedBuildings = wallSearchPreview?.buildings || wallBuildings
  displayedBuildings.forEach((building, index) => {
    const resultPlacement = wallSearchPreview ? null : wallResult?.placements?.[index]
    const hasIssues = Boolean(resultPlacement?.issues?.length)
    const footprint = resultPlacement?.footprint || [
      building.tile[0] * 32, building.tile[1] * 32,
      (building.tile[0] + building.footprint[0]) * 32, (building.tile[1] + building.footprint[1]) * 32,
    ]
    const centerX = building.tile[0] * 32 + building.footprint[0] * 16
    const centerY = building.tile[1] * 32 + building.footprint[1] * 16
    const collision = resultPlacement?.collision || [
      centerX - building.collision[0], centerY - building.collision[1],
      centerX + building.collision[2] + 1, centerY + building.collision[3] + 1,
    ]
    ctx.fillStyle = hasIssues ? '#ff535966' : '#75f0a855'
    ctx.strokeStyle = hasIssues ? '#ff7378' : '#8bffbc'
    ctx.lineWidth = unit
    ctx.fillRect(footprint[0] / 8, footprint[1] / 8, (footprint[2] - footprint[0]) / 8, (footprint[3] - footprint[1]) / 8)
    ctx.strokeRect(footprint[0] / 8, footprint[1] / 8, (footprint[2] - footprint[0]) / 8, (footprint[3] - footprint[1]) / 8)
    ctx.strokeStyle = hasIssues ? '#ffe1e1' : '#fff3ab'
    ctx.setLineDash([2 * unit, 2 * unit])
    ctx.strokeRect(collision[0] / 8, collision[1] / 8, (collision[2] - collision[0]) / 8, (collision[3] - collision[1]) / 8)
    ctx.setLineDash([])
  })
  if (wallEndpoints) {
    ctx.font = `bold ${13 * unit}px system-ui`
    ctx.lineWidth = 2 * unit
    wallEndpoints.forEach(([x, y], index) => {
      ctx.fillStyle = index ? '#ffca67' : '#67e8ff'
      ctx.strokeStyle = '#13202b'
      ctx.beginPath()
      ctx.arc(x / 8, y / 8, 3 * unit, 0, Math.PI * 2)
      ctx.fill()
      ctx.stroke()
      drawMapLabel(ctx, unit, index ? 'Wall B' : 'Wall A', x / 8 + 4 * unit, y / 8 - 4 * unit)
    })
  }
  const displayedRoute = wallSearchPreview?.allowRoute || wallResult?.route
  if (displayedRoute?.length) {
    ctx.strokeStyle = '#ff5dbe'
    ctx.lineWidth = 3 * unit
    ctx.beginPath()
    displayedRoute.forEach(([x, y], index) => {
      if (index) ctx.lineTo(x / 8, y / 8)
      else ctx.moveTo(x / 8, y / 8)
    })
    ctx.stroke()
  }
  ctx.restore()
}
function drawOverlay() {
  refreshPlacementReach()
  drawPlacementPreview()
  el('baseAreaLegend').hidden = !terrain || !baseAreas || el('overlay').value !== 'baseAreas'
  el('entranceLegend').hidden = !terrain || !entrances?.surveys.some((survey) => survey.route) || !el('showEntrances').checked
  el('clearanceLegend').hidden = !terrain || el('overlay').value !== 'clearance'
  el('regionLegend').hidden = !terrain || el('overlay').value !== 'regions' || !regions
  el('clearanceScaleControl').hidden = el('clearanceLegend').hidden
  const clearanceMax = Number(el('clearanceMax').value)
  el('clearanceLegendText').textContent = `square radius: 4px (orange), ${(clearanceMax + 4) / 2}px (teal), ${clearanceMax}+px (violet)`
  el('elevationLegend').hidden = !terrain || el('overlay').value !== 'elevation'
  if (!terrain) return
  const canvas = el('terrainOverlay')
  const width = terrain.widthWalkTiles
  const height = terrain.heightWalkTiles
  const density = Math.min(4, 2048 / Math.max(width, height))
  if (canvas.width !== width * density) canvas.width = width * density
  if (canvas.height !== height * density) canvas.height = height * density
  const textureContext = canvas.getContext('2d')
  textureContext.setTransform(density, 0, 0, density, 0, 0)
  textureContext.clearRect(0, 0, width, height)
  textureContext.globalAlpha = Number(el('opacity').value) / 100
  textureContext.imageSmoothingEnabled = false
  textureContext.drawImage(overlayTexture(el('overlay').value), 0, 0, width, height)

  drawAnnotations()
}

function drawAnnotations() {
  if (!terrain) return
  const canvas = el('terrainOverlay')
  const width = terrain.widthWalkTiles
  const height = terrain.heightWalkTiles
  // Keep raster terrain in map space, but rasterize vectors and text at the current
  // viewport resolution. Zooming must not enlarge already-rasterized glyphs, or
  // require a canvas as large as the entire zoomed map.
  const annotations = el('mapAnnotations')
  const viewport = el('mapViewport').getBoundingClientRect()
  const rect = canvas.getBoundingClientRect()
  const dpr = Math.min(window.devicePixelRatio || 1, 2)
  annotations.width = Math.round(viewport.width * dpr)
  annotations.height = Math.round(viewport.height * dpr)
  const ctx = annotations.getContext('2d')
  ctx.setTransform(dpr * rect.width / width, 0, 0, dpr * rect.height / height,
    dpr * (rect.left - viewport.left), dpr * (rect.top - viewport.top))
  ctx.beginPath()
  ctx.rect(0, 0, width, height)
  ctx.clip()
  const unit = Math.max(width / (rect.width || 960), height / (rect.height || 960))
  ctx.lineWidth = unit
  drawBases(ctx, unit)
  if (regions && el('overlay').value === 'regions') {
    ctx.save()
    ctx.font = `bold ${12 * unit}px system-ui`
    ctx.textAlign = 'center'
    ctx.textBaseline = 'middle'
    ctx.fillStyle = '#ffffff'
    for (const region of regions.metadata.regions) {
      if (region.cellCount < 64) continue
      const [x, y] = region.peak
      ctx.beginPath()
      ctx.arc(x + 0.5, y + 0.5, 2 * unit, 0, Math.PI * 2)
      ctx.fill()
      drawMapLabel(ctx, unit, String(region.id), x + 4 * unit, y + 0.5)
    }
    if (el('showPassages').checked) {
      ctx.strokeStyle = '#fff0a8'
      ctx.fillStyle = '#fff0a8'
      for (const passage of regions.metadata.passages) {
        const [[ax, ay], [bx, by]] = passage.endpoints
        const x = (ax + bx) / 2 + 0.5
        const y = (ay + by) / 2 + 0.5
        ctx.beginPath()
        ctx.arc(x, y, 3 * unit, 0, Math.PI * 2)
        ctx.stroke()
        drawMapLabel(ctx, unit, String(passage.clearanceRadiusPixels) + 'px', x, y - 6 * unit)
      }
    }
    ctx.restore()
  }
  const point = (cell) => [cell.x + 0.5, cell.y + 0.5]
  const radius = 4 * unit
  ctx.lineWidth = 2 * unit
  ctx.strokeStyle = '#ffffff'
  ctx.fillStyle = '#eef7ff'
  ctx.font = `bold ${12 * unit}px system-ui`
  selected.forEach((cell, index) => {
    const [x, y] = point(cell)
    ctx.beginPath()
    ctx.arc(x, y, radius, 0, Math.PI * 2)
    ctx.stroke()
    drawMapLabel(ctx, unit, index ? 'B' : 'A', x + radius, y - radius)
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
  if (entrances && el('showEntrances').checked) {
    ctx.save()
    ctx.strokeStyle = '#54e7f2'
    ctx.setLineDash([3 * unit, 3 * unit])
    ctx.lineWidth = unit
    for (const survey of entrances.surveys) {
      ctx.beginPath()
      survey.route?.points?.forEach(([x, y], index) => {
        if (index) ctx.lineTo(x + 0.5, y + 0.5)
        else ctx.moveTo(x + 0.5, y + 0.5)
      })
      if (survey.route?.points?.length) ctx.stroke()
    }
    ctx.setLineDash([])
    ctx.lineWidth = 2 * unit
    ctx.fillStyle = '#9ffaff'
    ctx.strokeStyle = '#9ffaff'
    ctx.font = `bold ${12 * unit}px system-ui`
    for (const survey of entrances.surveys) {
      survey.candidates.forEach((candidate, index) => {
        const [[left, top], [right, bottom]] = candidate.endpoints
        const x1 = left / 8
        const y1 = top / 8
        const x2 = right / 8
        const y2 = bottom / 8
        ctx.beginPath()
        ctx.moveTo(x1, y1)
        ctx.lineTo(x2, y2)
        ctx.stroke()
        drawMapLabel(ctx, unit, `${survey.from}${index + 1}: ${(candidate.widthPixels / 32).toFixed(1)} tiles`, (x1 + x2) / 2 + 3 * unit, (y1 + y2) / 2)
      })
    }
    ctx.restore()
  }
  if (baseAreas && el('showEntrances').checked) {
    ctx.save()
    ctx.lineWidth = 2 * unit
    ctx.font = `bold ${12 * unit}px system-ui`
    baseAreas.boundaries.forEach((span, index) => {
      const bypassed = span.removedEdgeCount > span.separatedEdgeCount
      const remoteFromBuildable = span.buildableNearMidpoint === false
      ctx.globalAlpha = remoteFromBuildable ? 0.55 : 1
      ctx.strokeStyle = remoteFromBuildable || !span.removedEdgeCount ? '#a0a5af' : bypassed ? '#ffc15a' : '#72ffac'
      ctx.fillStyle = ctx.strokeStyle
      ctx.setLineDash(bypassed ? [3 * unit, 2 * unit] : [])
      const [[x1, y1], [x2, y2]] = span.endpoints.map(([x, y]) => [x / 8, y / 8])
      ctx.beginPath()
      ctx.moveTo(x1, y1)
      ctx.lineTo(x2, y2)
      ctx.stroke()
      drawMapLabel(ctx, unit, span.kind === 'ramp' ? `R${span.rampId}${span.rampEnd === 'lower' ? 'L' : 'U'}` : `E${index + 1}`, (x1 + x2) / 2 + 3 * unit, (y1 + y2) / 2 - 4 * unit)
    })
    ctx.restore()
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
  if (wallEnabled()) {
    const highlighted = wallPickPending ? baseAreas?.boundaries[wallPickHover] : selectedSearchSide().span
    if (highlighted) {
      ctx.save()
      ctx.strokeStyle = '#fff4a8'
      ctx.lineWidth = 4 * unit
      ctx.beginPath()
      ctx.moveTo(...highlighted.endpoints[0].map(v => v / 8))
      ctx.lineTo(...highlighted.endpoints[1].map(v => v / 8))
      ctx.stroke()
      ctx.restore()
    }
  }
  drawWallEditor(ctx, unit)
}
