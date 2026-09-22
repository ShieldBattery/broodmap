import assert from 'node:assert/strict'
import { readFile } from 'node:fs/promises'
import test from 'node:test'
import vm from 'node:vm'
import * as editor from './wall-editor.js'
import * as requests from './editor-requests.js'

// Exercise the actual page handlers with deterministic worker replies. Rendering and browser
// event delivery have separate browser checks; this fixture isolates asynchronous UI ownership.
const source = (await readFile(new URL('./main.js', import.meta.url), 'utf8'))
  .replace(/^import[\s\S]*?from ['"][^'"]+['"]\r?\n/gm, '')
  .replaceAll('import.meta.url', '"http://localhost/main.js"')
class Element {
  constructor() { this.value = ''; this.options = []; this.listeners = new Map(); this.style = {}; this.hidden = false; this.disabled = false }
  addEventListener(type, fn) { this.listeners.set(type, [...(this.listeners.get(type) || []), fn]) }
  querySelectorAll() { return [] }
  replaceChildren(...items) { this.options = items }
  prepend(item) { this.options.unshift(item) }
  setAttribute() {}
  getBoundingClientRect() { return { width: 100, height: 100, left: 0, top: 0 } }
  emit(type, event) { return Promise.all((this.listeners.get(type) || []).map(fn => fn(event))) }
}
function harness() {
  const elements = new Map(), sent = [], document = new Element(), window = new Element()
  const el = id => { if (!elements.has(id)) elements.set(id, new Element()); return elements.get(id) }
  document.getElementById = el
  let cameraChange
  const context = vm.createContext({ ...editor, ...requests, document, window, URL, console, setTimeout, clearTimeout,
    Option: class { constructor(text, value) { this.textContent = text; this.value = value } },
    createMapCamera({ onChange }) { cameraChange = onChange; return { scale: 1 } },
    send: data => sent.push(data), counts: { raster: 0, annotations: 0 },
  })
  const run = script => vm.runInContext(script, context)
  run(source)
  run(`
    drawOverlay = () => counts.raster++;
    drawAnnotations = () => counts.annotations++;
    drawPlacementPreview = () => {};
    worker = { postMessage: send };
    mapInfo = {};
    terrain = { widthWalkTiles: 32, heightWalkTiles: 32, textures: new Map() };
    wallProfiles = { buildings: [109,111].map(id => ({ id, name: String(id), footprint: [1,1], collision: [1,1,1,1] })), movers: [{ id:65, name:'Zealot', collision:[1,1,1,1] }] };
    wallWindow = [0,0,128,128]; wallEndpoints = [[4,64],[124,64]];
    wallPlacementCenter = [64,64]; wallPlacementDirection = [1,0];
    baseDiscovery = { bases: [] }; baseAreas = { boundaries: [], baseAreas: [] };
  `)
  el('wallEnabled').checked = true
  for (const [id, value] of Object.entries({ wallSpan:'manual', wallSpanScope:'all', searchGround:'high', searchDepots:'1', searchBarracks:'1', searchBlock:'65', searchAllow:'none', searchSize:'256', searchBudget:'2000' })) el(id).value = value
  const reply = (id, error = null) => { context.replyId = id; context.replyError = error; run(`
    { const job = pending.get(replyId); pending.delete(replyId);
      if (replyError) job.reject(new Error(replyError));
      else job.resolve({ layouts: [], candidatePlacements: 5, exploredStates: 6, elapsedMs: 1, reason: 'exhausted' }); }
  `) }
  return { run, el, sent, reply, document, cameraChange, context }
}

test('wall jobs disable conflicting analysis controls and handlers do not enqueue work', async () => {
  const h = harness(), done = h.run('findWalls()')
  assert.equal(h.el('findBases').disabled, true)
  assert.equal(h.el('findRegions').disabled, true)
  assert.equal(h.el('analyze').disabled, true)
  await h.run('discoverBases()')
  assert.equal(h.sent.filter(q => q.type === 'findBases').length, 0)
  h.reply(h.sent.find(q => q.type === 'findWalls').id)
  await done
  assert.equal(h.el('findBases').disabled, false)
  h.run('basesInFlight = true; syncControls()')
  assert.equal(h.el('findWalls').disabled, true)
  const count = h.sent.length
  await h.run('findWalls()')
  assert.equal(h.sent.length, count)
})

test('entrance invalidation cancels the old search and late success/error cannot clear a new job', async () => {
  for (const error of [null, 'old worker failed']) {
    const h = harness(), old = h.run('findWalls()')
    const oldId = h.sent.find(q => q.type === 'findWalls').id
    h.run('invalidateBaseAreas()')
    assert(h.sent.some(q => q.type === 'cancelWallSearch'))
    const next = h.run('findWalls()')
    const nextId = h.sent.filter(q => q.type === 'findWalls').at(-1).id
    const before = h.el('wallSearchStatus').textContent
    h.reply(oldId, error)
    await old
    assert.equal(h.el('wallSearchStatus').textContent, before)
    assert.equal(h.run('wallSearchInFlight'), true)
    assert.equal(h.run('wallSearchResult'), null)
    h.reply(nextId)
    await next
    assert.equal(h.run('wallSearchInFlight'), false)
    assert.equal(h.run('wallSearchResult.exploredStates'), 6)
  }
})

test('map/terrain replacement rejects stale search progress and results', async () => {
  const h = harness(), done = h.run('findWalls()')
  const id = h.sent.find(q => q.type === 'findWalls').id
  h.run('resetWall(); terrain = {}; worker = { postMessage: send }')
  const before = h.el('wallSearchStatus').textContent
  h.context.oldId = id
  h.run('pending.get(oldId).onProgress({ exploredStates: 999, foundLayouts: 9 })')
  assert.equal(h.el('wallSearchStatus').textContent, before)
  h.reply(id)
  await done
  assert.equal(h.run('wallSearchResult'), null)
  assert.equal(h.el('wallSearchStatus').textContent, before)
})

test('camera movement repaints annotations without re-rasterizing the map overlay', () => {
  const h = harness()
  for (let i = 0; i < 20; i++) h.cameraChange()
  assert.equal(h.context.counts.raster, 0)
  assert.equal(h.context.counts.annotations, 20)
})

test('non-file drops prevent browser navigation without trying to load a map', async () => {
  const h = harness()
  let prevented = false
  await h.document.emit('drop', { dataTransfer: { types: ['text/uri-list'] }, preventDefault() { prevented = true } })
  assert.equal(prevented, true)
  assert.equal(h.sent.length, 0)
})

test('missing stock wall inventory explains why automatic layouts are unavailable', () => {
  const h = harness()
  h.run('wallProfiles.buildings.pop(); syncControls()')
  assert.equal(h.el('findWalls').disabled, true)
  assert.match(h.el('wallSearchStatus').textContent, /profiles.*unavailable/)
  assert.equal(h.el('wallBuilding').disabled, false)
})


test('each analysis job locks wall controls until its busy state clears', () => {
  const h = harness()
  for (const flag of ['basesInFlight', 'baseRouteInFlight', 'regionInFlight', 'entranceInFlight', 'areaInFlight', 'routeInFlight', 'analysisInFlight', 'obstacleInFlight']) {
    h.run(`${flag} = true; syncControls()`)
    assert.equal(h.el('findWalls').disabled, true, flag)
    h.run(`${flag} = false; syncControls()`)
    assert.equal(h.el('findWalls').disabled, false, flag)
  }
})
