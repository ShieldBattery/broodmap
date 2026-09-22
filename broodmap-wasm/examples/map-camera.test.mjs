import assert from 'node:assert/strict'
import test from 'node:test'
import { clampTranslation, createMapCamera, fitScale, focusCamera, screenToMap, zoomAnchor } from './map-camera.js'

test('fit scale shows the full map', () => {
  assert.equal(fitScale(2000, 1000, 800, 600), 0.4)
  assert.equal(fitScale(0, 1000, 800, 600), 0)
})

test('camera bounds keep a larger map covering the viewport and center a smaller axis', () => {
  assert.deepEqual(clampTranslation({ x: 20, y: -900, scale: 1 }, 2000, 400, 800, 600), {
    x: 0, y: 100, scale: 1,
  })
  assert.deepEqual(clampTranslation({ x: -1800, y: 900, scale: 1 }, 2000, 1000, 800, 600), {
    x: -1200, y: 0, scale: 1,
  })
})

test('zoom anchor preserves the map point under the pointer before edge clamping', () => {
  const before = { scale: 2, x: -150, y: -90 }
  const point = { x: 310, y: 170 }
  const mapPoint = screenToMap(point, before)
  const after = zoomAnchor(before, 1.5, point, 0.25, 8)
  assert.equal(after.scale, 3)
  assert.deepEqual(screenToMap(point, after), mapPoint)
})

test('focus frames a map rectangle with padding and respects camera limits', () => {
  const focused = focusCamera([400, 200, 800, 400], 50, 2000, 1000, 1000, 600, 0.5, 8)
  assert.deepEqual(focused, { scale: 2, x: -700, y: -300 })
  const limited = focusCamera([0, 0, 50, 50], 0, 2000, 1000, 1000, 600, 0.5, 3)
  assert.equal(limited.scale, 3)
})


class FakeTarget {
  constructor(rect = { left: 0, top: 0, width: 1000, height: 600 }) {
    this.rect = rect
    this.style = {}
    this.listeners = new Map()
  }

  addEventListener(type, listener, capture = false) {
    const listeners = this.listeners.get(type) || []
    listeners.push({ listener, capture })
    this.listeners.set(type, listeners)
  }

  removeEventListener(type, listener) {
    this.listeners.set(type, (this.listeners.get(type) || []).filter((entry) => entry.listener !== listener))
  }

  getBoundingClientRect() { return this.rect }
  contains() { return false }
  setPointerCapture() {}
  hasPointerCapture() { return false }
  releasePointerCapture() {}

  emit(type, properties = {}) {
    const event = {
      type, target: this, defaultPrevented: false, stopped: false,
      preventDefault() { this.defaultPrevented = true },
      stopImmediatePropagation() { this.stopped = true },
      ...properties,
    }
    for (const { listener } of this.listeners.get(type) || []) {
      listener(event)
      if (event.stopped) break
    }
    return event
  }
}

test('refresh preserves the world centre and a pan gesture suppresses its click', () => {
  const previousWindow = globalThis.window
  const previousDocument = globalThis.document
  const viewport = new FakeTarget()
  const surface = new FakeTarget()
  const changes = []
  globalThis.window = new FakeTarget()
  globalThis.document = { activeElement: null }
  try {
    const camera = createMapCamera({ viewport, surface, onChange: (state) => changes.push(state) })
    camera.setMapSize(2000, 1000)
    camera.zoomBy(2)
    viewport.rect = { left: 0, top: 0, width: 1200, height: 600 }
    camera.refresh()
    assert.deepEqual(changes.at(-1), {
      scale: 1, x: -400, y: -200, mapWidth: 2000, mapHeight: 1000, fitted: false,
    })

    let ordinaryClicks = 0
    viewport.addEventListener('click', () => { ordinaryClicks++ })
    viewport.emit('pointerenter')
    globalThis.window.emit('keydown', { code: 'Space', target: viewport })
    viewport.emit('pointerdown', { button: 0, pointerId: 3, clientX: 100, clientY: 100 })
    assert.equal(camera.isPanning, true)
    viewport.emit('pointerup', { pointerId: 3 })
    assert.equal(camera.isPanning, false)
    viewport.emit('click')
    assert.equal(ordinaryClicks, 0)
    viewport.emit('click')
    assert.equal(ordinaryClicks, 1)
    camera.destroy()
  } finally {
    globalThis.window = previousWindow
    globalThis.document = previousDocument
  }
})


test('Space preserves focused controls even while the pointer is over the map', () => {
  const previousWindow = globalThis.window, previousDocument = globalThis.document
  const viewport = new FakeTarget()
  globalThis.window = new FakeTarget()
  globalThis.document = { activeElement: null }
  try {
    const camera = createMapCamera({ viewport, surface: new FakeTarget() })
    camera.setMapSize(2000, 1000)
    viewport.emit('pointerenter')
    for (const target of [
      { tagName: 'BUTTON' }, { tagName: 'SELECT' }, { tagName: 'SUMMARY' },
      { tagName: 'SPAN', closest: () => ({ tagName: 'BUTTON' }) },
    ]) {
      assert.equal(window.emit('keydown', { code: 'Space', target }).defaultPrevented, false)
      viewport.emit('pointerdown', { button: 0, pointerId: 1 })
      assert.equal(camera.isPanning, false)
    }
    assert.equal(window.emit('keydown', { code: 'Space', target: viewport }).defaultPrevented, true)
    viewport.emit('pointerdown', { button: 0, pointerId: 2, clientX: 10, clientY: 10 })
    assert.equal(camera.isPanning, true)
    camera.destroy()
  } finally {
    globalThis.window = previousWindow
    globalThis.document = previousDocument
  }
})
