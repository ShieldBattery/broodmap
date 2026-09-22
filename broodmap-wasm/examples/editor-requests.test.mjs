import assert from 'node:assert/strict'
import test from 'node:test'
import { setImmediate } from 'node:timers/promises'
import { captureRequest, createPreviewQueue } from './editor-requests.js'

function previewHarness() {
  const timers = new Map(), calls = [], changes = []
  let id = 0
  const queue = createPreviewQueue(request => new Promise((resolve, reject) => {
    calls.push({ request, resolve, reject })
  }), () => changes.push(true), {
    schedule(fn) { timers.set(++id, fn); return id },
    cancel(id) { timers.delete(id) },
  })
  return { queue, calls, changes, async flush() {
    const callbacks = [...timers.values()]
    timers.clear()
    callbacks.forEach(fn => fn())
    await setImmediate()
  } }
}

test('preview queue debounces rapid choices and only runs the latest waiting request', async () => {
  const h = previewHarness(), a = {}, b = {}, c = {}, d = {}
  h.queue.select(a)
  h.queue.select(b)
  await h.flush()
  assert.deepEqual(h.calls.map(c => c.request), [b])
  h.queue.select(c)
  h.queue.select(d)
  await h.flush()
  assert.equal(h.calls.length, 1)
  h.calls[0].resolve('obsolete')
  await setImmediate()
  assert.equal(b.result, undefined)
  assert.equal(h.changes.length, 0)
  assert.deepEqual(h.calls.map(c => c.request), [b, d])
  h.calls[1].resolve('current')
  await setImmediate()
  assert.equal(d.result, 'current')
  assert.equal(h.changes.length, 1)
})

test('clearing previews drops queued work and old failures cannot replace a new preview', async () => {
  const h = previewHarness(), old = {}, next = {}, unused = {}
  h.queue.select(old)
  await h.flush()
  h.queue.select(unused)
  h.queue.select(null)
  await h.flush()
  h.calls[0].reject(new Error('old worker failed'))
  await setImmediate()
  assert.equal(old.error, undefined)
  assert.equal(h.calls.length, 1)
  assert.equal(h.changes.length, 0)
  h.queue.select(next)
  await h.flush()
  const error = new Error('current worker failed')
  h.calls[1].reject(error)
  await setImmediate()
  assert.equal(next.error, error)
  assert.equal(h.changes.length, 1)
})

test('request ownership expires on worker, terrain, revision, or entrance catalog changes', () => {
  for (let changed = 0; changed < 4; changed++) {
    const state = [{}, {}, 5, {}]
    const current = captureRequest(() => state)
    assert.equal(current(), true)
    state[changed] = changed === 2 ? 6 : {}
    assert.equal(current(), false)
  }
})
