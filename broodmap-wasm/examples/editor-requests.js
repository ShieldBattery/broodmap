/** Capture the identities and revision which own an asynchronous UI update. */
export function captureRequest(readState) {
  const saved = [...readState()]
  return () => {
    const current = readState()
    return current.length === saved.length && current.every((value, index) => value === saved[index])
  }
}

/** Debounce preview changes and keep at most one request running and one latest request waiting. */
export function createPreviewQueue(load, onChange, {
  delay = 60, schedule = setTimeout, cancel = clearTimeout,
} = {}) {
  let latest = null
  let waiting = null
  let timer = null
  let active = false
  const launch = () => {
    if (active || timer !== null || !waiting) return
    const request = waiting
    waiting = null
    active = true
    Promise.resolve().then(() => load(request)).then(result => {
      if (latest !== request) return
      request.result = result
      onChange()
    }, error => {
      if (latest !== request) return
      request.error = error
      onChange()
    }).finally(() => {
      active = false
      launch()
    })
  }
  return {
    select(request) {
      if (latest === request) return
      latest = waiting = request
      if (timer !== null) cancel(timer)
      timer = null
      if (request) timer = schedule(() => { timer = null; launch() }, delay)
    },
  }
}
