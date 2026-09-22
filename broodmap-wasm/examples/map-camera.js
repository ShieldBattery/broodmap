const DEFAULT_PADDING = 64
const MAX_SCALE = 8

function finitePositive(value) {
  return Number.isFinite(value) && value > 0
}

function clamp(value, minimum, maximum) {
  return Math.max(minimum, Math.min(maximum, value))
}

/** Returns the scale that shows the complete map in the viewport. */
export function fitScale(mapWidth, mapHeight, viewportWidth, viewportHeight) {
  if (![mapWidth, mapHeight, viewportWidth, viewportHeight].every(finitePositive)) return 0
  return Math.min(viewportWidth / mapWidth, viewportHeight / mapHeight)
}

/** Keeps a translated map inside its viewport, centering an undersized axis. */
export function clampTranslation({ x, y, scale }, mapWidth, mapHeight, viewportWidth, viewportHeight) {
  if (![mapWidth, mapHeight, viewportWidth, viewportHeight, scale].every(finitePositive)) return { x: 0, y: 0, scale }
  const clampAxis = (offset, mapLength, viewportLength) => {
    const scaledLength = mapLength * scale
    if (scaledLength <= viewportLength) return (viewportLength - scaledLength) / 2
    return clamp(offset, viewportLength - scaledLength, 0)
  }
  return {
    x: clampAxis(x, mapWidth, viewportWidth),
    y: clampAxis(y, mapHeight, viewportHeight),
    scale,
  }
}

/** Converts viewport-relative CSS pixels to map CSS pixels. */
export function screenToMap({ x, y }, camera) {
  return [(x - camera.x) / camera.scale, (y - camera.y) / camera.scale]
}

/** Converts map CSS pixels to viewport-relative CSS pixels. */
export function mapToScreen([x, y], camera) {
  return [camera.x + x * camera.scale, camera.y + y * camera.scale]
}

/** Zooms around a viewport-relative point before viewport bounds are applied. */
export function zoomAnchor(camera, factor, point, minimumScale = 0, maximumScale = Infinity) {
  if (!finitePositive(camera.scale) || !finitePositive(factor)) return { ...camera }
  const scale = clamp(camera.scale * factor, minimumScale, maximumScale)
  const [mapX, mapY] = screenToMap(point, camera)
  return { scale, x: point.x - mapX * scale, y: point.y - mapY * scale }
}

/** Produces a bounded camera which frames a map rectangle and optional padding. */
export function focusCamera(bounds, padding, mapWidth, mapHeight, viewportWidth, viewportHeight, minimumScale = 0, maximumScale = Infinity) {
  if (!Array.isArray(bounds) || bounds.length !== 4) return null
  const [left, top, right, bottom] = bounds
  const width = right - left
  const height = bottom - top
  if (![left, top, right, bottom, width, height, mapWidth, mapHeight, viewportWidth, viewportHeight].every(Number.isFinite) || width <= 0 || height <= 0 || !finitePositive(mapWidth) || !finitePositive(mapHeight) || !finitePositive(viewportWidth) || !finitePositive(viewportHeight)) return null
  const safePadding = Math.max(0, Number.isFinite(padding) ? padding : 0)
  const scale = clamp(
    Math.min(viewportWidth / (width + safePadding * 2), viewportHeight / (height + safePadding * 2)),
    minimumScale,
    maximumScale,
  )
  return clampTranslation({
    scale,
    x: viewportWidth / 2 - (left + width / 2) * scale,
    y: viewportHeight / 2 - (top + height / 2) * scale,
  }, mapWidth, mapHeight, viewportWidth, viewportHeight)
}

/**
 * Adds mouse/touch camera controls to a map surface. Coordinates are CSS map
 * pixels: callers should size canvas layers to the map, then let this element
 * transform their common surface.
 */
export function createMapCamera({ viewport, surface, onChange = () => {} }) {
  if (!viewport?.addEventListener || !surface?.style) throw new TypeError('createMapCamera requires viewport and surface elements')
  if (typeof onChange !== 'function') throw new TypeError('onChange must be a function')

  let mapWidth = 0
  let mapHeight = 0
  let scale = 1
  let x = 0
  let y = 0
  let fitted = true
  let destroyed = false
  let spaceDown = false
  let pointerInside = false
  let drag = null
  let suppressClick = false
  let suppressTimer = null
  let lastViewport = null

  viewport.style.overflow = 'hidden'
  viewport.style.touchAction = 'none'
  surface.style.transformOrigin = '0 0'

  const hasMap = () => finitePositive(mapWidth) && finitePositive(mapHeight)
  const dimensions = () => {
    const rect = viewport.getBoundingClientRect()
    return { width: rect.width, height: rect.height, left: rect.left, top: rect.top }
  }
  const isUsable = () => {
    const { width, height } = dimensions()
    return hasMap() && finitePositive(width) && finitePositive(height)
  }
  const snapshot = () => ({ scale, x, y, mapWidth, mapHeight, fitted })
  const apply = (changed = true) => {
    surface.style.width = `${mapWidth}px`
    surface.style.height = `${mapHeight}px`
    surface.style.transform = `translate(${x}px, ${y}px) scale(${scale})`
    const viewportSize = dimensions()
    if (finitePositive(viewportSize.width) && finitePositive(viewportSize.height)) lastViewport = viewportSize
    if (changed) onChange(snapshot())
  }
  const constrain = () => {
    const { width, height } = dimensions()
    if (!hasMap() || !finitePositive(width) || !finitePositive(height)) return
    ;({ x, y } = clampTranslation({ x, y, scale }, mapWidth, mapHeight, width, height))
  }
  const minimumScale = () => {
    const { width, height } = dimensions()
    return fitScale(mapWidth, mapHeight, width, height)
  }
  const releaseDrag = () => {
    if (!drag) return
    const pointerId = drag.pointerId
    drag = null
    if (viewport.hasPointerCapture?.(pointerId)) viewport.releasePointerCapture(pointerId)
  }
  const scheduleClickReset = () => {
    if (suppressTimer !== null) clearTimeout(suppressTimer)
    suppressTimer = setTimeout(() => {
      suppressClick = false
      suppressTimer = null
    }, 0)
  }
  const fit = () => {
    const { width, height } = dimensions()
    const nextScale = fitScale(mapWidth, mapHeight, width, height)
    fitted = true
    if (!nextScale) {
      apply()
      return
    }
    scale = nextScale
    x = (width - mapWidth * scale) / 2
    y = (height - mapHeight * scale) / 2
    apply()
  }
  const zoomBy = (factor, clientPoint = null) => {
    if (!isUsable() || !finitePositive(factor)) return
    const { width, height, left, top } = dimensions()
    const point = clientPoint
      ? { x: (clientPoint.x ?? clientPoint.clientX) - left, y: (clientPoint.y ?? clientPoint.clientY) - top }
      : { x: width / 2, y: height / 2 }
    const next = zoomAnchor({ scale, x, y }, factor, point, minimumScale(), MAX_SCALE)
    if (next.scale === scale && next.x === x && next.y === y) return
    scale = next.scale
    x = next.x
    y = next.y
    fitted = scale === minimumScale()
    constrain()
    apply()
  }
  const focus = (bounds, padding = DEFAULT_PADDING) => {
    if (!isUsable()) return
    const { width, height } = dimensions()
    const next = focusCamera(bounds, padding, mapWidth, mapHeight, width, height, minimumScale(), MAX_SCALE)
    if (!next) return
    ;({ scale, x, y } = next)
    fitted = scale === minimumScale()
    apply()
  }
  const refresh = () => {
    if (!hasMap()) {
      apply(false)
      return
    }
    if (fitted) {
      fit()
      return
    }
    const { width, height } = dimensions()
    if (!finitePositive(width) || !finitePositive(height)) return
    // The previous viewport centre remains at the new viewport centre.
    const oldWidth = lastViewport?.width ?? width
    const oldHeight = lastViewport?.height ?? height
    const worldCenter = [(oldWidth / 2 - x) / scale, (oldHeight / 2 - y) / scale]
    x = width / 2 - worldCenter[0] * scale
    y = height / 2 - worldCenter[1] * scale
    constrain()
    apply()
  }
  const editableTarget = (target) => {
    if (!target) return false
    if (target.isContentEditable) return true
    return ['INPUT', 'SELECT', 'TEXTAREA', 'BUTTON', 'SUMMARY', 'A'].includes(target.tagName)
      || Boolean(target.closest?.('button, summary, a[href], [role=button], [contenteditable=true]'))
  }
  const keyboardApplies = (event) => !editableTarget(event.target) && (pointerInside || viewport.contains(document.activeElement))
  const onKeyDown = (event) => {
    if (event.code !== 'Space' || !keyboardApplies(event)) return
    spaceDown = true
    event.preventDefault()
  }
  const onKeyUp = (event) => {
    if (event.code !== 'Space') return
    if (spaceDown) event.preventDefault()
    spaceDown = false
  }
  const onWheel = (event) => {
    if (!isUsable()) return
    event.preventDefault()
    const delta = clamp(event.deltaY, -240, 240)
    zoomBy(Math.exp(-delta * 0.002), { x: event.clientX, y: event.clientY })
  }
  const onPointerDown = (event) => {
    if (!hasMap() || (event.button !== 1 && !(event.button === 0 && spaceDown))) return
    event.preventDefault()
    drag = { pointerId: event.pointerId, x: event.clientX, y: event.clientY, startX: event.clientX, startY: event.clientY, moved: false }
    // A Space+left press is a camera gesture even if it does not move.
    suppressClick = true
    viewport.setPointerCapture?.(event.pointerId)
    // Consumers use this notification to clear hover-only UI immediately.
    onChange(snapshot())
  }
  const onPointerMove = (event) => {
    if (!drag || event.pointerId !== drag.pointerId) return
    const dx = event.clientX - drag.x
    const dy = event.clientY - drag.y
    if (dx || dy) {
      drag.x = event.clientX
      drag.y = event.clientY
      if (Math.hypot(event.clientX - drag.startX, event.clientY - drag.startY) >= 2) drag.moved = true
      x += dx
      y += dy
      fitted = false
      constrain()
      apply()
    }
    event.preventDefault()
  }
  const onPointerUp = (event) => {
    if (!drag || event.pointerId !== drag.pointerId) return
    releaseDrag()
    scheduleClickReset()
    event.preventDefault()
  }
  const onPointerCancel = (event) => {
    if (!drag || event.pointerId !== drag.pointerId) return
    releaseDrag()
    suppressClick = false
    if (suppressTimer !== null) clearTimeout(suppressTimer)
    suppressTimer = null
  }
  const onClickCapture = (event) => {
    if (!suppressClick) return
    suppressClick = false
    if (suppressTimer !== null) clearTimeout(suppressTimer)
    suppressTimer = null
    event.preventDefault()
    event.stopImmediatePropagation()
  }
  const onBlur = () => {
    spaceDown = false
    releaseDrag()
    suppressClick = false
    if (suppressTimer !== null) clearTimeout(suppressTimer)
    suppressTimer = null
  }
  const onPointerEnter = () => { pointerInside = true }
  const onPointerLeave = () => { if (!drag) pointerInside = false }

  viewport.addEventListener('wheel', onWheel, { passive: false })
  viewport.addEventListener('pointerdown', onPointerDown)
  viewport.addEventListener('pointermove', onPointerMove)
  viewport.addEventListener('pointerup', onPointerUp)
  viewport.addEventListener('pointercancel', onPointerCancel)
  viewport.addEventListener('click', onClickCapture, true)
  viewport.addEventListener('pointerenter', onPointerEnter)
  viewport.addEventListener('pointerleave', onPointerLeave)
  window.addEventListener('keydown', onKeyDown)
  window.addEventListener('keyup', onKeyUp)
  window.addEventListener('blur', onBlur)
  const observer = typeof ResizeObserver === 'undefined' ? null : new ResizeObserver(refresh)
  observer?.observe(viewport)

  return {
    get scale() { return scale },
    get isPanning() { return Boolean(drag) },
    setMapSize(widthPixels, heightPixels) {
      mapWidth = finitePositive(widthPixels) ? widthPixels : 0
      mapHeight = finitePositive(heightPixels) ? heightPixels : 0
      fit()
    },
    fit,
    focus,
    zoomBy,
    refresh,
    destroy() {
      if (destroyed) return
      destroyed = true
      releaseDrag()
      if (suppressTimer !== null) clearTimeout(suppressTimer)
      observer?.disconnect()
      viewport.removeEventListener('wheel', onWheel)
      viewport.removeEventListener('pointerdown', onPointerDown)
      viewport.removeEventListener('pointermove', onPointerMove)
      viewport.removeEventListener('pointerup', onPointerUp)
      viewport.removeEventListener('pointercancel', onPointerCancel)
      viewport.removeEventListener('click', onClickCapture, true)
      viewport.removeEventListener('pointerenter', onPointerEnter)
      viewport.removeEventListener('pointerleave', onPointerLeave)
      window.removeEventListener('keydown', onKeyDown)
      window.removeEventListener('keyup', onKeyUp)
      window.removeEventListener('blur', onBlur)
    },
  }
}
