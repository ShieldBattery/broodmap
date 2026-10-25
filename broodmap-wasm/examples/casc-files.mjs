// The file callbacks `CascStorage` reads a SC:R install through, over sync `node:fs`.
import { closeSync, fstatSync, openSync, readdirSync, readSync } from 'node:fs'
import path from 'node:path'

/** File access to the SC:R install at `root` (the directory containing `.build.info`). */
export function nodeCascFiles(root) {
  return {
    open: p => openSync(path.join(root, p), 'r'),
    size: fd => fstatSync(fd).size,
    read: (fd, buffer, position) => readSync(fd, buffer, 0, buffer.length, position),
    close: fd => closeSync(fd),
    listDir: p => {
      try {
        return readdirSync(path.join(root, p))
      } catch (err) {
        if (err.code === 'ENOENT') return []
        throw err
      }
    },
  }
}
