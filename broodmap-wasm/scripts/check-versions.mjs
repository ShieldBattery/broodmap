// Checks that the npm package's version matches the crates' shared `[workspace.package]` version.
// Cargo already rejects path dependencies whose version requirements don't match the crates.
import fs from 'node:fs'
import path from 'node:path'

const wasmDir = path.resolve(import.meta.dirname, '..')
const manifestPath = path.resolve(wasmDir, '..', 'Cargo.toml')

function workspaceVersion() {
  let inWorkspacePackage = false
  for (const line of fs.readFileSync(manifestPath, 'utf8').split(/\r?\n/)) {
    const table = line.match(/^\s*\[([^\]]+)]\s*$/)
    if (table) {
      inWorkspacePackage = table[1] === 'workspace.package'
      continue
    }
    const version = inWorkspacePackage && line.match(/^\s*version\s*=\s*"([^"]+)"/)
    if (version) return version[1]
  }
  throw new Error(`No [workspace.package] version found in ${manifestPath}`)
}

const crateVersion = workspaceVersion()
const packageVersion = JSON.parse(fs.readFileSync(path.join(wasmDir, 'package.json'), 'utf8')).version
if (packageVersion !== crateVersion) {
  throw new Error(
    `package.json is at ${packageVersion}, but the crates ([workspace.package]) are at ${crateVersion}`,
  )
}

console.log(`Package versions are aligned at ${packageVersion}`)
