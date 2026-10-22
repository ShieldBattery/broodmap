import { copyFileSync, rmSync, writeFileSync } from 'node:fs'
import path from 'node:path'

const output = process.argv[2]
if (output !== 'pkg' && output !== 'pkg-node') {
  throw new Error('Expected a wasm-pack output directory: pkg or pkg-node')
}

const packageDir = path.resolve(import.meta.dirname, '..')
const outputDir = path.join(packageDir, output)

// The parent package defines the exports. wasm-pack's generated ignore file would also make npm
// leave these artifacts out of the published tarball; the parent .gitignore still covers them.
rmSync(path.join(outputDir, '.gitignore'), { force: true })
// The parent package is an ES module package, so each output directory declares its own module
// format: the web build is ESM, the Node build CommonJS.
writeFileSync(
  path.join(outputDir, 'package.json'),
  JSON.stringify({ type: output === 'pkg' ? 'module' : 'commonjs' }, null, 2) + '\n',
)

// The licenses live at the repository root; the published package carries copies.
for (const license of ['LICENSE-APACHE', 'LICENSE-MIT']) {
  copyFileSync(path.join(packageDir, '..', license), path.join(packageDir, license))
}
