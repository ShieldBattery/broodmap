import { copyFileSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import path from 'node:path'

const packageDir = path.resolve(import.meta.dirname, '..')
const outputDir = path.join(packageDir, 'pkg')

// wasm-pack's generated ignore file would make npm leave the build out of the published tarball;
// the package's own .gitignore still covers it.
rmSync(path.join(outputDir, '.gitignore'), { force: true })

// wasm-bindgen emits Symbol.dispose methods. Include their types even when consumers target an
// earlier ECMAScript version, without requiring them to change their tsconfig's lib setting.
const declarationsPath = path.join(outputDir, 'broodmap_wasm.d.ts')
const declarations = readFileSync(declarationsPath, 'utf8')
const disposableTypes = '/// <reference lib="esnext.disposable" />\n'
if (!declarations.startsWith(disposableTypes)) {
  writeFileSync(declarationsPath, disposableTypes + declarations)
}

// The licenses live at the repository root; the published package carries copies.
for (const license of ['LICENSE-APACHE', 'LICENSE-MIT']) {
  copyFileSync(path.join(packageDir, '..', license), path.join(packageDir, license))
}
