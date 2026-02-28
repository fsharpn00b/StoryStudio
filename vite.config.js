import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import { chmodSync, cpSync, existsSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

const configDir = dirname(fileURLToPath(import.meta.url))

function copyStaticFolders() {
  return {
    name: 'copy-static-folders',
    closeBundle() {
      for (const folderName of ['0_assets']) {
        const source = resolve(configDir, 'src', folderName)
        const target = resolve(configDir, 'dist', folderName)

        if (existsSync(source)) {
          cpSync(source, target, { recursive: true, force: true })
        }
      }

      const runScriptSource = resolve(configDir, 'src', 'run.sh')
      const runScriptTarget = resolve(configDir, 'dist', 'run.sh')
      if (existsSync(runScriptSource)) {
        cpSync(runScriptSource, runScriptTarget, { force: true })
        chmodSync(runScriptTarget, 0o755)
      }
      
      const runCaddyConfigSource = resolve(configDir, 'src', 'caddy_configuration')
      const runCaddyConfigTarget = resolve(configDir, 'dist', 'caddy_configuration')
      if (existsSync(runCaddyConfigSource)) {
        cpSync(runCaddyConfigSource, runCaddyConfigTarget, { force: true })
      }
    },
  }
}

export default defineConfig({
  plugins: [react(), copyStaticFolders()],
  root: "./src",
  build: {
    rollupOptions: {
      input: "./src/0_pages/index.html",
    },
    outDir: "../dist",
  }
})
