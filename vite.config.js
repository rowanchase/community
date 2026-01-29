import { defineConfig } from 'vite'
import elm from 'vite-plugin-elm'

export default defineConfig({
  plugins: [elm()],
  server: {
    port: 8000
  }
})
