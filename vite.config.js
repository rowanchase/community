import { defineConfig } from 'vite'
import elm from 'vite-plugin-elm'

export default defineConfig({
  plugins: [elm()],
  server: {
    port: 8000,
    host: true  // Listen on all addresses (0.0.0.0) - works with both localhost and 127.0.0.1
  }
})
