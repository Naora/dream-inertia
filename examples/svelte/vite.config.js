import { defineConfig } from "vite";
import { svelte } from "@sveltejs/vite-plugin-svelte";

// https://vite.dev/config/
export default defineConfig({
  server: {
    cors: {
      origin: "http://localhost:8080",
    },
  },
  build: {
    manifeste: true,
    rollupOptions: {
      input: "/views/main.js",
    },
  },
  plugins: [svelte({ prebundleSvelteLibraries: true })],
});
