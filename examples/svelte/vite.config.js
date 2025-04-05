import { defineConfig } from "vite";
import { svelte } from "@sveltejs/vite-plugin-svelte";

export default defineConfig({
  server: {
    cors: {
      origin: "http://localhost:8080",
    },
  },
  build: {
    manifest: true,
    rollupOptions: {
      input: "/views/main.js",
    },
  },
  plugins: [svelte({ prebundleSvelteLibraries: true })],
});
