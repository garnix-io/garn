import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import rehypePrism from "@mapbox/rehype-prism";
import { remarkMdxCodeMeta } from "remark-mdx-code-meta";
import remarkGfm from "remark-gfm";
import compress from "vite-plugin-compression";
import * as path from "path";
import remarkToc from "remark-toc";
import withSlugs from "rehype-slug";
import { createRequire } from "node:module";
import { mkdirSync } from "fs";

// I don't really understand the whole module system, but this seems to work
const _require = createRequire(import.meta.url);
const vitePrerender = _require("vite-plugin-prerender");

const Renderer = vitePrerender.PuppeteerRenderer;

// https://vitejs.dev/config/
export default defineConfig(async () => {
  const mdx = await import("@mdx-js/rollup");
  const mdxOpts = {
    rehypePlugins: [rehypePrism, withSlugs],
    remarkPlugins: [remarkGfm, remarkMdxCodeMeta, remarkToc],
  };
  const vitePrerenderOpts = {
    staticDir: path.join(__dirname, "dist"),
    routes: ["/", "/docs/concepts", "/docs/installing"],
    renderer: new Renderer({
      args: ["--no-sandbox", "--single-process", "--disable-gpu"],
    }),
  };
  const useVitestCache = (() => {
    try {
      mkdirSync("node_modules/.vitest/", { recursive: true });
    } catch (_) {
      return false;
    }
    return true;
  })();
  return {
    assetsInclude: ["**/*.cast"],
    plugins: [
      react(),
      mdx.default(mdxOpts),
      vitePrerender(vitePrerenderOpts),
      compress(),
    ],
    // See https://github.com/mdx-js/mdx/discussions/1794#discussioncomment-1581513
    resolve: {
      alias: {
        "react/jsx-runtime": "react/jsx-runtime.js",
      },
    },
    server: {
      proxy: {
        "/api": "http://127.0.0.1:8017",
      },
    },
    test: useVitestCache ? {} : { cache: false },
  };
});
