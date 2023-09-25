import { Application, denoPlugin, esbuild, path } from "./deps.server.ts";
import { getDocs } from "./doc_gen.ts";

await esbuild.initialize({});

const DOCS_ROOT = path.dirname(path.fromFileUrl(import.meta.url));
const app = new Application();

async function build(...buildOptions: Parameters<typeof esbuild.build>) {
  const output = await esbuild.build(...buildOptions);
  return new TextDecoder().decode(output.outputFiles![0]!.contents);
}

app.use(async (ctx) => {
  console.log("req", ctx.request.url.pathname);
  switch (ctx.request.url.pathname) {
    case "/wait-for-change": {
      const watcher = Deno.watchFs("./src");
      await watcher[Symbol.asyncIterator]().next();
      watcher.close();
      ctx.response.body = "";
      break;
    }

    case "/app.js": {
      ctx.response.headers.append("Content-Type", "application/javascript");
      ctx.response.body = await build({
        plugins: [denoPlugin()],
        entryPoints: ["./src/index.tsx"],
        write: false,
        bundle: true,
        format: "iife",
        absWorkingDir: DOCS_ROOT,
        sourceRoot: DOCS_ROOT,
        nodePaths: [],
      });
      break;
    }

    case "/app.css": {
      ctx.response.headers.append("Content-Type", "text/css");
      ctx.response.body = await build({
        entryPoints: ["./src/styles/index.css"],
        write: false,
        bundle: true,
        absWorkingDir: Deno.cwd(),
        sourceRoot: Deno.cwd(),
        nodePaths: [],
      });
      break;
    }

    case "/doc.json": {
      ctx.response.headers.append("Content-Type", "application/json");
      ctx.response.body = JSON.stringify(await getDocs());
      break;
    }

    default: {
      ctx.response.body = `
        <!doctype html>
        <html>
          <head>
            <title>Garner Docs</title>
            <link rel="stylesheet" href="/app.css">
            <link href="https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@300&amp;family=Space+Mono:ital,wght@0,400;0,700;1,400;1,700&amp;family=Spectral:ital,wght@0,200;0,300;0,400;0,600;1,300;1,400&amp;display=swap" rel="stylesheet">
          </head>
          <body>
            <div id="app" />
            <script src="/app.js"></script>
            <script>
              // Auto reload on file change
              fetch('/wait-for-change').then(() => window.location.reload())
            </script>
          </body>
        </html>
      `;
      break;
    }
  }
});

console.log("Listening on 8000");
await app.listen({ port: 8000 });
