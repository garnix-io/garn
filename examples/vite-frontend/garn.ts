import * as garn from "http://localhost:8777/mod.ts";

export const frontend = garn.javascript
  .mkNpmProject({
    description: "An NPM project",
    src: ".",
    nodeVersion: "18",
  })
  .add(garn.javascript.vite);
