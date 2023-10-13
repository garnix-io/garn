import * as garn from "http://localhost:8777/mod.ts";

export const project = garn.javascript
  .mkNpmProject({
    src: ".",
    description: "An NPM frontend",
    nodeVersion: "18",
  })
  .addCheck("test")`npm run test`.addCheck("tsc")`npm run tsc`;
