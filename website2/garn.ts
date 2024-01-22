import * as garn from "../ts/mod.ts";

export const website2 = garn.javascript
  .mkNpmProject({
    description: "garn website rewrite",
    src: ".",
    nodeVersion: "18",
  })
  .addPackage("bundle", "npm run build ; mv out/* $out/")
  .addCheck("lint", "npm run lint");
