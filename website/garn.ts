import * as garn from "../ts/mod.ts";

const base = garn.javascript
  .mkNpmProject({
    description: "The garn.io website",
    nodeVersion: "18",
    src: ".",
  })
  .addCheck("tsc")`npm run tsc`;

export default {
  ...base,
  defaultExecutable: base.shell`npm install ; npm run dev`,
};
