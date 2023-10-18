import * as garn from "../ts/mod.ts";

export const website = garn.javascript
  .mkNpmProject({
    description: "The garn.io website",
    nodeVersion: "18",
    src: ".",
  })
  .addCheck("tsc")`npm run tsc`;

const topLevelExecutable = (
  description: string,
  executable: garn.Executable
): garn.Project =>
  garn.mkProject({ description, defaultExecutable: executable }, {});

export const dev = topLevelExecutable(
  "run the website in development mode",
  website.shell`npm install ; npm run dev`
);

export const build = topLevelExecutable(
  "build the website",
  website.shell`npm install ; npm run build`
);
