import * as garn from "../ts/mod.ts";
import { nixRaw } from "../ts/nix.ts";

export const website = garn.javascript
  .mkNpmProject({
    description: "The garn.io website",
    nodeVersion: "18",
    src: ".",
  })
  .withDevTools([
    garn.mkPackage(nixRaw("pkgs.nodePackages.typescript-language-server")),
    garn.mkPackage(nixRaw("pkgs.nodePackages.prettier")),
  ])
  .addCheck("tsc")`npm run tsc`
  .addCheck("fmt-check")`prettier src/**/*.tsx src/**/*.ts --check`
  .addExecutable("fmt")`prettier src/**/*.tsx src/**/*.ts --write`;

export const dev = website.shell`npm install ; npm run dev`;

export const build = website.shell`npm install ; npm run build`;

export const edit = garn.editGarnConfig;
