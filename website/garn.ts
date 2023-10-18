import * as garn from "../ts/mod.ts";
import { nixRaw } from "../ts/nix.ts";

export const website = garn.javascript
  .mkNpmProject({
    description: "The garn.io website",
    nodeVersion: "18",
    src: ".",
  })
  .addCheck("tsc")`npm run tsc`.withDevTools([
  garn.mkPackage(nixRaw("pkgs.nodePackages.typescript-language-server")),
]);

export const dev = website.shell`npm install ; npm run dev`;

export const build = website.shell`npm install ; npm run build`;
