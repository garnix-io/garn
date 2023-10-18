import * as garn from "../ts/mod.ts";
import { nixRaw } from "../ts/nix.ts";

export default garn.javascript.mkNpmProject({
  description: "The garn.io website",
  nodeVersion: "18",
  src: ".",
  startCommand: "npm run dev",
}).withDevTools([
  garn.mkPackage(nixRaw('pkgs.nodePackages.typescript-language-server')),
]);
