import { Package } from "http://localhost:8777/base.ts";
import { mkNpmFrontend } from "http://localhost:8777/typescript.ts";

export const main: Package = mkNpmFrontend({
  name: "foo",
  src: "./.",
}).setSetupCommand(["yarn"]);
