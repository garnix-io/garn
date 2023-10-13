import * as garn from "../ts/mod.ts";

const base = garn.javascript.mkNpmFrontend({
  description: "The garn.io website",
  nodeVersion: "18",
  src: ".",
  startCommand: "npm run dev",
  testCommand: "true",
}).addCheck("tsc")`tsc`;

export default {
  ...base,
  defaultExecutable: base.shell`tsc`,
}
