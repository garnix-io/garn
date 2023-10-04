import * as garner from "https://garn.io/ts/v0.0.1/mod.ts";

export const frontend = garner.typescript.mkNpmFrontend({
  src: ".",
  description: "An NPM frontend",
  nodeVersion: "18",
  testCommand: "",
});
