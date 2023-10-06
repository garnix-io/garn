import * as garn from "http://localhost:8777/mod.ts";

export const frontend = garn.typescript.mkNpmFrontend({
  src: ".",
  description: "An NPM frontend",
  nodeVersion: "18",
  testCommand: "",
});
