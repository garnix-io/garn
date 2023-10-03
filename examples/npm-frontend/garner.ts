import * as garner from "http://localhost:8777/mod.ts";

export const frontend = garner.typescript.mkNpmFrontend({
  src: ".",
  description: "An NPM frontend",
  nodeVersion: "18",
  testCommand: "",
});
