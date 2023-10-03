import * as garner from "http://localhost:8777/mod.ts";

export const frontend: garner.Package = garner.typescript.mkYarnFrontend({
  description: "my nice yarn project",
  src: ".",
  nodeVersion: "18",
  testCommand: "yarn mocha",
  serverStartCommand: "yarn start",
});
