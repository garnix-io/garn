import * as garner from "https://garn.io/ts/v0.0.1/mod.ts";

export const frontend: garner.Package = garner.typescript.mkYarnFrontend({
  description: "my nice yarn project",
  src: ".",
  nodeVersion: "18",
  testCommand: "yarn mocha",
  serverStartCommand: "yarn start",
});
