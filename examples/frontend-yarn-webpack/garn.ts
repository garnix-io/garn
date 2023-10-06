import * as garn from "http://localhost:8777/mod.ts";

export const frontend = garn.typescript.mkYarnFrontend({
  description: "my nice yarn project",
  src: ".",
  nodeVersion: "18",
  testCommand: "yarn mocha",
  serverStartCommand: "yarn start",
});
