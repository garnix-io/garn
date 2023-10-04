import * as garner from "../../ts/mod.ts";

export const frontend = garner.typescript.mkYarnFrontend({
  description: "my nice yarn project",
  src: ".",
  nodeVersion: "18",
  testCommand: "yarn mocha",
  serverStartCommand: "yarn start",
});
