import * as garn from "http://localhost:8777/mod.ts";

export const frontend = garn.javascript.mkYarnProject({
  description: "my nice yarn project",
  src: ".",
  nodeVersion: "18",
  startCommand: "yarn start",
  testCommand: "yarn mocha",
});
