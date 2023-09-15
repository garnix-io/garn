import { Package } from "http://localhost:8777/base.ts";
import { mkYarnFrontend } from "http://localhost:8777/typescript.ts";

export const frontend: Package = mkYarnFrontend({
  description: "my nice yarn project",
  src: ".",
  nodeVersion: "18",
  testCommand: "yarn mocha",
});
