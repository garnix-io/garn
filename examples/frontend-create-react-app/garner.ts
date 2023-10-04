import * as garner from "../../ts/mod.ts";

export const main = garner.typescript.mkNpmFrontend({
  description: "frontend test app created by create-react-app",
  src: ".",
  nodeVersion: "18",
  testCommand: "npm run test -- --watchAll=false",
});
