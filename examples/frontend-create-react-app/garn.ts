import * as garn from "http://localhost:8777/mod.ts";

export const main = garn.javascript.mkNpmProject({
  description: "frontend test app created by create-react-app",
  src: ".",
  nodeVersion: "18",
});
