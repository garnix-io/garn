import * as garn from "../ts/mod.ts";

export default garn.javascript.mkNpmProject({
  description: "The garn.io website",
  nodeVersion: "18",
  src: ".",
  startCommand: "npm run dev",
});
