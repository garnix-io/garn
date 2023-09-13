import { mkNpmFrontend } from "http://localhost:8777/typescript.ts";

export const main = mkNpmFrontend({
  description: "frontend test app created by create-react-app",
  src: "./.",
  nodeVersion: "18",
  testCommand: "npm run test -- --watchAll=false",
});
