import { mkNpmFrontend } from "http://localhost:8777/typescript.ts";

export const main = mkNpmFrontend({
  name: "frontend",
  src: "./.",
  nodeVersion: '18',
  testCommand: 'npm run test -- --watchAll=false',
});
