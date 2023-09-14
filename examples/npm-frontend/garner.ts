import { mkNpmFrontend } from "http://localhost:8777/typescript.ts";

export const frontend = mkNpmFrontend({
  src: "./.",
  description: "An NPM frontend",
  nodeVersion: "18",
  testCommand: "",
});
