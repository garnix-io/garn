import { mkNpmFrontend } from "http://localhost:8777/typescript.ts";

export const frontend = mkNpmFrontend({
  name: "frontend",
  src: "./.",
});
