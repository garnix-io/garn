import { Package } from "http://localhost:8777/base.ts";
import { mkGoProject } from "http://localhost:8777/go.ts";

export const server: Package = mkGoProject({
  description: "example backend server in go",
  src: "./.",
});
