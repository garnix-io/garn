import * as garner from "http://localhost:8777/mod.ts";

export const server: garner.Project = garner.go.mkGoProject({
  description: "example backend server in go",
  moduleName: "server",
  src: "./.",
});
