import * as garn from "http://localhost:8777/mod.ts";

export const server: garn.Project = garn.go.mkGoProject({
  description: "example backend server in go",
  moduleName: "server",
  src: "./.",
});
