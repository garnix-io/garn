import * as garn from "http://localhost:8777/mod.ts";

export const server: garn.Project<{
  pkg: garn.Package;
  devShell: garn.Environment;
  main: garn.Executable;
}> = garn.go.mkGoProject({
  description: "example backend server in go",
  moduleName: "server",
  src: "./.",
});
