import * as garner from "https://garn.io/ts/v0.0.1/mod.ts";

export const server: garner.Package = garner.go.mkGoProject({
  description: "example backend server in go",
  src: "./.",
});
