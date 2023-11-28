import * as garn from "https://garn.io/ts/v0.0.17/mod.ts";
import * as pkgs from "https://garn.io/ts/v0.0.17/nixpkgs.ts";

export const frontend = garn.javascript
  .mkNpmProject({
    description: "my frontend",
    nodeVersion: "18",
    src: "./frontend",
  })
  .addCheck("test", "npm test");

export const backend = garn.go.mkGoProject({
  description: "my backend",
  src: "./backend",
  goVersion: "1.20",
});

export const deno = garn.mkProject(
  {
    description: "garn configuration environment",
    defaultEnvironment: garn.emptyEnvironment.withDevTools([pkgs.deno]),
  },
  {},
);
