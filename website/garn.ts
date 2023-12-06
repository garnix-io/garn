import * as garn from "../ts/mod.ts";
import { nixRaw } from "../ts/nix.ts";

export const website = garn.javascript
  .mkNpmProject({
    description: "The garn.io website",
    nodeVersion: "18",
    src: ".",
  })
  .withDevTools([
    garn.mkPackage(
      nixRaw("pkgs.nodePackages.typescript-language-server"),
      "ts language server",
    ),
    garn.mkPackage(nixRaw("pkgs.nodePackages.prettier"), "prettier"),
  ])
  .addCheck("tsc", "npm run tsc")
  .addCheck("fmt-check", "prettier '**/*.{ts,tsx}' --check")
  .addExecutable("fmt", "prettier '**/*.{ts,tsx}' --write");

export const dev = website.shell("npm install ; npm run dev");

export const build = website.shell("npm install ; npm run build");

const flake = garn.importFromGithub({
  repo: "martinvonz/jj",
  revOrRef: "v0.11.0",
});

export const devEnv = garn.emptyEnvironment.withDevTools([
  flake.getPackage("default"),
]);
