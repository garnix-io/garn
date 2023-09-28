import * as garner from "./deps.ts";

export const goBackend = garner.mkGoProject({
  src: "goBackend",
  bins: {
    server: "src/server.go",
    migrate: "utils/migrator.go",
  },
}).withTasks({
  codeGen: garner.bash`${garner.pkgs.protoc} generate protobufs/*.proto`,
}).withGeneratorCheck("codeGen");

// garner enter
//   - yarn
//   - all dependencies
//   - tools from node_modules/.bin
// garner run typescriptFrontend.dev
// garner run typescriptFrontend.start
// garner run typescriptFrontend.lint
// garner run typescriptFrontend.serveStatically
// garner build typescriptFrontend.bundled ; rsync result/* ssh://somewhere
// garner build typescriptFrontend.serveStatically; ./result
export const typescriptFrontend = garner.mkYarn({
  src: "typescriptFrontend",
})
  .withPackageJsonScripts("start", "dev", "lint")
  .withTasks({
    bundle: garner.bash`webpack --whatever`,
    // deploy: garner.bash`rsync ${typescriptFrontend.bundle}`,
  }).withArtifacts({
    bundled: {
      cmd: garner.bash`webpack --whatever`,
      artifacts: ["dist"],
    },
  }).withTasks((self) => ({
    serveStatically: garner
      .bash`${garner.pkgs.python3.bin("python3")} -m http ${self.bundled}/dist`,
  }));

export const startFrontend = typescriptFrontend.bundled;
