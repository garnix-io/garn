import * as garner from "./deps.ts";

const goBackendBase = garner
  .mkGoProject({
    src: "goBackend",
    bins: {
      server: "src/server.go",
      migrate: "utils/migrator.go",
    },
  })
  .withTasks({
    codeGen: garner.bash`${garner.pkgs.protoc} generate protobufs/*.proto`,
  })
  // withGeneratorCheck will add a check for a given task  by running the task
  // in a check and ensuring that no changes exist after running this task.
  // This is useful for things like formatting, codegen, etc..
  .withGeneratorCheck("codeGen")
  .withCheck("todos", garner.bash`! grep -r TODO src`)
  .withTasks({
    dev: garner.bash`go run src/server.go`,
  });

export const goBackend = goBackendBase.withTasks({
  prod: garner.bash`${goBackendBase}/bin/server`,
});

const frontendBase = garner
  .mkYarn({
    src: "typescriptFrontend",
  })
  .withPackageJsonScripts("start", "dev", "lint")
  .withTasks({
    bundle: garner.bash`webpack --whatever`,
    // deploy: garner.bash`rsync ${typescriptFrontend.bundle}`,
  })
  .withArtifacts({
    bundled: {
      cmd: garner.bash`webpack --whatever ${"huhu"}`,
      artifacts: ["dist"],
    },
  })
  .withCheck(
    "typecheck",
    garner.bash`
    tsc scripts/*.ts
  `
  );

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
export const typescriptFrontend = frontendBase.withTasks({
  serveStatically: garner.bash`${garner.pkgs.python3.bin("python3")} -m http ${
    frontendBase.bundled
  }/dist`,
});
