import * as garner from "./deps.ts";

const goBackend = garner
  .mkGoProject({
    src: "goBackend",
    bins: {
      // These both turn into runnables, and also artifacts in the final `release` NixPackage
      server: "src/server.go",
      migrate: "utils/migrator.go",
    },
  })
  .withAdditionalDevDependencies([garner.pkgs.protoc])
  .withEnvironment({ SKIP_CHECKS: "true" });

const yarnFrontend = garner
  .mkYarn({
    src: "typescriptFrontend",
  });

// $ garner enter                   (shell with backend.goBackend and frontend.yarnFrontend; use symlinkJoin)
// $ garner enter backend           (shell with backend.goBackend)
// $ garner enter backend.goBackend (shell with backend.goBackend)

// $ garner run backend
//   --> what do you want to run?
//       1. backend.codeGen
//       2. backend.goBackend.server
//       3. backend.goBackend.migrate
//       3. backend.goBackend.fmt

// $ garner run backend.goBackend.server

// $ garner build backend.goBackend

export const backend = {
  goBackend,

  codeGen: goBackend.run`${garner.pkgs.protoc} generate protobufs/*.proto`,

  quickChecks: {
    checkNoTodos: [
      garner.check`! grep -r TODO src`,
      garner.check`! grep -r FIXME src`,
    ],

    runTests: goBackend.check`go test ./...`,
  },
};

export const frontend = {
  yarnFrontend,

  runTests: yarnFrontend.check`yarn run mocha`,
};
