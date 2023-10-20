import * as garn from "http://localhost:8777/mod.ts";

export const backend = garn.go.mkGoProject({
  description: "example backend server in go",
  src: "backend",
});

export const runBackend = backend.shell`cd backend && go run ./main.go`;

export const haskell = garn.haskell.mkHaskellProject({
  description: "My haskell executable",
  executable: "helloFromHaskell",
  compiler: "ghc94",
  src: "haskell",
});

export const npmFrontend = garn.javascript.mkNpmProject({
  description: "frontend test app created by create-react-app",
  src: "frontend-npm",
  nodeVersion: "18",
});

export const startAll = garn.processCompose({
  backend: runBackend,
  haskell: haskell.defaultExecutable!,
  frontend: npmFrontend.shell`cd frontend-npm && npm install && npm start`,
});
