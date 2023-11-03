import * as garn from "http://localhost:8777/mod.ts";

export const backend = garn.go.mkGoProject({
  description: "example backend server in go",
  src: "backend",
})
  .addExecutable("run")`cd backend && go run ./main.go`;

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
})
  .addExecutable("run")`cd frontend-npm && npm install && npm start`;

export const startAll = garn.processCompose({
  backend: backend.run,
  haskell: haskell.defaultExecutable!,
  frontend: npmFrontend.run,
});
