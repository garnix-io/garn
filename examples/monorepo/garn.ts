import * as garn from "http://localhost:8777/mod.ts";

export const backend = garn.go.mkGoProject({
  description: "example backend server in go",
  moduleName: "go-http-backend",
  src: "backend",
});

export const yarnFrontend = garn.javascript.mkYarnFrontend({
  description: "my nice yarn project",
  src: "frontend-yarn",
  nodeVersion: "18",
  testCommand: "yarn mocha",
  serverStartCommand: "yarn start",
});

export const npmFrontend = garn.javascript.mkNpmFrontend({
  description: "frontend test app created by create-react-app",
  src: "frontend-npm",
  nodeVersion: "18",
  testCommand: "npm run test -- --watchAll=false",
});

export const startAll = garn.processCompose({
  backend: backend.defaultExecutable!,
  'yarn frontend': yarnFrontend.defaultExecutable!,
  'npm frontend': npmFrontend.defaultExecutable!,
});
