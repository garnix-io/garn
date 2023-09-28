// import garn from "https://garn.io/mod.ts";
import * as garn from "./deps.ts";

export const frontend = garn.mkYarnPackage({
  buildCmd: "webpack build",
  artifacts: ["dist"],
});

export const backend = garn.mkFooBackend()
  .addTasks({ runMigrations: garn.shell`echo running migrations...` });
  // .addSubProject("someCronJob", garn.mkHaskellProject("backend/someCronJob"))

export const deploy = garn
  .shell`cat ${frontend}/dist/main.js`;

// garner check all.frontend
// garner check all
// garner check all.backend
// garner check all.backend.someCronJob
export const all = {
  frontend,
  backend,
  foo: { somethingElse, evenAnotherThing }
}

export const fmtFrontend = frontend.shell`yarn fmt -w src`;

export const serveFrontend = garn.shell`${
  garn.nixPkg("python3")
}/bin/python3 -m http.server --directory ${frontend}/dist 8000`;

export const devFrontend = garn.processCompose({
  frontend: frontend.tasks.develop,
  backend: backend.tasks.server,
});

/*

export const runFeatureTests = bash`${myPackage.bin('runFeatureTests')} -- --coverage`
export const runUnitTests = mkPackage.shell`cabal test`

const packageTag = Symbol();

type PackageInternal = {
  huhu: string
  foo: 'bar',
  fizz: 'bar',
  dev: 'whatever',
}

type Package = {
  [packageTag]: PackageInternal,
  withTasks(): Package,
  utils: {
    helper: null,
    functions: null,
  }
}

function mkYarnFrontend(): Package & { dev: g.Runnable, huhu: g.Runnable } {
  throw 'huhu';
}

// garn run whatever.dev
export const whatever = mkYarnFrontend();

*/
