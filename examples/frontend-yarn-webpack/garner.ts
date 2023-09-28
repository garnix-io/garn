// import * as garn from "https://garn.io/garn.ts";
import * as garn from "./deps.ts";

export const frontend = garn.mkYarnPackage({
  buildCmd: "webpack build",
  artifacts: ["dist"],
});

export const backend = garn.mkFooBackend();

export const deploy = garn
  .shell`cat ${frontend}/dist/main.js`;

export const fmtFrontend = frontend.shell`yarn fmt -w src`;

export const serveFrontend = garn.shell`${
  garn.nixPkg("python3")
}/bin/python3 -m http.server --directory ${frontend}/dist 8000`;

export const devFrontend = garn.processCompose({
  frontend: frontend.tasks.develop,
  backend: backend.tasks.develop,
});
