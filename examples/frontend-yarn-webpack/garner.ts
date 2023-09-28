// import * as garn from "https://garn.io/garn.ts";
import * as garn from "./deps.ts";

export const frontend = garn.mkYarnPackage({
  buildCmd: "webpack build",
  artifacts: ["dist"],
});

export const deploy = garn
  .shell`cat ${frontend}/dist/main.js`;

export const fmtFrontend = frontend.shell`yarn fmt -w src`;

export const devFrontend = garn.processCompose({
  frontend: frontend.shell`yarn start`,
  help: frontend.shell`yarn --help`,
});
