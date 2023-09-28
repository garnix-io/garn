// import * as garn from "https://garn.io/garn.ts";
import * as garn from "./deps.ts";

export const frontend = garn.mkYarnPackage();

export const printWebpackHelp = garn.shell`ls ${frontend}/libexec/frontend-yarn-webpack/node_modules/.bin/webpack --help`;

export const devFrontend = garn.processCompose({
  frontend: frontend.shell`yarn start`,
  help: frontend.shell`yarn --help`,
});
