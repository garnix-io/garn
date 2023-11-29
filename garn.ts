import * as garn from "http://localhost:8777/mod.ts";

const flake = garn.importFlake("./.garn/legacy");

export default garn.mkProject({
  description: "garn",
  defaultEnvironment: garn.emptyEnvironment,
}, {})
  .add(() => ({
    fileserver: flake.getApp("fileserver"),
    checks: flake.allChecks,
  }));
