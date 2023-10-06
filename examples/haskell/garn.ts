import * as garn from "http://localhost:8777/mod.ts";
import { mkProject } from "http://localhost:8777/mod.ts";
import * as nixpkgs from "http://localhost:8777/nixpkgs.ts";

const haskellExecutableBase = garn.haskell
  .mkHaskell({
    description: "My haskell executable",
    executable: "garnTest",
    compiler: "ghc94",
    src: ".",
  })
  .withDevTools([nixpkgs.hlint]);

export const haskellExecutable: garn.Project & { hlint: garn.Check } = {
  ...haskellExecutableBase,
  hlint: haskellExecutableBase.check`hlint *.hs`,
};

export const hello = mkProject(
  "My hello executable",
  { hello: garn.shell`${nixpkgs.hello}/bin/hello` },
  { defaults: { executable: "hello" } }
);
