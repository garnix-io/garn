import * as garner from "http://localhost:8777/mod.ts";
import { mkProject } from "http://localhost:8777/mod.ts";
import * as nixpkgs from "http://localhost:8777/nixpkgs.ts";

const haskellExecutableBase = garner.haskell
  .mkHaskell({
    description: "My haskell executable",
    executable: "garnerTest",
    compiler: "ghc94",
    src: ".",
  })
  .withDevTools([nixpkgs.hlint]);

export const haskellExecutable: garner.Project & { hlint: garner.Check } = {
  ...haskellExecutableBase,
  hlint: haskellExecutableBase.check`hlint *.hs`,
};

export const hello = mkProject(
  "My hello executable",
  { hello: garner.shell`${nixpkgs.hello}/bin/hello` },
  { defaults: { executable: "hello" } }
);
