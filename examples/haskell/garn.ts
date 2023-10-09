import * as garn from "http://localhost:8777/mod.ts";
import * as nixpkgs from "http://localhost:8777/nixpkgs.ts";

const base = garn.haskell
  .mkHaskell({
    description: "My haskell executable",
    executable: "helloFromHaskell",
    compiler: "ghc94",
    src: ".",
  })
  .withDevTools([nixpkgs.hlint]);

export const helloFromHaskell: garn.Project & { hlint: garn.Check } = {
  ...base,
  hlint: base.check`hlint *.hs`,
};
