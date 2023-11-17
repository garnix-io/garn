import * as garn from "http://localhost:8777/mod.ts";
import * as nixpkgs from "http://localhost:8777/nixpkgs.ts";

export const helloFromHaskell = garn.haskell
  .mkHaskellProject({
    description: "My haskell executable",
    compiler: "ghc94",
    src: ".",
  })
  .withDevTools([nixpkgs.hlint])
  .addCheck("hlint", "hlint *.hs")
  .withCabalExecutable("helloFromHaskell");
