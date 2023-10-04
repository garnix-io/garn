import * as garner from "https://garn.io/ts/v0.0.1/mod.ts";
import * as nixpkgs from "https://garn.io/ts/v0.0.1/nixpkgs.ts";

export const haskellExecutable = garner.haskell.mkHaskell({
  description: "My haskell executable",
  executable: "garnerTest",
  compiler: "ghc94",
  src: ".",
});

export const hello = nixpkgs.hello;

