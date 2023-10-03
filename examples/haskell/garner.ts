import * as garner from "http://localhost:8777/mod.ts";
import * as nixpkgs from "http://localhost:8777/nixpkgs.ts";

export const haskellExecutable = garner.haskell.mkHaskell({
  description: "My haskell executable",
  executable: "garnerTest",
  compiler: "ghc94",
  src: ".",
});

export const hello = nixpkgs.hello;

