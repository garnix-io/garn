import { mkHaskell } from "http://localhost:8777/haskell.ts";
import * as nixpkgs from "http://localhost:8777/nixpkgs.ts";

export const haskellExecutable = mkHaskell({
  description: "My haskell executable",
  executable: "garnerTest",
  compiler: "ghc94",
  src: "./.",
});

export const hello = nixpkgs.hello;
