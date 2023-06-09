import { mkHaskell } from "../ts/haskell.ts";
import * as nixpkgs from "../ts/nixpkgs.ts";

export const haskellExecutable = mkHaskell({
  name: "mkHaskell-test",
  executable: "garnerTest",
  compiler: "ghc94",
  src: "./.",
});

export const hello = nixpkgs.hello;
