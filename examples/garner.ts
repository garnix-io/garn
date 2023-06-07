import { mkHaskell } from "../ts/haskell.ts";

export const garnerTest = mkHaskell({
  name: "mkHaskell-test",
  executable: "garnerTest",
  compiler: "ghc94",
  src: "./.",
});
