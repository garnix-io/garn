import { mkHaskell } from "../ts/haskell.ts"

export const foo = mkHaskell({
  name: "mkHaskell-test",
  executable: "garner-test",
  compiler: "ghc94",
  src: "./."
})
