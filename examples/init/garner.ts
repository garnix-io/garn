import { mkHaskell } from "http://localhost:8777/haskell.ts"

export const foo = mkHaskell({
  description: "",
  executable: "",
  compiler: "ghc94",
  src: "."
})