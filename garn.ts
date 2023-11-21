import * as garn from "https://garn.io/ts/v0.0.15/mod.ts";
import * as pkgs from "https://garn.io/ts/v0.0.15/nixpkgs.ts";

export const garn = garn.haskell.mkHaskellProject({
  description: "",
  compiler: "ghc94",
  executables: ["garn","codegen"],
  src: "."
})
