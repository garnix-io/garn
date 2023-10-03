import * as garner from "../../ts/mod.ts";
import * as nixpkgs from "http://localhost:8777/nixpkgs.ts";
import { mkNewPackage } from "../../ts/package.ts";

export const haskellExecutable = garner.haskell.mkHaskell({
  description: "My haskell executable",
  executable: "garnerTest",
  compiler: "ghc94",
  src: ".",
});

export const hello = nixpkgs.hello;

export const foo = haskellExecutable.withDevTools([
  mkNewPackage(hello.nixExpression),
]);
