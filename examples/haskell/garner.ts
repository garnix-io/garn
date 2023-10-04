import { mkProject } from "http://localhost:8777/project.ts";
import * as garner from "../../ts/mod.ts";
import * as nixpkgs from "http://localhost:8777/nixpkgs.ts";

export const haskellExecutable = garner.haskell.mkHaskell({
  description: "My haskell executable",
  executable: "garnerTest",
  compiler: "ghc94",
  src: ".",
});

export const hello = mkProject(
  { hello: garner.shell`${nixpkgs.hello}/bin/hello` },
  { defaults: { executable: "hello" } }
);
