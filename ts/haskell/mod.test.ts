import { describe, it } from "https://deno.land/std@0.206.0/testing/bdd.ts";
import * as haskell from "./mod.ts";
import { assertStdout, assertSuccess, runExecutable } from "../testUtils.ts";

describe("mkHaskellProject", () => {
  describe("addCabalExecutable", () => {
    it("allows adding existing cabal executables with .addCabalExecutable", () => {
      const path = Deno.makeTempDirSync();
      Deno.writeTextFileSync(
        `${path}/project.cabal`,
        `
            name: project
            version: 0.0.1
            executable foo
              main-is: Main.hs
              build-depends: base
              default-language: Haskell2010
          `,
      );
      Deno.writeTextFileSync(
        `${path}/Main.hs`,
        `
            main = putStrLn "yo"
          `,
      );
      const project = haskell
        .mkHaskellProject({
          description: "",
          compiler: "ghc94",
          src: ".",
        })
        .addCabalExecutable("foo");
      const output = runExecutable(project.foo, { cwd: path });
      assertSuccess(output);
      assertStdout(output, "yo\n");
    });
  });
});
