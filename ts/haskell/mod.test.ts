import { describe, it } from "https://deno.land/std@0.206.0/testing/bdd.ts";
import outdent from "https://deno.land/x/outdent@v0.8.0/mod.ts";
import * as haskell from "./mod.ts";
import {
  assertStdout,
  assertSuccess,
  assertStderr,
  runExecutable,
} from "../testUtils.ts";

describe("mkHaskellProject", () => {
  describe("withVersions", () => {
    it("allows specifying alternative versions from Hackage", () => {
      const path = Deno.makeTempDirSync();
      Deno.writeTextFileSync(
        `${path}/project.cabal`,
        outdent`
            name: project
            version: 0.0.1
            executable foo
              main-is: Main.hs
              build-depends: base, string-conversions
              default-language: Haskell2010
          `,
      );
      Deno.writeTextFileSync(
        `${path}/Main.hs`,
        outdent`
        {-# LANGUAGE CPP #-}

        #if MIN_VERSION_string_conversions(0,4,0)
        main = putStrLn ">= 0.4"
        #else
        main = putStrLn "< 0.4"
        #endif
        `,
      );
      const project1 = haskell.mkHaskellProject({
        description: "",
        compiler: "ghc94",
        src: ".",
        executables: ["foo"],
        overrideDependencies: {
          "string-conversions": "0.3.0.3",
        },
      });
      const project2 = haskell.mkHaskellProject({
        description: "",
        compiler: "ghc94",
        src: ".",
        executables: ["foo"],
        overrideDependencies: {
          "string-conversions": "0.4.0.1",
        },
      });
      const output1 = runExecutable(project1.foo, { cwd: path });
      const output2 = runExecutable(project2.foo, { cwd: path });
      assertSuccess(output1);
      assertStdout(output1, "< 0.4\n");
      assertSuccess(output2);
      assertStdout(output2, ">= 0.4\n");
    });
  }),
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
