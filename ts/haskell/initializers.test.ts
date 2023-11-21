import { assertEquals } from "https://deno.land/std@0.201.0/assert/mod.ts";
import outdent from "https://deno.land/x/outdent@v0.8.0/mod.ts";
import { join } from "https://deno.land/std@0.201.0/path/mod.ts";
import { mkHaskellProjectInitializer } from "./initializers.ts";

Deno.test(
  "Haskell initializer does not run when no cabal file is present",
  () => {
    const tempDir = Deno.makeTempDirSync();
    const result = mkHaskellProjectInitializer(tempDir);
    assertEquals(result.tag, "ShouldNotRun");
  },
);

Deno.test("Haskell initializer errors if the cabal file is unparseable", () => {
  const tempDir = Deno.makeTempDirSync();
  Deno.writeTextFileSync(
    join(tempDir, "foo.cabal"),
    `
    name: foo
  `,
  );
  const result = mkHaskellProjectInitializer(tempDir);
  assertEquals(result.tag, "UnexpectedError");
  if (result.tag === "UnexpectedError") {
    assertEquals(result.reason, "Found but could not parse cabal file");
  }
});

Deno.test(
  "Haskell initializer returns a simple string if a cabal file exists",
  () => {
    const tempDir = Deno.makeTempDirSync();
    Deno.writeTextFileSync(
      join(tempDir, "foo.cabal"),
      `
    name: foo
    version: 0.0.1
  `,
    );
    const result = mkHaskellProjectInitializer(tempDir);
    assertEquals(result.tag, "ShouldRun");
    if (result.tag === "ShouldRun") {
      assertEquals(
        result.makeTarget(),
        outdent`
          export const foo = garn.haskell.mkHaskellProject({
            description: "",
            compiler: "ghc94",
            executables: [],
            src: "."
          })`,
      );
    }
  },
);

Deno.test("Haskell initializer includes executables", () => {
  const tempDir = Deno.makeTempDirSync();
  Deno.writeTextFileSync(
    join(tempDir, "foo.cabal"),
    `
      name: foo
      version: 0.0.1

      executable bar
        main-is: Main.hs
    `,
  );
  const result = mkHaskellProjectInitializer(tempDir);
  assertEquals(result.tag, "ShouldRun");
  if (result.tag === "ShouldRun") {
    assertEquals(
      result.makeTarget(),
      outdent`
          export const foo = garn.haskell.mkHaskellProject({
            description: "",
            compiler: "ghc94",
            executables: ["bar"],
            src: "."
          })`,
    );
  }
});
