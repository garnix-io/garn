import {
  assertEquals,
  assert,
} from "https://deno.land/std@0.201.0/assert/mod.ts";
import outdent from "https://deno.land/x/outdent@v0.8.0/mod.ts";
import { join } from "https://deno.land/std@0.201.0/path/mod.ts";
import { pythonInitializer } from "./initializers.ts";
import * as toml from "https://deno.land/std@0.109.0/encoding/toml.ts";

Deno.test(
  "Python initializer does not run when pyproject.toml is not present",
  () => {
    const tempDir = Deno.makeTempDirSync();
    const result = pythonInitializer(tempDir);
    assertEquals(result.tag, "ShouldNotRun");
  },
);

Deno.test("Python initializer errors if pyproject.toml is unparseable", () => {
  const tempDir = Deno.makeTempDirSync();
  Deno.writeTextFileSync(
    join(tempDir, "pyproject.toml"),
    `
      some invalid: toml
    `,
  );
  const result = pythonInitializer(tempDir);
  if (result.tag !== "UnexpectedError") {
    throw new Error("expected an UnexpectedError");
  }
  assert(result.reason.startsWith("Could not parse pyproject.toml"));
});

Deno.test("Python initializer returns the code to be generated", () => {
  const tempDir = Deno.makeTempDirSync();
  Deno.writeTextFileSync(
    join(tempDir, "pyproject.toml"),
    toml.stringify({
      project: {
        name: "somepackage",
        version: "0.0.0",
        description: "just some package",
      },
    }),
  );
  const result = pythonInitializer(tempDir);
  if (result.tag !== "ShouldRun") {
    throw new Error("Expected initializer to run");
  }
  assertEquals(
    result.makeTarget(),
    outdent`
      export const somepackage = garn.python.mkPythonProject({
        src: ".",
        pythonInterpreter: garn.python.interpreters.python310,
      });
    `,
  );
});
