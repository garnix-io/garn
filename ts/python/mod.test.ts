import { describe, it } from "https://deno.land/std@0.206.0/testing/bdd.ts";
import * as garn from "./mod.ts";
import { buildPackage, runCommand } from "../testUtils.ts";
import { assert } from "https://deno.land/std@0.206.0/assert/mod.ts";
import { existsSync } from "https://deno.land/std@0.201.0/fs/exists.ts";
import { assertStringIncludes } from "https://deno.land/std@0.206.0/assert/assert_string_includes.ts";
import outdent from "https://deno.land/x/outdent@v0.8.0/mod.ts";

describe("mkPythonProject", () => {
  it("allows building a python project and running scripts from it", () => {
    const tempDir = Deno.makeTempDirSync({
      prefix: "garn-mkPythonProject-test",
    });
    const pyprojectToml = outdent`
      [build-system]
      requires = ["setuptools"]
      build-backend = "setuptools.build_meta"

      [project]
      name = "my_package"
      version = "0.0.0"

      dependencies = [
        "requests"
      ]

      [project.scripts]
      my-script = "my_package:main"
    `;
    const myPackagePy = outdent`
      import requests

      def main():
          print(f"using requests version {requests.__version__}")
          print("Hello, world!")

      if __name__ == "__main__":
          main()
    `;
    Deno.writeTextFileSync(`${tempDir}/pyproject.toml`, pyprojectToml);
    Deno.writeTextFileSync(`${tempDir}/my_package.py`, myPackagePy);
    const project = garn.mkPythonProjectSimple({
      src: "./.",
      description: "A python project",
    });
    const output = buildPackage(project.package, { dir: tempDir });
    assert(existsSync(`${output}/bin/my-script`));
    // run the script
    const myScriptOutput = runCommand(
      new Deno.Command(`${output}/bin/my-script`, {}),
    );
    assertStringIncludes(myScriptOutput.stdout, "Hello, world!");
  });
});
