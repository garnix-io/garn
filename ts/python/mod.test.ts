import {
  beforeEach,
  describe,
  it,
} from "https://deno.land/std@0.206.0/testing/bdd.ts";
import * as garn from "./mod.ts";
import {
  assertStderrContains,
  buildPackage,
  nixpkgsInput,
  pkgs,
  runCommand,
  runInDevShell,
} from "../testUtils.ts";
import { assert } from "https://deno.land/std@0.206.0/assert/mod.ts";
import { existsSync } from "https://deno.land/std@0.201.0/fs/exists.ts";
import { assertStringIncludes } from "https://deno.land/std@0.206.0/assert/assert_string_includes.ts";
import outdent from "https://deno.land/x/outdent@v0.8.0/mod.ts";
import * as pythonInterpreters from "../internal/nixpkgs/pythonInterpreters/mod.ts";
import * as nix from "../nix.ts";
import { nixAttrSet } from "../nix.ts";

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

describe("mkPythonProject", () => {
  let tempDir: string;
  beforeEach(() => {
    tempDir = Deno.makeTempDirSync({ prefix: "garn-mkPythonProject-test" });
    Deno.writeTextFileSync(`${tempDir}/pyproject.toml`, pyprojectToml);
    Deno.writeTextFileSync(`${tempDir}/my_package.py`, myPackagePy);
  });

  it("allows building a python project and running scripts from it", () => {
    const project = garn.mkPythonProjectSimple({
      src: "./.",
      description: "A python project",
      pythonInterpreter: pythonInterpreters.python310,
    });
    const output = buildPackage(project.package, { dir: tempDir });
    assert(existsSync(`${output}/bin/my-script`));
    // run the script
    const myScriptOutput = runCommand(
      new Deno.Command(`${output}/bin/my-script`, {}),
    );
    assertStringIncludes(myScriptOutput.stdout, "Hello, world!");
  });

  it("generates a working devShell containing the python deps", () => {
    const project = garn.mkPythonProjectSimple({
      src: "./.",
      description: "A python project",
      pythonInterpreter: pythonInterpreters.python310,
    });
    runInDevShell(project.devShell, {
      dir: tempDir,
      cmd: "python -c 'import requests; print(requests.__version__)'",
    });
  });

  it("raises informative error when dependency not available", () => {
    const pyprojectToml = outdent`
      [build-system]
      requires = ["setuptools"]
      build-backend = "setuptools.build_meta"

      [project]
      name = "my_package"
      version = "0.0.0"

      dependencies = [
        "some-nonexistent-package"
      ]
    `;
    Deno.writeTextFileSync(`${tempDir}/pyproject.toml`, pyprojectToml);
    const project = garn.mkPythonProjectSimple({
      src: "./.",
      description: "A python project",
      pythonInterpreter: pythonInterpreters.python310,
    });
    const pkg = project.package;
    const flakeFile = nix.renderFlakeFile(
      nixAttrSet({
        packages: nixAttrSet({
          "x86_64-linux": nixAttrSet({
            default: nix.nixRaw`
              let
                nixpkgs = ${nixpkgsInput};
                pkgs = ${pkgs};
                inherit (pkgs) system;
              in ${pkg.nixExpression}
            `,
          }),
        }),
      }),
    );
    Deno.writeTextFileSync(`${tempDir}/flake.nix`, flakeFile);
    const output = runCommand(
      new Deno.Command("nix", {
        args: ["build", tempDir + "#default"],
        cwd: tempDir,
      }),
    );
    assertStderrContains(
      output,
      "Package not supported or not available: some-nonexistent-package",
    );
  });
});
