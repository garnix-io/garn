import { Environment, mkEnvironment } from "../environment.ts";
import { mkPackage, Package } from "../package.ts";
import { mkProject, Project } from "../project.ts";
import { nixSource } from "../internal/utils.ts";
import { nixRaw } from "../nix.ts";
import * as pythonInterpreters from "../internal/nixpkgs/pythonInterpreters/mod.ts";
import { parsePyprojectToml } from "./utils.ts";
export { plugin as vite } from "../javascript/vite.ts";

export const interpreters = pythonInterpreters;

/**
 * Creates a pyproject.toml based python project.
 *
 * This will create a Project with
 *  - a `pkg` key that builds the entire python project;
 *  - a default shell that provides all python dependencies as well as all scripts
 *    defined via pyproject.toml
 *
 * Information like name, version, description are retrieved from the pyproject.toml file.
 *
 * @param src - The source directory
 * @param pythonInterpreter - The python interpreter to use. Pick one from garn.python.interpreters
 */
export function mkPythonProject(args: {
  src: string;
  pythonInterpreter: Package;
}): Project & {
  package: Package;
  devShell: Environment;
} {
  const python = args.pythonInterpreter.nixExpression;
  const pyproject = parsePyprojectToml(
    Deno.readTextFileSync(`${args.src}/pyproject.toml`),
  );
  if (pyproject.error) {
    throw new Error(
      `Could not parse pyproject.toml: ${pyproject.error.message}`,
    );
  }
  const constants = nixRaw`
    lib = pkgs.lib;
    python = ${python};
    src = ${nixSource(args.src)};
    pyproject = builtins.fromTOML
      (builtins.readFile "\${src}/pyproject.toml");
    dependencies = pyproject.project.dependencies or [];
    pickPackage = dep: python.pkgs.\${dep} or (throw ''
      Package not supported or not available: \${dep}
      Dependency not found in nixpkgs python packages
    '');
    toPackages = map pickPackage;
    pythonDependencies = toPackages dependencies;
    pythonSetupDependencies = toPackages pyproject.build-system.requires or ["setuptools"];
  `;
  const pkg: Package = mkPackage(
    nixRaw`
      let
        ${constants}
      in
        python.pkgs.buildPythonPackage {
          pname = pyproject.project.name;
          version = pyproject.project.version;
          src = ${nixSource(args.src)};
          format = "pyproject";
          buildInputs = pythonSetupDependencies;
          propagatedBuildInputs = pythonDependencies;
        }
    `,
    "package",
  );
  const devShell: Environment = mkEnvironment({
    nixExpression: nixRaw`
      let
        ${constants}
        pythonWithDeps = python.withPackages (ps:
          pythonDependencies
          ++ pythonSetupDependencies
          ++ [
            python.pkgs.pip
            python.pkgs.wheel
          ]
        );
        shellHook = ''
          tmp_path=$(realpath ./.pythonenv)
          mkdir -p "$tmp_path"

          repo_root=$(realpath .)
          mkdir -p "$tmp_path/python/\${python.sitePackages}"

          # Install the package in editable mode
          # This allows executing scripts from within the dev-shell using the current
          # version of the code and its dependencies.
          \${pythonWithDeps.interpreter} -m pip install \
            --quiet \
            --disable-pip-version-check \
            --no-index \
            --no-build-isolation \
            --prefix "$tmp_path/python" \
            --editable $repo_root

          export PATH="$tmp_path/python/bin:$PATH"
          export PYTHONPATH="$repo_root:$tmp_path/python/\${pythonWithDeps.sitePackages}:"
        '';
      in
        pythonWithDeps.env.overrideAttrs (old: {
          shellHook = old.shellHook or "" + "\\n" + shellHook;
        })
    `,
  });
  return mkProject(
    {
      description: pyproject.data.project?.description || "A python project",
      defaultEnvironment: devShell,
    },
    {
      package: pkg,
      devShell,
    },
  );
}
