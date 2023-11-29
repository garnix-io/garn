import { Environment, mkEnvironment } from "../environment.ts";
import { mkPackage, Package } from "../package.ts";
import { mkProject, Project } from "../project.ts";
import { nixSource } from "../internal/utils.ts";
import { nixRaw } from "../nix.ts";
export { plugin as vite } from "../javascript/vite.ts";

/**
 * Creates a pyproject based python garn Project.
 */
export function mkPythonProjectSimple(args: {
  description: string;
  src: string;
  pythonInterpreter: Package;
}): Project & {
  package: Package;
  devShell: Environment;
} {
  const python = args.pythonInterpreter.nixExpression;
  const pythonDependencies = nixRaw`
    let
      python = ${python};
      src = ${nixSource(args.src)};
      pyproject = builtins.fromTOML
        (builtins.readFile "\${src}/pyproject.toml");
      dependencies = pyproject.project.dependencies;
      toPackages = map (dep: python.pkgs.\${dep});
    in
      toPackages dependencies
  `;
  const pkg: Package = mkPackage(
    nixRaw`
      let
        lib = pkgs.lib;
        python = ${python};
        src = ${nixSource(args.src)};
        pyproject = builtins.fromTOML
          (builtins.readFile "\${src}/pyproject.toml");
        dependencies = pyproject.project.dependencies;
        toPackages = map (dep: python.pkgs.\${dep});
      in
        python.pkgs.buildPythonPackage {
          pname = pyproject.project.name;
          version = pyproject.project.version;
          src = ${nixSource(args.src)};
          format = "pyproject";
          buildInputs = toPackages pyproject.build-system.requires;
          propagatedBuildInputs = ${pythonDependencies};
        }
    `,
    "package",
  );
  const devShell: Environment = mkEnvironment({
    nixExpression: nixRaw`
      let
        python = ${python};
        pythonDependencies = ${pythonDependencies};
        pythonWithDeps = python.withPackages (ps:
          pythonDependencies
          ++ [python.pkgs.pip]
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
      description: args.description,
      defaultEnvironment: devShell,
    },
    {
      package: pkg,
      devShell,
    },
  );
}
