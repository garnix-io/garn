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
}): Project & {
  package: Package;
  // devShell: Environment;
} {
  const pkg: Package = mkPackage(
    nixRaw`
      let
        lib = pkgs.lib;
        python = pkgs.python3;
        src = ${nixSource(args.src)};
        pyproject = builtins.fromTOML
          (builtins.readFile "\${src}/pyproject.toml");
        dependencies = pyproject.project.dependencies;
        toPackages = map (dep: python.pkgs.\${dep});
      in
        pkgs.python3.pkgs.buildPythonPackage {
          pname = pyproject.project.name;
          version = pyproject.project.version;
          src = ${nixSource(args.src)};
          format = "pyproject";
          buildInputs = toPackages pyproject.build-system.requires;
          propagatedBuildInputs = toPackages dependencies;
        }
    `,
    "package",
  );
  const devShell: Environment = mkEnvironment({
    nixExpression: nixRaw`echo TODO`,
  });
  return mkProject(
    {
      description: args.description,
      // defaultEnvironment: devShell,
    },
    {
      package: pkg,
      // devShell,
    },
  );
}
