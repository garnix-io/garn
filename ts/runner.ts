import { isProject, Project } from "./project.ts";
import { Package } from "./base.ts";
import { isEnvironment } from "./environment.ts";

export const formatFlake = (
  nixpkgsInput: string,
  config: Record<string, Package | unknown>,
): string => {
  if (isPackageRecord(config)) {
    return oldFormatFlake(nixpkgsInput, config);
  } else {
    return newFormatFlake(nixpkgsInput, config);
  }
};

function isPackageRecord(
  config: Record<string, unknown>,
): config is Record<string, Package> {
  return (
    Object.values(config).find(
      (c) =>
        !(
          typeof c === "object" &&
          c != null &&
          "tag" in c &&
          c.tag == "package"
        ),
    ) == null
  );
}

const oldFormatFlake = (
  nixpkgsInput: string,
  config: Record<string, Package>,
): string => {
  const packages = Object.entries(config).reduce(
    (acc, [name, pkg]) => acc + `${name} = ${pkg.nixExpression};`,
    "",
  );
  const shells = Object.entries(config).reduce((acc, [name, pkg]) => {
    const pkgAttr = `self.packages.\${system}.${name}`;
    const env = pkg.envExpression(pkgAttr);
    return acc + `${name} = ${env};`;
  }, "");
  return `{
    inputs.nixpkgs.url = "${nixpkgsInput}";

    inputs.npmlock2nix-repo = {
      url = "github:nix-community/npmlock2nix?rev=9197bbf397d76059a76310523d45df10d2e4ca81";
      flake = false;
    };

    outputs = { self, nixpkgs, npmlock2nix-repo }:
      let
        systems = [ "x86_64-linux" ];
        forAllSystems = nixpkgs.lib.genAttrs systems;
      in
      {
        packages = forAllSystems (system:
          let
            pkgs = import "\${nixpkgs}" {
              config.allowUnfree = true;
              inherit system;
            };
          in
          {
            ${packages}
          });
        devShells = forAllSystems (system:
          let
            pkgs = import "\${nixpkgs}" {
              config.allowUnfree = true;
              inherit system;
            };
          in
          {
            ${shells}
          });
        formatter = forAllSystems (system:
          let
            pkgs = import "\${nixpkgs}" {
              config.allowUnfree = true;
              inherit system;
            };
          in
          pkgs.nixpkgs-fmt);
      };
  }`;
};

export const newFormatFlake = (
  nixpkgsInput: string,
  config: Record<string, unknown>,
): string => {
  const shells = findEnterable(config);
  const shellsString = Object.entries(shells)
    .map(
      ([name, enterable]) =>
        `${name} = ${
          getEnterExpression(enterable) ||
          (() => {
            throw new Error(`bottom`);
          })()
        };`,
    )
    .join("\n");
  return `{
    inputs.nixpkgs.url = "${nixpkgsInput}";
    outputs = { self, nixpkgs }:
      let
        systems = [ "x86_64-linux" ];
        forAllSystems = nixpkgs.lib.genAttrs systems;
      in
      {
        devShells = forAllSystems (system:
          let
            pkgs = import "\${nixpkgs}" {
              config.allowUnfree = true;
              inherit system;
            };
          in
          {
            ${shellsString}
          });
      };
  }`;
};

type Enterable = Project & { settings: { defaults: { environment: string } } };

const getEnterExpression = (e: Enterable): string => {
  if (!(e.settings.defaults.environment in e)) {
    throw new Error(
      `defaults.environment points to a non-existing field: ${e.settings.defaults.environment}`,
    );
  }
  const environment: unknown =
    e[e.settings.defaults.environment as keyof typeof e];
  if (!isEnvironment(environment)) {
    throw new Error(
      `defaults.environment points to a non-environment: ${e.settings.defaults.environment}`,
    );
  }
  if (!environment.nixExpr) {
    throw new Error(`TODO: Handle empty environment`);
  }
  return environment.nixExpr;
};

const findEnterable = (
  config: Record<string, unknown>,
): Record<string, Enterable> => {
  const result: Record<string, Enterable> = {};
  for (const [name, value] of Object.entries(config)) {
    if (isProject(value) && value.settings.defaults?.environment) {
      result[name] = value as Enterable;
    }
  }
  return result;
};
