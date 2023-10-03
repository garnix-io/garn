import { Package, Project, projectSchema } from "./base.ts";
import { Environment, isEnvironment } from "./environment.ts";
import { dbg } from "./utils.ts";
import { Schema, z } from "https://deno.land/x/zod@v3.22.2/mod.ts";

export const formatFlake = (
  nixpkgsInput: string,
  config: Record<string, Package | unknown>
): string => {
  if (isPackageRecord(config)) {
    return oldFormatFlake(nixpkgsInput, config);
  } else {
    return newFormatFlake(nixpkgsInput, config);
  }
};

function isPackageRecord(
  config: Record<string, unknown>
): config is Record<string, Package> {
  return (
    Object.values(config).find(
      (c) =>
        !(
          typeof c === "object" &&
          c != null &&
          "tag" in c &&
          c.tag == "package"
        )
    ) == null
  );
}

const oldFormatFlake = (
  nixpkgsInput: string,
  config: Record<string, Package>
): string => {
  const packages = Object.entries(config).reduce(
    (acc, [name, pkg]) => acc + `${name} = ${pkg.nixExpression};`,
    ""
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
  config: Record<string, Package | unknown>
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
        };`
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

type Enterable = Project & { defaults: { environment: string } };

const getEnterExpression = (e: Enterable): string => {
  if (!(e.defaults.environment in e)) {
    throw new Error(
      `defaults.environment points to a non-existing field: ${e.defaults.environment}`
    );
  }
  const environment: unknown = e[e.defaults.environment as keyof typeof e];
  if (!isEnvironment(environment)) {
    throw new Error(
      `defaults.environment points to a non-environment: ${e.defaults.environment}`
    );
  }
  return (
    environment.nixExpr ??
    (() => {
      throw new Error(`bottom`);
    })()
  );
};

const findEnterable = (
  config: Record<string, unknown>
): Record<string, Enterable> => {
  const result: Record<string, Enterable> = {};
  for (const [name, value] of Object.entries(config)) {
    const project = projectSchema.safeParse(value);
    if (project.success) {
      if (project.data.defaults?.environment) {
        result[name] = project.data as Enterable;
      }
    }
  }
  return result;
};
