import {
  isProject,
  Project,
  projectDefaultEnvironment,
  projectDefaultExecutable,
} from "../project.ts";
import { Package, isPackage } from "../package.ts";
import { Executable } from "../executable.ts";
import { Environment } from "../environment.ts";
import { Check, isCheck } from "../check.ts";
import { mapKeys } from "./utils.ts";
import { GOMOD2NIX_REPO } from "../go.ts";

// This needs to be in sync with `GarnConfig` in GarnConfig.hs
export type GarnConfig = {
  targets: Targets;
  flakeFile: string;
};

type Targets = Record<
  string,
  {
    description: string;
    packages: Array<string>;
    checks: Array<string>;
  }
>;

export const toGarnConfig = (
  nixpkgsInput: string,
  garnExports: Record<string, unknown>
): GarnConfig => ({
  targets: toTargets(garnExports),
  flakeFile: formatFlake(nixpkgsInput, garnExports),
});

const toTargets = (garnExports: Record<string, unknown>): Targets => {
  const result: Targets = {};
  for (const [projectName, project] of Object.entries(
    findProjects(garnExports)
  )) {
    const packages = collectProjectPackages(projectName, project);
    const checks = collectProjectChecks(projectName, project);
    result[projectName] = {
      description: project.description,
      packages: Object.keys(packages),
      checks: Object.keys(checks),
    };
  }
  return result;
};

const formatFlake = (
  nixpkgsInput: string,
  garnExports: Record<string, unknown>
): string => {
  const projects = findProjects(garnExports);
  const packages = collectPackages(projects);
  const packagesString = Object.entries(packages)
    .map(([name, pkg]) => `${name} = ${pkg.nixExpression};`)
    .join("\n");
  const checks = collectChecks(projects);
  const checksString = Object.entries(checks)
    .map(([name, check]) => `${name} = ${check.nixExpression};`)
    .join("\n");
  const shellsString = Object.entries(projects)
    .map(
      ([name, project]) => [name, projectDefaultEnvironment(project)] as const
    )
    .filter((x): x is [string, Environment] => x[1] != null)
    .map(
      ([name, defaultEnvironment]) =>
        `${name} = ${defaultEnvironment.nixExpr || "pkgs.mkShell {}"};`
    )
    .join("\n");
  const appsString = Object.entries(projects)
    .map(
      ([name, project]) => [name, projectDefaultExecutable(project)] as const
    )
    .filter((x): x is [string, Executable] => x[1] != null)
    .map(
      ([name, executable]) =>
        `
        ${name} = {
          type = "app";
          program = ${executable.nixExpression};
        };
      `
    )
    .join("\n");
  return `{
    inputs.nixpkgs.url = "${nixpkgsInput}";
    inputs.flake-utils.url = "github:numtide/flake-utils";
    inputs.gomod2nix-repo.url = "${GOMOD2NIX_REPO}";
    inputs.npmlock2nix-repo = {
      url = "github:nix-community/npmlock2nix?rev=9197bbf397d76059a76310523d45df10d2e4ca81";
      flake = false;
    };
    outputs = { self, nixpkgs, flake-utils, npmlock2nix-repo, gomod2nix-repo }:
      let
        systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
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
            ${packagesString}
          });
        checks = forAllSystems (system:
          let
            pkgs = import "\${nixpkgs}" {
              config.allowUnfree = true;
              inherit system;
            };
          in
          {
            ${checksString}
          });
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
        apps = forAllSystems (system: let
            pkgs = import "\${nixpkgs}" { inherit system; };
        in
        {
          ${appsString}
        });
      };
  }`;
};

const findProjects = (
  config: Record<string, unknown>
): Record<string, Project> => {
  const result: Record<string, Project> = {};
  for (const [name, value] of Object.entries(config)) {
    if (isProject(value)) {
      result[name] = value;
    }
  }
  return result;
};

const collectPackages = (
  config: Record<string, Project>
): Record<string, Package> => {
  let result: Record<string, Package> = {};
  for (const [projectName, project] of Object.entries(config)) {
    result = {
      ...result,
      ...collectProjectPackages(projectName, project),
    };
  }
  return result;
};

const collectProjectPackages = (
  projectName: string,
  project: Project
): Record<string, Package> =>
  mapKeys(
    (name) => `${projectName}_${name}`,
    collectByPredicate(isPackage, project)
  );

const collectChecks = (
  config: Record<string, Project>
): Record<string, Check> => {
  let result: Record<string, Check> = {};
  for (const [projectName, project] of Object.entries(config)) {
    result = {
      ...result,
      ...collectProjectChecks(projectName, project),
    };
  }
  return result;
};

const collectProjectChecks = (
  projectName: string,
  project: Project
): Record<string, Check> =>
  mapKeys(
    (name) => `${projectName}_${name}`,
    collectByPredicate(isCheck, project)
  );

const collectByPredicate = <T>(
  predicate: (t: unknown) => t is T,
  project: Project
): Record<string, T> => {
  const result: Record<string, T> = {};
  for (const [name, value] of Object.entries(project)) {
    if (predicate(value)) {
      result[name] = value;
    }
  }
  return result;
};
