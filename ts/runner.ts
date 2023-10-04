import {
  isProject,
  Project,
  projectDefaultEnvironment,
  projectDefaultExecutable,
} from "./project.ts";
import { Package, isPackage } from "./package.ts";
import outdent from "https://deno.land/x/outdent@v0.8.0/mod.ts";
import { Executable } from "./executable.ts";
import { Environment } from "./environment.ts";

type Targets = Record<
  string,
  {
    description: string;
    checks?: Array<string>;
  }
>;

export const toTargets = <A>(
  garnerExports: Record<string, unknown>
): Targets => {
  const result: Targets = {};
  for (const [projectName, project] of Object.entries(garnerExports)) {
    if (isProject(project)) {
      const packages = collectProjectPackages(projectName, project);
      result[projectName] = {
        description: project.description,
        checks: Object.keys(packages),
      };
    }
  }
  return result;
};

export const formatFlake = (
  nixpkgsInput: string,
  config: Record<string, unknown>
): string => {
  const projects = findProjects(config);
  const packages = collectPackages(projects);
  const packagesString = Object.entries(packages)
    .map(([name, pkg]) => `${name} = ${pkg.nixExpression};`)
    .join("\n");
  const shellsString = Object.entries(projects)
    .map(
      ([name, project]) => [name, projectDefaultEnvironment(project)] as const
    )
    .filter((x): x is [string, Environment] => x[1] != null)
    .map(
      ([name, defaultEnvironment]) =>
        `${name} = ${
          defaultEnvironment.nixExpr ||
          (() => {
            throw new Error(`bottom`);
          })()
        };`
    )
    .join("\n");
  const executables = Object.entries(projects)
    .map(
      ([name, project]) => [name, projectDefaultExecutable(project)] as const
    )
    .filter((x): x is [string, Executable] => x[1] != null)
    .map(
      ([name, executable]) =>
        outdent`
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
    inputs.npmlock2nix-repo = {
      url = "github:nix-community/npmlock2nix?rev=9197bbf397d76059a76310523d45df10d2e4ca81";
      flake = false;
    };
    outputs = { self, nixpkgs, flake-utils, npmlock2nix-repo }:
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
            ${packagesString}
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
          ${executables}
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
): Record<string, Package> => {
  const result: Record<string, Package> = {};
  for (const [name, value] of Object.entries(project)) {
    if (isPackage(value)) {
      result[`${projectName}_${name}`] = value;
    }
  }
  return result;
};
