import { isProject, Project } from "../project.ts";
import { isPackage, Package } from "../package.ts";
import { Check, isCheck } from "../check.ts";
import { mapKeys, mapValues } from "./utils.ts";
import { GOMOD2NIX_REPO } from "../go.ts";
import { nixAttrSet, NixExpression, nixRaw, nixStrLit } from "../nix.ts";

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
  flakeFile: formatFlake(nixpkgsInput, garnExports).expr,
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

const getNixExpression = (n: { nixExpression: NixExpression }): NixExpression =>
  n.nixExpression;

const formatFlake = (
  nixpkgsInput: string,
  garnExports: Record<string, unknown>
): NixExpression => {
  const projects = findProjects(garnExports);

  const packages = mapValues(getNixExpression, collectPackages(projects));
  const checks = mapValues(getNixExpression, collectChecks(projects));
  const shells = mapValues(
    (project) => project.defaultEnvironment?.nixExpression,
    projects
  );
  const apps = mapValues(
    ({ defaultExecutable }) =>
      defaultExecutable
        ? nixAttrSet({
            type: nixStrLit`app`,
            program: defaultExecutable.nixExpression,
          })
        : undefined,
    projects
  );
  return nixRaw`{
    inputs.nixpkgs.url = ${nixStrLit(nixpkgsInput)};
    inputs.flake-utils.url = "github:numtide/flake-utils";
    inputs.gomod2nix-repo.url = ${nixStrLit(GOMOD2NIX_REPO)};
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
            ${nixAttrSet(packages)}
          );
        checks = forAllSystems (system:
          let
            pkgs = import "\${nixpkgs}" {
              config.allowUnfree = true;
              inherit system;
            };
          in
            ${nixAttrSet(checks)}
          );
        devShells = forAllSystems (system:
          let
            pkgs = import "\${nixpkgs}" {
              config.allowUnfree = true;
              inherit system;
            };
          in
            ${nixAttrSet(shells)}
          );
        apps = forAllSystems (system: let
            pkgs = import "\${nixpkgs}" { inherit system; };
        in
          ${nixAttrSet(apps)}
        );
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
