import { isProject, Project } from "../project.ts";
import { isPackage, Package } from "../package.ts";
import { Check, isCheck } from "../check.ts";
import { checkExhaustiveness, mapKeys, mapValues } from "./utils.ts";
import { GOMOD2NIX_REPO } from "../go/consts.ts";
import { nixAttrSet, NixExpression, nixRaw, nixStrLit } from "../nix.ts";
import { Executable } from "../mod.ts";
import { isExecutable } from "../executable.ts";
import { assertMayExport } from "./may_not_export.ts";
import { UserError } from "./errors.ts";

// This needs to be in sync with `DenoOutput` in GarnConfig.hs
export type DenoOutput =
  | { tag: "Success"; contents: GarnConfig }
  | { tag: "UserError"; contents: string };

export type GarnConfig = {
  targets: Targets;
  flakeFile: string;
};

type Targets = Record<string, TargetConfig>;

type TargetConfig = ProjectTarget | ExecutableTarget;

type ProjectTarget = {
  tag: "project";
  description: string;
  packages: Array<string>;
  checks: Array<string>;
};

type ExecutableTarget = {
  tag: "executable";
  description: string;
};

export const toDenoOutput = (
  nixpkgsInput: string,
  garnExports: Record<string, unknown>
): DenoOutput => {
  try {
    return {
      tag: "Success",
      contents: {
        targets: toTargets(garnExports),
        flakeFile: formatFlake(nixpkgsInput, garnExports)
          .rawNixExpressionString,
      },
    };
  } catch (err: unknown) {
    if (err instanceof UserError) {
      return {
        tag: "UserError",
        contents: err.message,
      };
    }
    throw err;
  }
};

const toTargets = (garnExports: Record<string, unknown>): Targets => {
  const result: Targets = {};
  for (const [name, exportable] of Object.entries(
    findExportables(garnExports)
  )) {
    if (isProject(exportable)) {
      const packages = collectProjectPackages(name, exportable);
      const checks = collectProjectChecks(name, exportable);
      result[name] = {
        tag: "project",
        description: exportable.description,
        packages: Object.keys(packages),
        checks: Object.keys(checks),
      };
    } else if (isExecutable(exportable)) {
      result[name] = {
        tag: "executable",
        description: exportable.description,
      };
    } else {
      checkExhaustiveness(exportable);
    }
  }
  return result;
};

const formatFlake = (
  nixpkgsInput: string,
  garnExports: Record<string, unknown>
): NixExpression => {
  const exportables = findExportables(garnExports);

  const packages = mapValues(
    (p) => p.nixExpression,
    collectPackages(exportables)
  );
  const checks = mapValues((p) => p.nixExpression, collectChecks(exportables));
  const shells = mapValues(
    (exportable) =>
      isProject(exportable) && exportable.defaultEnvironment
        ? exportable.defaultEnvironment.nixExpression
        : undefined,
    exportables
  );
  const apps = mapValues((exportable) => {
    if (isProject(exportable) && exportable.defaultExecutable) {
      return nixAttrSet({
        type: nixStrLit`app`,
        program: exportable.defaultExecutable.nixExpression,
      });
    } else if (isExecutable(exportable)) {
      return nixAttrSet({
        type: nixStrLit`app`,
        program: exportable.nixExpression,
      });
    } else {
      return undefined;
    }
  }, exportables);
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

type Exportable = Project | Executable;

const findExportables = (
  config: Record<string, unknown>
): Record<string, Exportable> => {
  const result: Record<string, Exportable> = {};
  for (const [name, value] of Object.entries(config)) {
    assertMayExport(name, value);
    if (isProject(value)) {
      result[name] = value;
      const nested = findExportables(value);
      for (const key in nested) {
        if (key === "defaultExecutable") continue;
        result[`${name}/${key}`] = nested[key];
      }
    } else if (isExecutable(value)) {
      result[name] = value;
    }
  }
  return result;
};

const collectPackages = (
  config: Record<string, Exportable>
): Record<string, Package> => {
  let result: Record<string, Package> = {};
  for (const [name, exportable] of Object.entries(config)) {
    if (isProject(exportable)) {
      result = {
        ...result,
        ...collectProjectPackages(name, exportable),
      };
    }
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
  config: Record<string, Exportable>
): Record<string, Check> => {
  let result: Record<string, Check> = {};
  for (const [name, exportable] of Object.entries(config)) {
    if (isProject(exportable)) {
      result = {
        ...result,
        ...collectProjectChecks(name, exportable),
      };
    }
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
