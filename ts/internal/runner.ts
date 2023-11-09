import { isProject, Project } from "../project.ts";
import { isPackage, Package } from "../package.ts";
import { Check, isCheck } from "../check.ts";
import { checkExhaustiveness, mapKeys, mapValues } from "./utils.ts";
import {
  nixFlakeDep,
  nixAttrSet,
  NixExpression,
  nixRaw,
  nixStrLit,
  renderFlakeFile,
} from "../nix.ts";
import { Executable } from "../mod.ts";
import { isExecutable } from "../executable.ts";
import { assertMayExport } from "./may_not_export.ts";
import { UserError } from "./errors.ts";
import { GARN_TS_LIB_VERSION } from "./version.ts";

// This needs to be in sync with `DenoOutput` in GarnConfig.hs
export type DenoOutput =
  | { garnTsLibVersion: string; tag: "Success"; contents: GarnConfig }
  | { garnTsLibVersion: string; tag: "UserError"; contents: string };

export type GarnConfig = {
  targets: Targets;
  flakeFile: string;
};

type Targets = Record<string, TargetConfig>;

type TargetConfig = ProjectTarget | PackageTarget | ExecutableTarget;

type ProjectTarget = {
  tag: "project";
  description: string;
  packages: Array<string>;
  checks: Array<string>;
  runnable: boolean;
};

type PackageTarget = {
  tag: "package";
  description: string;
};

type ExecutableTarget = {
  tag: "executable";
  description: string;
};

export const toDenoOutput = (
  nixpkgsInput: string,
  garnExports: Record<string, unknown>,
): DenoOutput => {
  try {
    return {
      garnTsLibVersion: GARN_TS_LIB_VERSION,
      tag: "Success",
      contents: {
        targets: toTargets(garnExports),
        flakeFile: renderFlakeFile(formatFlake(nixpkgsInput, garnExports)),
      },
    };
  } catch (err: unknown) {
    if (err instanceof UserError) {
      return {
        garnTsLibVersion: GARN_TS_LIB_VERSION,
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
    findExportables(garnExports),
  )) {
    if (isProject(exportable)) {
      const packages = collectProjectPackages(name, exportable);
      const checks = collectProjectChecks(name, exportable);
      result[name] = {
        tag: "project",
        description: exportable.description,
        packages: Object.keys(packages),
        checks: Object.keys(checks),
        runnable: !!exportable.defaultExecutable,
      };
    } else if (isPackage(exportable)) {
      result[name] = {
        tag: "package",
        description: exportable.description ?? "TODO",
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
  garnExports: Record<string, unknown>,
): NixExpression => {
  const exportables = findExportables(garnExports);

  const packages = mapValues(
    (p) => p.nixExpression,
    collectPackages(exportables),
  );
  const checks = mapValues((p) => p.nixExpression, collectChecks(exportables));
  const shells = mapValues(
    (exportable) =>
      isProject(exportable) && exportable.defaultEnvironment
        ? exportable.defaultEnvironment.nixExpression
        : undefined,
    exportables,
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
  return nixRaw`
    let
      nixpkgs = ${nixFlakeDep("nixpkgs-repo", { url: nixpkgsInput })};
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
    }
  `;
};

type Exportable = Project | Package | Executable;

const findExportables = (
  config: Record<string, unknown>,
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
    } else if (isPackage(value)) {
      result[name] = value;
    } else if (isExecutable(value)) {
      result[name] = value;
    }
  }
  return result;
};

const collectPackages = (
  config: Record<string, Exportable>,
): Record<string, Package> => {
  const result: Record<string, Package> = {};
  for (const [name, exportable] of Object.entries(config)) {
    if (isPackage(exportable)) {
      result[name] = exportable;
    }
  }
  return result;
};

const collectProjectPackages = (
  projectName: string,
  project: Project,
): Record<string, Package> =>
  mapKeys(
    (name) => `${projectName}/${name}`,
    collectByPredicate(isPackage, project),
  );

const collectChecks = (
  config: Record<string, Exportable>,
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
  project: Project,
): Record<string, Check> =>
  mapKeys(
    (name) => `${projectName}_${name}`, // TODO: make slash too?
    collectByPredicate(isCheck, project),
  );

const collectByPredicate = <T>(
  predicate: (t: unknown) => t is T,
  project: Project,
): Record<string, T> => {
  const result: Record<string, T> = {};
  for (const [name, value] of Object.entries(project)) {
    if (predicate(value)) {
      result[name] = value;
    }
  }
  return result;
};
