import { Check } from "./check.ts";
import { Environment, isEnvironment } from "./environment.ts";
import { Executable, isExecutable } from "./executable.ts";
import { NewPackage } from "./package.ts";
import { hasTag } from "./utils.ts";

export type Project = {
  tag: "project";
  settings: ProjectSettings;
};

type ProjectSettings = {
  defaults?: {
    executable?: string;
    environment?: string;
  };
};

// In the future we plan on adding Project, Check, etc..
type Nestable = Environment | NewPackage | Executable;

function proxyEnvironmentHelpers(environment: Environment) {
  return {
    shell() {
      throw 1;
    },
    check() {
      throw 1;
    },
    withDevTools<
      T extends Project & { settings: { defaults: { environment: string } } }
    >(this: T, devTools: Array<NewPackage>): T {
      const newEnvironment = environment.withDevTools(devTools);
      return {
        ...this,
        [this.settings.defaults.environment]: newEnvironment,
      };
    },
  };
}

export type ProjectWithDefaultEnvironment = Project & {
  withDevTools<T extends ProjectWithDefaultEnvironment>(
    this: T,
    devTools: Array<NewPackage>
  ): T;
  shell(
    this: ProjectWithDefaultEnvironment,
    _s: TemplateStringsArray,
    ..._args: Array<string>
  ): Executable;
  check(
    this: ProjectWithDefaultEnvironment,
    _s: TemplateStringsArray,
    ..._args: Array<string>
  ): Check;
};

export function mkProject<Deps extends Record<string, Nestable>>(
  deps: Deps,
  settings: ProjectSettings & { defaults: { environment: string } }
): Deps & ProjectWithDefaultEnvironment;

export function mkProject<Deps extends Record<string, Nestable>>(
  deps: Deps,
  settings: ProjectSettings
): Deps & Project;

export function mkProject<Deps extends Record<string, Nestable>>(
  deps: Deps
): Deps & Project;

export function mkProject<Deps extends Record<string, Nestable>>(
  deps: Deps,
  settings: { defaults: { environment: string } } | ProjectSettings = {}
): (Deps & ProjectWithDefaultEnvironment) | (Deps & Project) {
  const environment = getDefaultEnvironment(deps, settings);
  const helpers =
    environment != null ? proxyEnvironmentHelpers(environment) : {};
  return {
    ...deps,
    ...helpers,
    tag: "project",
    description: "huhu",
    settings,
  };
}

export function isProject(p: unknown): p is Project {
  return hasTag(p, "project");
}

export const projectDefaultEnvironment = (
  project: Project
): Environment | undefined => {
  return getDefaultEnvironment(project, project.settings);
};

export const projectDefaultExecutable = (
  project: Project
): Executable | undefined => {
  return getDefault("executable", isExecutable)(project, project.settings);
};

const getDefault =
  <T>(
    key: keyof NonNullable<ProjectSettings["defaults"]>,
    test: (x: unknown) => x is T
  ) =>
  (
    project: Record<string, unknown>,
    settings: ProjectSettings
  ): T | undefined => {
    const defaultKey = settings.defaults?.[key];
    if (defaultKey == null) {
      return undefined;
    }
    if (!(defaultKey in project)) {
      throw new Error(
        `defaults.${key} points to a non-existing field: ${defaultKey}`
      );
    }
    const value: unknown = project[defaultKey as keyof typeof project];
    if (!test(value)) {
      throw new Error(`defaults.${key} points to a non-${key}: ${defaultKey}`);
    }
    // if (!value.nixExpr) {
    //   throw new Error(`TODO: Handle empty ${key}`);
    // }
    return value;
  };

const getDefaultEnvironment = getDefault("environment", isEnvironment);
