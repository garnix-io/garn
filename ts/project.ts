import { Check } from "./check.ts";
import { Environment, isEnvironment } from "./environment.ts";
import { Executable, isExecutable } from "./executable.ts";
import { Interpolatable } from "./nix.ts";
import { Package } from "./package.ts";
import { hasTag } from "./internal/utils.ts";
import {} from "./internal/registerInternalLib.ts";

export type Project = {
  tag: "project";
  settings: ProjectSettings;
  description: string;
};

export function isProject(p: unknown): p is Project {
  return hasTag(p, "project");
}

type ProjectSettings = {
  defaults?: {
    executable?: string;
    environment?: string;
  };
};

export type ProjectWithDefaultEnvironment = Project & {
  withDevTools<T extends ProjectWithDefaultEnvironment>(
    this: T,
    devTools: Array<Package>
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

// In the future we plan on adding Project & Check.
type Nestable = Environment | Package | Executable;

export function mkProject<Deps extends Record<string, Nestable>>(
  description: string,
  deps: Deps,
  settings: ProjectSettings & { defaults: { environment: string } }
): Deps & ProjectWithDefaultEnvironment;

export function mkProject<Deps extends Record<string, Nestable>>(
  description: string,
  deps: Deps,
  settings: ProjectSettings
): Deps & Project;

export function mkProject<Deps extends Record<string, Nestable>>(
  description: string,
  deps: Deps
): Deps & Project;

export function mkProject<Deps extends Record<string, Nestable>>(
  description: string,
  deps: Deps,
  settings: { defaults: { environment: string } } | ProjectSettings = {}
): (Deps & ProjectWithDefaultEnvironment) | (Deps & Project) {
  const environment = getDefault("environment", isEnvironment, deps, settings);
  const helpers = environment != null ? proxyEnvironmentHelpers() : {};
  return {
    ...deps,
    ...helpers,
    tag: "project",
    description,
    settings,
  };
}

const proxyEnvironmentHelpers = () => ({
  shell() {
    throw new Error(`not yet implemented`);
  },

  check<
    T extends Project & { settings: { defaults: { environment: string } } }
  >(this: T, s: TemplateStringsArray, ...args: Array<Interpolatable>) {
    const environment = projectDefaultEnvironment(this);
    if (environment == null) {
      throw new Error(
        `'.check' can only be called on projects with a default environment`
      );
    }
    return environment.check(s, ...args);
  },

  withDevTools<
    T extends Project & { settings: { defaults: { environment: string } } }
  >(this: T, devTools: Array<Package>): T {
    const environment = projectDefaultEnvironment(this);
    if (environment == null) {
      throw new Error(
        `'.withDevTools' can only be called on projects with a default environment`
      );
    }
    const newEnvironment = environment.withDevTools(devTools);
    return {
      ...this,
      [this.settings.defaults.environment]: newEnvironment,
    };
  },
});

export const projectDefaultEnvironment = (
  project: Project
): Environment | undefined => {
  return getDefault("environment", isEnvironment, project, project.settings);
};

export const projectDefaultExecutable = (
  project: Project
): Executable | undefined => {
  return getDefault("executable", isExecutable, project, project.settings);
};

const getDefault = <T>(
  key: keyof NonNullable<ProjectSettings["defaults"]>,
  test: (x: unknown) => x is T,
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
  return value;
};
