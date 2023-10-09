import { Check } from "./check.ts";
import { Environment, isEnvironment } from "./environment.ts";
import { Executable, isExecutable } from "./executable.ts";
import { Interpolatable } from "./nix.ts";
import { Package } from "./package.ts";
import { hasTag } from "./utils.ts";
import { Paths } from "./nested-key.ts";

// Project settings establish such things as defaulting rules.
type ProjectSettings<C> = {
  defaults?: {
    executable?: Paths<C>;
    environment?: Paths<C>;
  };
};

// Attributes and methods that all projects have
type ProjectCommon<T extends ProjectComponents> = {
  tag: "project";
  settings: ProjectSettings<T>;
  description: string;
  withDevTools<X extends Project<T>>(this: X, devTools: Array<Package>): X;
  shell(
    this: Project<T>,
    _s: TemplateStringsArray,
    ..._args: Array<string>
  ): Executable;
  check(
    this: Project<T>,
    _s: TemplateStringsArray,
    ..._args: Array<string>
  ): Check;
};

// A Project is an organized and cohesive group of Executables, Packages, Checks
// and Environments, as well as standard functions for operating on those.
export type Project<T extends ProjectComponents> = ProjectCommon<T> & {
  [K in keyof T]: K extends keyof ProjectCommon<T> ? ProjectCommon<T>[K] : T[K];
};

export function isProject<T extends ProjectComponents>(
  p: unknown
): p is Project<T> {
  return hasTag(p, "project");
}

type Nestable =
  | Environment
  | Package
  | Executable
  | Check
  | Project<ProjectComponents>;

export interface ProjectComponents {
  [i: string]: Nestable;
}

export function mkProject<C extends ProjectComponents>(
  description: string,
  deps: C,
  settings: ProjectSettings<C>
): Project<C> {
  const helpers = proxyEnvironmentHelpers();
  return {
    tag: "project",
    description,
    settings,
    ...deps,
    ...helpers,
  } as Project<C>;
}

const proxyEnvironmentHelpers = <T extends ProjectComponents>() => ({
  shell() {
    throw new Error(`not yet implemented`);
  },

  check<
    X extends Project<T> & { settings: { defaults: { environment: string } } }
  >(this: X, s: TemplateStringsArray, ...args: Array<Interpolatable>) {
    const environment = projectDefaultEnvironment(this);
    if (environment == null) {
      throw new Error(
        `'.check' can only be called on projects with a default environment`
      );
    }
    return environment.check(s, ...args);
  },

  withDevTools<
    X extends Project<T> & { settings: { defaults: { environment: string } } }
  >(this: X, devTools: Array<Package>): X {
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

export const projectDefaultEnvironment = <T extends ProjectComponents>(
  project: Project<T>
): Environment | undefined => {
  return getDefault("environment", isEnvironment, project, project.settings);
};

export const projectDefaultExecutable = <T extends ProjectComponents>(
  project: Project<T>
): Executable | undefined => {
  return getDefault("executable", isExecutable, project, project.settings);
};

const getDefault = <T>(
  key: keyof NonNullable<ProjectSettings<T>["defaults"]>,
  test: (x: unknown) => x is T,
  project: Project<ProjectComponents>,
  settings: ProjectSettings<unknown>
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
