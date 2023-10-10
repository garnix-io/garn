import { Check } from "./check.ts";
import { Environment, isEnvironment } from "./environment.ts";
import { Executable, isExecutable } from "./executable.ts";
import { Interpolatable } from "./nix.ts";
import { Package } from "./package.ts";
import { hasTag } from "./utils.ts";

type ProjectSettings = {
  defaults?: {
    executable?: string;
    environment?: string;
  };
};

/**
 * A Project is a logical grouping of Packages and Environments. For example,
 * you may have a 'frontend' Project, a 'backend' Project, a 'cli' Project, etc.
 */
export type Project = {
  tag: "project";
  settings: ProjectSettings;
  description: string;
  /**
   * Returns a new Project with the provided devtools added to the default
   * Environment.
   */
  withDevTools<T extends Project>(this: T, devTools: Array<Package>): T;
  /**
   * A tagged template literal that runs the given command inside the Project's
   * default Environment.
   *
   * Example:
   * ```typescript
   * const myExecutable = myProject.shell`echo "hello world"`;
   * ```
   */
  shell(
    this: Project,
    _s: TemplateStringsArray,
    ..._args: Array<string>
  ): Executable;
  /**
   * Returns a check that runs in a *pure* version of the Project's default
   * Environment.
   */
  check(
    this: Project,
    _s: TemplateStringsArray,
    ..._args: Array<string>
  ): Check;
};

export function isProject(p: unknown): p is Project {
  return hasTag(p, "project");
}

type Nestable = Environment | Package | Executable | Check;

/**
 * Create a new Project.
 *
 * @param description A human-readable description of the Project.
 * @param deps A record of Environments, Packages and Executables,
 * @param settings Settings such as defaults for Environments and Executables.
 */
export function mkProject<Deps extends Record<string, Nestable>>(
  description: string,
  deps: Deps,
  settings: ProjectSettings = {}
): Deps & Project {
  const helpers = proxyEnvironmentHelpers();
  return {
    ...deps,
    ...helpers,
    tag: "project",
    description,
    settings,
  };
}

const proxyEnvironmentHelpers = () => ({
  shell(
    this: Project,
    s: TemplateStringsArray,
    ...args: Array<Interpolatable>
  ) {
    const environment = projectDefaultEnvironment(this);
    if (environment == null) {
      throw new Error(
        `'.shell' can only be called on projects with a default environment`
      );
    }
    return environment.shell(s, ...args);
  },

  check(
    this: Project,
    s: TemplateStringsArray,
    ...args: Array<Interpolatable>
  ) {
    const environment = projectDefaultEnvironment(this);
    if (environment == null) {
      throw new Error(
        `'.check' can only be called on projects with a default environment`
      );
    }
    return environment.check(s, ...args);
  },

  withDevTools<T extends Project>(this: T, devTools: Array<Package>): T {
    const environment = projectDefaultEnvironment(this);
    if (environment == null || this.settings.defaults?.environment == null) {
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
