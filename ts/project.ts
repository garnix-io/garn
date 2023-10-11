import "./internal/registerInternalLib.ts";

import { Check } from "./check.ts";
import { Environment } from "./environment.ts";
import { Executable } from "./executable.ts";
import { hasTag } from "./internal/utils.ts";
import { Interpolatable } from "./nix.ts";
import { Package } from "./package.ts";

/**
 * A Project is a logical grouping of Packages and Environments. For example,
 * you may have a 'frontend' Project, a 'backend' Project, a 'cli' Project, etc.
 */
export type Project = {
  tag: "project";
  description: string;
  defaultEnvironment?: Environment;
  defaultExecutable?: Executable;
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

type ProjectSettings = {
  description: string;
  defaultEnvironment?: Environment;
  defaultExecutable?: Executable;
};

/**
 * Create a new Project.
 *
 * @param description A human-readable description of the Project.
 * @param deps A record of Environments, Packages and Executables,
 * @param settings Settings such as defaults for Environments and Executables.
 */
export function mkProject<Deps extends Record<string, Nestable>>(
  args: ProjectSettings,
  deps: Deps
): Deps & Project {
  const helpers = proxyEnvironmentHelpers();
  return {
    ...deps,
    ...helpers,
    tag: "project",
    description: args.description,
    defaultEnvironment: args.defaultEnvironment,
    defaultExecutable: args.defaultExecutable,
  };
}

const proxyEnvironmentHelpers = () => ({
  shell(
    this: Project,
    s: TemplateStringsArray,
    ...args: Array<Interpolatable>
  ) {
    if (this.defaultEnvironment == null) {
      throw new Error(
        `'.shell' can only be called on projects with a default environment`
      );
    }
    return this.defaultEnvironment.shell(s, ...args);
  },

  check(
    this: Project,
    s: TemplateStringsArray,
    ...args: Array<Interpolatable>
  ) {
    if (this.defaultEnvironment == null) {
      throw new Error(
        `'.check' can only be called on projects with a default environment`
      );
    }
    return this.defaultEnvironment.check(s, ...args);
  },

  withDevTools<T extends Project>(this: T, devTools: Array<Package>): T {
    if (this.defaultEnvironment == null) {
      throw new Error(
        `'.withDevTools' can only be called on projects with a default environment`
      );
    }
    const newEnvironment = this.defaultEnvironment.withDevTools(devTools);
    return {
      ...this,
      defaultEnvironment: newEnvironment,
    };
  },
});
