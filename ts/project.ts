import "./internal/registerInternalLib.ts";

import { Check } from "./check.ts";
import { Environment } from "./environment.ts";
import { Executable } from "./executable.ts";
import { hasTag } from "./internal/utils.ts";
import { NixStrLitInterpolatable } from "./nix.ts";
import { Package } from "./package.ts";
import { markAsMayNotExport } from "./internal/may_not_export.ts";

/**
 * A Project is a logical grouping of `Package`s and `Environment`s. For
 * example, you may have a 'frontend' Project, a 'backend' Project, a 'cli'
 * Project, etc.
 */
export type Project = ProjectHelpers & {
  tag: "project";
  description: string;
  defaultEnvironment?: Environment;
  defaultExecutable?: Executable;
};

type ProjectHelpers = {
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

  build(
    this: Project,
    _s: TemplateStringsArray,
    ..._args: Array<string>
  ): Package;

  /**
   * Adds an `Executable` with the given name to the Project
   *
   * Example:
   * ```typescript
   * myProject.addExecutable("run-dev")`python run-dev.py`
   * ```
   */
  addExecutable<T extends Project, Name extends string>(
    this: T,
    name: Name
  ): (
    _s: TemplateStringsArray,
    ..._args: Array<string>
  ) => T & { [n in Name]: Executable };

  /**
   * Adds a Check with the given name to the Project that runs in a *pure*
   * version of the Project's default Environment.
   *
   * Example:
   * ```typescript
   * myProject.addCheck("noTodos")`! grep -r TODO .`
   * ```
   */
  addCheck<T extends Project, Name extends string>(
    this: T,
    name: Name
  ): (
    _s: TemplateStringsArray,
    ..._args: Array<string>
  ) => T & { [n in Name]: Check };
};

export function isProject(p: unknown): p is Project {
  return hasTag(p, "project");
}

type Nestable = Environment | Package | Executable | Check | Project;

/**
 * Creates a new `Project`.
 *
 * @param settings description and defaults for this `Project`.
 * @param deps A record of Environments, Packages and Executables to include in this `Project`.
 */
export function mkProject<Deps extends Record<string, Nestable>>(
  settings: {
    description: string;
    defaultEnvironment?: Environment;
    defaultExecutable?: Executable;
  },
  deps: Deps
): Deps & Project {
  const helpers = proxyEnvironmentHelpers();
  return {
    ...deps,
    ...helpers,
    tag: "project",
    description: settings.description,
    defaultEnvironment: settings.defaultEnvironment,
    defaultExecutable: settings.defaultExecutable,
  };
}

const proxyEnvironmentHelpers = (): ProjectHelpers => ({
  shell(
    this: Project,
    s: TemplateStringsArray,
    ...args: Array<NixStrLitInterpolatable>
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
    ...args: Array<NixStrLitInterpolatable>
  ) {
    if (this.defaultEnvironment == null) {
      throw new Error(
        `'.check' can only be called on projects with a default environment`
      );
    }
    return this.defaultEnvironment.check(s, ...args);
  },

  build(
    this: Project,
    s: TemplateStringsArray,
    ...args: Array<NixStrLitInterpolatable>
  ) {
    if (this.defaultEnvironment == null) {
      throw new Error(
        `'.build' can only be called on projects with a default environment`
      );
    }
    return this.defaultEnvironment.build(s, ...args);
  },

  addExecutable<T extends Project, Name extends string>(this: T, name: Name) {
    if (this.defaultEnvironment == null) {
      throw new Error(
        `'.addExecutable' can only be called on projects with a default environment`
      );
    }
    const templateLiteralFn = (
      s: TemplateStringsArray,
      ...args: Array<string>
    ) => {
      const newExecutable = { [name]: this.shell(s, ...args) } as {
        [n in Name]: Executable;
      };
      return {
        ...this,
        ...newExecutable,
      };
    };
    markAsMayNotExport(templateLiteralFn, (exportName: string) =>
      [
        `${exportName} exports the return type of "addExecutable", but this is not the proper usage of addExecutable.`,
        'Did you forget the template literal? Example usage: project.addExecutable("executable-name")`shell script to run`',
      ].join(" ")
    );
    return templateLiteralFn;
  },

  addCheck<T extends Project, Name extends string>(this: T, name: Name) {
    if (this.defaultEnvironment == null) {
      throw new Error(
        `'.addCheck' can only be called on projects with a default environment`
      );
    }
    const templateLiteralFn = (
      s: TemplateStringsArray,
      ...args: Array<string>
    ) => {
      const newCheck = { [name]: this.check(s, ...args) } as {
        [n in Name]: Check;
      };
      return {
        ...this,
        ...newCheck,
      };
    };
    markAsMayNotExport(templateLiteralFn, (exportName: string) =>
      [
        `${exportName} exports the return type of "addCheck", but this is not the proper usage of addCheck.`,
        'Did you forget the template literal? Example usage: project.addCheck("check-name")`shell script to run`',
      ].join(" ")
    );
    return templateLiteralFn;
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
