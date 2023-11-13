import "./internal/registerInternalLib.ts";

import { Check, mkCheck } from "./check.ts";
import { Environment } from "./environment.ts";
import { Executable, mkShellExecutable } from "./executable.ts";
import { hasTag } from "./internal/utils.ts";
import { NixStrLitInterpolatable } from "./nix.ts";
import { mkShellPackage, Package } from "./package.ts";
import { markAsMayNotExport } from "./internal/may_not_export.ts";

/**
 * A Project is a logical grouping of `Package`s and `Environment`s. For
 * example, you may have a 'frontend' Project, a 'backend' Project, a 'cli'
 * Project, etc.
 */
export type Project = ProjectHelpers & ProjectData;

export type ProjectData = {
  tag: "project";
  description: string;
  defaultEnvironment?: Environment;
  defaultExecutable?: Executable;
};

export type Plugin<Input, Output> = (project: Input) => Output;

type ProjectHelpers = {
  /**
   * Returns a new Project with the provided devtools added to the default
   * Environment.
   */
  withDevTools<T extends ProjectData>(this: T, devTools: Array<Package>): T;

  /**
   * Returns an executable that runs the given command inside the Project's
   * default Environment.
   *
   * Example:
   * ```typescript
   * const myExecutable = myProject.shell`echo "hello world"`;
   * ```
   */
  shell(this: ProjectData, script: string): Executable;
  shell(
    this: ProjectData,
    _s: TemplateStringsArray,
    ..._args: Array<NixStrLitInterpolatable>
  ): Executable;

  /**
   * Returns a check that runs in a *pure* version of the Project's default
   * Environment.
   */
  check(this: ProjectData, check: string): Check;
  check(
    this: ProjectData,
    _s: TemplateStringsArray,
    ..._args: Array<NixStrLitInterpolatable>
  ): Check;

  /**
   * Returns a buildable package that runs the given command inside the
   * Project's default environment.
   */
  build(this: Project, build: string): Package;
  build(
    this: ProjectData,
    _s: TemplateStringsArray,
    ..._args: Array<NixStrLitInterpolatable>
  ): Package;

  /**
   * Modify the given project.
   *
   * This can be useful for modifying a project in a method chaining style while
   * being able to reference that project. For example:
   *
   * ```typescript
   * export const myProject = garn.mkHaskellProject(...)
   *   .add(self => self.addExecutable("codegen")`${self.mainPackage}/bin/codegen`)
   * ```
   */
  add<Input extends ProjectData, Output>(
    this: Input,
    fn: Plugin<Input, Output>,
  ): Omit<Input, keyof Output> & Output;

  /**
   * Adds an `Executable` with the given name to the Project
   *
   * Example:
   * ```typescript
   * myProject.addExecutable("run-dev", "python run-dev.py")
   * ```
   */
  addExecutable<T extends ProjectData, Name extends string>(
    this: T,
    name: Name,
    executable: string,
  ): Omit<T, Name> & { [n in Name]: Executable };
  addExecutable<T extends ProjectData, Name extends string>(
    this: T,
    name: Name,
  ): (
    _s: TemplateStringsArray,
    ..._args: Array<NixStrLitInterpolatable>
  ) => Omit<T, Name> & { [n in Name]: Executable };

  /**
   * Adds a Check with the given name to the Project that runs in a *pure*
   * version of the Project's default Environment.
   *
   * Example:
   * ```typescript
   * myProject.addCheck("noTodos", "! grep -r TODO .")
   * ```
   */
  addCheck<T extends ProjectData, Name extends string>(
    this: T,
    name: Name,
    check: string,
  ): Omit<T, Name> & { [n in Name]: Check };
  addCheck<T extends ProjectData, Name extends string>(
    this: T,
    name: Name,
  ): (
    _s: TemplateStringsArray,
    ..._args: Array<NixStrLitInterpolatable>
  ) => Omit<T, Name> & { [n in Name]: Check };

  /**
   * Adds a `Package` with the given name to the Project.
   *
   * Example:
   * ```typescript
   * myProject.addPackage("bundle", "npm run build && mv dist/* $out")
   * ```
   */
  addPackage<T extends ProjectData, Name extends string>(
    this: T,
    name: Name,
    pkg: string,
  ): Omit<T, Name> & { [n in Name]: Package };
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
  deps: Deps,
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
    this: ProjectData,
    s: TemplateStringsArray | string,
    ...args: Array<NixStrLitInterpolatable>
  ) {
    const { defaultEnvironment } = this;
    if (defaultEnvironment == null) {
      throw new Error(
        `'.shell' can only be called on projects with a default environment`,
      );
    }
    return mkShellExecutable(defaultEnvironment, s, ...args);
  },

  check(
    this: ProjectData,
    s: TemplateStringsArray | string,
    ...args: Array<NixStrLitInterpolatable>
  ) {
    const { defaultEnvironment } = this;
    if (defaultEnvironment == null) {
      throw new Error(
        `'.check' can only be called on projects with a default environment`,
      );
    }
    return mkCheck(defaultEnvironment, s, ...args);
  },

  build(
    this: ProjectData,
    s: TemplateStringsArray | string,
    ...args: Array<NixStrLitInterpolatable>
  ) {
    const { defaultEnvironment } = this;
    if (defaultEnvironment == null) {
      throw new Error(
        `'.build' can only be called on projects with a default environment`,
      );
    }
    return mkShellPackage(defaultEnvironment, s, ...args);
  },

  add<Input extends ProjectData, Output>(
    this: Input,
    fn: Plugin<Input, Output>,
  ): Omit<Input, keyof Output> & Output {
    return { ...this, ...fn(this) };
  },

  addExecutable<T extends ProjectData, Name extends string>(
    this: T,
    name: Name,
    script?: string,
  ) {
    const { defaultEnvironment } = this;
    if (defaultEnvironment == null) {
      throw new Error(
        `'.addExecutable' can only be called on projects with a default environment`,
      );
    }
    if (script != null) {
      return {
        ...this,
        [name]: mkShellExecutable(defaultEnvironment, script),
      };
    }
    const templateLiteralFn = (
      s: TemplateStringsArray,
      ...args: Array<NixStrLitInterpolatable>
    ) => {
      return {
        ...this,
        [name]: mkShellExecutable(defaultEnvironment, s, ...args),
      };
    };
    markAsMayNotExport(templateLiteralFn, (exportName: string) =>
      [
        `${exportName} exports the return type of "addExecutable", but this is not the proper usage of addExecutable.`,
        'Did you forget the template literal? Example usage: project.addExecutable("executable-name")`shell script to run`',
      ].join(" "),
    );
    return templateLiteralFn;
  },

  addCheck<T extends ProjectData, Name extends string>(
    this: T,
    name: Name,
    check?: string,
  ) {
    const { defaultEnvironment } = this;
    if (defaultEnvironment == null) {
      throw new Error(
        `'.addCheck' can only be called on projects with a default environment`,
      );
    }
    if (check != null) {
      return {
        ...this,
        [name]: mkCheck(defaultEnvironment, check),
      };
    }
    const templateLiteralFn = (
      s: TemplateStringsArray,
      ...args: Array<NixStrLitInterpolatable>
    ) => {
      return {
        ...this,
        [name]: mkCheck(defaultEnvironment, s, ...args),
      };
    };
    markAsMayNotExport(templateLiteralFn, (exportName: string) =>
      [
        `${exportName} exports the return type of "addCheck", but this is not the proper usage of addCheck.`,
        'Did you forget the template literal? Example usage: project.addCheck("check-name")`shell script to run`',
      ].join(" "),
    );
    return templateLiteralFn;
  },

  addPackage<T extends ProjectData, Name extends string>(
    this: T,
    name: Name,
    pkg: string,
  ) {
    const { defaultEnvironment } = this;
    if (defaultEnvironment == null) {
      throw new Error(
        `'.addPackage' can only be called on projects with a default environment`,
      );
    }
    const build = mkShellPackage(defaultEnvironment, pkg);
    return {
      ...this,
      ...({ [name]: build } as { [n in Name]: Package }),
    };
  },

  withDevTools<T extends ProjectData>(this: T, devTools: Array<Package>): T {
    if (this.defaultEnvironment == null) {
      throw new Error(
        `'.withDevTools' can only be called on projects with a default environment`,
      );
    }
    const newEnvironment = this.defaultEnvironment.withDevTools(devTools);
    return {
      ...this,
      defaultEnvironment: newEnvironment,
    };
  },
});
