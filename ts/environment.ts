import { Check, mkCheck } from "./check.ts";
import { Executable, mkShellExecutable } from "./executable.ts";
import { hasTag, nixSource } from "./internal/utils.ts";
import {
  NixStrLitInterpolatable,
  NixExpression,
  nixList,
  nixRaw,
  nixStrLit,
  joinNixStrings,
} from "./nix.ts";
import { Package, mkShellPackage } from "./package.ts";

/**
 * `Environment`s define what files and tools are available to `Executables`,
 * `Check`s and development shells.
 *
 * For example they can contain compilers and developer tools that you want to
 * use on a project, but they may also define other things like environment
 * variables that need to be set.
 *
 * You can enter an `Environment` with `garn enter`.
 */
export type Environment = {
  tag: "environment";
  nixExpression: NixExpression;
  setup: Array<{
    type: "common" | "sandbox";
    snippet: NixExpression;
  }>;
  description?: string;

  /**
   * Update the description for this `Environment`
   */
  setDescription: (this: Environment, newDescription: string) => Environment;
  /**
   * Creates a new environment based on this one that includes the specified nix
   * packages.
   */
  withDevTools(devTools: Array<Package>): Environment;
  /**
   * Creates a new shell script `Executable`, run inside this `Environment`
   */
  shell(script: string): Executable;
  shell(
    _s: TemplateStringsArray,
    ..._args: Array<NixStrLitInterpolatable>
  ): Executable;
  /**
   * Creates a new shell script `Check`, run inside this `Environment`
   */
  check(check: string): Check;
  check(
    _s: TemplateStringsArray,
    ..._args: Array<NixStrLitInterpolatable>
  ): Check;
  /**
   * Creates a new `Package` built with the given shell script, run inside this `Environment`
   */
  build(build: string): Package;
  build(
    _s: TemplateStringsArray,
    ..._args: Array<NixStrLitInterpolatable>
  ): Package;
};

/**
 * Appends the given bash snippet to the setup of the `Environment`. The setup
 * will be executed when running things in an environment.
 *
 * If `type` is set to `"sandbox"`, it'll be executed as a setup step for
 * `Check`s and `Package`s, but *not* for `Executable`s and `garn enter`. If
 * `type` is set to `"common"`, the setup step is going to be executed in all 4
 * cases.
 *
 * (`Check`s and `Package`s are running in sandboxes to make them completely
 * deterministic. That's why they have to be treated differently here. The
 * most common example for how the `Environment` for these sandboxes works
 * differently is, that we have to *copy* the source files into the sandbox,
 * to allow `Check`s and `Package`s to access them. Whereas with `Executable`s
 * and `garn enter`, your source files are already available normally through
 * the file system.)
 */
export function addToSetup(
  type: "sandbox" | "common",
  env: Environment,
  snippet: string | NixExpression,
): Environment {
  return {
    ...env,
    setup: [
      ...env.setup,
      {
        type,
        snippet: typeof snippet === "string" ? nixStrLit(snippet) : snippet,
      },
    ],
  };
}

/**
 * Add the common setup of an `Environment` to a given script. This should be
 * used for `Executable`s and `garn enter`.
 */
export const commonScript = (
  env: Environment,
  script: NixExpression,
): NixExpression =>
  joinNixStrings("\n", [
    ...env.setup.filter((x) => x.type === "common").map((x) => x.snippet),
    script,
  ]);

/**
 * Add the sandbox setup of an `Environment` to a given script. This should be
 * used for `Package`s and `Check`s.
 */
export const sandboxScript = (
  env: Environment,
  script: NixExpression,
): NixExpression =>
  joinNixStrings("\n", [
    nixStrLit`mkdir -p $out`,
    ...env.setup.map((x) => x.snippet),
    script,
  ]);

/**
 * Creates a new shell script `Executable`, run in the `emptyEnvironment`.
 */
export function shell(script: string): Executable;
export function shell(
  s: TemplateStringsArray,
  ...args: Array<NixStrLitInterpolatable>
): Executable;
export function shell(
  s: TemplateStringsArray | string,
  ...args: Array<NixStrLitInterpolatable>
) {
  return mkShellExecutable(emptyEnvironment, s, ...args);
}

/**
 * Creates a new shell script `Check`, run in the `emptyEnvironment`.
 *
 * Example:
 * ```typescript
 * // Will fail if any `TODO`s are found in the build artifacts of myPkg:
 * garn.check`! grep -r TODO ${myPkg}`;
 * ```
 */
export function check(check: string): Check;
export function check(
  s: TemplateStringsArray,
  ...args: Array<NixStrLitInterpolatable>
): Check;
export function check(
  s: TemplateStringsArray | string,
  ...args: Array<NixStrLitInterpolatable>
) {
  return mkCheck(emptyEnvironment, s, ...args);
}

/**
 * Creates a new `Package`, which will be built using the given shell script.
 * It's run in the `emptyEnvironment`.
 *
 * The build script should copy any artifacts that you want to keep in the `Package`
 * into `$out`.
 *
 * Example:
 * ```typescript
 * import * as pkgs from "https://garn.io/ts/v0.0.19/nixpkgs.ts";
 *
 * garn.build`
 *   ${pkgs.cowsay}/bin/cowsay moo > $out/moo
 * `;
 * ```
 */
export function build(s: string): Package;
export function build(
  s: TemplateStringsArray,
  ...args: Array<NixStrLitInterpolatable>
): Package;
export function build(
  s: TemplateStringsArray | string,
  ...args: Array<NixStrLitInterpolatable>
): Package {
  return mkShellPackage(emptyEnvironment, s, ...args);
}

/**
 * A low-level helper to create new `Environment`s.
 *
 * @param nixExpression - A nix expression to use for this environment, defaults to `pkgs.mkShell {}`.
 *   All build inputs from the given expression will be available in the created environment.
 * @param src - An optional directory containing source files that are available in the environment.
 *   For `Package`s and `Check`s, all (non-gitignored) files are copied from the given `src` directory
 *   into the sandbox in which the package is built or the check is executed.
 * @param sandboxSetup - An optional shell script to set up the environment. This script will be run for
 *   every `Package` and `Check` before building the package or running the check.
 *
 * Example:
 *
 * ```typescript
 * // Create a new environment with the current working directory as `src`:
 * const myEnv = mkEnvironment({
 *   src: "."
 * });
 *
 * // Check that there's no TODOs in the source files:
 * const check = myEnv.check("! grep -ir TODO .");
 * ```
 */
export function mkEnvironment(
  args: {
    nixExpression?: NixExpression;
    src?: string;
  } = {},
): Environment {
  return {
    tag: "environment",
    nixExpression: args.nixExpression ?? nixRaw`pkgs.mkShell {}`,
    setup:
      args.src != null
        ? [
            {
              type: "sandbox",
              snippet: nixStrLit`
                echo copying source
                cp -r ${nixSource(args.src)}/. .
                chmod -R u+rwX .
              `,
            },
          ]
        : [],
    setDescription(this: Environment, newDescription: string): Environment {
      return {
        ...this,
        description: newDescription,
      };
    },
    check(
      this: Environment,
      s: TemplateStringsArray | string,
      ...args: Array<NixStrLitInterpolatable>
    ): Check {
      return mkCheck(this, s, ...args);
    },
    shell(
      this: Environment,
      s: TemplateStringsArray | string,
      ...args: Array<NixStrLitInterpolatable>
    ) {
      return mkShellExecutable(this, s, ...args);
    },
    build(
      this: Environment,
      s: TemplateStringsArray | string,
      ...args: Array<NixStrLitInterpolatable>
    ) {
      return mkShellPackage(this, s, ...args);
    },
    withDevTools(this, extraDevTools) {
      return {
        ...this,
        nixExpression: nixRaw`
        (${this.nixExpression}).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            ${nixList(
              extraDevTools.map((pkg) => nixRaw`(${pkg.nixExpression})`),
            )};
        })
      `,
      };
    },
  };
}

/**
 * The empty environment - this `Environment` has nothing installed in it. It
 * can be easily extended using `withDevTools`.
 *
 * For example:
 * ```typescript
 * import * as pkgs from "https://garn.io/ts/v0.0.19/nixpkgs.ts";
 *
 * // Create an environment with nothing but go and gopls installed:
 * emptyEnvironment.withDevTools([pkgs.go, pkgs.gopls])
 * ```
 */
export const emptyEnvironment: Environment = mkEnvironment();

export const isEnvironment = (e: unknown): e is Environment => {
  return hasTag(e, "environment");
};

export const packageToEnvironment = (pkg: Package, src: string): Environment =>
  mkEnvironment({
    nixExpression: nixRaw`
        let expr = ${pkg.nixExpression};
        in
          (if expr ? env
            then expr.env
            else pkgs.mkShell { inputsFrom = [ expr ]; }
          )
      `,
    src,
  });
