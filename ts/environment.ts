import { Package } from "./package.ts";
import { hasTag, nixSource } from "./internal/utils.ts";
import { Check } from "./check.ts";
import { Executable } from "./executable.ts";
import {
  NixStrLitInterpolatable,
  NixExpression,
  nixList,
  nixRaw,
  nixStrLit,
} from "./nix.ts";

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
  /**
   * Creates a new environment based on this one that includes the specified nix packages.
   */
  withDevTools(devTools: Array<Package>): Environment;
  /**
   * Creates a new shell script `Executable`, run inside this `Environment`
   */
  shell(
    _s: TemplateStringsArray,
    ..._args: Array<NixStrLitInterpolatable>
  ): Executable;
  /**
   * Creates a new shell script `Check`, run inside this `Environment`
   */
  check(
    _s: TemplateStringsArray,
    ..._args: Array<NixStrLitInterpolatable>
  ): Check;
};

/**
 * Creates a new shell script `Executable`, run in the `emptyEnvironment`.
 */
export function shell(
  s: TemplateStringsArray,
  ...args: Array<NixStrLitInterpolatable>
) {
  return emptyEnvironment.shell(s, ...args);
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
export function check(
  s: TemplateStringsArray,
  ...args: Array<NixStrLitInterpolatable>
) {
  return emptyEnvironment.check(s, ...args);
}

/**
 * A low-level helper to create new `Environment`s from `NixExpression`s.
 *
 * @param nixExpression - A nix expression to use for this environment defaults to `pkgs.mkShell {}`
 * @param setup - An optional shell script to set up the environment.
 *
 * Example:
 * ```typescript
 * // Create a new environment where the current working directory is available under `src`:
 * const myEnv = mkEnvironment(
 *   nixRaw`pkgs.mkShell {}`,
 *   nixStrLit`
 *     cp -r ${nixRaw`./.`} src
 *   `,
 * );
 * ```
 */
export function mkEnvironment(
  nixExpression = nixRaw`pkgs.mkShell {}`,
  setup?: NixExpression
): Environment {
  return {
    tag: "environment",
    nixExpression,
    check(this, s, ...args): Check {
      const checkScript = nixStrLit(s, ...args);
      const wrappedScript = nixStrLit`
      touch $out
      ${setup || ""}
      ${checkScript}
    `;
      return {
        tag: "check",
        nixExpression: nixRaw`
        let
            dev = ${this.nixExpression};
        in
        pkgs.runCommand "check" {
          buildInputs = dev.buildInputs ++ dev.nativeBuildInputs;
        } ${wrappedScript}
      `,
      };
    },
    shell(this, s, ...args) {
      const cmdToExecute = nixStrLit(s, ...args);
      const shellEnv = nixRaw`
      let
        dev = ${this.nixExpression};
        shell = ${cmdToExecute};
        buildPath = pkgs.runCommand "build-inputs-path" {
          inherit (dev) buildInputs nativeBuildInputs;
        } "echo $PATH > $out";
      in
      pkgs.writeScript "shell-env"  ''
        #!\${pkgs.bash}/bin/bash
        export PATH=$(cat \${buildPath}):$PATH
        \${dev.shellHook}
        \${shell} "$@"
      ''
    `;
      return {
        tag: "executable",
        description: `Executes ${cmdToExecute.rawNixExpressionString}`,
        nixExpression: nixStrLit`${shellEnv}`,
      };
    },
    withDevTools(this, extraDevTools) {
      return {
        ...this,
        nixExpression: nixRaw`
        (${this.nixExpression}).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            ${nixList(extraDevTools.map((pkg) => pkg.nixExpression))};
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
 * // Create an environment with nothing but go and gopls installed:
 * emptyEnvironment.withDevTools([pkgs.go, pkgs.gopls])
 * ```
 */
export const emptyEnvironment: Environment = mkEnvironment();

export const isEnvironment = (e: unknown): e is Environment => {
  return hasTag(e, "environment");
};

export const packageToEnvironment = (pkg: Package, src: string): Environment =>
  mkEnvironment(
    nixRaw`
    let expr = ${pkg.nixExpression};
    in
      (if expr ? env
        then expr.env
        else pkgs.mkShell { inputsFrom = [ expr ]; }
      )
    `,
    nixStrLit`
      echo copying source
      cp -r ${nixSource(src)} src
      chmod -R u+rwX src
      cd src
    `
  );
