import { hasTag } from "./internal/utils.ts";
import {
  NixExpression,
  nixRaw,
  nixStrLit,
  NixStrLitInterpolatable,
  toHumanReadable,
} from "./nix.ts";
import { Environment } from "./environment.ts";

/**
 * `Executable`s are commands (usually shell snippets) that are being run on
 * your local machine (not in a sandbox).
 *
 * They are based on an underlying `Environment`, and can make use of e.g. the
 * executables that that `Environment` provides.
 *
 * You can use `Executables` for a variety of purposes:
 *   - Running your projects main executable during development,
 *   - Running a code formatter,
 *   - Running a code generator,
 *   - Spinning up a database for development,
 *   - etc.
 *
 * You can run `Executable`s with `garn run`.
 */
export type Executable = {
  tag: "executable";
  nixExpression: NixExpression;

  /**
   * The executable description is rendered in the executable list when running `garn run`.
   */
  description: string;
};

export function isExecutable(e: unknown): e is Executable {
  return hasTag(e, "executable");
}

/**
 * Create an executable from a nix expression.
 *
 * This is a fairly low-level function. You may want to use `garn.shell`.
 */
export function mkExecutable(
  nixExpression: NixExpression,
  description: string,
): Executable {
  return {
    tag: "executable",
    nixExpression,
    description,
  };
}

export function mkShellExecutable(
  env: Environment,
  s: TemplateStringsArray | string,
  ...args: Array<NixStrLitInterpolatable>
): Executable {
  const cmdToExecute =
    typeof s === "string" ? nixStrLit(s) : nixStrLit(s, ...args);
  const shellEnv = nixRaw`
      let
        dev = ${env.nixExpression};
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
  return mkExecutable(
    nixStrLit`${shellEnv}`,
    `Executes ${toHumanReadable(cmdToExecute)}`,
  );
}
