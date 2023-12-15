import { Environment, wrapSandboxedScript } from "./environment.ts";
import { hasTag } from "./internal/utils.ts";
import {
  NixExpression,
  NixStrLitInterpolatable,
  nixRaw,
  nixStrLit,
} from "./nix.ts";

/**
 * `Check`s are commands (usually shell commands) that are used to define
 * reproducible automated tests or other checks.
 *
 * `Check`s are run in a sandbox. This means they don't have access to your
 * locally installed tools, local file system, or the internet. Instead, they
 * are always based on an underlying `Environment`, and through that will have
 * access to the tools and everything else that the environment provides.
 *
 * You can run `Check`s with `garn check`.
 */
export type Check = {
  tag: "check";
  nixExpression: NixExpression;
};

export function mkCheck(
  env: Environment,
  s: TemplateStringsArray | string,
  ...args: Array<NixStrLitInterpolatable>
): Check {
  const checkScript =
    typeof s === "string" ? nixStrLit(s) : nixStrLit(s, ...args);
  const wrappedScript = wrapSandboxedScript(env, checkScript);
  return {
    tag: "check",
    nixExpression: nixRaw`
      let
          dev = ${env.nixExpression};
      in
      pkgs.runCommand "check" {
        buildInputs = dev.buildInputs ++ dev.nativeBuildInputs;
      } ${wrappedScript}
    `,
  };
}

export function isCheck(x: unknown): x is Check {
  return hasTag(x, "check");
}
