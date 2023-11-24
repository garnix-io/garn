import { Environment, sandboxScript } from "./environment.ts";
import { hasTag } from "./internal/utils.ts";
import {
  NixExpression,
  NixStrLitInterpolatable,
  nixRaw,
  nixStrLit,
  toHumanReadable,
} from "./nix.ts";

/**
 * `Package`s are instructions to `garn` about how to _build_ a set of files.
 *
 * For example for a go backend a package would define how to compile it into
 * executables. For an npm project it may define how the project can be bundled
 * into a set of files that can be served by a webserver.
 *
 * You can build `Package`s with `garn build`.
 */
export type Package = {
  tag: "package";
  nixExpression: NixExpression;
  description: string;

  /**
   * Update the description for this `Package`
   */
  setDescription: (this: Package, newDescription: string) => Package;
};

export function isPackage(x: unknown): x is Package {
  return hasTag(x, "package");
}

/**
 * A low-level helper to create new `Package`s from `NixExpression`s.
 */
export function mkPackage(
  nixExpression: NixExpression,
  description: string,
): Package {
  return {
    tag: "package",
    nixExpression,
    description,

    setDescription(this: Package, newDescription: string): Package {
      return {
        ...this,
        description: newDescription,
      };
    },
  };
}

export function mkShellPackage(
  env: Environment,
  s: TemplateStringsArray | string,
  ...args: Array<NixStrLitInterpolatable>
) {
  const cmdToExecute =
    typeof s === "string" ? nixStrLit(s) : nixStrLit(s, ...args);
  const wrappedScript = sandboxScript(env, cmdToExecute);
  const pkg = nixRaw`
    let dev = ${env.nixExpression}; in
    pkgs.runCommand "garn-pkg" {
      buildInputs = dev.buildInputs ++ dev.nativeBuildInputs;
    } ${wrappedScript}
  `;
  return mkPackage(pkg, `Builds ${toHumanReadable(cmdToExecute)}`);
}
