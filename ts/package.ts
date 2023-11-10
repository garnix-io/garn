import { hasTag } from "./internal/utils.ts";
import { NixExpression } from "./nix.ts";

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
  };
}
