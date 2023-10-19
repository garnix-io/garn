import { hasTag } from "./internal/utils.ts";
import { NixExpression } from "./nix.ts";

export type Package = {
  tag: "package";
  nixExpression: NixExpression;
  description?: string;

  // disableCheck(this: Package): Package;
};

export function isPackage(x: unknown): x is Package {
  return hasTag(x, "package");
}

export function mkPackage(
  nixExpression: NixExpression,
  description?: string
): Package {
  return {
    tag: "package",
    nixExpression,
    description,
  };
}
