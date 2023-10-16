import { hasTag } from "./internal/utils.ts";
import { NixExpression } from "./nix.ts";

export type Package = {
  tag: "package";
  nixExpression: NixExpression;
  description?: string;

  // disableCheck(this: Package): Package;
};

export const isPackage = (x: unknown): x is Package => hasTag(x, "package");

export const mkPackage = (
  nixExpression: NixExpression,
  description?: string
): Package => ({
  tag: "package",
  nixExpression,
  description,
});
