import { hasTag } from "./internal/utils.ts";
import { NixExpression } from "./nix.ts";

export type Check = {
  tag: "check";
  nixExpression: NixExpression;
};

export function isCheck(x: unknown): x is Check {
  return hasTag(x, "check");
}
