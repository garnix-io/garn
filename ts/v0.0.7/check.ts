import { hasTag } from "./internal/utils.ts";
import { NixExpression } from "./nix.ts";

export type Check = {
  tag: "check";
  nixExpression: NixExpression;
};

export const isCheck = (x: unknown): x is Check => hasTag(x, "check");
