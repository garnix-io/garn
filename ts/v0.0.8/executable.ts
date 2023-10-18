import { hasTag } from "./internal/utils.ts";
import { NixExpression } from "./nix.ts";

export type Executable = {
  tag: "executable";
  description: string;
  nixExpression: NixExpression;
};

export const isExecutable = (e: unknown): e is Executable => {
  return hasTag(e, "executable");
};
