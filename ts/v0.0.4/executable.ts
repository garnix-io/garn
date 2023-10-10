import { hasTag } from "./utils.ts";

export type Executable = {
  tag: "executable";
  description: string;
  nixExpression: string;
};

export const isExecutable = (e: unknown): e is Executable => {
  return hasTag(e, "executable");
};
