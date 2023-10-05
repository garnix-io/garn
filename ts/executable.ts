import { assertEquals } from "https://deno.land/std@0.201.0/assert/mod.ts";
import { hasTag } from "./utils.ts";

export type Executable = {
  tag: "executable";
  description: string;
  nixExpression: string;
};

export const isExecutable = (e: unknown): e is Executable => {
  return hasTag(e, "executable");
};
