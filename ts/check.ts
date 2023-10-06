import { hasTag } from "./utils.ts";

export type Check = {
  tag: "check";
  nixExpression: string;
};

export const isCheck = (x: unknown): x is Check => hasTag(x, "check");
