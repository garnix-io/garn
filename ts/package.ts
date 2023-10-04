import { hasTag } from "./utils.ts";

export type Package = {
  tag: "package";
  nixExpression: string;

  // disableCheck(this: Package): Package;
};

export const isPackage = (x: unknown): x is Package => hasTag(x, "package");

export const mkPackage = (nixExpression: string): Package => ({
  tag: "package",
  nixExpression,
});
