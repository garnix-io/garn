import { hasTag } from "./utils.ts";

export type NewPackage = {
  tag: "package";
  nixExpression: string;

  // disableCheck(this: Package): Package;
};

export const isNewPackage = (x: unknown): x is NewPackage =>
  hasTag(x, "package");

export const mkNewPackage = (nixExpression: string): NewPackage => ({
  tag: "package",
  nixExpression,
});
