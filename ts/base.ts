import { Package } from "./haskell.ts";

export const mkPackage = (args: { attribute: string }): Package => ({
  tag: "package",
  nixExpression: args.attribute,
});
