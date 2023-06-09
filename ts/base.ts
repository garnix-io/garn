import { Package } from "./haskell.ts";

export const mkDerivation = (args: { attribute: string }): Package => ({
  tag: "package",
  nixExpression: args.attribute,
});
