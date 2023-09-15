import { mkPackage, Package } from "./base.ts";
import { nixSource } from "./utils.ts";

type MkHaskellArgs = {
  description: string;
  executable: string;
  compiler: string;
  src: string;
};

export const mkHaskell = (args: MkHaskellArgs): Package => {
  const expression = `
    (pkgs.haskell.packages.${args.compiler}.callCabal2nix
      "garner-pkg"
      ${nixSource(args.src)}
      { })
      // {
        meta.mainProgram = "${args.executable}";
      }
  `;
  return mkPackage({
    expression,
    description: args.description,
  });
};
