import { Package } from "./base.ts";

type MkHaskellArgs = {
  name: string;
  executable: string;
  compiler: string;
  src: string;
};

export const mkHaskell = (args: MkHaskellArgs): Package => {
  return {
    tag: "package",
    nixExpression: `(pkgs.haskell.packages.${args.compiler}.callCabal2nix "${args.name}" ${args.src} { } ).override { } // { meta.mainProgram = "${args.executable}"; }`,
  };
};
