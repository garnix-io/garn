import { mkPackage, Package } from "./base.ts";

type MkHaskellArgs = {
  name: string;
  executable: string;
  compiler: string;
  src: string;
};

export const mkHaskell = (args: MkHaskellArgs): Package => {
  const expression = `(pkgs.haskell.packages.${args.compiler}.callCabal2nix "${args.name}" ${args.src} { } ) // { meta.mainProgram = "${args.executable}"; }`;
  return mkPackage({
    expression,
  });
};
