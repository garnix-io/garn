
type MkHaskellArgs = {
  name: string,
  executable: string,
  compiler: string,
  src: string
}

export type Package = {
  tag: "package",
  nixExpression: string
}

export const mkHaskell = (args : MkHaskellArgs) : Package => {
   return {
      tag: "package",
      nixExpression: `(pkgs.haskell.packages.${args.compiler}.callCabal2nix "${args.name}" ${args.src} {} ) // { meta.mainProgram = "${args.executable}"; }`
   }

}
