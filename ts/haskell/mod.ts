import { packageToEnvironment } from "../environment.ts";
import { mkPackage, Package } from "../package.ts";
import { mkProject, Project } from "../project.ts";
import { Executable } from "../executable.ts";
import { nixSource } from "../internal/utils.ts";
import { nixRaw } from "../nix.ts";


export type HaskellAddenda = {
  /**
   * A package building the entire Haskell project
   */
  pkg: Package
  /**
   * Make an executable from the cabal file available to garn.
   */
  withCabalExecutable:<T extends Project & HaskellAddenda, Name extends string>(this: T, executableName : Name) =>
    Omit<T, Name> & { [n in Name] : Executable }
}
/**
 * Creates a haskell-based garn Project. You can
 *
 * @param description - A short description of the project
 * @param compiler - The compiler version (e.g.: "ghc94")
 * @param src - The source directory
 */
export function mkHaskellProject(args: {
  description: string;
  compiler: string;
  src: string;
}): Project & HaskellAddenda {
  const pkg: Package = mkPackage(
    nixRaw`
      (pkgs.haskell.packages.${nixRaw(args.compiler)}.callCabal2nix
        "garn-pkg"
        ${nixSource(args.src)}
        { })
    `,
    "main package",
  );

  const projectBase = mkProject(
    {
      description: args.description,
      defaultEnvironment: packageToEnvironment(pkg, args.src),
    },
    {
      pkg,
    },
  ).withDevTools([
    mkPackage(
      nixRaw`pkgs.haskell.packages.${nixRaw(args.compiler)}.cabal-install`,
      "cabal-install",
    ),
  ]);

  return {
    ...projectBase,
    withCabalExecutable: function<T extends Project & HaskellAddenda, Name extends string>(this: T, executableName: Name) {
        return this.addExecutable(executableName)`${this.pkg}/bin/${executableName}`
    }
  }
}
