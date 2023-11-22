import { packageToEnvironment } from "../environment.ts";
import { mkPackage, Package } from "../package.ts";
import { mkProject, Project } from "../project.ts";
import { Executable } from "../executable.ts";
import { nixSource } from "../internal/utils.ts";
import { nixRaw } from "../nix.ts";

/**
 * Project components returned by `mkHaskellProject`
 */
export type HaskellAddenda = {
  /**
   * A package building the entire Haskell project
   */
  pkg: Package;
  /**
   * Make an executable from the cabal file available to garn.
   */
  addCabalExecutable: <T extends Project & HaskellAddenda, Name extends string>(
    this: T,
    executableName: Name,
  ) => Omit<T, Name> & { [n in Name]: Executable };
};

type Execs<Exe extends string[]> = {
  [e in Exe[number]]: Executable;
};

/**
 * Creates a haskell-based garn Project.
 *
 * This will create a Project with
 *   - a `pkg` key that builds the entire haskell project (i.e., `cabal build`);
 *   - a default shell that provides cabal as well as all Haskell dependencies;
 *   - one garn executable for each cabal executable in `executables`.
 *
 * @param description - A short description of the project
 * @param compiler - The compiler version (e.g.: "ghc94")
 * @param executables - The name of the executables in the cabal file
 * @param src - The source directory
 */
export function mkHaskellProject<const Executables extends string[]>(args: {
  description: string;
  compiler: string;
  executables?: Executables;
  src: string;
}): Project & HaskellAddenda & Execs<Executables> {
  const pkg: Package = mkPackage(
    nixRaw`
      (pkgs.haskell.packages.${nixRaw(args.compiler)}.callCabal2nix
        "garn-pkg"
        ${nixSource(args.src)}
        { })
    `,
    "main package",
  );

  const defaultEnvironment = packageToEnvironment(pkg, args.src);

  const execs = args.executables
    ? args.executables.reduce((prev, cur) => {
        return { ...prev, [cur]: defaultEnvironment.shell`${pkg}/bin/${cur}` };
      }, {} as Execs<Executables>)
    : ({} as Execs<Executables>);

  const projectBase = mkProject(
    {
      description: args.description,
      defaultEnvironment: defaultEnvironment,
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
    addCabalExecutable: function <
      T extends Project & HaskellAddenda,
      Name extends string,
    >(this: T, executableName: Name) {
      return this.addExecutable(
        executableName,
      )`${this.pkg}/bin/${executableName}`;
    },
    ...execs,
  };
}
