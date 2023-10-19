import { packageToEnvironment, shell } from "../environment.ts";
import { mkPackage, Package } from "../package.ts";
import { mkProject, Project } from "../project.ts";
import { nixSource } from "../internal/utils.ts";
import { nixRaw, nixStrLit } from "../nix.ts";

export const mkHaskellProject = (args: {
  description: string;
  executable: string;
  compiler: string;
  src: string;
}): Project & { pkg: Package } => {
  const pkg: Package = mkPackage(nixRaw`
    (pkgs.haskell.packages.${nixRaw(args.compiler)}.callCabal2nix
      "garn-pkg"
      ${nixSource(args.src)}
      { })
      // {
        meta.mainProgram = ${nixStrLit(args.executable)};
      }
  `);
  return mkProject(
    {
      description: args.description,
      defaultEnvironment: packageToEnvironment(pkg, args.src),
      defaultExecutable: shell`${pkg}/bin/${args.executable}`,
    },
    {
      pkg,
    }
  ).withDevTools([
    mkPackage(
      nixRaw`pkgs.haskell.packages.${nixRaw(args.compiler)}.cabal-install`
    ),
  ]);
};
