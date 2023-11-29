import { Check } from "./check.ts";
import { Environment, mkEnvironment } from "./environment.ts";
import { Executable, mkExecutable } from "./executable.ts";
import { NixExpression, nixFlakeDep, nixRaw, nixStrLit } from "./nix.ts";
import { mkPackage, Package } from "./package.ts";

function getFlake(flakeDir: string): NixExpression {
  if (flakeDir.match(/^[.][.]?[/]/)) {
    const callFlake = nixFlakeDep("call-flake", {
      url: "github:divnix/call-flake",
    });
    return nixRaw`(${callFlake} ${nixRaw(flakeDir)})`;
  }
  return nixFlakeDep(flakeDir.replaceAll(/[^a-zA-Z0-9]/g, "-"), {
    url: flakeDir,
  });
}

export function importFlake(flakeDir: string): {
  allChecks: Check;
  allPackages: Package;
  getApp: (appName: string) => Executable;
  getCheck: (appName: string) => Check;
  getDevShell: (devShellName: string) => Environment;
  getPackage: (packageName: string) => Package;
} {
  const flake = getFlake(flakeDir);
  return {
    allChecks: {
      tag: "check",
      nixExpression: nixRaw`pkgs.linkFarm "all-checks" ${flake}.checks.\${system}`,
    },

    allPackages: mkPackage(
      nixRaw`pkgs.linkFarm "all-packages" ${flake}.packages.\${system}`,
      `All packages from flake.nix in ${flakeDir}`,
    ),

    getApp: (appName) =>
      mkExecutable(
        nixRaw`${flake}.apps.\${system}.${nixStrLit(appName)}.program`,
        `Execute ${appName} from flake.nix in ${flakeDir}`,
      ),

    getCheck: (checkName) => ({
      tag: "check",
      nixExpression: nixRaw`${flake}.checks.\${system}.${nixStrLit(checkName)}`,
    }),

    getDevShell: (devShellName) =>
      mkEnvironment({
        nixExpression: nixRaw`${flake}.devShells.\${system}.${nixStrLit(
          devShellName,
        )}`,
      }),

    getPackage: (packageName) =>
      mkPackage(
        nixRaw`${flake}.packages.\${system}.${nixStrLit(packageName)}`,
        `${packageName} from flake.nix in ${flakeDir}`,
      ),
  };
}
