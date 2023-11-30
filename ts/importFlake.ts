import { Check } from "./check.ts";
import { Environment, mkEnvironment } from "./environment.ts";
import { Executable, mkExecutable } from "./executable.ts";
import {
  getPathOrError,
  NixExpression,
  nixFlakeDep,
  nixRaw,
  nixStrLit,
} from "./nix.ts";
import { mkPackage, Package } from "./package.ts";

function getFlake(flakeUrl: string): NixExpression {
  if (flakeUrl.match(/^[.][.]?[/]/)) {
    const callFlake = nixFlakeDep("call-flake", {
      url: "github:divnix/call-flake",
    });
    return nixRaw`(${callFlake} ${nixRaw(flakeUrl)})`;
  }
  return nixFlakeDep(flakeUrl.replaceAll(/[^a-zA-Z0-9]/g, "-"), {
    url: flakeUrl,
  });
}

export function importFlake(flakeUrl: string): {
  allChecks: Check;
  allPackages: Package;
  getApp: (appName: string) => Executable;
  getCheck: (appName: string) => Check;
  getDevShell: (devShellName: string) => Environment;
  getPackage: (packageName: string) => Package;
} {
  const flake = getFlake(flakeUrl);

  return {
    allChecks: {
      tag: "check",
      nixExpression: nixRaw`pkgs.linkFarm "all-checks" ${flake}.checks.\${system}`,
    },

    allPackages: mkPackage(
      nixRaw`pkgs.linkFarm "all-packages" ${flake}.packages.\${system}`,
      `All packages from flake.nix in ${flakeUrl}`,
    ),

    getApp: (appName) =>
      mkExecutable(
        getPathOrError(
          flake,
          ["apps", nixRaw`\${system}`, appName, "program"],
          nixStrLit`The app "${appName}" was not found in ${flakeUrl}`,
        ),
        `Execute ${appName} from flake.nix in ${flakeUrl}`,
      ),

    getCheck: (checkName) => ({
      tag: "check",
      nixExpression: getPathOrError(
        flake,
        ["checks", nixRaw`\${system}`, checkName],
        nixStrLit`The check "${checkName}" was not found in ${flakeUrl}`,
      ),
    }),

    getDevShell: (devShellName) =>
      mkEnvironment({
        nixExpression: getPathOrError(
          flake,
          ["devShells", nixRaw`\${system}`, devShellName],
          nixStrLit`The devShell "${devShellName}" was not found in ${flakeUrl}`,
        ),
      }),

    getPackage: (packageName) =>
      mkPackage(
        getPathOrError(
          flake,
          ["packages", nixRaw`\${system}`, packageName],
          nixStrLit`The package "${packageName}" was not found in ${flakeUrl}`,
        ),
        `${packageName} from flake.nix in ${flakeUrl}`,
      ),
  };
}
