import { Environment, mkEnvironment } from "../environment.ts";
import { Executable } from "../executable.ts";
import { Package, mkPackage } from "../package.ts";
import { mkProject, Project } from "../project.ts";
import { nixSource } from "../internal/utils.ts";
import { NixExpression, nixList, nixRaw, nixStrLit } from "../nix.ts";

const nodeVersions = {
  "14": {
    pkg: nixRaw`pkgs.nodejs-14_x`,
    permittedInsecurePackages: nixList([
      nixStrLit`nodejs-14.21.3`,
      nixStrLit`openssl-1.1.1v`,
    ]),
  },
  "16": {
    pkg: nixRaw`pkgs.nodejs-16_x`,
    permittedInsecurePackages: nixList([nixStrLit`nodejs-16.20.2`]),
  },
  "18": {
    pkg: nixRaw`pkgs.nodejs-18_x`,
    permittedInsecurePackages: nixList([]),
  },
};

type NodeVersion = keyof typeof nodeVersions;

const fromNodeVersion = (
  version: NodeVersion
): { pkgs: NixExpression; nodejs: NixExpression } => {
  const { pkg, permittedInsecurePackages } = nodeVersions[version];
  return {
    pkgs: nixRaw`
      import "\${nixpkgs}" {
        config.permittedInsecurePackages = ${permittedInsecurePackages};
        inherit system;
      }
    `,
    nodejs: pkg,
  };
};

/**
 * Creates a npm-based garn Project.
 */
export function mkNpmProject(args: {
  description: string;
  src: string;
  nodeVersion: NodeVersion;
}): Project & {
  devShell: Environment;
  node_modules: Package;
} {
  const { pkgs, nodejs } = fromNodeVersion(args.nodeVersion);
  const node_modules = mkPackage(nixRaw`
    let
      npmlock2nix = import npmlock2nix-repo {
        inherit pkgs;
      };
      pkgs = ${pkgs};
    in
    npmlock2nix.v2.node_modules
      {
        src = ${nixSource(args.src)};
        nodejs = ${nodejs};
      }
  `);
  const devShell: Environment = mkEnvironment(
    undefined,
    nixStrLit`
      echo copying source
      cp -r ${nixSource(args.src)} src
      chmod -R u+rwX src
      cd src
      echo copying node_modules
      cp -r ${node_modules}/node_modules .
    `
  ).withDevTools([mkPackage(nodejs)]);
  return mkProject(
    {
      description: args.description,
      defaultEnvironment: devShell,
    },
    {
      devShell,
      node_modules,
    }
  );
}

/**
 * Creates a yarn-based garn Project.
 */
export function mkYarnProject(args: {
  description: string;
  src: string;
  nodeVersion: keyof typeof nodeVersions;
  startCommand?: string;
  testCommand?: string;
}): Project {
  const startCommand = args.startCommand ?? "yarn start";
  const testCommand = args.testCommand ?? "yarn test";
  const { pkgs, nodejs } = fromNodeVersion(args.nodeVersion);
  const yarnPackage = nixRaw`
    pkgs.yarn2nix-moretea.mkYarnPackage {
      nodejs = ${nodejs};
      yarn = pkgs.yarn;
      src = ${nixSource(args.src)};
      buildPhase = ${nixStrLit(testCommand)};
      dontStrip = true;
    }`;
  const pkg = mkPackage(nixRaw`
    let
        pkgs = ${pkgs};
        packageJson = pkgs.lib.importJSON ${nixRaw(args.src)}/package.json;
        yarnPackage = ${yarnPackage};
        nodeModulesPath = ${nixStrLit`${nixRaw("yarnPackage")}/libexec/${nixRaw(
          "packageJson.name"
        )}/node_modules`};
    in
      (pkgs.writeScriptBin "start-server" ${nixStrLit`
        #!/usr/bin/env bash

        set -eu

        export PATH=${nixRaw("pkgs.yarn")}/bin:$PATH
        export PATH=${nixRaw("nodeModulesPath")}/.bin:$PATH
        yarn --version
        ${startCommand}
      `})
  `);
  const devShell: Environment = mkEnvironment(
    nixRaw`
      let
          pkgs = ${pkgs};
          packageJson = pkgs.lib.importJSON ${nixRaw(args.src)}/package.json;
          yarnPackage = ${yarnPackage};
          nodeModulesPath = ${nixStrLit`${nixRaw(
            "yarnPackage"
          )}/libexec/${nixRaw("packageJson.name")}/node_modules`};
      in
        pkgs.mkShell {
          buildInputs = [ pkgs.yarn ];
          shellHook = ${nixStrLit`
            export PATH=${nixRaw("nodeModulesPath")}/.bin:$PATH
            export NODE_PATH=${nixRaw("nodeModulesPath")}:$NODE_PATH
          `};
        }
    `,
    nixStrLit`
      echo copying source
      cp -r ${nixSource(args.src)} src
      chmod -R u+rwX src
      cd src
    `
  );
  const startDev: Executable = devShell.shell`cd ${args.src} && ${startCommand}`;
  return mkProject(
    {
      description: args.description,
      defaultEnvironment: devShell,
      defaultExecutable: startDev,
    },
    {
      pkg,
      devShell,
      startDev,
    }
  );
}
