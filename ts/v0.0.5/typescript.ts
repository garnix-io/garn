import { Environment, mkEnvironment } from "./environment.ts";
import { Executable } from "./executable.ts";
import { mkPackage } from "./package.ts";
import { Project, mkProject } from "./project.ts";
import { nixSource } from "./internal/utils.ts";

const nodeVersions = {
  "14": {
    pkg: "nodejs-14_x",
    permittedInsecurePackages: ["nodejs-14.21.3", "openssl-1.1.1v"],
  },
  "16": {
    pkg: "nodejs-16_x",
    permittedInsecurePackages: ["nodejs-16.20.2"],
  },
  "18": {
    pkg: "nodejs-18_x",
    permittedInsecurePackages: [],
  },
} satisfies Record<
  string,
  { pkg: string; permittedInsecurePackages: Array<string> }
>;

type NodeVersion = keyof typeof nodeVersions;

const fromNodeVersion = (version: NodeVersion) => {
  const { pkg, permittedInsecurePackages } = nodeVersions[version];
  return {
    pkgs: `
      import "\${nixpkgs}" {
        config.permittedInsecurePackages = [${permittedInsecurePackages
          .map((x) => JSON.stringify(x))
          .join(" ")}];
        inherit system;
      }
    `.trim(),
    nodejs: `pkgs.${pkg}`,
  };
};

export const mkNpmFrontend = (args: {
  description: string;
  src: string;
  nodeVersion: NodeVersion;
  testCommand: string;
}): Project => {
  const { pkgs, nodejs } = fromNodeVersion(args.nodeVersion);
  const pkg = mkPackage(`
    let
      npmlock2nix = import npmlock2nix-repo {
        inherit pkgs;
      };
      pkgs = ${pkgs};
    in
    npmlock2nix.v2.build
      {
        src = ${nixSource(args.src)};
        buildCommands = [ ${JSON.stringify(args.testCommand)} "mkdir $out" ];
        installPhase = "true";
        node_modules_attrs = {
          nodejs = ${nodejs};
        };
      }
  `);
  const devShell: Environment = mkEnvironment(
    `
      let
        npmlock2nix = import npmlock2nix-repo {
          inherit pkgs;
        };
      in
      npmlock2nix.v2.shell {
        src = ${nixSource(args.src)};
        node_modules_attrs = {
          nodejs = ${nodejs};
        };
      }
    `,
    args.src
  );
  const startDev: Executable = devShell.shell`npm run start`;
  return mkProject(
    args.description,
    {
      pkg,
      devShell,
      startDev,
    },
    {
      defaults: {
        environment: "devShell",
        executable: "startDev",
      },
    }
  );
};

export const mkYarnFrontend = (args: {
  description: string;
  src: string;
  nodeVersion: keyof typeof nodeVersions;
  testCommand: string;
  serverStartCommand: string;
}): Project => {
  const { pkgs, nodejs } = fromNodeVersion(args.nodeVersion);
  const pkg = mkPackage(`
    let
        pkgs = ${pkgs};
        packageJson = pkgs.lib.importJSON ./package.json;
        yarnPackage = pkgs.yarn2nix-moretea.mkYarnPackage {
          nodejs = ${nodejs};
          yarn = pkgs.yarn;
          src = ${nixSource(args.src)};
          buildPhase = ${JSON.stringify(args.testCommand)};
        };
    in
      (pkgs.writeScriptBin "start-server" ''
        #!/usr/bin/env bash

        set -eu

        export PATH=\${pkgs.yarn}/bin:$PATH
        export PATH=\${yarnPackage}/libexec/\${packageJson.name}/node_modules/.bin:$PATH
        yarn --version
        ${args.serverStartCommand}
      '')
  `);
  const devShell: Environment = mkEnvironment(
    `
      let
          pkgs = ${pkgs};
          packageJson = pkgs.lib.importJSON ./package.json;
          yarnPackage = pkgs.yarn2nix-moretea.mkYarnPackage {
            nodejs = ${nodejs};
            yarn = pkgs.yarn;
            src = ${nixSource(args.src)};
            buildPhase = ${JSON.stringify(args.testCommand)};
          };
      in
        pkgs.mkShell {
          buildInputs = [ pkgs.yarn ];
          shellHook = ''
            export PATH=\${yarnPackage}/libexec/\${packageJson.name}/node_modules/.bin:$PATH
            export NODE_PATH=\${yarnPackage}/libexec/\${packageJson.name}/node_modules:$NODE_PATH
          '';
        }
    `,
    args.src
  );
  const startDev: Executable = devShell.shell`${args.serverStartCommand}`;
  return mkProject(
    args.description,
    {
      pkg,
      devShell,
      startDev,
    },
    {
      defaults: {
        environment: "devShell",
        executable: "startDev",
      },
    }
  );
};
