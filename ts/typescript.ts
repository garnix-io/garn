import { OldPackage, mkOldPackage } from "./base.ts";
import { nixSource } from "./utils.ts";

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
}): OldPackage => {
  const { pkgs, nodejs } = fromNodeVersion(args.nodeVersion);
  return mkOldPackage({
    description: args.description,
    expression: `
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
  `,
  }).setStartCommand(["npm", "run", "start"]);
};

export const mkYarnFrontend = (args: {
  description: string;
  src: string;
  nodeVersion: keyof typeof nodeVersions;
  testCommand: string;
  serverStartCommand: string;
}): OldPackage => {
  const { pkgs, nodejs } = fromNodeVersion(args.nodeVersion);
  return mkOldPackage({
    description: args.description,
    expression: `
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
    `,
  });
};
