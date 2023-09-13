import { Package, mkPackage } from "./base.ts";

const nodeVersions = {
  '14': {
    pkg: 'nodejs-14_x',
    permittedInsecurePackages: ['nodejs-14.21.3', 'openssl-1.1.1v'],
  },
  '16': {
    pkg: 'nodejs-16_x',
    permittedInsecurePackages: ['nodejs-16.20.2'],
  },
  '18': {
    pkg: 'nodejs-18_x',
    permittedInsecurePackages: [],
  },
} satisfies Record<string, { pkg: string; permittedInsecurePackages: Array<string> }>;


export const mkNpmFrontend = (args: {
  name: string;
  src: string;
  nodeVersion: keyof typeof nodeVersions;
  testCommand: string;
}): Package => {
  const { pkg, permittedInsecurePackages } = nodeVersions[args.nodeVersion];
  return mkPackage({
    expression: `
      let
        npmlock2nix = import npmlock2nix-repo {
          inherit pkgs;
        };
        pkgs = import "\${nixpkgs}" {
          config.permittedInsecurePackages = [${permittedInsecurePackages.map(JSON.stringify).join(' ')}];
          inherit system;
        };
      in
      npmlock2nix.v2.build
        {
          src = ./.;
          buildCommands = [ ${JSON.stringify(args.testCommand)} "mkdir $out" ];
          installPhase = "true";
          node_modules_attrs = {
            nodejs = pkgs.${pkg};
          };
        }
  `,
  }).setStartCommand(["npm", "run", "start"]);
};
