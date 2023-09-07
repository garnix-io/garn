import { Package, mkPackage } from "./base.ts";

export const mkNpmFrontend = (args: { name: string; src: string }): Package =>
  mkPackage({
    expression: `
    pkgs.stdenv.mkDerivation {
      name = ${JSON.stringify(args.name)};
      buildInputs = [ pkgs.nodejs-18_x ];
    }
  `,
  }).setStartCommand(["npm", "run", "start"]);
