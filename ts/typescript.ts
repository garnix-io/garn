import { Package, mkPackage } from "./base.ts";

export const mkNpmFrontend = (args: { description: string; src: string }): Package =>
  mkPackage({
    description: args.description,
    expression: `
    pkgs.stdenv.mkDerivation {
      name = "garner-mkNpmFrontend";
      buildInputs = [ pkgs.nodejs-18_x ];
    }
  `,
  }).setStartCommand(["npm", "run", "start"]);
