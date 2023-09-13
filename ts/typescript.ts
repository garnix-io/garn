import { Package, mkPackage } from "./base.ts";

export const mkNpmFrontend = (args: { name: string; src: string }): Package =>
  mkPackage({
    expression: `
      let
        npmlock2nix = import npmlock2nix-repo {
          inherit pkgs;
        };
      in
      npmlock2nix.v2.build
        {
          src = ./.;
          buildCommands = [ "npm run test -- --watchAll=false" "mkdir $out" ];
          installPhase = "true";
          node_modules_attrs = {
            nodejs = pkgs.nodejs-18_x;
          };
        }
  `,
  }).setStartCommand(["npm", "run", "start"]);
