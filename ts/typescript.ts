import { Package } from "./base.ts";

export const mkNpmFrontend = (args: { name: string; src: string }): Package => {
  return {
    tag: "package",
    nixExpression: `
      pkgs.stdenv.mkDerivation {
        name = ${JSON.stringify(args.name)};
        buildInputs = [ pkgs.nodejs-18_x ];
      }
    `,
    envExpression: (nixExpression) =>
      ` let expr = ${nixExpression};
        in if ${nixExpression} ? env
          then ${nixExpression}.env
          else pkgs.mkShell({
            inputsFrom = [ ${nixExpression} ];
          })`,
  };
};
