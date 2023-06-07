import { Package } from "./haskell.ts";

const encoder = new TextEncoder();

const formatFlake = (config: Record<string, Package>): string => {
  let outputs = Object.entries(config).reduce(
    (acc, [name, pkg]) => acc + `\n    ${name} = ${pkg.nixExpression};`,
    ""
  );
  return `{
        inputs.nixpkgs.url = "github:NixOS/nixpkgs";
        outputs = { self, nixpkgs } :
           let
             system = "x86_64-linux";
             pkgs = import "\${nixpkgs}" {
               config.allowUnfree = true;
               inherit system;
             };
           in
        {
           ${outputs}
        };
     }`;
};

export const writeFlake = (config: Record<string, Package>) => {
  const data = encoder.encode(formatFlake(config));
  Deno.writeFileSync("flake.nix", data);
};
