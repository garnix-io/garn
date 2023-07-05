import { Package } from "./base.ts";

const encoder = new TextEncoder();

const formatFlake = (config: Record<string, Package>): string => {
  const packages = Object.entries(config).reduce(
    (acc, [name, pkg]) => acc + `${name} = ${pkg.nixExpression};`,
    ""
  );
  const shells = Object.entries(config).reduce((acc, [name, pkg]) => {
    const pkgAttr = `self.packages.\${system}.${name}`;
    const env = pkg.envExpression(pkgAttr);
    return acc + `${name} = ${env};`;
  }, "");
  return `{
    inputs.nixpkgs.url = "github:NixOS/nixpkgs";
    outputs = { self, nixpkgs }:
      let
        systems = [ "x86_64-linux" ];
        forAllSystems = nixpkgs.lib.genAttrs systems;
      in
      {
        packages = forAllSystems (system:
          let
            pkgs = import "\${nixpkgs}" {
              config.allowUnfree = true;
              inherit system;
            };
          in
          {
            ${packages}
          });
        devShells = forAllSystems (system:
          let
            pkgs = import "\${nixpkgs}" {
              config.allowUnfree = true;
              inherit system;
            };
          in
          {
            ${shells}
          });
        formatter = forAllSystems (system:
          let
            pkgs = import "\${nixpkgs}" {
              config.allowUnfree = true;
              inherit system;
            };
          in
          pkgs.nixpkgs-fmt);
      };
  }`;
};

export const writeFlake = (config: Record<string, Package>) => {
  const data = encoder.encode(formatFlake(config));
  Deno.writeFileSync("flake.nix", data);
};
