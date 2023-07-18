{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        {
          frontend =
            pkgs.stdenv.mkDerivation {
              name = "frontend";
              buildInputs = [ pkgs.nodejs-18_x ];
            }
          ;
        });
      devShells = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        {
          frontend =
            let expr = self.packages.${system}.frontend;
            in if self.packages.${system}.frontend ? env
            then self.packages.${system}.frontend.env
            else
              pkgs.mkShell ({
                inputsFrom = [ self.packages.${system}.frontend ];
              });
        });
      formatter = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        pkgs.nixpkgs-fmt);
    };
}
