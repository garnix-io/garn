{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/841889913dfd06a70ffb39f603e29e46f45f0c1a";
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
          main =
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
          main =
            let expr = self.packages.${system}.main;
            in
            (if expr ? env
            then expr.env
            else pkgs.mkShell { inputsFrom = [ expr ]; }
            );
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
