{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/841889913dfd06a70ffb39f603e29e46f45f0c1a";

  inputs.npmlock2nix-repo = {
    url = "github:nix-community/npmlock2nix?rev=9197bbf397d76059a76310523d45df10d2e4ca81";
    flake = false;
  };

  outputs = { self, nixpkgs, npmlock2nix-repo }:
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
            let
              npmlock2nix = import npmlock2nix-repo {
                inherit pkgs;
              };
              pkgs = import "${nixpkgs}" {
                config.permittedInsecurePackages = [ ];
                inherit system;
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