{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/841889913dfd06a70ffb39f603e29e46f45f0c1a";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
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
          server_pkg =
            pkgs.buildGoModule {
              name = "go-project";
              src = ./.;
              vendorHash = null;
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
          server =
            let
              expr =
                pkgs.buildGoModule {
                  name = "go-project";
                  src = ./.;
                  vendorHash = null;
                }
              ;
            in
            (if expr ? env
            then expr.env
            else pkgs.mkShell { inputsFrom = [ expr ]; }
            )
          ;
        });
      apps = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" { inherit system; };
        in
        {
          server = {
            type = "app";
            program = "${
    pkgs.buildGoModule {
      name = "go-project";
      src = ./.;
      vendorHash = null;
    }
  }/bin/server";
          };
        });
    };
}
