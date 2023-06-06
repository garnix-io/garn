{
  description = "Flake utils demo";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import "${nixpkgs}" {
          inherit system;
          config.allowUnfree = true;
        };
      in
      {
        packages = {
          default =
            (pkgs.haskellPackages.callCabal2nix "garn" ./. { }).overrideAttrs
              (
                original: {
                  buildInputs = original.buildInputs ++ [ pkgs.deno pkgs.nix ];
                  checkPhase = "true";
                }
              );
        };
        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              ghcid
              hpack
              cabal-install
              (ghc.withPackages (p: self.packages.${system}.default.buildInputs))
              self.packages.${system}.default.buildInputs
              (haskell-language-server.override {
                dynamic = true;
                supportedGhcVersions = [ "928" ];
              })
              haskellPackages.cabal2nix
            ];
          };
        };
      }
    );
}
