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
        strings = pkgs.lib.strings;
        lists = pkgs.lib.lists;
      in
      {
        packages = {
          default =
            let
              # directories shouldn't have leading or trailing slashes
              ignored = [ "examples" "justfile" ];
              src = builtins.path
                {
                  path = ./.;
                  name = "garner-source";
                  filter = path: type:
                    let
                      repoPath =
                        builtins.concatStringsSep "/"
                          (lists.drop 4
                            (strings.splitString "/" path));
                    in
                      ! (builtins.elem repoPath ignored);
                };
            in
            (pkgs.haskellPackages.callCabal2nix "garn" src { }).overrideAttrs
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
              ormolu
              cabal-install
              (ghc.withPackages (p: self.packages.${system}.default.buildInputs))
              self.packages.${system}.default.buildInputs
              (haskell-language-server.override {
                dynamic = true;
                supportedGhcVersions = [ "928" ];
              })
              haskellPackages.cabal2nix
              nodePackages.prettier
              ormolu
            ];
          };
        };
        formatter = pkgs.nixpkgs-fmt;
      }
    );
}
