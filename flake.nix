{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.fhi.url = "github:soenkehahn/format-haskell-interpolate";

  outputs = { self, nixpkgs, flake-utils, fhi }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import "${nixpkgs}" {
          inherit system;
          config.allowUnfree = true;
        };
        strings = pkgs.lib.strings;
        lists = pkgs.lib.lists;
        ourHaskell = pkgs.haskell.packages.ghc945;
      in
      {
        lib = pkgs.lib;
        app.default = {
          type = "program";
          program = "${self.packages.${system}.default}/bin/garner";
        };
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
            (ourHaskell.callCabal2nix "garn" src { }).overrideAttrs
              (
                original: {
                  nativebuildInputs = original.nativeBuildInputs ++ [
                    pkgs.deno
                    pkgs.nix
                    pkgs.zsh
                  ];
                  doCheck = false;
                }
              );
        };
        devShells = {
          default = pkgs.mkShell {
            inputsFrom = [ self.packages.${system}.default ];
            nativeBuildInputs = with pkgs; [
              ghcid
              ormolu
              cabal-install
              (ourHaskell.ghc.withPackages (p:
                [
                  p.shake
                  p.wai
                  p.wai-app-static
                  p.warp
                ]))
              (haskell-language-server.override {
                dynamic = true;
                supportedGhcVersions = [ "945" ];
              })
              ourHaskell.cabal2nix
              nodePackages.prettier
              nixpkgs-fmt
              fd
              fhi.packages.${system}.default
              just
            ];
          };
        };
        formatter = pkgs.nixpkgs-fmt;
      }
    );
}
