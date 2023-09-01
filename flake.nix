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
        apps.default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/garner";
        };
        packages = {
          default = self.packages.${system}.garner;
          garner =
            let
              # directories shouldn't have leading or trailing slashes
              ignored = [ ".github" "examples" "justfile" "scripts" "test/smoke-test.sh" ];
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
                  nativeBuildInputs = original.nativeBuildInputs ++ [
                    pkgs.deno
                    pkgs.makeWrapper
                    pkgs.nix
                    pkgs.zsh
                  ];
                  postInstall = ''
                    wrapProgram "$out/bin/codegen" \
                        --set LC_ALL C.UTF-8 \
                        --prefix PATH : ${pkgs.lib.makeBinPath [
                      pkgs.git
                      pkgs.nix
                    ]}
                    wrapProgram "$out/bin/garner" \
                        --set LC_ALL C.UTF-8 \
                        --prefix PATH : ${pkgs.lib.makeBinPath [
                      pkgs.deno
                      pkgs.git
                      pkgs.nix
                    ]}
                  '';

                  doCheck = false;
                }
              );
        };
        devShells = {
          default = pkgs.mkShell {
            inputsFrom =
              builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.packages.${system};
            nativeBuildInputs = with pkgs; [
              bashInteractive
              ghcid
              cabal-install
              (ourHaskell.ghc.withPackages (p:
                [
                  ## Needed for `just fileserver`.
                  p.shake
                  p.wai-app-static
                  p.warp
                ] ++ self.packages.${system}.default.buildInputs))
              (haskell-language-server.override {
                dynamic = true;
                supportedGhcVersions = [ "945" ];
              })
              ourHaskell.cabal2nix
              fhi.packages.${system}.default
            ];
          };
        };
        checks =
          let
            justRecipe = recipe: inputs: pkgs.runCommand recipe
              { nativeBuildInputs = [ pkgs.just ] ++ inputs; }
              ''
                touch $out
                cp -r ${self}/* .
                substituteInPlace justfile \
                  --replace !/usr/bin/env !${pkgs.coreutils}/bin/env
                just ${recipe}
              '';
          in
          {
            nix-fmt = justRecipe "fmt-nix-check" [ self.formatter.${system} ];
            typescript-fmt =
              justRecipe "fmt-typescript-check" [
                pkgs.fd
                pkgs.nodePackages.prettier
              ];
            haskell-fmt = justRecipe "fmt-haskell-check" [
              pkgs.hpack
              pkgs.ormolu
            ];
          };
        formatter = pkgs.nixpkgs-fmt;
      }
    );
}
