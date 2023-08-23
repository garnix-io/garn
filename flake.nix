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
                  nativeBuildInputs = original.nativeBuildInputs ++ [
                    pkgs.deno
                    pkgs.makeWrapper
                    pkgs.nix
                    pkgs.zsh
                  ];

                  postInstall = ''
                    wrapProgram "$out/bin/garner" \
                        --prefix PATH : ${pkgs.lib.makeBinPath [
                      pkgs.deno
                      pkgs.nix
                    ]}
                  '';

                  doCheck = false;
                }
              );
          nixpkgs-ts = pkgs.stdenv.mkDerivation {
            name = "nixpkgs.ts";
            src = ./.;

            nativeBuildInputs = [
              pkgs.cacert
              pkgs.nix
              self.packages.${system}.garner
            ];

            LC_ALL = "C.UTF-8";

            buildPhase = ''
              export HOME="$PWD/fake-home"
              mkdir -p "$HOME"
              codegen
            '';

            installPhase = ''
              mkdir -p "$out/share/garner"
              cp ts/nixpkgs.ts "$out/share/garner"
            '';

            outputHash = "TgnOyeVN2wpsM2vIYwk3jd0Gx14G6lY2XTfJ8+M4PGA=";
            outputHashAlgo = "sha256";
            outputHashMode = "recursive";
          };
          typescript = pkgs.stdenv.mkDerivation {
            name = "garner-typescript";
            src = ./ts;

            nativeBuildInputs = [
              self.packages.${system}.nixpkgs-ts
            ];

            buildPhase = ''
              cp --no-preserve=mode --recursive "$src" ./
              ## This is for garner itself, not users
              rm runner.ts
            '';

            installPhase = ''
              mkdir -p "$out/share"
              cp --recursive ./ "$out/share/garner"
              cp ${self.packages.${system}.nixpkgs-ts}/share/garner/* \
                 "$out/share/garner"
            '';
          };
        };
        devShells = {
          default = pkgs.mkShell {
            inputsFrom =
              builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.packages.${system};
            nativeBuildInputs = with pkgs; [
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
          ## An empty devShell for running garner in isolation
          ## NB: If you use this shell within another shell or `direnv`, the
          ##     environment will leak, and isolation will be lost.
          barren = pkgs.mkShell {
            nativeBuildInputs = [ self.packages.${system}.garner ];
            shellHook = ''
              find . -name 'garner.ts' -exec \
                bash -c \
                  'source "${pkgs.stdenv}/setup" && \
                   substituteInPlace "$0" \
                     --replace \
                     "http://localhost:8777/" \
                     "${self.packages.${system}.typescript}/share/garner/"' {} \;
            '';
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
