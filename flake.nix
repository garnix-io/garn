{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/6fc7203e423bbf1c8f84cccf1c4818d097612566";
  inputs.fhi.url = "github:soenkehahn/format-haskell-interpolate";
  inputs.nix-tool-installer.url = "github:garnix-io/nix-tool-installer";
  inputs.call-flake.url = "github:divnix/call-flake";
  inputs.cradle = {
    url = "github:garnix-io/cradle";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, fhi, nix-tool-installer, call-flake, cradle }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import "${nixpkgs}" {
          inherit system;
          config.allowUnfree = true;
        };
        strings = pkgs.lib.strings;
        lists = pkgs.lib.lists;
        ourHaskell = pkgs.haskell.packages.ghc947.override {
          overrides = self: super: {
            cradle = cradle.lib.${system}.mkCradle pkgs.haskell.packages.ghc947;
          };
        };
        websiteFlake = call-flake ./website;
        websitePackages =
          if system == "x86_64-linux" then
            pkgs.lib.attrsets.mapAttrs'
              (name: value: { name = "website_${name}"; inherit value; })
              websiteFlake.packages.${system} else { };
      in
      {
        lib = pkgs.lib;
        apps.default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/garn";
        };
        packages = rec {
          default = self.packages.${system}.garn;
          garn =
            pkgs.runCommand "garn"
              {
                nativeBuildInputs = [ pkgs.makeWrapper ];
              }
              ''
                mkdir -p $out/bin
                cp ${garnHaskellPackage}/bin/garn $out/bin
                wrapProgram "$out/bin/garn" \
                    --set LC_ALL C.UTF-8 \
                    --prefix PATH : ${pkgs.lib.makeBinPath [
                  pkgs.coreutils
                  pkgs.deno
                  pkgs.git
                  pkgs.nix
                  self.packages.${system}.cabal2json
                ]}
              '';
          garnHaskellPackage =
            let
              # directories shouldn't have leading or trailing slashes
              ignored = [
                "examples"
                "flake.nix"
                ".github"
                "justfile"
                "scripts"
                "test/check-isolated-garn.sh"
                "website"
              ];
              src = builtins.path
                {
                  path = ./.;
                  name = "garn-source";
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
            (ourHaskell.callPackage ./garn.nix { }).overrideAttrs
              (
                original: {
                  inherit src;
                  nativeBuildInputs = original.nativeBuildInputs ++ [
                    pkgs.deno
                    pkgs.makeWrapper
                    pkgs.nix
                    pkgs.zsh
                    self.packages.${system}.cabal2json
                  ];
                  postInstall = ''
                    wrapProgram "$out/bin/codegen" \
                        --set LC_ALL C.UTF-8 \
                        --prefix PATH : ${pkgs.lib.makeBinPath [
                      pkgs.git
                      pkgs.nix
                    ]}
                  '';

                  doCheck = false;
                }
              );
          cabal2json =
            let
              ghc = ourHaskell.ghc.withPackages (p:
                [
                  p.Cabal-syntax
                  p.aeson
                  p.bytestring
                ]);
            in
            (pkgs.runCommand "cabal2json"
              {
                nativeBuildInputs =
                  if system == "aarch64-darwin" || system == "x86_64-darwin"
                  then [ pkgs.clang ]
                  else [ ];
              }
              ''
                cp ${./scripts/cabal2json.hs} ./cabal2json.hs
                ${ghc}/bin/ghc ./cabal2json.hs -o cabal2json
                mkdir -p $out/bin
                cp cabal2json $out/bin
              '');
          fileserver =
            let
              ghc = ourHaskell.ghc.withPackages (p:
                [
                  p.cradle
                  p.fsnotify
                  p.getopt-generics
                  p.wai-app-static
                  p.warp
                ]);
            in
            (pkgs.writeScriptBin "fileserver" ''
              export PATH=${pkgs.curl}/bin:$PATH
              export PATH=${pkgs.deno}/bin:$PATH
              exec ${ghc}/bin/runhaskell ${./scripts/fileserver.hs} "$@"
            '');
          installScriptFiles = nix-tool-installer.lib.${system}.mkInstallScriptFiles {
            toolName = "garn";
            baseUrl = "https://garn.io";
            flakeLocation = "github:garnix-io/garn/v0.0.16";
            testCommand = "garn --help";
          };
        } // websitePackages;
        devShells = {
          default = pkgs.mkShell {
            shellHook =
              # For prerendering the frontend,
              (pkgs.lib.optionalString (! pkgs.stdenv.isDarwin)
                ''
                  export PUPPETEER_EXECUTABLE_PATH="${pkgs.chromium}/bin/chromium"
                  export PUPPETEER_SKIP_CHROMIUM_DOWNLOAD=true
                '');
            inputsFrom =
              builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.packages.${system};
            nativeBuildInputs = with pkgs; [
              bashInteractive
              ghcid
              cabal-install
              (ourHaskell.ghc.withHoogle (p:
                self.packages.${system}.garnHaskellPackage.buildInputs))
              (haskell-language-server.override {
                dynamic = true;
                supportedGhcVersions = [ "947" ];
              })
              ourHaskell.cabal2nix
              fhi.packages.${system}.default
              self.packages.${system}.cabal2json
              sd
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
            websiteChecks =
              if system == "x86_64-linux" then
                pkgs.lib.attrsets.mapAttrs'
                  (name: check: { name = "website_${name}"; value = check; })
                  websiteFlake.checks.${system} else { };
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
            fileserver = pkgs.runCommand "fileserver-check"
              {
                buildInputs = [ self.packages.${system}.fileserver ];
              } ''
              fileserver --help
              touch $out
            '';
          } // websiteChecks;
        formatter = pkgs.nixpkgs-fmt;
        apps = {
          fileserver = flake-utils.lib.mkApp
            { drv = self.packages.${system}.fileserver; };
        };
      });
}
