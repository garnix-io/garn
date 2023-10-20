{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.fhi.url = "github:soenkehahn/format-haskell-interpolate";
  inputs.nix-tool-installer.url = "github:garnix-io/nix-tool-installer";
  inputs.call-flake.url = "github:divnix/call-flake";

  outputs = { self, nixpkgs, flake-utils, fhi, nix-tool-installer, call-flake }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import "${nixpkgs}" {
          inherit system;
          config.allowUnfree = true;
        };
        strings = pkgs.lib.strings;
        lists = pkgs.lib.lists;
        ourHaskell = pkgs.haskell.packages.ghc945;
        websitePackages =
          if system == "x86_64-linux" then
            pkgs.lib.attrsets.mapAttrs'
              (name: value: { name = "website_${name}"; inherit value; })
              (call-flake ./website).packages.${system} else { };
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
            let
              # directories shouldn't have leading or trailing slashes
              ignored = [
                "examples"
                "flake.nix"
                ".github"
                "justfile"
                "scripts"
                "test/check-isolated-garn.sh"
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
            (ourHaskell.callCabal2nix "garn" src { }).overrideAttrs
              (
                original: {
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
                    wrapProgram "$out/bin/garn" \
                        --set LC_ALL C.UTF-8 \
                        --prefix PATH : ${pkgs.lib.makeBinPath [
                      pkgs.deno
                      pkgs.git
                      pkgs.nix
                      cabal2json
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
            (pkgs.writeScriptBin "cabal2json" ''
              exec ${ghc}/bin/runhaskell ${./scripts/cabal2json.hs} "$@"
            '');
          fileserver =
            let
              ghc = ourHaskell.ghc.withPackages (p:
                [
                  p.fsnotify
                  p.getopt-generics
                  p.shake
                  p.wai-app-static
                  p.warp
                ]);
            in
            (pkgs.writeScriptBin "fileserver" ''
              export PATH=${pkgs.curl}/bin:$PATH
              export PATH=${pkgs.deno}/bin:$PATH
              exec ${ghc}/bin/runhaskell ${./scripts/fileserver.hs} "$@"
            '');
          installScript = nix-tool-installer.lib.${system}.mkInstallScript {
            toolName = "garn";
            flakeLocation = "github:garnix-io/garn";
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
                self.packages.${system}.default.buildInputs))
              (haskell-language-server.override {
                dynamic = true;
                supportedGhcVersions = [ "945" ];
              })
              ourHaskell.cabal2nix
              fhi.packages.${system}.default
              self.packages.${system}.cabal2json
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
                  (call-flake ./website).checks.${system} else { };
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
