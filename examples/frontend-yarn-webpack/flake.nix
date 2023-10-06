{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/841889913dfd06a70ffb39f603e29e46f45f0c1a";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.npmlock2nix-repo = {
    url = "github:nix-community/npmlock2nix?rev=9197bbf397d76059a76310523d45df10d2e4ca81";
    flake = false;
  };
  outputs = { self, nixpkgs, flake-utils, npmlock2nix-repo }:
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
          frontend_pkg =
            let
              pkgs = import "${nixpkgs}" {
                config.permittedInsecurePackages = [ ];
                inherit system;
              };
              packageJson = pkgs.lib.importJSON ./package.json;
              yarnPackage = pkgs.yarn2nix-moretea.mkYarnPackage {
                nodejs = pkgs.nodejs-18_x;
                yarn = pkgs.yarn;
                src =
                  (
                    let
                      lib = pkgs.lib;
                      lastSafe = list:
                        if lib.lists.length list == 0
                        then null
                        else lib.lists.last list;
                    in
                    builtins.path
                      {
                        path = ./.;
                        filter = path: type:
                          let
                            fileName = lastSafe (lib.strings.splitString "/" path);
                          in
                          fileName != "flake.nix";
                      }
                  )
                ;
                buildPhase = "yarn mocha";
              };
            in
            (pkgs.writeScriptBin "start-server" ''
              #!/usr/bin/env bash

              set -eu

              export PATH=${pkgs.yarn}/bin:$PATH
              export PATH=${yarnPackage}/libexec/${packageJson.name}/node_modules/.bin:$PATH
              yarn --version
              yarn start
            '')
          ;
        });
      checks = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        { });
      devShells = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        {
          frontend =
            let
              pkgs = import "${nixpkgs}" {
                config.permittedInsecurePackages = [ ];
                inherit system;
              };
              packageJson = pkgs.lib.importJSON ./package.json;
              yarnPackage = pkgs.yarn2nix-moretea.mkYarnPackage {
                nodejs = pkgs.nodejs-18_x;
                yarn = pkgs.yarn;
                src =
                  (
                    let
                      lib = pkgs.lib;
                      lastSafe = list:
                        if lib.lists.length list == 0
                        then null
                        else lib.lists.last list;
                    in
                    builtins.path
                      {
                        path = ./.;
                        filter = path: type:
                          let
                            fileName = lastSafe (lib.strings.splitString "/" path);
                          in
                          fileName != "flake.nix";
                      }
                  )
                ;
                buildPhase = "yarn mocha";
              };
            in
            pkgs.mkShell {
              buildInputs = [ pkgs.yarn ];
              shellHook = ''
                export PATH=${yarnPackage}/libexec/${packageJson.name}/node_modules/.bin:$PATH
                export NODE_PATH=${yarnPackage}/libexec/${packageJson.name}/node_modules:$NODE_PATH
              '';
            }
          ;
        });
      apps = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" { inherit system; };
        in
        {

          frontend = {
            type = "app";
            program = "${
          let dev = 
      let
          pkgs = import "${nixpkgs}" {
        config.permittedInsecurePackages = [];
        inherit system;
      };
          packageJson = pkgs.lib.importJSON ./package.json;
          yarnPackage = pkgs.yarn2nix-moretea.mkYarnPackage {
            nodejs = pkgs.nodejs-18_x;
            yarn = pkgs.yarn;
            src = 
  (let
    lib = pkgs.lib;
    lastSafe = list :
      if lib.lists.length list == 0
        then null
        else lib.lists.last list;
  in
  builtins.path
    {
      path = ./.;
      filter = path: type:
        let
          fileName = lastSafe (lib.strings.splitString "/" path);
        in
         fileName != "flake.nix";
    })
;
            buildPhase = "yarn mocha";
          };
      in
        pkgs.mkShell {
          buildInputs = [ pkgs.yarn ];
          shellHook = ''
            export PATH=${yarnPackage}/libexec/${packageJson.name}/node_modules/.bin:$PATH
            export NODE_PATH=${yarnPackage}/libexec/${packageJson.name}/node_modules:$NODE_PATH
          '';
        }
    ; in
          pkgs.runCommand "shell-env" {
            buildInputs = dev.buildInputs;
            nativeBuildInputs = dev.nativeBuildInputs;
          } ''
            echo "export PATH=$PATH:$PATH" > $out
            echo ${pkgs.lib.strings.escapeShellArg dev.shellHook} >> $out
            echo ${pkgs.lib.strings.escapeShellArg "yarn start"} >> $out
            chmod +x $out
          ''
        }";
          };

        });
    };
}
