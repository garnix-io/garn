{
  inputs.nixpkgs-repo.url = "github:NixOS/nixpkgs/6fc7203e423bbf1c8f84cccf1c4818d097612566";
  outputs = { self, nixpkgs-repo }:
    let
      nixpkgs = nixpkgs-repo;
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
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
          "frontend/pkg" =
            let
              pkgs = import "${nixpkgs}" {
                config.permittedInsecurePackages = [ ];
                inherit system;
              };
              packageJson = pkgs.lib.importJSON ./package.json;
              yarnPackage = pkgs.yarn2nix-moretea.mkYarnPackage {
                nodejs = pkgs.nodejs-18_x;
                yarn = pkgs.yarn;
                src = (
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
                      name = "source";
                      filter = path: type:
                        let
                          fileName = lastSafe (lib.strings.splitString "/" path);
                        in
                        fileName != "flake.nix" &&
                        fileName != "garn.ts";
                    }
                );
                buildPhase = "yarn mocha";
                dontStrip = true;
              };
              nodeModulesPath = "${yarnPackage}/libexec/${packageJson.name}/node_modules";
            in
            (pkgs.writeScriptBin "start-server" "
          #!/usr/bin/env bash

          set -eu

          export PATH=${pkgs.yarn}/bin:\$PATH
          export PATH=${nodeModulesPath}/.bin:\$PATH
          yarn --version
          ${"yarn start"}
        ");
        }
      );
      checks = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        { }
      );
      devShells = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        {
          "frontend" =
            let
              pkgs = import "${nixpkgs}" {
                config.permittedInsecurePackages = [ ];
                inherit system;
              };
              packageJson = pkgs.lib.importJSON ./package.json;
              yarnPackage = pkgs.yarn2nix-moretea.mkYarnPackage {
                nodejs = pkgs.nodejs-18_x;
                yarn = pkgs.yarn;
                src = (
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
                      name = "source";
                      filter = path: type:
                        let
                          fileName = lastSafe (lib.strings.splitString "/" path);
                        in
                        fileName != "flake.nix" &&
                        fileName != "garn.ts";
                    }
                );
                buildPhase = "yarn mocha";
                dontStrip = true;
              };
              nodeModulesPath = "${yarnPackage}/libexec/${packageJson.name}/node_modules";
            in
            pkgs.mkShell {
              buildInputs = [ pkgs.yarn ];
              shellHook = "
            export PATH=${nodeModulesPath}/.bin:\$PATH
            export NODE_PATH=${nodeModulesPath}:\$NODE_PATH
          ";
            };
        }
      );
      apps = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        {
          "frontend" = {
            "type" = "app";
            "program" = "${let
        dev = let
          pkgs = import "${nixpkgs}" {
        config.permittedInsecurePackages = [];
        inherit system;
      };
          packageJson = pkgs.lib.importJSON ./package.json;
          yarnPackage = pkgs.yarn2nix-moretea.mkYarnPackage {
      nodejs = pkgs.nodejs-18_x;
      yarn = pkgs.yarn;
      src = (let
    lib = pkgs.lib;
    lastSafe = list :
      if lib.lists.length list == 0
        then null
        else lib.lists.last list;
  in
  builtins.path
    {
      path = ./.;
      name = "source";
      filter = path: type:
        let
          fileName = lastSafe (lib.strings.splitString "/" path);
        in
         fileName != "flake.nix" &&
         fileName != "garn.ts";
    });
      buildPhase = "yarn mocha";
      dontStrip = true;
    };
          nodeModulesPath = "${yarnPackage}/libexec/${packageJson.name}/node_modules";
      in
        pkgs.mkShell {
          buildInputs = [ pkgs.yarn ];
          shellHook = "
            export PATH=${nodeModulesPath}/.bin:\$PATH
            export NODE_PATH=${nodeModulesPath}:\$NODE_PATH
          ";
        };
        shell = "cd ${"."} && ${"yarn start"}";
        buildPath = pkgs.runCommand "build-inputs-path" {
          inherit (dev) buildInputs nativeBuildInputs;
        } "echo $PATH > $out";
      in
      pkgs.writeScript "shell-env"  ''
        #!${pkgs.bash}/bin/bash
        export PATH=$(cat ${buildPath}):$PATH
        ${dev.shellHook}
        ${shell}
      ''}";
          };
          "frontend/startDev" = {
            "type" = "app";
            "program" = "${let
        dev = let
          pkgs = import "${nixpkgs}" {
        config.permittedInsecurePackages = [];
        inherit system;
      };
          packageJson = pkgs.lib.importJSON ./package.json;
          yarnPackage = pkgs.yarn2nix-moretea.mkYarnPackage {
      nodejs = pkgs.nodejs-18_x;
      yarn = pkgs.yarn;
      src = (let
    lib = pkgs.lib;
    lastSafe = list :
      if lib.lists.length list == 0
        then null
        else lib.lists.last list;
  in
  builtins.path
    {
      path = ./.;
      name = "source";
      filter = path: type:
        let
          fileName = lastSafe (lib.strings.splitString "/" path);
        in
         fileName != "flake.nix" &&
         fileName != "garn.ts";
    });
      buildPhase = "yarn mocha";
      dontStrip = true;
    };
          nodeModulesPath = "${yarnPackage}/libexec/${packageJson.name}/node_modules";
      in
        pkgs.mkShell {
          buildInputs = [ pkgs.yarn ];
          shellHook = "
            export PATH=${nodeModulesPath}/.bin:\$PATH
            export NODE_PATH=${nodeModulesPath}:\$NODE_PATH
          ";
        };
        shell = "cd ${"."} && ${"yarn start"}";
        buildPath = pkgs.runCommand "build-inputs-path" {
          inherit (dev) buildInputs nativeBuildInputs;
        } "echo $PATH > $out";
      in
      pkgs.writeScript "shell-env"  ''
        #!${pkgs.bash}/bin/bash
        export PATH=$(cat ${buildPath}):$PATH
        ${dev.shellHook}
        ${shell}
      ''}";
          };
        }
      );
    };
}
