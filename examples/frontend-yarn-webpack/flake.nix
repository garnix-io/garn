
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/841889913dfd06a70ffb39f603e29e46f45f0c1a";

  inputs.npmlock2nix-repo = {
    url = "github:nix-community/npmlock2nix?rev=9197bbf397d76059a76310523d45df10d2e4ca81";
    flake = false;
  };

  outputs = { self, nixpkgs, npmlock2nix-repo }:
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
        rec {
          backend = 1;
backend__develop = pkgs.writeScriptBin "develop" ''
      
      while true; do
echo -e 'HTTP/1.1 200 OK\n\nHello' | nc -w 1 -l 8000
done
    '';
backend__format = pkgs.writeScriptBin "format" ''
      
      echo formatting...
    '';
deploy = pkgs.writeScriptBin "deploy" ''
      
      cat ${
      pkgs.stdenv.mkDerivation {
        name = "${(pkgs.lib.importJSON ./package.json).name}";
        src = ./.;
        nativeBuildInputs = [ pkgs.yarn (
      pkgs.runCommand "node-bins" {} ''
        mkdir $out
        ln -s ${
      pkgs.yarn2nix-moretea.mkYarnPackage {
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
            filter = path: type:
              let
            fileName = lastSafe (lib.strings.splitString "/" path);
            in
            fileName == "package.json" || fileName == "yarn.lock";
          }
        );
      }
    }/libexec/${(pkgs.lib.importJSON ./package.json).name}/node_modules/.bin $out/bin
      ''
    ) ];
        buildPhase = ''
          ln -s ${
      pkgs.yarn2nix-moretea.mkYarnPackage {
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
            filter = path: type:
              let
            fileName = lastSafe (lib.strings.splitString "/" path);
            in
            fileName == "package.json" || fileName == "yarn.lock";
          }
        );
      }
    }/libexec/${(pkgs.lib.importJSON ./package.json).name}/node_modules node_modules
          webpack build
        '';
        installPhase = ''
          mkdir $out
          mv dist $out
        '';
      }
    }/dist/main.js
    '';
devFrontend = pkgs.writeScriptBin "devFrontend" ''
      
      ${pkgs.process-compose}/bin/process-compose -f ${
      pkgs.writeText "process-compose.yml" (builtins.toJSON {
        version = "0.5";
        processes = {
            frontend = {
              command = "yarn start";
              environment = [ "PATH=${pkgs.yarn}/bin:${
      pkgs.yarn2nix-moretea.mkYarnPackage {
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
            filter = path: type:
              let
            fileName = lastSafe (lib.strings.splitString "/" path);
            in
            fileName == "package.json" || fileName == "yarn.lock";
          }
        );
      }
    }/libexec/${(pkgs.lib.importJSON ./package.json).name}/node_modules/.bin" ];
              availability = { restart = "always"; };
            };
          

            backend = {
              command = "while true; do
echo -e 'HTTP/1.1 200 OK\n\nHello' | nc -w 1 -l 8000
done";
              
              availability = { restart = "always"; };
            };
          };
      })
    }
    '';
fmtFrontend = pkgs.writeScriptBin "fmtFrontend" ''
      export PATH=${pkgs.yarn}/bin:${
      pkgs.yarn2nix-moretea.mkYarnPackage {
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
            filter = path: type:
              let
            fileName = lastSafe (lib.strings.splitString "/" path);
            in
            fileName == "package.json" || fileName == "yarn.lock";
          }
        );
      }
    }/libexec/${(pkgs.lib.importJSON ./package.json).name}/node_modules/.bin:$PATH
      yarn fmt -w src
    '';
frontend = pkgs.stdenv.mkDerivation {
        name = "${(pkgs.lib.importJSON ./package.json).name}";
        src = ./.;
        nativeBuildInputs = [ pkgs.yarn (
      pkgs.runCommand "node-bins" {} ''
        mkdir $out
        ln -s ${
      pkgs.yarn2nix-moretea.mkYarnPackage {
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
            filter = path: type:
              let
            fileName = lastSafe (lib.strings.splitString "/" path);
            in
            fileName == "package.json" || fileName == "yarn.lock";
          }
        );
      }
    }/libexec/${(pkgs.lib.importJSON ./package.json).name}/node_modules/.bin $out/bin
      ''
    ) ];
        buildPhase = ''
          ln -s ${
      pkgs.yarn2nix-moretea.mkYarnPackage {
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
            filter = path: type:
              let
            fileName = lastSafe (lib.strings.splitString "/" path);
            in
            fileName == "package.json" || fileName == "yarn.lock";
          }
        );
      }
    }/libexec/${(pkgs.lib.importJSON ./package.json).name}/node_modules node_modules
          webpack build
        '';
        installPhase = ''
          mkdir $out
          mv dist $out
        '';
      };
frontend__develop = pkgs.writeScriptBin "develop" ''
      export PATH=${pkgs.yarn}/bin:${
      pkgs.yarn2nix-moretea.mkYarnPackage {
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
            filter = path: type:
              let
            fileName = lastSafe (lib.strings.splitString "/" path);
            in
            fileName == "package.json" || fileName == "yarn.lock";
          }
        );
      }
    }/libexec/${(pkgs.lib.importJSON ./package.json).name}/node_modules/.bin:$PATH
      yarn start
    '';
serveFrontend = pkgs.writeScriptBin "serveFrontend" ''
      
      ${pkgs.python3}/bin/python3 -m http.server --directory ${
      pkgs.stdenv.mkDerivation {
        name = "${(pkgs.lib.importJSON ./package.json).name}";
        src = ./.;
        nativeBuildInputs = [ pkgs.yarn (
      pkgs.runCommand "node-bins" {} ''
        mkdir $out
        ln -s ${
      pkgs.yarn2nix-moretea.mkYarnPackage {
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
            filter = path: type:
              let
            fileName = lastSafe (lib.strings.splitString "/" path);
            in
            fileName == "package.json" || fileName == "yarn.lock";
          }
        );
      }
    }/libexec/${(pkgs.lib.importJSON ./package.json).name}/node_modules/.bin $out/bin
      ''
    ) ];
        buildPhase = ''
          ln -s ${
      pkgs.yarn2nix-moretea.mkYarnPackage {
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
            filter = path: type:
              let
            fileName = lastSafe (lib.strings.splitString "/" path);
            in
            fileName == "package.json" || fileName == "yarn.lock";
          }
        );
      }
    }/libexec/${(pkgs.lib.importJSON ./package.json).name}/node_modules node_modules
          webpack build
        '';
        installPhase = ''
          mkdir $out
          mv dist $out
        '';
      }
    }/dist 8000
    '';
        });
    };
}
