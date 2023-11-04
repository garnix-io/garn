{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/6fc7203e423bbf1c8f84cccf1c4818d097612566";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.gomod2nix-repo.url = "github:nix-community/gomod2nix?rev=f95720e89af6165c8c0aa77f180461fe786f3c21";
  inputs.npmlock2nix-repo = {
    url = "github:nix-community/npmlock2nix?rev=9197bbf397d76059a76310523d45df10d2e4ca81";
    flake = false;
  };
  outputs = { self, nixpkgs, flake-utils, npmlock2nix-repo, gomod2nix-repo }:
    let
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
          "bundle_package" =
            let
              dev = (pkgs.mkShell { }).overrideAttrs (finalAttrs: previousAttrs: {
                nativeBuildInputs =
                  previousAttrs.nativeBuildInputs
                  ++
                  [ pkgs.nodejs-18_x ];
              });
            in
            pkgs.runCommand "garn-pkg"
              {
                buildInputs = dev.buildInputs ++ dev.nativeBuildInputs;
              } "
      #!\${pkgs.bash}/bin/bash
      mkdir \$out
      ${"
      echo copying source
      cp -r ${(let
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
    })} src
      chmod -R u+rwX src
      cd src
      echo copying node_modules
      cp -r ${let
      npmlock2nix = import npmlock2nix-repo {
        inherit pkgs;
      };
      pkgs = import "${nixpkgs}" {
        config.permittedInsecurePackages = [];
        inherit system;
      };
    in
    npmlock2nix.v2.node_modules
      {
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
        nodejs = pkgs.nodejs-18_x;
      }}/node_modules .
      chmod -R u+rwX node_modules
    "}
      ${"
      npm run build
      cp -rv build/* \$out
    "}
    ";
          "main_node_modules" =
            let
              npmlock2nix = import npmlock2nix-repo {
                inherit pkgs;
              };
              pkgs = import "${nixpkgs}" {
                config.permittedInsecurePackages = [ ];
                inherit system;
              };
            in
            npmlock2nix.v2.node_modules
              {
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
                nodejs = pkgs.nodejs-18_x;
              };
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
          "main" = (pkgs.mkShell { }).overrideAttrs (finalAttrs: previousAttrs: {
            nativeBuildInputs =
              previousAttrs.nativeBuildInputs
              ++
              [ pkgs.nodejs-18_x ];
          });
        }
      );
      apps = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" { inherit system; };
        in
        {
          "start" = {
            "type" = "app";
            "program" = "${let
        dev = (pkgs.mkShell {}).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            [pkgs.nodejs-18_x];
        });
        shell = "npm install && npm start";
        buildPath = pkgs.runCommand "build-inputs-path" {
          inherit (dev) buildInputs nativeBuildInputs;
        } "echo $PATH > $out";
      in
      pkgs.writeScript "shell-env"  ''
        #!${pkgs.bash}/bin/bash
        export PATH=$(cat ${buildPath}):$PATH
        ${dev.shellHook}
        ${shell} "$@"
      ''}";
          };
        }
      );
    };
}
