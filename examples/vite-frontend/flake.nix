{
  inputs.nixpkgs-repo.url = "github:NixOS/nixpkgs/6fc7203e423bbf1c8f84cccf1c4818d097612566";
  inputs.npmlock2nix-repo = { url = "github:nix-community/npmlock2nix?rev=9197bbf397d76059a76310523d45df10d2e4ca81"; flake = false; };
  outputs = { self, nixpkgs-repo, npmlock2nix-repo }:
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
          "frontend/node_modules" =
            let
              npmlock2nix = import npmlock2nix-repo {
                inherit pkgs;
              };
              pkgs = import nixpkgs-repo {
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
          "frontend/build" =
            let
              dev = (pkgs.mkShell { }).overrideAttrs (finalAttrs: previousAttrs: {
                nativeBuildInputs =
                  previousAttrs.nativeBuildInputs
                  ++
                  [ (pkgs.nodejs-18_x) ];
              });
            in
            pkgs.runCommand "garn-pkg"
              {
                buildInputs = dev.buildInputs ++ dev.nativeBuildInputs;
              } "${"mkdir -p \$out"}
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
    })}/. .
                chmod -R u+rwX .
              "}
${"
        echo copying node_modules
        cp -r ${let
        npmlock2nix = import npmlock2nix-repo {
          inherit pkgs;
        };
        pkgs = import nixpkgs-repo {
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
      set -eu

      export PATH=${let
        npmlock2nix = import npmlock2nix-repo {
          inherit pkgs;
        };
        pkgs = import nixpkgs-repo {
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
        }}/bin:\$PATH
      if ! ${pkgs.which}/bin/which vite 2> /dev/null; then
        echo 'vite is not a dependency of the project, maybe run:'
        echo '  npm install --save-dev vite'
        exit 1
      fi
      vite build --outDir \$out
    "}";
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
          "frontend" = (pkgs.mkShell { }).overrideAttrs (finalAttrs: previousAttrs: {
            nativeBuildInputs =
              previousAttrs.nativeBuildInputs
              ++
              [ (pkgs.nodejs-18_x) ];
          });
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
          "frontend/dev" = {
            "type" = "app";
            "program" = "${let
        dev = (pkgs.mkShell {}).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            [(pkgs.nodejs-18_x)];
        });
        shell = "${"
      export PATH=${let
        npmlock2nix = import npmlock2nix-repo {
          inherit pkgs;
        };
        pkgs = import nixpkgs-repo {
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
        }}/bin:\$PATH
      vite
    "}";
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
          "frontend/preview" = {
            "type" = "app";
            "program" = "${let
        dev = (pkgs.mkShell {}).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            [(pkgs.nodejs-18_x)];
        });
        shell = "${"
      export PATH=${let
        npmlock2nix = import npmlock2nix-repo {
          inherit pkgs;
        };
        pkgs = import nixpkgs-repo {
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
        }}/bin:\$PATH
      vite preview
    "}";
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
