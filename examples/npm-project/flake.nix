{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/841889913dfd06a70ffb39f603e29e46f45f0c1a";
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
          "project_node_modules" =
            let
              npmlock2nix = import npmlock2nix-repo {
                inherit pkgs;
              };
              pkgs =
                import "${nixpkgs}" {
                  config.permittedInsecurePackages = [ ];
                  inherit system;
                }
              ;
            in
            npmlock2nix.v2.node_modules
              {
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
                        name = "source";
                        filter = path: type:
                          let
                            fileName = lastSafe (lib.strings.splitString "/" path);
                          in
                          fileName != "flake.nix" &&
                          fileName != "garn.ts";
                      }
                  )
                ;
                nodejs = pkgs.nodejs-18_x;
              }
          ;
        }
      );
      checks = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        {
          "project_test" =
            let
              dev =
                (pkgs.mkShell { }).overrideAttrs (finalAttrs: previousAttrs: {
                  nativeBuildInputs =
                    previousAttrs.nativeBuildInputs
                    ++
                    [ pkgs.nodejs-18_x ];
                })
              ;
            in
            pkgs.runCommand "check"
              {
                buildInputs = dev.buildInputs ++ dev.nativeBuildInputs;
              } "
      touch \$out
      ${"
      echo copying source
      cp -r ${
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
      name = "source";
      filter = path: type:
        let
          fileName = lastSafe (lib.strings.splitString "/" path);
        in
         fileName != "flake.nix" &&
         fileName != "garn.ts";
    })
} src
      chmod -R u+rwX src
      cd src
      echo copying node_modules
      cp -r ${
    let
      npmlock2nix = import npmlock2nix-repo {
        inherit pkgs;
      };
      pkgs = 
      import "${nixpkgs}" {
        config.permittedInsecurePackages = [];
        inherit system;
      }
    ;
    in
    npmlock2nix.v2.node_modules
      {
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
      name = "source";
      filter = path: type:
        let
          fileName = lastSafe (lib.strings.splitString "/" path);
        in
         fileName != "flake.nix" &&
         fileName != "garn.ts";
    })
;
        nodejs = pkgs.nodejs-18_x;
      }
  }/node_modules .
    "}
      ${"npm run test"}
    "
          ;
          "project_tsc" =
            let
              dev =
                (pkgs.mkShell { }).overrideAttrs (finalAttrs: previousAttrs: {
                  nativeBuildInputs =
                    previousAttrs.nativeBuildInputs
                    ++
                    [ pkgs.nodejs-18_x ];
                })
              ;
            in
            pkgs.runCommand "check"
              {
                buildInputs = dev.buildInputs ++ dev.nativeBuildInputs;
              } "
      touch \$out
      ${"
      echo copying source
      cp -r ${
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
      name = "source";
      filter = path: type:
        let
          fileName = lastSafe (lib.strings.splitString "/" path);
        in
         fileName != "flake.nix" &&
         fileName != "garn.ts";
    })
} src
      chmod -R u+rwX src
      cd src
      echo copying node_modules
      cp -r ${
    let
      npmlock2nix = import npmlock2nix-repo {
        inherit pkgs;
      };
      pkgs = 
      import "${nixpkgs}" {
        config.permittedInsecurePackages = [];
        inherit system;
      }
    ;
    in
    npmlock2nix.v2.node_modules
      {
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
      name = "source";
      filter = path: type:
        let
          fileName = lastSafe (lib.strings.splitString "/" path);
        in
         fileName != "flake.nix" &&
         fileName != "garn.ts";
    })
;
        nodejs = pkgs.nodejs-18_x;
      }
  }/node_modules .
    "}
      ${"npm run tsc"}
    "
          ;
        }
      );
      devShells = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        {
          "project" =
            (pkgs.mkShell { }).overrideAttrs (finalAttrs: previousAttrs: {
              nativeBuildInputs =
                previousAttrs.nativeBuildInputs
                ++
                [ pkgs.nodejs-18_x ];
            })
          ;
        }
      );
      apps = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" { inherit system; };
        in
        {
          "run" = {
            "type" = "app";
            "program" = "${
      let
        dev = 
        (pkgs.mkShell {}).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            [pkgs.nodejs-18_x];
        })
      ;
        shell = "npm install ; npm run run";
        buildPath = pkgs.runCommand "build-inputs-path" {
          inherit (dev) buildInputs nativeBuildInputs;
        } "echo $PATH > $out";
      in
      pkgs.writeScript "shell-env"  ''
        #!${pkgs.bash}/bin/bash
        export PATH=$(cat ${buildPath}):$PATH
        ${dev.shellHook}
        ${shell} "$@"
      ''
    }";
          };
        }
      );
    };
}
