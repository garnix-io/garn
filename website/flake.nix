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
          default_pkg =
            let
              npmlock2nix = import npmlock2nix-repo {
                inherit pkgs;
              };
              pkgs = import "${nixpkgs}" {
                config.permittedInsecurePackages = [ ];
                inherit system;
              };
            in
            npmlock2nix.v2.build
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
                preBuild = ''
                  mkdir fake-home
                  HOME=$(pwd)/fake-home
                '';
                buildCommands = [ "npm test" "mkdir $out" ];
                installPhase = "true";
                node_modules_attrs = {
                  nodejs = pkgs.nodejs-18_x;
                };
              }
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
          default =
            let
              npmlock2nix = import npmlock2nix-repo {
                inherit pkgs;
              };
            in
            npmlock2nix.v2.shell {
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
              node_modules_mode = "copy";
              node_modules_attrs = {
                nodejs = pkgs.nodejs-18_x;
              };
            }
          ;
        });
      apps = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" { inherit system; };
        in
        {

          default = {
            type = "app";
            program = "${
        let
          dev = 
      let
        npmlock2nix = import npmlock2nix-repo {
          inherit pkgs;
        };
      in
      npmlock2nix.v2.shell {
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
        node_modules_mode = "copy";
        node_modules_attrs = {
          nodejs = pkgs.nodejs-18_x;
        };
      }
    ;
          shell = "cd . && npm run dev";
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

        });
    };
}