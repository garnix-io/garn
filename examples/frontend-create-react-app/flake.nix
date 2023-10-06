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
          main_pkg =
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
                        filter = path: type:
                          let
                            fileName = lastSafe (lib.strings.splitString "/" path);
                          in
                          fileName != "flake.nix";
                      }
                  )
                ;
                buildCommands = [ "npm run test -- --watchAll=false" "mkdir $out" ];
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
          main =
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
                      filter = path: type:
                        let
                          fileName = lastSafe (lib.strings.splitString "/" path);
                        in
                        fileName != "flake.nix";
                    }
                )
              ;
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

          main = {
            type = "app";
            program = "${
          let dev = 
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
      filter = path: type:
        let
          fileName = lastSafe (lib.strings.splitString "/" path);
        in
         fileName != "flake.nix";
    })
;
        node_modules_attrs = {
          nodejs = pkgs.nodejs-18_x;
        };
      }
    ; in
          pkgs.runCommand "shell-env" {
            buildInputs = dev.buildInputs;
            nativeBuildInputs = dev.nativeBuildInputs;
          } ''
            echo "export PATH=$PATH:$PATH" > $out
            echo ${pkgs.lib.strings.escapeShellArg dev.shellHook} >> $out
            echo ${pkgs.lib.strings.escapeShellArg "npm run start"} >> $out
            chmod +x $out
          ''
        }";
          };

        });
    };
}
