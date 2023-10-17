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
          "backend_pkg" =
            let
              gomod2nix = gomod2nix-repo.legacyPackages.${system};
              gomod2nix-toml = pkgs.writeText "gomod2nix-toml" "schema = 3

[mod]
";
            in
            gomod2nix.buildGoApplication {
              pname = "backend";
              version = "0.1";
              go = pkgs.go_1_20;
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
                      path = ././backend;
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
              modules = gomod2nix-toml;
            }
          ;
          "frontend_pkg" =
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
                        path = ././frontend;
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
          "frontend_test" =
            let
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
                      path = ././frontend;
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
              dev =
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
                          path = ././frontend;
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
            in
            pkgs.runCommand "check"
              {
                buildInputs = dev.buildInputs ++ dev.nativeBuildInputs;
              } "
        touch \$out
        cp -r ${src} src
        cd src
        ${"npm test"}
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
          "backend" =
            (
              let
                expr =
                  let
                    gomod2nix = gomod2nix-repo.legacyPackages.${system};
                    gomod2nix-toml = pkgs.writeText "gomod2nix-toml" "schema = 3

[mod]
";
                  in
                  gomod2nix.buildGoApplication {
                    pname = "backend";
                    version = "0.1";
                    go = pkgs.go_1_20;
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
                            path = ././backend;
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
                    modules = gomod2nix-toml;
                  }
                ;
              in
              (if expr ? env
              then expr.env
              else pkgs.mkShell { inputsFrom = [ expr ]; }
              )
            ).overrideAttrs (finalAttrs: previousAttrs: {
              nativeBuildInputs =
                previousAttrs.nativeBuildInputs
                ++
                [ pkgs.gopls ];
            })
          ;
          "deno" =
            (pkgs.mkShell { }).overrideAttrs (finalAttrs: previousAttrs: {
              nativeBuildInputs =
                previousAttrs.nativeBuildInputs
                ++
                [ pkgs.deno ];
            })
          ;
          "frontend" =
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
                      path = ././frontend;
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
        }
      );
      apps = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" { inherit system; };
        in
        {
          "backend" = {
            "type" = "app";
            "program" = "${
      let
        dev = pkgs.mkShell {};
        shell = "${
      let
        gomod2nix = gomod2nix-repo.legacyPackages.${system};
        gomod2nix-toml = pkgs.writeText "gomod2nix-toml" "schema = 3

[mod]
";
      in
        gomod2nix.buildGoApplication {
          pname = "backend";
          version = "0.1";
          go = pkgs.go_1_20;
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
      path = ././backend;
      name = "source";
      filter = path: type:
        let
          fileName = lastSafe (lib.strings.splitString "/" path);
        in
         fileName != "flake.nix" &&
         fileName != "garn.ts";
    })
;
          modules = gomod2nix-toml;
        }
    }/bin/backend";
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
          "frontend" = {
            "type" = "app";
            "program" = "${
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
      path = ././frontend;
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
        shell = "cd ./frontend && npm start";
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
