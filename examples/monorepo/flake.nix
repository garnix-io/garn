{
  inputs.nixpkgs-repo.url = "github:NixOS/nixpkgs/6fc7203e423bbf1c8f84cccf1c4818d097612566";
  inputs.gomod2nix-repo.url = "github:nix-community/gomod2nix?rev=f95720e89af6165c8c0aa77f180461fe786f3c21";
  inputs.npmlock2nix-repo = { url = "github:nix-community/npmlock2nix?rev=9197bbf397d76059a76310523d45df10d2e4ca81"; flake = false; };
  outputs = { self, nixpkgs-repo, gomod2nix-repo, npmlock2nix-repo }:
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
          "backend/pkg" =
            let
              gomod2nix = gomod2nix-repo.legacyPackages.${system};
              gomod2nix-toml = pkgs.writeText "gomod2nix-toml" "schema = 3

[mod]
  [mod.\"github.com/gorilla/mux\"]
    version = \"v1.8.0\"
    hash = \"sha256-s905hpzMH9bOLue09E2JmzPXfIS4HhAlgT7g13HCwKE=\"
";
            in
            gomod2nix.buildGoApplication {
              pname = "go-package";
              version = "0.1";
              go = pkgs.go_1_21;
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
                    path = ./backend;
                    name = "source";
                    filter = path: type:
                      let
                        fileName = lastSafe (lib.strings.splitString "/" path);
                      in
                      fileName != "flake.nix" &&
                      fileName != "garn.ts";
                  }
              );
              modules = gomod2nix-toml;
            };
          "haskell/pkg" =
            let
              haskell = pkgs.haskell.packages.ghc94.override {
                overrides = final: prev:
                  { }
                ;
              };
            in
            haskell.callCabal2nix
              "garn-pkg"
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
                    path = ./haskell;
                    name = "source";
                    filter = path: type:
                      let
                        fileName = lastSafe (lib.strings.splitString "/" path);
                      in
                      fileName != "flake.nix" &&
                      fileName != "garn.ts";
                  }
              )
              { };
          "npmFrontend/node_modules" =
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
                      path = ./frontend-npm;
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
          "backend" = (
            let
              expr =
                let
                  gomod2nix = gomod2nix-repo.legacyPackages.${system};
                  gomod2nix-toml = pkgs.writeText "gomod2nix-toml" "schema = 3

[mod]
  [mod.\"github.com/gorilla/mux\"]
    version = \"v1.8.0\"
    hash = \"sha256-s905hpzMH9bOLue09E2JmzPXfIS4HhAlgT7g13HCwKE=\"
";
                in
                gomod2nix.buildGoApplication {
                  pname = "go-package";
                  version = "0.1";
                  go = pkgs.go_1_21;
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
                        path = ./backend;
                        name = "source";
                        filter = path: type:
                          let
                            fileName = lastSafe (lib.strings.splitString "/" path);
                          in
                          fileName != "flake.nix" &&
                          fileName != "garn.ts";
                      }
                  );
                  modules = gomod2nix-toml;
                };
            in
            (if expr ? env
            then expr.env
            else pkgs.mkShell { inputsFrom = [ expr ]; }
            )
          ).overrideAttrs (finalAttrs: previousAttrs: {
            nativeBuildInputs =
              previousAttrs.nativeBuildInputs
              ++
              [ (pkgs.gopls) ];
          });
          "haskell" = (
            let
              expr =
                let
                  haskell = pkgs.haskell.packages.ghc94.override {
                    overrides = final: prev:
                      { }
                    ;
                  };
                in
                haskell.callCabal2nix
                  "garn-pkg"
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
                        path = ./haskell;
                        name = "source";
                        filter = path: type:
                          let
                            fileName = lastSafe (lib.strings.splitString "/" path);
                          in
                          fileName != "flake.nix" &&
                          fileName != "garn.ts";
                      }
                  )
                  { };
            in
            (if expr ? env
            then expr.env
            else pkgs.mkShell { inputsFrom = [ expr ]; }
            )
          ).overrideAttrs (finalAttrs: previousAttrs: {
            nativeBuildInputs =
              previousAttrs.nativeBuildInputs
              ++
              [ (pkgs.haskell.packages.ghc94.cabal-install) ];
          });
          "npmFrontend" = (pkgs.mkShell { }).overrideAttrs (finalAttrs: previousAttrs: {
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
          "backend/run" = {
            "type" = "app";
            "program" = "${let
        dev = (let expr = let
        gomod2nix = gomod2nix-repo.legacyPackages.${system};
        gomod2nix-toml = pkgs.writeText "gomod2nix-toml" "schema = 3

[mod]
  [mod.\"github.com/gorilla/mux\"]
    version = \"v1.8.0\"
    hash = \"sha256-s905hpzMH9bOLue09E2JmzPXfIS4HhAlgT7g13HCwKE=\"
";
      in
        gomod2nix.buildGoApplication {
          pname = "go-package";
          version = "0.1";
          go = pkgs.go_1_21;
          src = (let
    lib = pkgs.lib;
    lastSafe = list :
      if lib.lists.length list == 0
        then null
        else lib.lists.last list;
  in
  builtins.path
    {
      path = ./backend;
      name = "source";
      filter = path: type:
        let
          fileName = lastSafe (lib.strings.splitString "/" path);
        in
         fileName != "flake.nix" &&
         fileName != "garn.ts";
    });
          modules = gomod2nix-toml;
        };
        in
          (if expr ? env
            then expr.env
            else pkgs.mkShell { inputsFrom = [ expr ]; }
          )).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            [(pkgs.gopls)];
        });
        shell = "cd backend && go run ./main.go";
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
          "haskell/hello" = {
            "type" = "app";
            "program" = "${let
        dev = let expr = let
      haskell = pkgs.haskell.packages.ghc94.override {
        overrides = final: prev:
          {  }
        ;
      };
      in haskell.callCabal2nix
        "garn-pkg"
        (let
    lib = pkgs.lib;
    lastSafe = list :
      if lib.lists.length list == 0
        then null
        else lib.lists.last list;
  in
  builtins.path
    {
      path = ./haskell;
      name = "source";
      filter = path: type:
        let
          fileName = lastSafe (lib.strings.splitString "/" path);
        in
         fileName != "flake.nix" &&
         fileName != "garn.ts";
    })
        {};
        in
          (if expr ? env
            then expr.env
            else pkgs.mkShell { inputsFrom = [ expr ]; }
          );
        shell = "${let
      haskell = pkgs.haskell.packages.ghc94.override {
        overrides = final: prev:
          {  }
        ;
      };
      in haskell.callCabal2nix
        "garn-pkg"
        (let
    lib = pkgs.lib;
    lastSafe = list :
      if lib.lists.length list == 0
        then null
        else lib.lists.last list;
  in
  builtins.path
    {
      path = ./haskell;
      name = "source";
      filter = path: type:
        let
          fileName = lastSafe (lib.strings.splitString "/" path);
        in
         fileName != "flake.nix" &&
         fileName != "garn.ts";
    })
        {}}/bin/${"hello"}";
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
          "npmFrontend/run" = {
            "type" = "app";
            "program" = "${let
        dev = (pkgs.mkShell {}).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            [(pkgs.nodejs-18_x)];
        });
        shell = "cd frontend-npm && npm install && npm start";
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
          "startAll" = {
            "type" = "app";
            "program" = "${let
        dev = pkgs.mkShell {};
        shell = "${pkgs.process-compose}/bin/process-compose up -f ${pkgs.writeText "process-compose.yml" (builtins.toJSON { "version" = "0.5"; "processes" = { "backend" = { "command" = "${let
        dev = (let expr = let
        gomod2nix = gomod2nix-repo.legacyPackages.${system};
        gomod2nix-toml = pkgs.writeText "gomod2nix-toml" "schema = 3

[mod]
  [mod.\"github.com/gorilla/mux\"]
    version = \"v1.8.0\"
    hash = \"sha256-s905hpzMH9bOLue09E2JmzPXfIS4HhAlgT7g13HCwKE=\"
";
      in
        gomod2nix.buildGoApplication {
          pname = "go-package";
          version = "0.1";
          go = pkgs.go_1_21;
          src = (let
    lib = pkgs.lib;
    lastSafe = list :
      if lib.lists.length list == 0
        then null
        else lib.lists.last list;
  in
  builtins.path
    {
      path = ./backend;
      name = "source";
      filter = path: type:
        let
          fileName = lastSafe (lib.strings.splitString "/" path);
        in
         fileName != "flake.nix" &&
         fileName != "garn.ts";
    });
          modules = gomod2nix-toml;
        };
        in
          (if expr ? env
            then expr.env
            else pkgs.mkShell { inputsFrom = [ expr ]; }
          )).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            [(pkgs.gopls)];
        });
        shell = "cd backend && go run ./main.go";
        buildPath = pkgs.runCommand "build-inputs-path" {
          inherit (dev) buildInputs nativeBuildInputs;
        } "echo $PATH > $out";
      in
      pkgs.writeScript "shell-env"  ''
        #!${pkgs.bash}/bin/bash
        export PATH=$(cat ${buildPath}):$PATH
        ${dev.shellHook}
        ${shell} "$@"
      ''}"; "environment" = []; }; "haskell" = { "command" = "${let
        dev = let expr = let
      haskell = pkgs.haskell.packages.ghc94.override {
        overrides = final: prev:
          {  }
        ;
      };
      in haskell.callCabal2nix
        "garn-pkg"
        (let
    lib = pkgs.lib;
    lastSafe = list :
      if lib.lists.length list == 0
        then null
        else lib.lists.last list;
  in
  builtins.path
    {
      path = ./haskell;
      name = "source";
      filter = path: type:
        let
          fileName = lastSafe (lib.strings.splitString "/" path);
        in
         fileName != "flake.nix" &&
         fileName != "garn.ts";
    })
        {};
        in
          (if expr ? env
            then expr.env
            else pkgs.mkShell { inputsFrom = [ expr ]; }
          );
        shell = "${let
      haskell = pkgs.haskell.packages.ghc94.override {
        overrides = final: prev:
          {  }
        ;
      };
      in haskell.callCabal2nix
        "garn-pkg"
        (let
    lib = pkgs.lib;
    lastSafe = list :
      if lib.lists.length list == 0
        then null
        else lib.lists.last list;
  in
  builtins.path
    {
      path = ./haskell;
      name = "source";
      filter = path: type:
        let
          fileName = lastSafe (lib.strings.splitString "/" path);
        in
         fileName != "flake.nix" &&
         fileName != "garn.ts";
    })
        {}}/bin/${"hello"}";
        buildPath = pkgs.runCommand "build-inputs-path" {
          inherit (dev) buildInputs nativeBuildInputs;
        } "echo $PATH > $out";
      in
      pkgs.writeScript "shell-env"  ''
        #!${pkgs.bash}/bin/bash
        export PATH=$(cat ${buildPath}):$PATH
        ${dev.shellHook}
        ${shell} "$@"
      ''}"; "environment" = []; }; "frontend" = { "command" = "${let
        dev = (pkgs.mkShell {}).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            [(pkgs.nodejs-18_x)];
        });
        shell = "cd frontend-npm && npm install && npm start";
        buildPath = pkgs.runCommand "build-inputs-path" {
          inherit (dev) buildInputs nativeBuildInputs;
        } "echo $PATH > $out";
      in
      pkgs.writeScript "shell-env"  ''
        #!${pkgs.bash}/bin/bash
        export PATH=$(cat ${buildPath}):$PATH
        ${dev.shellHook}
        ${shell} "$@"
      ''}"; "environment" = []; }; }; })}";
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
