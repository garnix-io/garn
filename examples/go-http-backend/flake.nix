{
  inputs.nixpkgs-repo.url = "github:NixOS/nixpkgs/6fc7203e423bbf1c8f84cccf1c4818d097612566";
  inputs.gomod2nix-repo.url = "github:nix-community/gomod2nix?rev=f95720e89af6165c8c0aa77f180461fe786f3c21";
  outputs = { self, nixpkgs-repo, gomod2nix-repo }:
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
          "server_pkg" =
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
              modules = gomod2nix-toml;
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
          "server" = (
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
              [ pkgs.gopls ];
          });
        }
      );
      apps = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" { inherit system; };
        in
        {
          "server/migrate" = {
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
      path = ./.;
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
            [pkgs.gopls];
        });
        shell = "go run ./scripts/migrate.go";
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
          "server/dev" = {
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
      path = ./.;
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
            [pkgs.gopls];
        });
        shell = "go run ./main.go";
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
