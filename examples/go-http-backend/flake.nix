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
          server_pkg =
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
              pname = "go-http-backend";
              version = "0.1";
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
              modules = gomod2nix-toml;
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
          server =
            (
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
                    pname = "go-http-backend";
                    version = "0.1";
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
        });
      apps = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" { inherit system; };
        in
        {

          server = {
            type = "app";
            program = "${
        let
          dev = pkgs.mkShell {};
          shell = "${
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
          pname = "go-http-backend";
          version = "0.1";
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
          modules = gomod2nix-toml;
        }
    }/bin/go-http-backend";
        in
        pkgs.runCommand "shell-env" {
          buildInputs = dev.buildInputs;
          nativeBuildInputs = dev.nativeBuildInputs;
        } ''
          echo "export PATH=$PATH:$PATH" > $out
          echo ${pkgs.lib.strings.escapeShellArg dev.shellHook} >> $out
          echo ${pkgs.lib.strings.escapeShellArg shell} >> $out
          chmod +x $out
        ''
      }";
          };

        });
    };
}
