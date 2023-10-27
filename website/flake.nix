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
          "website_node_modules" =
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
          "website_tsc" =
            let
              dev =
                (
                  (pkgs.mkShell { }).overrideAttrs (finalAttrs: previousAttrs: {
                    nativeBuildInputs =
                      previousAttrs.nativeBuildInputs
                      ++
                      [ pkgs.nodejs-18_x ];
                  })
                ).overrideAttrs (finalAttrs: previousAttrs: {
                  nativeBuildInputs =
                    previousAttrs.nativeBuildInputs
                    ++
                    [ pkgs.nodePackages.typescript-language-server pkgs.nodePackages.prettier ];
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
      chmod -R u+rwX node_modules
    "}
      ${"npm run tsc"}
    "
          ;
          "website_fmt-check" =
            let
              dev =
                (
                  (pkgs.mkShell { }).overrideAttrs (finalAttrs: previousAttrs: {
                    nativeBuildInputs =
                      previousAttrs.nativeBuildInputs
                      ++
                      [ pkgs.nodejs-18_x ];
                  })
                ).overrideAttrs (finalAttrs: previousAttrs: {
                  nativeBuildInputs =
                    previousAttrs.nativeBuildInputs
                    ++
                    [ pkgs.nodePackages.typescript-language-server pkgs.nodePackages.prettier ];
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
      chmod -R u+rwX node_modules
    "}
      ${"prettier src/**/*.tsx src/**/*.ts --check"}
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
          "website" =
            (
              (pkgs.mkShell { }).overrideAttrs (finalAttrs: previousAttrs: {
                nativeBuildInputs =
                  previousAttrs.nativeBuildInputs
                  ++
                  [ pkgs.nodejs-18_x ];
              })
            ).overrideAttrs (finalAttrs: previousAttrs: {
              nativeBuildInputs =
                previousAttrs.nativeBuildInputs
                ++
                [ pkgs.nodePackages.typescript-language-server pkgs.nodePackages.prettier ];
            })
          ;
        }
      );
      apps = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" { inherit system; };
        in
        {
          "build" = {
            "type" = "app";
            "program" = "${
      let
        dev = 
        (
        (pkgs.mkShell {}).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            [pkgs.nodejs-18_x];
        })
      ).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            [pkgs.nodePackages.typescript-language-server pkgs.nodePackages.prettier];
        })
      ;
        shell = "npm install ; npm run build";
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
          "dev" = {
            "type" = "app";
            "program" = "${
      let
        dev = 
        (
        (pkgs.mkShell {}).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            [pkgs.nodejs-18_x];
        })
      ).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            [pkgs.nodePackages.typescript-language-server pkgs.nodePackages.prettier];
        })
      ;
        shell = "npm install ; npm run dev";
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
          "edit" = {
            "type" = "app";
            "program" = "${
      let
        dev = pkgs.mkShell {};
        shell = "
    set -euo pipefail

    TEMP_DIR=\$(mktemp -d --suffix garn-edit)

    # copy the vscodium config
    cp -r ${
      let dev = pkgs.mkShell {}; in
      pkgs.runCommand "garn-pkg" {
        buildInputs = dev.buildInputs ++ dev.nativeBuildInputs;
      } "
      #!\${pkgs.bash}/bin/bash
      mkdir \$out
      
      ${"
    USER_CONFIG=.config/VSCodium/User
    if test \$(uname) = \"Darwin\" ; then
      USER_CONFIG=\"Library/Application Support/VSCodium/User\"
    fi
    mkdir -p \"\$out/\$USER_CONFIG/User\"
    cp ${
  pkgs.writeTextFile {
    name = "vscodium-config";
    text = "{
  \"deno.enable\": true,
  \"deno.unstable\": true,
  \"typescript.validate.enable\": false,
  \"git.openRepositoryInParentFolders\": \"never\",
  \"editor.formatOnSave\": true,
  \"[typescript]\": {
    \"editor.defaultFormatter\": \"denoland.vscode-deno\"
  },
  \"update.mode\": \"none\"
}";
  }
} \"\$out/\$USER_CONFIG/settings.json\"
    mkdir -p \"\$out/\$USER_CONFIG/globalStorage\"
    cp ${
      let dev = pkgs.mkShell {}; in
      pkgs.runCommand "garn-pkg" {
        buildInputs = dev.buildInputs ++ dev.nativeBuildInputs;
      } "
      #!\${pkgs.bash}/bin/bash
      mkdir \$out
      
      ${"
    set -euo pipefail
    cat ${
  pkgs.writeTextFile {
    name = "sqlite-script";
    text = "PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
CREATE TABLE ItemTable (key TEXT UNIQUE ON CONFLICT REPLACE, value BLOB);
INSERT INTO ItemTable VALUES('denoland.vscode-deno','{\"deno.welcomeShown\":true}');
COMMIT;";
  }
} | ${pkgs.sqlite}/bin/sqlite3 \$out/state.vscdb
  "}
    "
    }/state.vscdb \"\$out/\$USER_CONFIG/globalStorage/state.vscdb\"
  "}
    "
    }/. \$TEMP_DIR
    chmod -R u+rwX \$TEMP_DIR

    # copy the deno cache
    DENO_CACHE=.cache/deno
    if test \$(uname) = \"Darwin\" ; then
      DENO_CACHE=\"Library/Caches/deno\"
    fi
    if test -e \$HOME/\$DENO_CACHE; then
      mkdir -p \$(dirname \$TEMP_DIR/\$DENO_CACHE)
      cp -r \$HOME/\$DENO_CACHE \$TEMP_DIR/\$DENO_CACHE
    fi

    export HOME=\$TEMP_DIR
    export XDG_CONFIG_HOME=\$TEMP_DIR/.config
    export XDG_CACHE_HOME=\$TEMP_DIR/.cache

    ${
  (pkgs.vscode-with-extensions.override
    {
      vscode = pkgs.vscodium;
      vscodeExtensions = [
        pkgs.vscode-extensions.denoland.vscode-deno
      ];
    }
  )
}/bin/codium --new-window --disable-workspace-trust ./garn.ts --wait

    rm -rf \$TEMP_DIR
  ";
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
          "website/fmt" = {
            "type" = "app";
            "program" = "${
      let
        dev = 
        (
        (pkgs.mkShell {}).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            [pkgs.nodejs-18_x];
        })
      ).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            [pkgs.nodePackages.typescript-language-server pkgs.nodePackages.prettier];
        })
      ;
        shell = "prettier src/**/*.tsx src/**/*.ts --write";
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
