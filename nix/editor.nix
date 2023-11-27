let
  pkgs = import
    (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/a78de4f116747585ebdb6f3d725268a7e068c554.tar.gz";
      sha256 = "14zicikllc86aqyj7swaa3nsbnpr4lmyxm8ldsqrr7slg44s2nsh";
    })
    { };
  settingsFile = pkgs.writeText "vscode-settings.json" ''
    {
     "deno.enable": true,
     "deno.unstable": true,
     "typescript.validate.enable": false,
     "git.openRepositoryInParentFolders": "never",
     "editor.formatOnSave": true,
     "[typescript]": {
       "editor.defaultFormatter": "denoland.vscode-deno",
     },
     "update.mode": "none"
    }
  '';
  sqliteScript = pkgs.writeText "sqliteScript" ''
    PRAGMA foreign_keys=OFF;
    BEGIN TRANSACTION;
    CREATE TABLE ItemTable (key TEXT UNIQUE ON CONFLICT REPLACE, value BLOB);
    INSERT INTO ItemTable VALUES('denoland.vscode-deno','{"deno.welcomeShown":true}');
    COMMIT;
  '';
  vscodium =
    pkgs.vscode-with-extensions.override
      {
        vscode = pkgs.vscodium;
        vscodeExtensions = [
          pkgs.vscode-extensions.denoland.vscode-deno
        ];
      }
  ;

  configDir = pkgs.runCommand "vscodium-config" { }
    ''
      USER_CONFIG=.config/VSCodium/User
      if test $(uname) = "Darwin" ; then
        USER_CONFIG="Library/Application Support/VSCodium/User"
      fi
      mkdir -p "$out/$USER_CONFIG/User"
      cp ${settingsFile} "$out/$USER_CONFIG/settings.json"
      mkdir -p "$out/$USER_CONFIG/globalStorage"
      cat ${sqliteScript} | ${pkgs.sqlite}/bin/sqlite3 "$out/$USER_CONFIG/globalStorage/state.vscdb"
    '';
in
pkgs.writeShellScriptBin "vscodium" ''
  set -euo pipefail

  TEMP_DIR=$(mktemp -d --suffix garn-edit)

  # copy the vscodium config
  cp -r ${configDir}/. $TEMP_DIR
  chmod -R u+rwX $TEMP_DIR

  # copy the deno cache
  DENO_CACHE=.cache/deno
  if test $(uname) = "Darwin" ; then
    DENO_CACHE="Library/Caches/deno"
  fi
  if test -e $HOME/$DENO_CACHE; then
    mkdir -p $(dirname $TEMP_DIR/$DENO_CACHE)
    cp -r $HOME/$DENO_CACHE $TEMP_DIR/$DENO_CACHE
  fi

  export HOME=$TEMP_DIR
  export XDG_CONFIG_HOME=$TEMP_DIR/.config
  export XDG_CACHE_HOME=$TEMP_DIR/.cache

  ${vscodium}/bin/codium --new-window --disable-workspace-trust ./garn.ts --wait "$@"

  rm -rf $TEMP_DIR
'';
