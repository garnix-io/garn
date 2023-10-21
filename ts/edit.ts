import outdent from "https://deno.land/x/outdent@v0.8.0/mod.ts";
import * as garn from "./mod.ts";
import { Package } from "./mod.ts";
import { NixExpression, nixRaw } from "./nix.ts";
import { writeTextFile } from "./internal/utils.ts";

const vscodium: NixExpression = nixRaw`
  (pkgs.vscode-with-extensions.override
    {
      vscode = pkgs.vscodium;
      vscodeExtensions = [
        pkgs.vscode-extensions.denoland.vscode-deno
      ];
    }
  )
`;

/**
 * A settings files that
 *  - enables deno,
 *  - disables the default typescript validator,
 *  - silences a notification popup about a git extension,
 *  - enables format on save,
 *  - disables automatic updates.
 */
const settingsFile = (): NixExpression => {
  const settings: unknown = {
    "deno.enable": true,
    "deno.unstable": true,
    "typescript.validate.enable": false,
    "git.openRepositoryInParentFolders": "never",
    "editor.formatOnSave": true,
    "[typescript]": {
      "editor.defaultFormatter": "denoland.vscode-deno",
    },
    "update.mode": "none",
  };
  return writeTextFile("vscodium-config", JSON.stringify(settings, null, 2));
};

/**
 * A preconfigure state.vscdb sqlite file that will prevent vscodium to open a
 * welcome tab for the deno extension.
 */
const stateFile = (): Package => {
  const sqliteScript = outdent`
    PRAGMA foreign_keys=OFF;
    BEGIN TRANSACTION;
    CREATE TABLE ItemTable (key TEXT UNIQUE ON CONFLICT REPLACE, value BLOB);
    INSERT INTO ItemTable VALUES('denoland.vscode-deno','{"deno.welcomeShown":true}');
    COMMIT;
  `;
  const sqliteScriptFile: NixExpression = writeTextFile(
    "sqlite-script",
    sqliteScript
  );
  return garn.build`
    set -euo pipefail
    cat ${sqliteScriptFile} | ${nixRaw`pkgs.sqlite`}/bin/sqlite3 $out/state.vscdb
  `;
};

/**
 * This is a fake home directory that contains preconfigured vscodium settings files,
 * to have a nice experience when editing garn.ts files.
 */
const configDir = (): Package => {
  return garn.build`
    USER_CONFIG=.config/VSCodium/User
    if test $(uname) = "Darwin" ; then
      USER_CONFIG="Library/Application Support/VSCodium/User"
    fi
    mkdir -p "$out/$USER_CONFIG/User"
    cp ${settingsFile()} "$out/$USER_CONFIG/settings.json"
    mkdir -p "$out/$USER_CONFIG/globalStorage"
    cp ${stateFile()}/state.vscdb "$out/$USER_CONFIG/globalStorage/state.vscdb"
  `;
};

/**
 * This is an `Executable` that will open a new vscodium editor that is
 * preconfigured to provide a pleasant editing experience for garn.ts files.
 *
 * The vscodium instance will *not* read or write to your vscodium configuration
 * in your home directory, (if that exists).
 */
export const editGarnConfig: garn.Executable = (() => {
  const result = garn.shell`
    set -euo pipefail

    TEMP_DIR=$(mktemp -d --suffix garn-edit)

    # copy the vscodium config
    cp -r ${configDir()}/. $TEMP_DIR
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

    ${vscodium}/bin/codium --new-window --disable-workspace-trust ./garn.ts --wait

    rm -rf $TEMP_DIR
  `;
  result.description = "edit your garn.ts in a preconfigured vscodium editor";
  return result;
})();
