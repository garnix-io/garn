import { Check } from "../check.ts";
import { Executable } from "../executable.ts";
import { NixExpression, escapeShellArg, nixRaw, nixStrLit } from "../nix.ts";
import { Package } from "../package.ts";
import { Plugin } from "../project.ts";

const mkConfigFile = (config: Record<string, unknown>): NixExpression =>
  nixRaw`(pkgs.writeText "prettier-config" ${nixStrLit(
    JSON.stringify(config),
  )})`;

/**
 * A garn plugin that adds a `format` `Executable` that formats your code using
 * prettier, and a `Check` to verify that your code is formatted.
 *
 * @param opts.glob An optional glob pattern to limit what to format/check
 *                  relative to the src of the containing project
 * @param opts.config An optional prettier configuration
 *
 * It will use the version of prettier defined in your package.json, otherwise
 * if that is not configured, it will fallback to using prettier from nixpkgs.
 *
 * Example:
 * ```typescript
 * export const myProject = mkNpmProject({ ... })
 *   .add(garn.javascript.prettier({
 *     glob: "./src",
 *     config: {
 *       trailingComma: "es5",
 *       tabWidth: 4,
 *       semi: false,
 *       singleQuote: true,
 *     },
 *   }));
 * ```
 *
 * Then running `garn run myProject.format` will format formattable files under
 * `src` with the given config. Running `garn check myProject` will error if
 * any files under `src` need to be formatted.
 */
export function plugin(
  opts: {
    glob?: string;
    config?: Record<string, unknown>;
  } = {},
): Plugin<
  { format: Executable; checkPrettier: Check },
  { node_modules?: Package }
> {
  return (base) => {
    if (base.defaultEnvironment == null) {
      throw new Error(
        "The 'garn.javascript.prettier' plugin can only be added to projects with a default environment.",
      );
    }
    const src = nixStrLit(opts.glob ?? ".");
    const config = opts.config
      ? nixStrLit`--config ${escapeShellArg(mkConfigFile(opts.config))}`
      : nixStrLit``;
    const setup = base.node_modules
      ? nixStrLit`
          set -e
          export PATH=${base.node_modules}/bin:$PATH
          function runPrettier {
            if ${nixRaw`pkgs.which`}/bin/which prettier > /dev/null; then
              prettier "$@"
            else
              ${nixRaw`pkgs.nodePackages.prettier`}/bin/prettier "$@"
            fi
          }
        `
      : nixStrLit`
          function runPrettier {
            ${nixRaw`pkgs.nodePackages.prettier`} "$@"
          }
        `;
    return {
      format: base.defaultEnvironment.shell`
        ${setup}
        runPrettier --write ${config} ${escapeShellArg(src)}
      `.setDescription("Uses prettier to format your project"),

      checkPrettier: base.defaultEnvironment.check`
        ${setup}
        runPrettier --check ${config} ${escapeShellArg(src)} | cat
      `,
    };
  };
}
