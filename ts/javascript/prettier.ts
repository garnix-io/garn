import { Check } from "../check.ts";
import { Executable } from "../executable.ts";
import { escapeShellArg, nixRaw, nixStrLit } from "../nix.ts";
import { Package } from "../package.ts";
import { Plugin } from "../project.ts";

export function plugin(
  source?: string,
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
    const src = nixStrLit(source ?? ".");
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
        runPrettier --write ${escapeShellArg(src)}
      `.setDescription("Uses prettier to format your project"),

      checkPrettier: base.defaultEnvironment.check`
        ${setup}
        cd ${nixRaw`./.`}
        runPrettier --check ${escapeShellArg(src)} | cat
      `,
    };
  };
}
