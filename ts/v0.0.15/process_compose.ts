import { emptyEnvironment } from "./environment.ts";
import { Executable } from "./executable.ts";
import { mapValues } from "./internal/utils.ts";
import { nixAttrSet, nixList, nixRaw, nixStrLit } from "./nix.ts";
import { mkPackage } from "./package.ts";

/**
 * Creates an executable project that runs all specified executables
 * simultaneously using `process-compose`.
 *
 * This can be useful for starting multiple long-running processes during
 * development, e.g. a backend server and a database.
 */
export function processCompose(
  executables: Record<string, Executable>,
): Executable {
  const processes = nixAttrSet(
    mapValues(
      (executable) =>
        nixAttrSet({
          command: executable.nixExpression,
          environment: nixList([]),
        }),
      executables,
    ),
  );

  const processComposeConfig = nixAttrSet({
    version: nixStrLit`0.5`,
    processes,
  });

  const configYml = mkPackage(
    nixRaw`pkgs.writeText "process-compose.yml" (builtins.toJSON ${processComposeConfig})`,
  );

  const result = emptyEnvironment.shell`${nixRaw`pkgs.process-compose`}/bin/process-compose up -f ${configYml}`;
  result.description = `processCompose(${Object.keys(executables).join(", ")})`;
  return result;
}
