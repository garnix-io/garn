import { emptyEnvironment } from "./environment.ts";
import { Executable } from "./executable.ts";
import { mapValues } from "./internal/utils.ts";
import { nixAttrSet, nixList, nixRaw, nixStrLit } from "./nix.ts";
import { mkPackage } from "./package.ts";
import { mkProject } from "./project.ts";

/**
 * processCompose creates an executable project that runs all specified
 * executables simultaneously using `process-compose`.
 */
export const processCompose = (executables: Record<string, Executable>) => {
  const processes = nixAttrSet(
    mapValues(
      (executable) =>
        nixAttrSet({
          command: executable.nixExpression,
          environment: nixList([]),
        }),
      executables
    )
  );

  const processComposeConfig = nixAttrSet({
    version: nixStrLit`0.5`,
    processes,
  });

  const configYml = mkPackage(
    nixRaw`pkgs.writeText "process-compose.yml" (builtins.toJSON ${processComposeConfig})`
  );

  return mkProject(
    {
      description: "",
      defaultEnvironment: emptyEnvironment,
      defaultExecutable: emptyEnvironment.shell`${nixRaw`pkgs.process-compose`}/bin/process-compose up -f ${configYml}`,
    },
    {}
  );
};
