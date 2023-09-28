import {
  Formattable,
  ProdServer,
  Project,
  mkCheck,
  mkProject,
} from "./base.ts";

type MkHaskellArgs = {
  description: string;
  src: string;
};

export const mkHaskell = (
  _args: MkHaskellArgs
): Project & ProdServer & Formattable => {
  const project = mkProject();
  // deno-lint-ignore no-unused-vars
  const { dev, ...withoutDev } = project.runnables;
  return {
    ...project,
    runnables: withoutDev,
    checks: {
      "cabal-tests": mkCheck(),
    },
  };
};
