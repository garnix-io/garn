import { DevServer, Formattable, ProdServer, Project } from "./base.ts";

type MkHaskellArgs = {
  description: string;
  src: string;
};

export const mkHaskell = (
  _args: MkHaskellArgs
): Project & DevServer & ProdServer & Formattable => {
  return (() => {
    throw new Error(`bottom`);
  })();
};
