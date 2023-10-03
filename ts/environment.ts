import { NewPackage } from "./package.ts";

export const environmentTag = Symbol();

export type Environment = {
  tag: typeof environmentTag;
  nixExpr?: string;
};

export const emptyEnvironment: Environment = {
  tag: environmentTag,
};

export const isEnvironment = (e: unknown): e is Environment => {
  return hasTag(e, environmentTag);
};

const hasTag = (x: unknown, tag: unknown): boolean =>
  typeof x === "object" && x != null && "tag" in x && x.tag === tag;

export const packageToEnvironment = (pkg: NewPackage): Environment => ({
  tag: environmentTag,
  nixExpr: `
    let expr = ${pkg.nixExpr};
    in
      (if expr ? env
        then expr.env
        else pkgs.mkShell { inputsFrom = [ expr ]; }
      )
    `,
});
