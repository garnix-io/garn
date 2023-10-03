import { NewPackage } from "./package.ts";
import { hasTag } from "./utils.ts";
import { Check } from "./check.ts";
import { Executable } from "./executable.ts";

export const environmentTag = Symbol();

export type Environment = {
  tag: typeof environmentTag;
  nixExpr?: string;
  withDevTools(devTools: Array<NewPackage>): Environment;
  shell(_s: TemplateStringsArray, ..._args: Array<string>): Executable;
  check(_s: TemplateStringsArray, ..._args: Array<string>): Check;
};

export const emptyEnvironment: Environment = {
  tag: environmentTag,
  check(this) {
    throw 1;
  },
  shell(this) {
    throw 1;
  },
  withDevTools(this) {
    throw 1;
  },
};

export const isEnvironment = (e: unknown): e is Environment => {
  return hasTag(e, environmentTag);
};

export const packageToEnvironment = (pkg: NewPackage): Environment => ({
  tag: environmentTag,
  nixExpr: `
    let expr = ${pkg.nixExpression};
    in
      (if expr ? env
        then expr.env
        else pkgs.mkShell { inputsFrom = [ expr ]; }
      )
    `,
  check(this) {
    throw 1;
  },
  shell(this) {
    throw 1;
  },
  withDevTools(this, extraDevTools) {
    if (this.nixExpr == null) {
      throw new Error(`not yet implemented`);
    } else {
      return {
        ...this,
        nixExpr: `
          (${this.nixExpr}).overrideAttrs (finalAttrs: previousAttrs: {
            nativeBuildInputs =
              previousAttrs.nativeBuildInputs
              ++
              [ ${extraDevTools.map((p) => p.nixExpression).join(" ")} ];
          })
        `,
      };
    }
  },
});
