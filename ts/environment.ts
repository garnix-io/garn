import { Package } from "./package.ts";
import { hasTag } from "./utils.ts";
import { Check } from "./check.ts";
import {
  Executable,
  nixStringFromTemplate,
  serializeNixStr,
} from "./executable.ts";

export type Environment = {
  tag: "environment";
  nixExpr?: string;
  withDevTools(devTools: Array<Package>): Environment;
  shell(_s: TemplateStringsArray, ..._args: Array<string>): Executable;
  check(_s: TemplateStringsArray, ..._args: Array<string>): Check;
};

export const emptyEnvironment: Environment = {
  tag: "environment",
  check(this) {
    throw new Error(`not yet implemented`);
  },
  shell(this) {
    throw new Error(`not yet implemented`);
  },
  withDevTools(this) {
    throw new Error(`not yet implemented`);
  },
};

export const mkEnvironment = (nixExpression: string): Environment => ({
  tag: "environment",
  nixExpr: nixExpression,
  check(this) {
    throw new Error(`not yet implemented`);
  },
  shell(this, s, ...args) {
    if (this.nixExpr == null) {
      return emptyEnvironment.shell(s, ...args);
    } else {
      const shellEnv = {
        nixExpression: `
          let dev = ${this.nixExpr}; in
          pkgs.runCommand "shell-env" {
            buildInputs = dev.buildInputs;
            nativeBuildInputs = dev.nativeBuildInputs;
          } ''
            echo "export PATH=$PATH:\$PATH" > $out
            echo \${pkgs.lib.strings.escapeShellArg dev.shellHook} >> $out
            echo \${pkgs.lib.strings.escapeShellArg ${
              serializeNixStr(nixStringFromTemplate(s, ...args)).nixExpression
            }} >> $out
            chmod +x $out
          ''
        `,
      };
      return {
        tag: "executable",
        description: `Executes TODO`,
        nixExpression: serializeNixStr(nixStringFromTemplate`${shellEnv}`)
          .nixExpression,
      };
    }
  },
  withDevTools(this, extraDevTools) {
    if (this.nixExpr == null) {
      return emptyEnvironment.withDevTools(extraDevTools);
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

export const isEnvironment = (e: unknown): e is Environment => {
  return hasTag(e, "environment");
};

export const packageToEnvironment = (pkg: Package): Environment =>
  mkEnvironment(
    `
    let expr = ${pkg.nixExpression};
    in
      (if expr ? env
        then expr.env
        else pkgs.mkShell { inputsFrom = [ expr ]; }
      )
    `
  );
