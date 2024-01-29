import { Package } from "./package.ts";
import { hasTag, nixSource } from "./internal/utils.ts";
import { Check } from "./check.ts";
import { Executable } from "./executable.ts";
import { Interpolatable, nixStrLit } from "./nix.ts";

export type Environment = {
  tag: "environment";
  nixExpr?: string;
  withDevTools(devTools: Array<Package>): Environment;
  shell(_s: TemplateStringsArray, ..._args: Array<Interpolatable>): Executable;
  check(_s: TemplateStringsArray, ..._args: Array<Interpolatable>): Check;
};

export const emptyEnvironment: Environment = {
  tag: "environment",
  check(this) {
    throw new Error(`not yet implemented`);
  },
  shell(this, s, ...args) {
    const shellScript = nixStrLit(s, ...args);
    return {
      tag: "executable",
      description: `Executes ${shellScript}`,
      nixExpression: `
        let
          shell = ${shellScript.nixExpression};
        in
          "\${pkgs.writeScriptBin "executable" shell}/bin/executable"`,
    };
  },
  withDevTools(this) {
    throw new Error(`not yet implemented`);
  },
};

export const shell = (
  s: TemplateStringsArray,
  ...args: Array<Interpolatable>
) => emptyEnvironment.shell(s, ...args);

export const mkEnvironment = (
  nixExpression: string,
  src: string
): Environment => ({
  tag: "environment",
  nixExpr: nixExpression,
  check(this, s, ...args): Check {
    if (this.nixExpr == null) {
      return emptyEnvironment.check(s, ...args);
    } else {
      const innerScript = nixStrLit(s, ...args);
      const wrappedScript = nixStrLit`
        touch $out
        cp -r ${{ nixExpression: "src" }} src
        cd src
        ${innerScript}
      `;
      return {
        tag: "check",
        nixExpression: `
          let
              src = ${nixSource(src)};
              dev = ${this.nixExpr};
          in
          pkgs.runCommand "check" {
            buildInputs = dev.buildInputs ++ dev.nativeBuildInputs;
          } ${wrappedScript.nixExpression}
        `,
      };
    }
  },
  shell(this, s, ...args) {
    if (this.nixExpr == null) {
      return emptyEnvironment.shell(s, ...args);
    } else {
      const cmdToExecute = nixStrLit(s, ...args);
      const shellEnv = {
        nixExpression: `
          let dev = ${this.nixExpr}; in
          pkgs.runCommand "shell-env" {
            buildInputs = dev.buildInputs;
            nativeBuildInputs = dev.nativeBuildInputs;
          } ''
            echo "export PATH=$PATH:\$PATH" > $out
            echo \${pkgs.lib.strings.escapeShellArg dev.shellHook} >> $out
            echo \${pkgs.lib.strings.escapeShellArg ${cmdToExecute.nixExpression}} >> $out
            chmod +x $out
          ''
        `,
      };
      return {
        tag: "executable",
        description: `Executes ${cmdToExecute.nixExpression}`,
        nixExpression: nixStrLit`${shellEnv}`.nixExpression,
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

export const packageToEnvironment = (pkg: Package, src: string): Environment =>
  mkEnvironment(
    `
    let expr = ${pkg.nixExpression};
    in
      (if expr ? env
        then expr.env
        else pkgs.mkShell { inputsFrom = [ expr ]; }
      )
    `,
    src
  );