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

export const shell = (
  s: TemplateStringsArray,
  ...args: Array<Interpolatable>
) => emptyEnvironment.shell(s, ...args);

export const check = (
  s: TemplateStringsArray,
  ...args: Array<Interpolatable>
) => emptyEnvironment.check(s, ...args);

export const mkEnvironment = (
  nixExpression = "pkgs.mkShell {}",
  src?: string
): Environment => ({
  tag: "environment",
  nixExpr: nixExpression,
  check(this, s, ...args): Check {
    const checkScript = nixStrLit(s, ...args);
    if (src == null) {
      const wrappedScript = nixStrLit`
        touch $out
        ${checkScript}
      `;
      return {
        tag: "check",
        nixExpression: `
          let
              dev = ${this.nixExpr};
          in
          pkgs.runCommand "check" {
            buildInputs = dev.buildInputs ++ dev.nativeBuildInputs;
          } ${wrappedScript.nixExpression}
        `,
      };
    } else {
      const wrappedScript = nixStrLit`
        touch $out
        cp -r ${{ nixExpression: "src" }} src
        cd src
        ${checkScript}
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
  },
  withDevTools(this, extraDevTools) {
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
  },
});

export const emptyEnvironment: Environment = mkEnvironment();

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
