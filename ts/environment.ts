import { Package } from "./package.ts";
import { hasTag, nixSource } from "./internal/utils.ts";
import { Check } from "./check.ts";
import { Executable } from "./executable.ts";
import {
  Interpolatable,
  nixList,
  NixExpression,
  nixRaw,
  nixStrLit,
} from "./nix.ts";

export type Environment = {
  tag: "environment";
  nixExpression: NixExpression;
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
  nixExpression = nixRaw`pkgs.mkShell {}`,
  src?: string
): Environment => ({
  tag: "environment",
  nixExpression,
  check(this, s, ...args): Check {
    const checkScript = nixStrLit(s, ...args);
    if (src == null) {
      const wrappedScript = nixStrLit`
        touch $out
        ${checkScript}
      `;
      return {
        tag: "check",
        nixExpression: nixRaw`
          let
              dev = ${this.nixExpression};
          in
          pkgs.runCommand "check" {
            buildInputs = dev.buildInputs ++ dev.nativeBuildInputs;
          } ${wrappedScript}
        `,
      };
    } else {
      const wrappedScript = nixStrLit`
        touch $out
        cp -r ${nixRaw("src")} src
        cd src
        ${checkScript}
      `;
      return {
        tag: "check",
        nixExpression: nixRaw`
          let
              src = ${nixSource(src)};
              dev = ${this.nixExpression};
          in
          pkgs.runCommand "check" {
            buildInputs = dev.buildInputs ++ dev.nativeBuildInputs;
          } ${wrappedScript}
        `,
      };
    }
  },
  shell(this, s, ...args) {
    const cmdToExecute = nixStrLit(s, ...args);
    const shellEnv = nixRaw`
      let
        dev = ${this.nixExpression};
        shell = ${cmdToExecute};
        buildPath = pkgs.runCommand "build-inputs-path" {
          inherit (dev) buildInputs nativeBuildInputs;
        } "echo $PATH > $out";
      in
      pkgs.writeScript "shell-env"  ''
        #!\${pkgs.bash}/bin/bash
        export PATH=$(cat \${buildPath}):$PATH
        \${dev.shellHook}
        \${shell} "$@"
      ''
    `;
    return {
      tag: "executable",
      description: `Executes ${cmdToExecute.rawNixExpressionString}`,
      nixExpression: nixStrLit`${shellEnv}`,
    };
  },
  withDevTools(this, extraDevTools) {
    return {
      ...this,
      nixExpression: nixRaw`
        (${this.nixExpression}).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            ${nixList(extraDevTools.map((pkg) => pkg.nixExpression))};
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
    nixRaw`
    let expr = ${pkg.nixExpression};
    in
      (if expr ? env
        then expr.env
        else pkgs.mkShell { inputsFrom = [ expr ]; }
      )
    `,
    src
  );
