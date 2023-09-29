export type NixPackage = {
  subPath(s: string): string;
  bin(s: string): string;
};

type Artifact = {
  cmd: Script;
  artifacts: Array<string>;
};

export const mkArtifact = (_args: Artifact): NixPackage =>
  (() => {
    throw new Error(`bottom`);
  })();

export type Script = "script";

export type Runnable = ["runnable", Script];

export type Check = ["check", Script];

export const packageTag = Symbol();

export type Project = {
  [packageTag]: {
    environmentPackage: NixPackage;
    mainPackage: NixPackage;
  };
  withTasks<T extends Project, E extends Record<string, Script>>(
    this: T,
    extend: E | ((s: T) => E)
  ): T & E;
  withPackageJsonScripts<T extends Project, ScriptUnion extends string>(
    this: T,
    ...scripts: Array<ScriptUnion>
  ): T & Record<ScriptUnion, Runnable>;
  withArtifacts<T extends Project, NameUnion extends string>(
    this: T,
    extend: Record<NameUnion, Artifact>
  ): T & Record<NameUnion, NixPackage>;
  withCheck<T extends Project, Name extends string>(
    this: T,
    name: Name,
    check: Script
  ): T & Record<Name, Check>;
  withGeneratorCheck<T extends Project, Name extends keyof T & string>(
    this: T,
    name: Name
  ): T & Record<`check-${Name}`, Check>;
};

export const pkgs = {
  protoc: (() => {
    throw new Error(`bottom`);
  })() as NixPackage,
  python3: (() => {
    throw new Error(`bottom`);
  })() as NixPackage,
};

export const bash = (
  _s: TemplateStringsArray,
  ..._args: Array<NixPackage | Project | string>
) => "script" as const;

export function mkYarn(_args: { src: string }): Project {
  throw "huh";
}

export const mkGoProject = (_args: {
  src: string;
  bins: Record<string, string>;
}): Project => {
  throw new Error(`not yet implemented`);
};
