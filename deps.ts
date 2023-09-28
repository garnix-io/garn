export type Artifact = {
  cmd: Runnable;
  artifacts: Array<string>;
  artifact(s: string): string;
  bin(s: string): string;
};

export type Runnable = "runnable";

export type Package = {
  withTasks<T extends Package, E extends Record<string, Runnable>>(
    this: T,
    extend: E | ((s: T) => E),
  ): T & E;
  withPackageJsonScripts<T extends Package, ScriptUnion extends string>(
    this: T,
    ...scripts: Array<ScriptUnion>
  ): T & Record<ScriptUnion, Runnable>;
  withArtifacts<T extends Package, NameUnion extends string>(
    this: T,
    extend: Record<NameUnion, Pick<Artifact, "cmd"| "artifacts">>,
  ): T & Record<NameUnion, Artifact>;
};

export const pkgs: Record<string, any> = {} as any;

export const bash = (s: TemplateStringsArray, ...args: Array<any>) => "runnable" as const;

// { [name: string]: Runnable | Check }

// Package & { bundle: Runnable }

export function mkYarn(args: {
  src: string;
}): Package {
  throw "huh";
}
