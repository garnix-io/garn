export type NixPackage = {
  subPath(s: string): string;
  bin(s: string): string;
};

const runnableTag = Symbol();
const projectTag = Symbol();
const checkTag = Symbol();

export type Runnable = {
  [runnableTag]: {
    package: NixPackage;
    mainProgram: string;
  };
};
export type Check = {
  [checkTag]: {
    package: NixPackage;
  };
};
export type Project = {
  [projectTag]: {
    // This ends up in devShell in flake.nix
    environmentPackage: NixPackage;
  };

  run: (
    _s: TemplateStringsArray,
    ..._args: Array<NixPackage | Project | string>
  ) => Runnable;

  check: (
    _s: TemplateStringsArray,
    ..._args: Array<NixPackage | Project | string>
  ) => Check;

  // These helper functions are necessary because they influence the dev
  // environment of the project
  withAdditionalDevDependencies<T extends Project>(
    this: T,
    deps: Array<NixPackage>,
  ): T;
  withEnvironment<T extends Project>(this: T, env: Record<string, string>): T;
};

export const pkgs: {
  protoc: NixPackage;
  python3: NixPackage;
  // deno-lint-ignore no-explicit-any
} = null as any;

export const run = (
  _s: TemplateStringsArray,
  ..._args: Array<NixPackage | Project | string>
): Runnable => {
  throw "todo";
};

export const check = (
  _s: TemplateStringsArray,
  ..._args: Array<NixPackage | Project | string>
): Check => {
  throw "todo";
};

export function mkYarn(_args: { src: string }): Project {
  throw "huh";
}

export function checkIdempotent(runnable: Runnable): Check {
  throw "todo";
}

export const mkGoProject = <Bins extends Record<string, string>>(_args: {
  src: string;
  bins: Bins;
}):
  & Project
  & { release: NixPackage; fmt: Runnable; fmtCheck: Check }
  & Record<keyof Bins, Runnable> => {
  throw new Error(`not yet implemented`);
};
