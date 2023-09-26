// * core nix package for projects. Tests have to be moved into checks.

type Package = { outPath: string };

const fromNixExpression = (expr: string): Package => {
  throw new Error(`not yet implemented`);
};

// * Runnables (aka flake apps)

type Runnable = Package & { mainExecutable: string };

const writeScriptBin = (name: string, script: string): Runnable => {
  const { outPath } = fromNixExpression("pkgs.writeScriptBin ....");
  return { outPath, mainExecutable: name };
};

writeScriptBin(
  "someName",
  shell`${otherPackageA}/bin/fancyTool ${otherPackageB}/index.html`
);

type RecursiveRunnable =
  | Runnable
  | Array<RecursiveRunnable>
  | { [key: string]: RecursiveRunnable };

const collectIntoCi = (...runnableOnCi: Array<RecursiveRunnable>): Runnable => {
  throw new Error(`not yet implemented`);
};

// * Checks

type Check = null;

type RecursiveChecks = Check | Array<Check>;

// * Projects

type Project<Runnables extends { [name: string]: Runnable } = {}> = Package & {
  runnables: Runnables;
  checks: Record<string, Check>;
  addScriptCheck: (name: string, script: string) => Project<Runnables>;
  addRunnable: <Name extends string>(
    name: Name,
    script: string
  ) => Project<Runnables & { [name in Name]: Runnable }>;
};

// * composition

const runInParallel = (...runnables: Array<RecursiveRunnable>): Runnable => {
  throw new Error(`not yet implemented`);
};

// * some toolchain specific helper function

const mkCreateReactApp = (_args: {
  description: string;
  src: string;
}): Project<{
  dev: Runnable;
  format: Runnable;
}> => {
  // return {
  //   outPath: "huhu",
  //   runnables: {
  //     dev: null,
  //     format: null,
  //   },
  //   checks: {
  //     unitTests: null,
  //   },
  // };
  throw new Error(`not yet implemented`);
};

// * actual garner config:

export const frontend = mkCreateReactApp({
  description: "my nice yarn project",
  src: ".",
})
  .addScriptCheck(
    "todos not allowed",
    `
    cat **/*.ts | grep TODO
  `
  )
  .addRunnable("foo", "bar");

const bla = frontend.runnables.foo;

// export const foo = mkNpm({ src: "." }).addScriptCheck("npm test");

export const backend = mkHaskellBackend({}).addCheck(`
 cabal test
 `);
