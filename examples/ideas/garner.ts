import { z } from "https://deno.land/x/zod@v3.16.1/mod.ts";
import { Check, Formattable, collectAllChecks } from "../../ts/base.ts";
import * as garner from "../../ts/mod.ts";
import {
  Runnable,
  processCompose,
  runParallel,
  runSequential,
  withCliArgs,
} from "../../ts/runnable.ts";
import { collectAllLinters } from "./utils.ts";

// Allows to run:
//   garner enter
//   garner run backend prod
//   garner run backend dev
//   garner ci backend (or garner check backend?)
//   garner format backend
export const backend = garner.go
  .mkGoProject({
    description: "my nice go project",
    src: "backend",
    // entryPoints: {
    //   server: "server.go",
    //   protobufGen: "generate.go",
    // },
  })
  // .addRunnable("generate protobuf", mkRunnable())
  .addCheck(
    "todos",
    `
      !(grep TODO -r backend)
    `
  );

const _todosCheck: Check = backend.checks.what;

export const frontend = garner.typescript.mkYarnFrontend({
  description: "react frontend",
  src: "frontend",
});

export const etlJob = garner.haskell.mkHaskell({
  description: "putting some stuff in the db",
  src: "etl-job",
});

// garner run workOnFrontend
export const workOnFrontend = runParallel(
  frontend.runnables.dev,
  backend.runnables.prod
);

// garner run workOnBackend
export const workOnBackend = runParallel(
  frontend.runnables.prod,
  backend.runnables.dev
);

// garner run workOnBoth
export const workOnBoth = runParallel(
  frontend.runnables.dev,
  backend.runnables.dev
);

export const all = processCompose(
  frontend.runnables.prod,
  backend.runnables.prod,
  etlJob.runnables.prod
);

// garner run foo --tmux
// garner run foo --zellij
export const foo = processCompose(
  frontend.runnables.dev,
  backend.runnables.dev,
  etlJob.runnables.prod
);

const allProjects = [backend, frontend, etlJob];
export default allProjects;

// garner run format
export const format = runSequential(
  ...allProjects.map((sub: Formattable) => sub.runnables.format)
);

// garner runn weirdRunnable --flag foo
// garner runn weirdRunnable --flag bar
export const weirdRunnable = withCliArgs(
  z.object({ flag: z.union([z.literal("foo"), z.literal("bar")]) }),
  (args) => {
    if (args.flag === "foo") {
      return backend.runnables.dev;
    } else {
      return frontend.runnables.prod;
    }
  }
);
// or
export const allChecks2 = collectAllChecks(...allProjects);

export const lint: Runnable = processCompose(...collectAllLinters(allProjects));

export const runComposed = withCliArgs(
  z.object({ tmux: z.boolean(), screen: z.boolean() }),
  (_args) =>
    (() => {
      throw new Error(`bottom`);
    })()
);

// todo:
//   - command line flags for runnables?
//   - helper functions with dot notation
//   - Do we need a Project type? Or a package type?
