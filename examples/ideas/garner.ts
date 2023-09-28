import { Formattable, Project, composeChecks } from "../../ts/base.ts";
import * as garner from "../../ts/mod.ts";
import {
  processCompose,
  runParallel,
  runSequential,
} from "../../ts/runnable.ts";

// Allows to run:
//   garner run backend prod
//   garner run backend dev
//   garner ci backend (or garner check backend?)
//   garner format backend
export const backend = garner.go
  .mkGoProject({
    description: "my nice go project",
    src: "backend",
  })
  .addCheck(
    "todos",
    `
      !(grep TODO -r backend)
    `
  );

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

const allProjects = [backend, frontend, etlJob];

// garner run format
export const format = runSequential(
  ...allProjects.map((sub: Formattable) => sub.runnables.format)
);

export const allChecks = composeChecks(
  ...allProjects.flatMap((p: Project) => Object.values(p.checks))
);

// todo:
//   - default exports?
//   - command line flags for runnables?
//   - helper functions with dot notation
//   - Do we need a Project type? Or a package type?
