// deno-lint-ignore-file no-unused-vars
export type Runnable = { tag: "runnable" };

export const processCompose = (...runnables: Array<Runnable>): Runnable =>
  (() => {
    throw new Error(`bottom`);
  })();

export const runParallel = (...runnables: Array<Runnable>): Runnable =>
  (() => {
    throw new Error(`bottom`);
  })();

export const runSequential = (...runnables: Array<Runnable>): Runnable =>
  (() => {
    throw new Error(`bottom`);
  })();
