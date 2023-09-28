export type Runnable = { tag: "runnable" };

export const mkRunnable = (): Runnable => ({ tag: "runnable" });

export const processCompose = (..._runnables: Array<Runnable>): Runnable =>
  (() => {
    throw new Error(`bottom`);
  })();

export const runParallel = (..._runnables: Array<Runnable>): Runnable =>
  (() => {
    throw new Error(`bottom`);
  })();

export const runSequential = (..._runnables: Array<Runnable>): Runnable =>
  (() => {
    throw new Error(`bottom`);
  })();
