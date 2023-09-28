export type Runnable = { tag: "runnable" };

export const mkRunnable = (): Runnable => ({ tag: "runnable" });

export const processCompose = (..._runnables: Array<Runnable>): Runnable =>
  mkRunnable();

export const runParallel = (..._runnables: Array<Runnable>): Runnable =>
  mkRunnable();

export const runSequential = (..._runnables: Array<Runnable>): Runnable =>
  mkRunnable();
