import { z } from "https://deno.land/x/zod@v3.16.1/mod.ts";
import { Check } from "./base.ts";

export type Runnable = { tag: "runnable" };

export const isRunnable = (x: unknown): x is Runnable =>
  typeof x === "object" && x != null && "tag" in x && x.tag === "runnable";

export const mkRunnable = (): Runnable => ({ tag: "runnable" });

export const processCompose = (..._runnables: Array<Runnable>): Runnable =>
  mkRunnable();

export const runParallel = (..._runnables: Array<Runnable>): Runnable =>
  mkRunnable();

export const runSequential = (..._runnables: Array<Runnable>): Runnable =>
  mkRunnable();

// make this polymorphic and work for checks too?
export const withCliArgs = <Args>(
  _argsSchema: z.Schema<Args>,
  _f: (args: Args) => Runnable
): Runnable => {
  return mkRunnable();
};
