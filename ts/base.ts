export type Project = {
  tag: "project";
  runnables: { [name: string]: Runnable };
  checks: { [name: string]: Check };
  addCheck: <T extends Project>(this: T, name: string, script: string) => T;
};

import { Runnable } from "./runnable.ts";

export type DevServer = { runnables: { dev: Runnable } };

export type ProdServer = { runnables: { prod: Runnable } };

export type Formattable = { runnables: { format: Runnable } };

export type Check = { tag: "check" };

export const composeChecks = (..._checks: Array<Check>): Check =>
  (() => {
    throw new Error(`bottom`);
  })();
