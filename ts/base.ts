export type Project = {
  tag: "project";
  runnables: { [name: string]: Runnable };
  checks: { [name: string]: Check };
  addCheck: <T extends Project, Name extends string>(
    this: T,
    name: Name,
    script: string
  ) => T & { checks: { "not sure": Check } };
  addRunnable: <T extends Project, Name extends string>(
    this: T,
    name: Name,
    runnable: Runnable
  ) => T;
};

export const mkProject = (): Project &
  DevServer &
  ProdServer &
  Formattable => ({
  tag: "project",
  runnables: {
    dev: mkRunnable(),
    prod: mkRunnable(),
    format: mkRunnable(),
  },
  checks: {},
  addCheck(_name, _script) {
    return { ...this, checks: { ...this.checks, "not sure": mkCheck() } };
  },
  addRunnable<T extends Project, Name extends string>(
    this: T,
    _name: Name,
    _runnable: Runnable
  ) {
    return (() => {
      throw new Error(`bottom`);
    })();
  },
});

import { Runnable, mkRunnable } from "./runnable.ts";

export type DevServer = { runnables: { dev: Runnable } };

export type ProdServer = { runnables: { prod: Runnable } };

export type Formattable = { runnables: { format: Runnable } };

export type Check = { tag: "check" };

export const mkCheck = (): Check => ({ tag: "check" });

export const composeChecks = (..._checks: Array<Check>): Check =>
  (() => {
    throw new Error(`bottom`);
  })();

export const collectAllChecks = (..._projects: Array<Project>): Check =>
  (() => {
    throw new Error(`bottom`);
  })();
