import { isProject } from "../../ts/base.ts";
import { Runnable } from "../../ts/runnable.ts";

type Lintable = { runnables: { lint: Runnable } };

const isLintable = (x: unknown): x is Lintable =>
  isProject(x) && "lint" in x.runnables;

export const collectAllLinters = (...things: Array<unknown>): Array<Runnable> =>
  things.filter(isLintable).map((l) => l.runnables.lint);
