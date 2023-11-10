import { Check } from "./check.ts";
import { Executable } from "./executable.ts";
import { Project } from "./project.ts";

const assertIsCheck = (_c: Check) => {};
const assertIsExecutable = (_e: Executable) => {};

const _testAddCheck = (project: Project) => {
  const p = project
    .addExecutable("unrelated1", "true")
    .addCheck("check", "true")
    .addExecutable("unrelated2", "true");
  assertIsCheck(p.check);
  assertIsExecutable(p.unrelated1);
  assertIsExecutable(p.unrelated2);
  // @ts-expect-error - check should be the actual check now, not the helper
  p.check``;
};

const _testAddCheckTemplate = (project: Project) => {
  const p = project
    .addExecutable("unrelated1", "true")
    .addCheck("check")`true`.addExecutable("unrelated2", "true");
  assertIsCheck(p.check);
  assertIsExecutable(p.unrelated1);
  assertIsExecutable(p.unrelated2);
  // @ts-expect-error - check should be the actual check now, not the helper
  p.check``;
};

const _testAddExecutable = (project: Project) => {
  const p = project
    .addExecutable("unrelated1", "true")
    .addExecutable("shell", "true")
    .addExecutable("unrelated2", "true");
  assertIsExecutable(p.shell);
  assertIsExecutable(p.unrelated1);
  assertIsExecutable(p.unrelated2);
  // @ts-expect-error - shell should be the actual executable now, not the helper
  p.shell``;
};

const _testAddExecutableTemplate = (project: Project) => {
  const p = project
    .addExecutable("unrelated1", "true")
    .addExecutable("shell")`true`.addExecutable("unrelated2", "true");
  assertIsExecutable(p.shell);
  assertIsExecutable(p.unrelated1);
  assertIsExecutable(p.unrelated2);
  // @ts-expect-error - shell should be the actual executable now, not the helper
  p.shell``;
};
