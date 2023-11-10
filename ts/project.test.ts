import { Check } from "./check.ts";
import { Executable } from "./executable.ts";
import { Project } from "./project.ts";

const assertTypeIsCheck = (_c: Check) => {};
const assertTypeIsExecutable = (_e: Executable) => {};

const _testTypeCheckingOfAddCheck = (project: Project) => {
  const p = project
    .addExecutable("unrelated1", "true")
    .addCheck("check", "true")
    .addExecutable("unrelated2", "true");
  assertTypeIsCheck(p.check);
  assertTypeIsExecutable(p.unrelated1);
  assertTypeIsExecutable(p.unrelated2);
  // @ts-expect-error - check should be the actual check now, not the helper
  p.check``;
};

const _testTypeCheckingOfAddCheckTemplate = (project: Project) => {
  const p = project
    .addExecutable("unrelated1", "true")
    .addCheck("check")`true`.addExecutable("unrelated2", "true");
  assertTypeIsCheck(p.check);
  assertTypeIsExecutable(p.unrelated1);
  assertTypeIsExecutable(p.unrelated2);
  // @ts-expect-error - check should be the actual check now, not the helper
  p.check``;
};

const _testTypeCheckingOfAddExecutable = (project: Project) => {
  const p = project
    .addExecutable("unrelated1", "true")
    .addExecutable("shell", "true")
    .addExecutable("unrelated2", "true");
  assertTypeIsExecutable(p.shell);
  assertTypeIsExecutable(p.unrelated1);
  assertTypeIsExecutable(p.unrelated2);
  // @ts-expect-error - shell should be the actual executable now, not the helper
  p.shell``;
};

const _testTypeCheckingOfAddExecutableTemplate = (project: Project) => {
  const p = project
    .addExecutable("unrelated1", "true")
    .addExecutable("shell")`true`.addExecutable("unrelated2", "true");
  assertTypeIsExecutable(p.shell);
  assertTypeIsExecutable(p.unrelated1);
  assertTypeIsExecutable(p.unrelated2);
  // @ts-expect-error - shell should be the actual executable now, not the helper
  p.shell``;
};
