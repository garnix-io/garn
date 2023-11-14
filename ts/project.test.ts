import { Check } from "./check.ts";
import { Executable } from "./executable.ts";
import { Plugin, Project } from "./project.ts";
import { describe, it } from "https://deno.land/std@0.206.0/testing/bdd.ts";
import * as garn from "./mod.ts";
import * as nix from "./nix.ts";
import { assertStdout, assertSuccess, runExecutable } from "./testUtils.ts";
import { Package } from "./mod.ts";

const assertTypeIsCheck = (_c: Check) => {};
const assertTypeIsExecutable = (_e: Executable) => {};
const assertTypeIsPackage = (_p: Package) => {};

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

describe("Project.add", () => {
  it("allows adding fields with .add", () => {
    const project = garn
      .mkProject({ description: "" }, {})
      .add((self) => ({ ...self, foo: garn.shell("echo foo") }));
    const output = runExecutable(project.foo);
    assertSuccess(output);
    assertStdout(output, "foo\n");
  });

  it("allows adding fields while referencing the Project", () => {
    const project = garn
      .mkProject(
        { description: "", defaultEnvironment: garn.emptyEnvironment },
        {},
      )
      .withDevTools([garn.mkPackage(nix.nixRaw`pkgs.hello`, "")])
      .add((self) => ({ ...self, foo: self.shell("hello") }));
    const output = runExecutable(project.foo);
    assertSuccess(output);
    assertStdout(output, "Hello, world!\n");
  });

  it("allows splicing in existing fields", () => {
    const project = garn
      .mkProject(
        { description: "", defaultEnvironment: garn.emptyEnvironment },
        {
          package: garn.build`
            mkdir -p $out/bin
            echo 'echo main executable' > $out/bin/main
            chmod +x $out/bin/main
          `,
        },
      )
      .add((self) => ({ ...self, foo: self.shell`${self.package}/bin/main` }));
    const output = runExecutable(project.foo);
    assertSuccess(output);
    assertStdout(output, "main executable\n");
  });

  it("allows bundling multiple changes into plugins", () => {
    const plugin = <P extends garn.Project>(p: P) =>
      p.addExecutable("foo", "echo foo").addExecutable("bar", "echo bar");
    const project = garn
      .mkProject(
        { description: "", defaultEnvironment: garn.emptyEnvironment },
        {},
      )
      .add(plugin);
    let output = runExecutable(project.foo);
    assertSuccess(output);
    assertStdout(output, "foo\n");
    output = runExecutable(project.bar);
    assertSuccess(output);
    assertStdout(output, "bar\n");
  });

  it("allows writing plugins with parameters", () => {
    const plugin =
      <P extends garn.Project>(config: { a: string; b: string }) =>
      (p: P) =>
        p
          .addExecutable("a", `echo ${config.a}`)
          .addExecutable("b", `echo ${config.b}`);
    const project = garn
      .mkProject(
        { description: "", defaultEnvironment: garn.emptyEnvironment },
        {},
      )
      .add(plugin({ a: "foo", b: "bar" }));
    let output = runExecutable(project.a);
    assertSuccess(output);
    assertStdout(output, "foo\n");
    output = runExecutable(project.b);
    assertSuccess(output);
    assertStdout(output, "bar\n");
  });

  it("provides a nice type synonym for plugins that add a field", () => {
    const plugin: Plugin<{ addedField: garn.Package }> = (p) => ({
      ...p,
      addedField: garn.build``,
    });
    const project = garn
      .mkProject(
        { description: "", defaultEnvironment: garn.emptyEnvironment },
        {},
      )
      .addExecutable("foo", "")
      .add(plugin);
    assertTypeIsExecutable(project.foo);
    assertTypeIsPackage(project.addedField);
  });

  it("provides a nice type synonym for plugins that add multiple fields", () => {
    const plugin: Plugin<{ one: garn.Package; two: garn.Check }> = (p) => ({
      ...p,
      one: garn.build``,
      two: garn.check(""),
    });
    const project = garn
      .mkProject(
        { description: "", defaultEnvironment: garn.emptyEnvironment },
        {},
      )
      .addExecutable("foo", "")
      .add(plugin);
    assertTypeIsExecutable(project.foo);
    assertTypeIsPackage(project.one);
    assertTypeIsCheck(project.two);
  });

  it("provides a nice interface for plugins that depend on a non-standard field", () => {
    const plugin: Plugin<{ addedField: Executable }, { dep: Package }> = (
      p,
    ) => ({
      ...p,
      addedField: garn.shell`${p.dep}/bin/whatever`,
    });
    const project = garn
      .mkProject(
        { description: "", defaultEnvironment: garn.emptyEnvironment },
        { dep: garn.build`` },
      )
      .addExecutable("foo", "")
      .add(plugin);
    assertTypeIsExecutable(project.foo);
    assertTypeIsExecutable(project.addedField);
    // @ts-expect-error - `dep` is missing
    () => garn.mkProject({ description: "" }, {}).add(plugin);
  });

  it("allows overwriting fields", () => {
    const plugin: Plugin<{ field: Package }> = (p) => ({
      ...p,
      field: garn.build``,
    });
    const project = garn
      .mkProject(
        { description: "", defaultEnvironment: garn.emptyEnvironment },
        { field: garn.shell("") },
      )
      .addExecutable("foo", "")
      .add(plugin);
    assertTypeIsExecutable(project.foo);
    // @ts-expect-error - should not be an `Executable` anymore
    assertTypeIsExecutable(project.field);
    assertTypeIsPackage(project.field);
  });
});
