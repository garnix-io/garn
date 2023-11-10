import { Check } from "./check.ts";
import { Executable } from "./executable.ts";
import { Project } from "./project.ts";
import { assertEquals } from "https://deno.land/std@0.206.0/assert/mod.ts";
import { describe, it } from "https://deno.land/std@0.206.0/testing/bdd.ts";
import * as garn from "./mod.ts";
import * as nix from "./nix.ts";
import { nixAttrSet } from "./nix.ts";

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

describe("Project.add", () => {
  it("allows to add fields with .add", () => {
    const project = garn
      .mkProject({ description: "" }, {})
      .add((self) => ({ ...self, foo: garn.shell("echo foo") }));
    const output = runExecutable(project.foo);
    assertSuccess(output);
    assertStdout(output, "foo\n");
  });

  it("allows to add fields while referencing the Project", () => {
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

  it("allows to splice in existing fields", () => {
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

  it("allows to bundle multiple changes into plugins", () => {
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

  it("allows to write plugins with parameters", () => {
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
});

interface Output {
  exitCode: number;
  stdout: string;
  stderr: string;
}

const printOutput = (output: Output) => {
  console.error(`
    exitcode: ${output.exitCode}
    stdout:
    ${output.stdout}
    stderr:
    ${output.stderr}
  `);
};

const assertSuccess = (output: Output) => {
  try {
    assertEquals(output.exitCode, 0);
  } catch (e) {
    printOutput(output);
    throw e;
  }
};

const assertStdout = (output: Output, expected: string) => {
  try {
    assertEquals(output.stdout, expected);
  } catch (e) {
    printOutput(output);
    throw e;
  }
};

const assertStderr = (output: Output, expected: string) => {
  try {
    assertEquals(output.stderr, expected);
  } catch (e) {
    printOutput(output);
    throw e;
  }
};

const runExecutable = (executable: garn.Executable): Output => {
  const tempDir = Deno.makeTempDirSync({ prefix: "garn-test" });
  const nixpkgsInput = nix.nixFlakeDep("nixpkgs-repo", {
    url: "github:NixOS/nixpkgs/6fc7203e423bbf1c8f84cccf1c4818d097612566",
  });
  const flakeFile = nix.renderFlakeFile(
    nixAttrSet({
      apps: nixAttrSet({
        "x86_64-linux": nixAttrSet({
          default: nixAttrSet({
            type: nix.nixStrLit`app`,
            program: nix.nixRaw`
                  let pkgs = import ${nixpkgsInput} {
                        config.allowUnfree = true;
                        system = "x86_64-linux";
                      };
                  in ${executable.nixExpression}
                `,
          }),
        }),
      }),
    }),
  );
  Deno.writeTextFileSync(`${tempDir}/flake.nix`, flakeFile);
  const output = new Deno.Command("nix", {
    args: ["run", tempDir],
  }).outputSync();
  return {
    exitCode: output.code,
    stdout: new TextDecoder().decode(output.stdout),
    stderr: new TextDecoder().decode(output.stderr),
  };
};
