import { describe, it } from "https://deno.land/std@0.206.0/testing/bdd.ts";
import { assertEquals } from "https://deno.land/std@0.206.0/assert/mod.ts";
import * as garn from "./mod.ts";
import {
  testPkgs,
  assertSuccess,
  runExecutable,
  assertStdout,
  runCheck,
  printOutputOnFailure,
  buildPackage,
} from "./testUtils.ts";
import { assertStringIncludes } from "https://deno.land/std@0.206.0/assert/assert_string_includes.ts";
import { addToSetup } from "./environment.ts";

describe("environments", () => {
  it("allows creating Executables from shell snippets", () => {
    const env = garn.emptyEnvironment;
    const output = assertSuccess(runExecutable(env.shell("echo foo")));
    assertStdout(output, "foo\n");
  });

  it("allows adding tools", () => {
    const env = garn.emptyEnvironment.withDevTools([testPkgs.hello]);
    const output = assertSuccess(runExecutable(env.shell("hello")));
    assertStdout(output, "Hello, world!\n");
  });

  it("allows creating Checks from shell snippets", () => {
    const env = garn.emptyEnvironment;
    const output = runCheck(env.check("echo test check output ; exit 1"));
    assertEquals(output.exitCode, 1);
    assertStdout(output, "");
    printOutputOnFailure(output, () =>
      assertStringIncludes(output.stderr, "test check output"),
    );
  });

  describe("addToSetup", () => {
    describe('type = "sandbox"', () => {
      it("allows adding scripts snippets to the sandbox setup", () => {
        const env = addToSetup(
          "sandbox",
          garn.emptyEnvironment,
          'FOO="env var value"',
        );
        const path = buildPackage(env.build("echo $FOO > $out/artifact"));
        assertEquals(
          Deno.readTextFileSync(`${path}/artifact`),
          "env var value\n",
        );
        assertSuccess(runCheck(env.check('test -n "$FOO"')));
      });

      it("doesn't affect the underlying environment", () => {
        const original = addToSetup(
          "sandbox",
          garn.emptyEnvironment,
          'FOO="original"',
        );
        const modified = addToSetup("sandbox", original, 'FOO="modified"');
        let path = buildPackage(original.build("echo $FOO > $out/artifact"));
        assertEquals(Deno.readTextFileSync(`${path}/artifact`), "original\n");
        path = buildPackage(modified.build("echo $FOO > $out/artifact"));
        assertEquals(Deno.readTextFileSync(`${path}/artifact`), "modified\n");
      });

      it("does not affect Executables", () => {
        const env = addToSetup(
          "sandbox",
          garn.emptyEnvironment,
          'FOO="env var value"',
        );
        const output = assertSuccess(
          runExecutable(env.shell('echo executable: "$FOO"')),
        );
        assertStdout(output, "executable: \n");
      });
    });

    describe('type = "common"', () => {
      it("allows adding scripts snippets to the setup for Packages, Checks & Executables", () => {
        const env = addToSetup(
          "common",
          garn.emptyEnvironment,
          'FOO="env var value"',
        );
        const path = buildPackage(env.build("echo $FOO > $out/artifact"));
        assertEquals(
          Deno.readTextFileSync(`${path}/artifact`),
          "env var value\n",
        );
        assertSuccess(runCheck(env.check('test -n "$FOO"')));
        const output = assertSuccess(
          runExecutable(env.shell('echo executable: "$FOO"')),
        );
        assertStdout(output, "executable: env var value\n");
      });

      it("doesn't affect the underlying environment", () => {
        const original = addToSetup(
          "common",
          garn.emptyEnvironment,
          'FOO="original"',
        );
        const modified = addToSetup("common", original, 'FOO="modified"');
        assertStdout(
          assertSuccess(runExecutable(original.shell("echo $FOO"))),
          "original\n",
        );
        assertStdout(
          assertSuccess(runExecutable(modified.shell("echo $FOO"))),
          "modified\n",
        );
      });
    });
  });

  describe("environments with source files", () => {
    it("allows accessing source files in Checks", () => {
      const src = Deno.makeTempDirSync();
      Deno.writeTextFileSync(`${src}/file`, "test source file");
      const env = garn.mkEnvironment({ src: "." });
      const output = assertSuccess(
        runCheck(
          env.check(`
            # ${Date.now()}
            cat file
          `),
          { dir: src },
        ),
      );
      printOutputOnFailure(output, () => {
        assertStringIncludes(output.stderr, "copying source\n");
        assertStringIncludes(output.stderr, "test source file\n");
      });
    });

    it("allows accessing source files in Packages", () => {
      const src = Deno.makeTempDirSync();
      Deno.writeTextFileSync(`${src}/file`, "test source file");
      const env = garn.mkEnvironment({ src: "." });
      const output = buildPackage(
        env.build(`
            echo -n built: >> $out/artifact
            cat file >> $out/artifact
          `),
        { dir: src },
      );
      assertEquals(
        Deno.readTextFileSync(`${output}/artifact`),
        "built:test source file",
      );
    });

    it("allows accessing hidden source files in Packages", () => {
      const src = Deno.makeTempDirSync();
      Deno.writeTextFileSync(`${src}/.file`, "hidden source file");
      const env = garn.mkEnvironment({ src: "." });
      const output = buildPackage(
        env.build(`
            echo -n built: >> $out/artifact
            cat .file >> $out/artifact
          `),
        { dir: src },
      );
      assertEquals(
        Deno.readTextFileSync(`${output}/artifact`),
        "built:hidden source file",
      );
    });

    it("does not allow accessing source files outside of the source directory", () => {
      const src = Deno.makeTempDirSync();
      Deno.mkdirSync(`${src}/subdir`);
      Deno.writeTextFileSync(
        `${src}/subdir/file`,
        "test source file in subdir",
      );
      Deno.writeTextFileSync(`${src}/file`, "test source file in parent dir");
      const env = garn.mkEnvironment({ src: "./subdir" });
      const output = buildPackage(
        env.build(`
            echo -n built: >> $out/artifact
            cat file >> $out/artifact
            cat ../file 2>> $out/error || true
          `),
        { dir: src },
      );
      assertEquals(
        Deno.readTextFileSync(`${output}/artifact`),
        "built:test source file in subdir",
      );
      assertEquals(
        Deno.readTextFileSync(`${output}/error`),
        "cat: ../file: No such file or directory\n",
      );
    });
  });
});
