import { describe, it } from "https://deno.land/std@0.206.0/testing/bdd.ts";
import { assertEquals } from "https://deno.land/std@0.206.0/assert/mod.ts";
import * as garn from "./mod.ts";
import {
  testPkgs,
  assertSuccess,
  runExecutable,
  assertStdout,
  runCheck,
  assertOnOutput,
  buildPackage,
} from "./testUtils.ts";
import { assertStringIncludes } from "https://deno.land/std@0.206.0/assert/assert_string_includes.ts";

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
    assertOnOutput(output, () =>
      assertStringIncludes(output.stderr, "test check output"),
    );
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
      assertOnOutput(output, () => {
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
  });
});
