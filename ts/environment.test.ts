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
} from "./testUtils.ts";
import { assertStringIncludes } from "https://deno.land/std@0.206.0/assert/assert_string_includes.ts";

describe("environments", () => {
  it("allows to create Executables from shell snippets", () => {
    const env = garn.emptyEnvironment;
    const output = assertSuccess(runExecutable(env.shell("echo foo")));
    assertStdout(output, "foo\n");
  });

  it("allows to add tools", () => {
    const env = garn.emptyEnvironment.withDevTools([testPkgs.hello]);
    const output = assertSuccess(runExecutable(env.shell("hello")));
    assertStdout(output, "Hello, world!\n");
  });

  it("allows to create Checks from shell snippets", () => {
    const env = garn.emptyEnvironment;
    const output = runCheck(env.check("echo test check output ; exit 1"));
    assertEquals(output.exitCode, 1);
    assertStdout(output, "");
    assertOnOutput(output, () =>
      assertStringIncludes(output.stderr, "test check output"),
    );
  });
});
