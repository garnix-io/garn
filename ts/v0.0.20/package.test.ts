import { describe, it } from "https://deno.land/std@0.206.0/testing/bdd.ts";
import { mkPackage } from "./package.ts";
import { nixRaw } from "./nix.ts";
import { assertStdout, assertSuccess, runExecutable } from "./testUtils.ts";

describe("bin", () => {
  it("escapes executable names", () => {
    const pkg = mkPackage(
      nixRaw`pkgs.writeScriptBin "bin with spaces" "echo running bin with spaces"`,
      "pkg",
    );
    const exe = pkg.bin("bin with spaces");
    const output = assertSuccess(runExecutable(exe));
    assertStdout(output, "running bin with spaces\n");
  });
});
