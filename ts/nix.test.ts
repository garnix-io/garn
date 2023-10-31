import { assertEquals } from "https://deno.land/std@0.201.0/assert/mod.ts";
import { nixStrLit } from "./nix.ts";

Deno.test("nixStrLit correctly serializes into a nix expression", () => {
  assertEquals(nixStrLit`foo`.rawNixExpressionString, '"foo"');
  assertEquals(
    nixStrLit`with ${"string"} interpolation`.rawNixExpressionString,
    '"with string interpolation"',
  );
  assertEquals(
    nixStrLit`with package ${{
      rawNixExpressionString: "pkgs.hello",
    }} works`.rawNixExpressionString,
    '"with package ${pkgs.hello} works"',
  );
  assertEquals(
    nixStrLit`escaped dollars in strings \${should not interpolate}`
      .rawNixExpressionString,
    '"escaped dollars in strings \\${should not interpolate}"',
  );
  assertEquals(
    nixStrLit`"double quotes" are correctly escaped`.rawNixExpressionString,
    '"\\"double quotes\\" are correctly escaped"',
  );
});
