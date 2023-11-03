import { assertEquals } from "https://deno.land/std@0.201.0/assert/mod.ts";
import {
  nixAttrSet,
  nixList,
  nixRaw,
  nixStrLit,
  toHumanReadable,
  toNixString,
} from "./nix.ts";

Deno.test("nixStrLit correctly serializes into a nix expression", () => {
  assertEquals(toNixString(nixStrLit`foo`), '"foo"');
  assertEquals(
    toNixString(nixStrLit`with ${"string"} interpolation`),
    '"with ${"string"} interpolation"',
  );
  assertEquals(
    toNixString(nixStrLit`with package ${nixRaw`pkgs.hello`} works`),
    '"with package ${pkgs.hello} works"',
  );
  assertEquals(
    toNixString(
      nixStrLit`escaped dollars in strings \${should not interpolate}`,
    ),
    '"escaped dollars in strings \\${should not interpolate}"',
  );
  assertEquals(
    toNixString(nixStrLit`"double quotes" are correctly escaped`),
    '"\\"double quotes\\" are correctly escaped"',
  );
});

Deno.test("toHumanReadable snips out incedental dependencies in string literals", () => {
  assertEquals(toHumanReadable(nixStrLit`foo`), "foo");
  assertEquals(
    toHumanReadable(nixStrLit`foo ${nixRaw`some-nix`} bar`),
    "foo [...] bar",
  );
});

Deno.test("nixList", () => {
  assertEquals(
    toNixString(nixList([nixStrLit`a`, nixStrLit`b`, nixStrLit`c`])),
    '["a" "b" "c"]',
  );
});

Deno.test("nixAttrSet", () => {
  assertEquals(
    toNixString(nixAttrSet({
      a: nixRaw`1`,
      b: nixRaw`2`,
      c: undefined,
    })),
    '{ "a" = 1; "b" = 2; }',
  );
});
