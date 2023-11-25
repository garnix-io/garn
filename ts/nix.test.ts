import {
  assertEquals,
  assertThrows,
} from "https://deno.land/std@0.201.0/assert/mod.ts";
import {
  nixAttrSet,
  nixList,
  nixRaw,
  nixStrLit,
  toHumanReadable,
  renderNixExpression,
  renderFlakeFile,
  nixFlakeDep,
  joinNixStrings,
} from "./nix.ts";

Deno.test("nixStrLit correctly serializes into a nix expression", () => {
  assertEquals(renderNixExpression(nixStrLit`foo`), '"foo"');
  assertEquals(
    renderNixExpression(nixStrLit`with ${"string"} interpolation`),
    '"with ${"string"} interpolation"',
  );
  assertEquals(
    renderNixExpression(nixStrLit`with package ${nixRaw`pkgs.hello`} works`),
    '"with package ${pkgs.hello} works"',
  );
  assertEquals(
    renderNixExpression(
      nixStrLit`escaped dollars in strings \${should not interpolate}`,
    ),
    '"escaped dollars in strings \\${should not interpolate}"',
  );
  assertEquals(
    renderNixExpression(nixStrLit`"double quotes" are correctly escaped`),
    '"\\"double quotes\\" are correctly escaped"',
  );
});

Deno.test("joinNixStrings joins strings using the given separator", () => {
  assertEquals(renderNixExpression(joinNixStrings("sep", [])), '""');
  assertEquals(
    renderNixExpression(
      joinNixStrings("sep", [nixStrLit`foo ${nixRaw`42`} bar`]),
    ),
    '"${"foo ${42} bar"}"',
  );
  assertEquals(
    renderNixExpression(
      joinNixStrings("sep", [
        nixStrLit`foo ${nixRaw`42`} bar`,
        nixStrLit`baz ${nixRaw`23`} boo`,
      ]),
    ),
    '"${"foo ${42} bar"}sep${"baz ${23} boo"}"',
  );
});

Deno.test(
  "toHumanReadable snips out incedental dependencies in string literals",
  () => {
    assertEquals(toHumanReadable(nixStrLit`foo`), "foo");
    assertEquals(
      toHumanReadable(nixStrLit`foo ${nixRaw`some-nix`} bar`),
      "foo [...] bar",
    );
  },
);

Deno.test("toHumanReadable limits the length of the string", () => {
  assertEquals(
    toHumanReadable(nixStrLit`${"foo".repeat(100)}`),
    "foofoofoofoofoofoofoofoofoo...",
  );
  assertEquals(toHumanReadable(nixStrLit`${"foo".repeat(100)}`).length, 30);
});

Deno.test("nixList", () => {
  assertEquals(
    renderNixExpression(nixList([nixStrLit`a`, nixStrLit`b`, nixStrLit`c`])),
    '["a" "b" "c"]',
  );
});

Deno.test("nixAttrSet", () => {
  assertEquals(
    renderNixExpression(
      nixAttrSet({
        a: nixRaw`1`,
        b: nixRaw`2`,
        c: undefined,
      }),
    ),
    '{ "a" = 1; "b" = 2; }',
  );
});

Deno.test(
  "renderFlakeFile serializes to a nix flake file with specified inputs",
  () => {
    assertEquals(
      ignoreFormatting(
        renderFlakeFile(
          nixAttrSet({
            foo: nixFlakeDep("foo-repo", { url: "http://example.org/foo" }),
            bar: nixFlakeDep("bar-repo", { url: "http://example.org/bar" }),
          }),
        ),
      ),
      ignoreFormatting(`
        {
          inputs.foo-repo.url = "http://example.org/foo";
          inputs.bar-repo.url = "http://example.org/bar";
          outputs = { self, foo-repo, bar-repo }: {
            "foo" = foo-repo;
            "bar" = bar-repo;
          };
        }
      `),
    );
  },
);

Deno.test(
  "renderFlakeFile throws an error if there are duplicate flake names",
  () => {
    assertThrows(
      () => {
        renderFlakeFile(
          nixAttrSet({
            foo: nixFlakeDep("foo-repo", { url: "http://example.org/foo" }),
            bar: nixFlakeDep("foo-repo", { url: "http://example.org/bar" }),
          }),
        );
      },
      Error,
      "Duplicate flake input name: foo-repo",
    );
  },
);

Deno.test("renderFlakeFile allows duplicate flake deps if they match", () => {
  assertEquals(
    ignoreFormatting(
      renderFlakeFile(
        nixAttrSet({
          foo: nixFlakeDep("foo-repo", {
            url: "http://example.org/foo",
            flake: true,
          }),
          bar: nixFlakeDep("foo-repo", {
            url: "http://example.org/foo",
            // flake: true is default
          }),
        }),
      ),
    ),
    ignoreFormatting(`
        {
          inputs.foo-repo.url = "http://example.org/foo";
          outputs = { self, foo-repo }: {
            "foo" = foo-repo;
            "bar" = foo-repo;
          };
        }
      `),
  );
});

const ignoreFormatting = (s: string) => s.replaceAll(/\s/g, "");
