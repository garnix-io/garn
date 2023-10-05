import { assertEquals } from "https://deno.land/std@0.201.0/assert/mod.ts";

export type Interpolatable = string | NixExpression;

export type NixExpression = { nixExpression: string };

/**
 * nixStrLit returns a NixExpression which represents a Nix string literal, but
 * with all typescript interpolations properly injected
 */
export const nixStrLit = (
  s: TemplateStringsArray,
  ...interpolations: Array<Interpolatable>
): NixExpression => {
  const escapedTemplate = s.map((part) =>
    ["\\", '"', "$"].reduce(
      (str, char) => str.replaceAll(char, `\\${char}`),
      part
    )
  );
  const nixExpression =
    '"' +
    escapedTemplate.reduce((acc, part, i) => {
      const interpolation = interpolations[i];
      switch (typeof interpolation) {
        case "string":
          return acc + part + interpolation;
        case "object":
          return acc + part + "${" + interpolation.nixExpression + "}";
        case "undefined":
          return acc + part;
      }
    }, "") +
    '"';
  return { nixExpression };
};

Deno.test("serializeNixStr correctly serializees into a nix expression", () => {
  assertEquals(nixStrLit`foo`.nixExpression, '"foo"');
  assertEquals(
    nixStrLit`with ${"string"} interpolation`.nixExpression,
    '"with string interpolation"'
  );
  assertEquals(
    nixStrLit`with package ${{
      nixExpression: "pkgs.hello",
    }} works`.nixExpression,
    '"with package ${pkgs.hello} works"'
  );
  assertEquals(
    nixStrLit`escaped dollars in strings \${should not interpolate}`
      .nixExpression,
    '"escaped dollars in strings \\${should not interpolate}"'
  );
  assertEquals(
    nixStrLit`"double quotes" are correctly escaped`.nixExpression,
    '"\\"double quotes\\" are correctly escaped"'
  );
});
