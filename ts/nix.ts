import { assertEquals } from "https://deno.land/std@0.201.0/assert/mod.ts";

export type Interpolatable =
  | string
  | NixExpression
  | {
      nixExpression: NixExpression;
    };

export type NixExpression = { expr: string };

export function nixRaw(
  s: TemplateStringsArray,
  ...interpolations: Array<NixExpression>
): NixExpression;
export function nixRaw(s: string): NixExpression;
export function nixRaw(
  s: TemplateStringsArray | string,
  ...interpolations: Array<NixExpression>
): NixExpression {
  if (typeof s === "string") return { expr: s };
  const expr = s.reduce(
    (acc, part, i) => acc + part + (interpolations[i]?.expr ?? ""),
    ""
  );
  return { expr };
}

export function nixList(elements: Array<NixExpression>): NixExpression {
  return nixRaw("[" + elements.map((p) => p.expr.trim()).join(" ") + "]");
}

export function nixAttrSet(
  attrSet: Record<string, NixExpression | undefined>
): NixExpression {
  return nixRaw(
    "{" +
      Object.entries(attrSet)
        .filter((x): x is [string, NixExpression] => x[1] != null)
        .map(([k, v]) => `${k} = ${v.expr.trim()};`)
        .join("\n") +
      "}"
  );
}

/**
 * nixStrLit returns a NixExpression which represents a Nix string literal, but
 * with all typescript interpolations properly injected
 */
export function nixStrLit(
  s: TemplateStringsArray,
  ...interpolations: Array<Interpolatable>
): NixExpression;
export function nixStrLit(s: string): NixExpression;
export function nixStrLit(
  s: TemplateStringsArray | string,
  ...interpolations: Array<Interpolatable>
): NixExpression {
  if (typeof s === "string") return nixStrLit`${s}`;
  const escape = (str: string) =>
    ["\\", '"', "$"].reduce(
      (str, char) => str.replaceAll(char, `\\${char}`),
      str
    );
  const escapedTemplate = s.map(escape);
  const expr =
    '"' +
    escapedTemplate.reduce((acc, part, i) => {
      const interpolation = interpolations[i];
      switch (typeof interpolation) {
        case "string":
          return acc + part + escape(interpolation);
        case "object":
          if ("expr" in interpolation) {
            return acc + part + "${" + interpolation.expr + "}";
          }
          return acc + part + "${" + interpolation.nixExpression.expr + "}";
        case "undefined":
          return acc + part;
      }
    }, "") +
    '"';
  return { expr };
}

Deno.test("nixStrLit correctly serializes into a nix expression", () => {
  assertEquals(nixStrLit`foo`.expr, '"foo"');
  assertEquals(
    nixStrLit`with ${"string"} interpolation`.expr,
    '"with string interpolation"'
  );
  assertEquals(
    nixStrLit`with package ${{
      expr: "pkgs.hello",
    }} works`.expr,
    '"with package ${pkgs.hello} works"'
  );
  assertEquals(
    nixStrLit`escaped dollars in strings \${should not interpolate}`.expr,
    '"escaped dollars in strings \\${should not interpolate}"'
  );
  assertEquals(
    nixStrLit`"double quotes" are correctly escaped`.expr,
    '"\\"double quotes\\" are correctly escaped"'
  );
});
