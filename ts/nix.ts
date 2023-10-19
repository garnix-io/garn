import { assertEquals } from "https://deno.land/std@0.201.0/assert/mod.ts";

export type NixStrLitInterpolatable =
  | string
  | NixExpression
  | {
      nixExpression: NixExpression;
    };

export type NixExpression = { rawNixExpressionString: string };

export function nixRaw(
  s: TemplateStringsArray,
  ...interpolations: Array<NixExpression>
): NixExpression;
export function nixRaw(s: string): NixExpression;
export function nixRaw(
  s: TemplateStringsArray | string,
  ...interpolations: Array<NixExpression>
): NixExpression {
  if (typeof s === "string") return { rawNixExpressionString: s };
  const rawNixExpressionString = s.reduce(
    (acc, part, i) =>
      acc + part + (interpolations[i]?.rawNixExpressionString ?? ""),
    ""
  );
  return { rawNixExpressionString };
}

export function nixList(elements: Array<NixExpression>): NixExpression {
  return nixRaw(
    "[" + elements.map((p) => p.rawNixExpressionString.trim()).join(" ") + "]"
  );
}

export function nixAttrSet(
  attrSet: Record<string, NixExpression | undefined>
): NixExpression {
  return nixRaw(
    "{" +
      Object.entries(attrSet)
        .filter((x): x is [string, NixExpression] => x[1] != null)
        .map(([k, v]) => nixRaw`${nixStrLit(k)} = ${v};`.rawNixExpressionString)
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
  ...interpolations: Array<NixStrLitInterpolatable>
): NixExpression;
export function nixStrLit(s: string): NixExpression;
export function nixStrLit(
  s: TemplateStringsArray | string,
  ...interpolations: Array<NixStrLitInterpolatable>
): NixExpression {
  if (typeof s === "string") return nixStrLit`${s}`;
  const escape = (str: string) =>
    ["\\", '"', "$"].reduce(
      (str, char) => str.replaceAll(char, `\\${char}`),
      str
    );
  const escapedTemplate = s.map(escape);
  const rawNixExpressionString =
    '"' +
    escapedTemplate.reduce((acc, part, i) => {
      const interpolation = interpolations[i];
      switch (typeof interpolation) {
        case "string":
          return acc + part + escape(interpolation);
        case "object":
          if ("rawNixExpressionString" in interpolation) {
            return (
              acc + part + "${" + interpolation.rawNixExpressionString + "}"
            );
          }
          return (
            acc +
            part +
            "${" +
            interpolation.nixExpression.rawNixExpressionString +
            "}"
          );
        case "undefined":
          return acc + part;
      }
    }, "") +
    '"';
  return { rawNixExpressionString };
}

Deno.test("nixStrLit correctly serializes into a nix expression", () => {
  assertEquals(nixStrLit`foo`.rawNixExpressionString, '"foo"');
  assertEquals(
    nixStrLit`with ${"string"} interpolation`.rawNixExpressionString,
    '"with string interpolation"'
  );
  assertEquals(
    nixStrLit`with package ${{
      rawNixExpressionString: "pkgs.hello",
    }} works`.rawNixExpressionString,
    '"with package ${pkgs.hello} works"'
  );
  assertEquals(
    nixStrLit`escaped dollars in strings \${should not interpolate}`
      .rawNixExpressionString,
    '"escaped dollars in strings \\${should not interpolate}"'
  );
  assertEquals(
    nixStrLit`"double quotes" are correctly escaped`.rawNixExpressionString,
    '"\\"double quotes\\" are correctly escaped"'
  );
});
