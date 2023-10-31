/**
 * A union of types that are allowed to be interpolated into the `nixStrLit`
 * template literal function. This is also used in some higher level functions, such as
 * `Environment.shell`.
 *
 * `string`s are treated as raw string data to be escaped and concatenated with
 * the rest of the Nix string literal.
 *
 * `NixExpression`s compile into Nix interpolations.
 *
 * `{ nixExpression: NixExpression }` works the same as `NixExpression` - It is
 * added to this union purely as a convenience since it is a super type of many
 * higher level types such as `Package`. This allows interpolating these higher
 * level types directly in `nixStrLit`.
 */
export type NixStrLitInterpolatable =
  | string
  | NixExpression
  | {
      nixExpression: NixExpression;
    };

/**
 * An opaque type representing a Nix expression.
 *
 * It is not advised to construct this type but instead use `nixRaw` or `nixStrLit`.
 *
 * It is not advised to access the `rawNixExpressionString` except within the
 * garn library itself right before constructing the final flake.nix. This way
 * all Nix expressions are contained within this opaque type and remain type safe.
 */
export type NixExpression = { rawNixExpressionString: string };

/**
 * A template literal function to construct `NixExpression`s from raw strings.
 *
 * Example:
 * ```typescript
 * const myNixExpr = nixRaw`
 *   let
 *     x = ${someNixExpr};
 *     y = ${otherNixExpr};
 *   in
 *     x y
 * `;
 * ```
 *
 * It explicitly does not allow interpolating strings since it is not clear
 * what the correct behavior should be. Instead:
 *
 * If the string you want to interpolate is a Nix expression, wrap the string
 * in `nixRaw` before interpolating, or consider using a `NixExpression` type
 * instead of `string`.
 *
 * If the string you want to interpolate is meant to turn into a string in Nix,
 * wrap the string in `nixStrLit` before interpolating.
 */
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
    "",
  );
  return { rawNixExpressionString };
}

/**
 * Turns a javascript array of `NixExpression`s into a Nix list.
 *
 * Example:
 * ```typescript
 * // returns the Nix expression `[ "a" "b" "c" ]`
 * nixList([
 *   nixStrLit`a`,
 *   nixStrLit`b`,
 *   nixStrLit`c`,
 * ])
 * ```
 */
export function nixList(elements: Array<NixExpression>): NixExpression {
  return nixRaw(
    "[" + elements.map((p) => p.rawNixExpressionString.trim()).join(" ") + "]",
  );
}

/**
 * Turns a javascript object of `NixExpression`s into a Nix attribute set.
 *
 * Example:
 * ```typescript
 * // returns the Nix expression `{ "a" = 1; "b" = 2; }`
 * nixAttrSet({
 *   a: nixRaw`1`,
 *   b: nixRaw`2`,
 * })
 * ```
 */
export function nixAttrSet(
  attrSet: Record<string, NixExpression | undefined>,
): NixExpression {
  return nixRaw(
    "{" +
      Object.entries(attrSet)
        .filter((x): x is [string, NixExpression] => x[1] != null)
        .map(([k, v]) => nixRaw`${nixStrLit(k)} = ${v};`.rawNixExpressionString)
        .join("\n") +
      "}",
  );
}

/**
 * Returns a `NixExpression` which represents a Nix string literal, but with
 * all typescript interpolations properly escaped and interpolated.
 *
 * See also `NixStrLitInterpolatable`.
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
      str,
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
