import {
  InterpolatedString,
  interpolatedStringFromString,
  interpolatedStringFromTemplate,
  mapStrings,
  renderInterpolatedString,
} from "./internal/interpolatedString.ts";
import { checkExhaustiveness, filterNullValues } from "./internal/utils.ts";

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
 */
export type NixExpression = { [__nixExpressionTag]: null } & (
  | { type: "raw"; raw: InterpolatedString<NixExpression> }
  | { type: "list"; elements: Array<NixExpression> }
  | { type: "attrSet"; elements: Record<string, NixExpression> }
  | { type: "strLit"; str: InterpolatedString<NixExpression> }
);

/**
 * Tag for `NixExpression`.
 *
 * This symbol is deliberately not exported, since you're not supposed to create
 * `NixExpression`s outside of this module. Instead use helper functions from
 * here.
 */
const __nixExpressionTag = Symbol("NixExpression is opaque!");

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
  return {
    [__nixExpressionTag]: null,
    type: "raw",
    raw:
      typeof s === "string"
        ? interpolatedStringFromString(s)
        : interpolatedStringFromTemplate(s, interpolations),
  };
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
  return {
    [__nixExpressionTag]: null,
    type: "list",
    elements: elements,
  };
}

/**
 * Turns a javascript object of `NixExpression`s into a Nix attribute set.
 * Filters out undefined values.
 *
 * Example:
 * ```typescript
 * // returns the Nix expression `{ "a" = 1; "b" = 2; }`
 * nixAttrSet({
 *   a: nixRaw`1`,
 *   b: nixRaw`2`,
 *   c: undefined,
 * })
 * ```
 */
export function nixAttrSet(
  attrSet: Record<string, NixExpression | undefined>,
): NixExpression {
  return {
    [__nixExpressionTag]: null,
    type: "attrSet",
    elements: filterNullValues(attrSet),
  };
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
  if (typeof s === "string") {
    return {
      [__nixExpressionTag]: null,
      type: "strLit",
      str: interpolatedStringFromString(s),
    };
  }
  return {
    [__nixExpressionTag]: null,
    type: "strLit",
    str: interpolatedStringFromTemplate(
      s,
      interpolations.map((interpolation): NixExpression => {
        if (typeof interpolation === "string") {
          return {
            [__nixExpressionTag]: null,
            type: "strLit",
            str: interpolatedStringFromString(interpolation),
          };
        }
        if ("nixExpression" in interpolation) {
          return interpolation.nixExpression;
        }
        return interpolation;
      }),
    ),
  };
}

/**
 * Converts a `NixExpression` to a (hopefully concise) human readable string
 * with incidental dependencies snipped out with "[...]"
 */
export function toHumanReadable(nixExpr: NixExpression): string {
  if (nixExpr.type !== "strLit") {
    return "[...]";
  }
  return renderInterpolatedString(nixExpr.str, toHumanReadable);
}

/**
 * Converts a `NixExpression` to a string representing that Nix expression.
 *
 * It is advised to defer calling this unless actually serializing to disk.
 * This way Nix expressions remain as the type `NixExpression`.
 */
export function renderNixExpression(nixExpr: NixExpression): string {
  switch (nixExpr.type) {
    case "raw":
      return renderInterpolatedString(nixExpr.raw, renderNixExpression).trim();
    case "list":
      return "[" + nixExpr.elements.map(renderNixExpression).join(" ") + "]";
    case "attrSet":
      return (
        "{ " +
        Object.entries(nixExpr.elements)
          .map(
            ([k, v]) =>
              `${renderNixExpression(nixStrLit(k))} = ${renderNixExpression(
                v,
              )};`,
          )
          .join(" ") +
        " }"
      );
    case "strLit": {
      const escape = (str: string) =>
        ["\\", '"', "$"].reduce(
          (str, char) => str.replaceAll(char, `\\${char}`),
          str,
        );
      return (
        '"' +
        renderInterpolatedString(
          mapStrings(escape, nixExpr.str),
          (astNode) => "${" + renderNixExpression(astNode) + "}",
        ) +
        '"'
      );
    }
  }
  checkExhaustiveness(nixExpr);
}
