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

type NixAst =
  | { type: "raw"; raw: InterpolatedString<NixAst> }
  | { type: "list"; elements: Array<NixAst> }
  | { type: "attrSet"; elements: Record<string, NixAst> }
  | { type: "strLit"; str: InterpolatedString<NixAst> };

/**
 * An opaque type representing a Nix expression.
 *
 * It is not advised to construct this type but instead use `nixRaw` or `nixStrLit`.
 *
 * It is not advised to access `__ast` directly except within this file itself.
 * This way all Nix expressions are contained within this opaque type and
 * remain type safe.
 */
export type NixExpression = { __ast: NixAst };

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
  if (typeof s === "string") {
    return { __ast: { type: "raw", raw: interpolatedStringFromString(s) } };
  }
  return {
    __ast: {
      type: "raw",
      raw: interpolatedStringFromTemplate(
        s,
        interpolations.map((i) => i.__ast),
      ),
    },
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
    __ast: {
      type: "list",
      elements: elements.map((e) => e.__ast),
    },
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
    __ast: {
      type: "attrSet",
      elements: Object.entries(attrSet)
        .filter((x): x is [string, NixExpression] => x[1] != null)
        .reduce(
          (acc, [k, v]) => ({ ...acc, [k]: v.__ast }),
          {} as Record<string, NixAst>,
        ),
    },
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
    return { __ast: { type: "strLit", str: interpolatedStringFromString(s) } };
  }
  return {
    __ast: {
      type: "strLit",
      str: interpolatedStringFromTemplate(
        s,
        interpolations.map((interpolation): NixAst => {
          if (typeof interpolation === "string") {
            return {
              type: "strLit",
              str: interpolatedStringFromString(interpolation),
            };
          }
          if ("__ast" in interpolation) {
            return interpolation.__ast;
          }
          return interpolation.nixExpression.__ast;
        }),
      ),
    },
  };
}

/**
 * Converts a `NixExpression` to a (hopefully concise) human readable string
 * with incidental dependencies snipped out with "[...]"
 */
export function toHumanReadable(nixExpr: NixExpression): string {
  const astToHumanReadable = (node: NixAst): string => {
    if (node.type !== "strLit") {
      return "[...]";
    }
    return renderInterpolatedString(node.str, astToHumanReadable);
  };
  return astToHumanReadable(nixExpr.__ast);
}

/**
 * Converts a `NixExpression` to a string representing that Nix expression.
 *
 * It is advised to defer calling this unless actually serializing to disk.
 * This way Nix expressions remain as the type `NixExpression`.
 */
export function toNixString(nixExpr: NixExpression): string {
  const astToNixString = (node: NixAst): string => {
    switch (node.type) {
      case "raw":
        return renderInterpolatedString(node.raw, astToNixString).trim();
      case "list":
        return (
          "[" + node.elements.map((e) => astToNixString(e)).join(" ") + "]"
        );
      case "attrSet":
        return (
          "{ " +
          Object.entries(node.elements)
            .map(
              ([k, v]) =>
                `${astToNixString(nixStrLit(k).__ast)} = ${astToNixString(v)};`,
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
            mapStrings(escape, node.str),
            (astNode) => "${" + astToNixString(astNode) + "}",
          ) +
          '"'
        );
      }
    }
  };
  return astToNixString(nixExpr.__ast);
}

/**
 * Represents a string with `T`s interspersed throughout the string.
 *
 * This is a more safe data structure than joining a `TemplateStringsArray`
 * with a `T[]` (which is what tag template functions give you) since it
 * enforces that given N strings, there will always be N-1 interpolations.
 *
 * Example:
 * ```typescript
 * // Given:
 * myTemplateFn`foo${1}bar${2}baz`
 *
 * function myTemplateFn(s: TemplateStringsArray, ...i: Array<number>) {
 *   // s is ["foo", "bar", "baz"]
 *   // i is [1, 2]
 *   // but nothing at the type level enforces that s.length - 1 == i.length
 *
 *   // Using `interpolatedStringFromTemplate` we can convert to a safer `InterpolatedString`:
 *   const interpolatedString = interpolatedStringFromTemplate(s, i);
 *   // interpolatedString is { initial: "foo", rest: [[1, "bar"], [2, "baz"]] }
 * }
 * ```
 */
type InterpolatedString<T> = {
  initial: string;
  rest: Array<[T, string]>;
};

/**
 * Converts tag template function parameters into `InterpolatedString`s
 */
function interpolatedStringFromTemplate<T>(
  s: TemplateStringsArray,
  interpolations: Array<T>,
): InterpolatedString<T> {
  return {
    initial: s[0],
    rest: s.slice(1).map((part, i) => [interpolations[i], part]),
  };
}

/**
 * Convenience function to create an `InterpolatedString` with only an
 * `initial` (no interpolations).
 */
function interpolatedStringFromString(s: string): InterpolatedString<never> {
  return {
    initial: s,
    rest: [],
  };
}

function renderInterpolatedString<T>(
  interpolated: InterpolatedString<T>,
  render: (t: T) => string,
): string {
  return interpolated.rest.reduce(
    (acc, [node, str]) => acc + render(node) + str,
    interpolated.initial,
  );
}

function mapStrings<T>(
  f: (s: string) => string,
  interpolated: InterpolatedString<T>,
): InterpolatedString<T> {
  return {
    initial: f(interpolated.initial),
    rest: interpolated.rest.map(([node, str]) => [node, f(str)]),
  };
}
