import {
  getInterpolations,
  InterpolatedString,
  interpolatedStringFromString,
  interpolatedStringFromTemplate,
  join,
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
  | { type: "flakeDep"; name: string; dep: FlakeDep }
);

/**
 * Tag for `NixExpression`.
 *
 * This symbol is deliberately not exported, since you're not supposed to create
 * `NixExpression`s outside of this module. Instead use helper functions from
 * here.
 */
const __nixExpressionTag = Symbol("NixExpression is opaque!");

type FlakeDep = {
  url: string;
  flake?: boolean;
};

function flakeDepEq(a: FlakeDep, b: FlakeDep): boolean {
  if (a.url !== b.url) return false;
  if ((a.flake === false) !== (b.flake === false)) return false;
  return true;
}

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

export function joinNixStrings(
  separator: string,
  list: Array<NixExpression>,
): NixExpression {
  return {
    [__nixExpressionTag]: null,
    type: "strLit",
    str: {
      initial: "",
      rest: list.map((expr, i) => [expr, i < list.length - 1 ? separator : ""]),
    },
  };
}

export function escapeShellArg(shellArg: NixExpression): NixExpression {
  return nixRaw`(pkgs.lib.strings.escapeShellArg ${shellArg})`;
}

export function getPathOrError(
  attrSet: NixExpression,
  path: Array<NixExpression | string>,
  error: NixExpression,
): NixExpression {
  const pathExpr: NixExpression = {
    [__nixExpressionTag]: null,
    type: "raw",
    raw: join(
      path.map((el) => (typeof el === "string" ? nixStrLit(el) : el)),
      ".",
    ),
  };
  return nixRaw`
    let
      x = ${attrSet};
    in
      if x ? ${pathExpr}
      then x.${pathExpr}
      else builtins.throw ${error}
  `;
}

/**
 * Returns a `NixExpression` that renders as an identifier that refers to a
 * flake input. At the same time it registers the flake input as a dependency,
 * so that it'll be included in the inputs of the generated flake file. See
 * `renderFlakeFile` for an example.
 */
export function nixFlakeDep(name: string, dep: FlakeDep): NixExpression {
  if (!name.match(/^[a-zA-Z][a-zA-Z0-9-_]+$/)) {
    throw Error(`flakeDep: "${name}" is not a valid nix variable name`);
  }
  return {
    [__nixExpressionTag]: null,
    type: "flakeDep",
    name,
    dep,
  };
}

/**
 * Converts a `NixExpression` to a (hopefully concise) human readable string
 * with incidental dependencies snipped out with "[...]"
 */
export function toHumanReadable(nixExpr: NixExpression): string {
  function go(nixExpr: NixExpression): string {
    if (nixExpr.type !== "strLit") {
      return "[...]";
    }
    return renderInterpolatedString(nixExpr.str, go);
  }
  let result = go(nixExpr);
  const maxLength = 30;
  if (result.length > maxLength) {
    result = result.slice(0, maxLength - 3) + "...";
  }
  return result;
}

function collectFlakeDeps(nixExpr: NixExpression): Record<string, FlakeDep> {
  const collect = (arr: Array<NixExpression>) =>
    arr.reduce(
      (acc, nixExpr) => {
        const newFlakeInputs = collectFlakeDeps(nixExpr);
        for (const name in newFlakeInputs) {
          if (name in acc && !flakeDepEq(acc[name], newFlakeInputs[name])) {
            throw new Error(`Duplicate flake input name: ${name}`);
          }
        }
        return { ...acc, ...newFlakeInputs };
      },
      {} as Record<string, FlakeDep>,
    );
  switch (nixExpr.type) {
    case "raw":
      return collect(getInterpolations(nixExpr.raw));
    case "list":
      return collect(nixExpr.elements);
    case "attrSet":
      return collect(Object.values(nixExpr.elements));
    case "strLit":
      return collect(getInterpolations(nixExpr.str));
    case "flakeDep":
      return { [nixExpr.name]: nixExpr.dep };
  }
  checkExhaustiveness(nixExpr);
}

/**
 * Renders the nix expression as the `outputs` of a flake file with all flake
 * deps provided as inputs.
 *
 * Example:
 * ```typescript
 * renderFlakeFile(nixAttrSet({
 *   foo: nixFlakeDep("foo-repo", { url: "http://example.org/foo" }),
 *   bar: nixFlakeDep("bar-repo", { url: "http://example.org/bar" }),
 * }))
 *
 * // Returns:
 * // {
 * //   inputs.foo-repo.url = "http://example.org/foo";
 * //   inputs.bar-repo.url = "http://example.org/bar";
 * //   outputs = { self, foo-repo, bar-repo }: {
 * //     "foo" = foo-repo;
 * //     "bar" = bar-repo;
 * //   };
 * // }
 * ```
 */
export function renderFlakeFile(nixExpr: NixExpression): string {
  const flakeDeps = collectFlakeDeps(nixExpr);
  const quoteStr = (s: string) => renderNixExpression(nixStrLit(s));
  // Not using `renderNixExpression(nixAttrSet(...))` to render the flake file
  // since the flake file is a subset of a valid Nix attribute set (e.g. quoted
  // keys are not allowed)
  return (
    "{" +
    Object.entries(flakeDeps)
      .map(([name, dep]) =>
        dep.flake !== false
          ? `inputs.${name}.url = ${quoteStr(dep.url)};`
          : `inputs.${name} = { url = ${quoteStr(dep.url)}; flake = false; };`,
      )
      .join("\n") +
    "\noutputs = { " +
    ["self", ...Object.keys(flakeDeps)].join(", ") +
    " }:" +
    renderNixExpression(nixExpr) +
    ";\n}"
  );
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
    case "flakeDep":
      return nixExpr.name;
  }
  checkExhaustiveness(nixExpr);
}
