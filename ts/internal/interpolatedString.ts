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
export type InterpolatedString<T> = {
  initial: string;
  rest: Array<[T, string]>;
};

/**
 * Converts tag template function parameters into `InterpolatedString`s
 */
export function interpolatedStringFromTemplate<T>(
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
export function interpolatedStringFromString(
  s: string,
): InterpolatedString<never> {
  return {
    initial: s,
    rest: [],
  };
}

export function renderInterpolatedString<T>(
  interpolated: InterpolatedString<T>,
  render: (t: T) => string,
): string {
  return interpolated.rest.reduce(
    (acc, [node, str]) => acc + render(node) + str,
    interpolated.initial,
  );
}

export function mapStrings<T>(
  f: (s: string) => string,
  interpolated: InterpolatedString<T>,
): InterpolatedString<T> {
  return {
    initial: f(interpolated.initial),
    rest: interpolated.rest.map(([node, str]) => [node, f(str)]),
  };
}

export function getInterpolations<T>(
  interpolated: InterpolatedString<T>,
): Array<T> {
  return interpolated.rest.map(([node, _str]) => node);
}

export function join<T>(arr: Array<T>, joiner: string): InterpolatedString<T> {
  return {
    initial: "",
    rest: arr.map((el, idx) => [el, idx < arr.length - 1 ? joiner : ""]),
  };
}
