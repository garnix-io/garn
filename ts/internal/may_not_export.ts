const MAY_NOT_EXPORT = Symbol();

/**
 * Mark a value with a symbol to annotate that exporting it from a `garn.ts`
 * file is almost certainly a mistake. Exporting anything with this mark will
 * cause garn to throw an error to alert the user of their mistake.
 *
 * The reason for this is to catch potential mistakes like the following:
 * ```typescript
 * export const myProject = getProjectSomehow().addCheck("my-test")
 * ```
 *
 * The above example is missing the call on the tag template literal function
 * returned by `addCheck`.
 */
export function markAsMayNotExprt(
  // deno-lint-ignore ban-types
  value: object,
  reason: (exportName: string) => string
) {
  // @ts-expect-error - SAFETY: typescript does not allow setting
  // `MAY_NOT_EXPORT` on T here. However, it is safe to set arbitrary keys on
  // non-primative types which `value: object` enforces
  value[MAY_NOT_EXPORT] = reason;
}

/**
 * Throws a runtime error if the passed value has been marked as may not export
 * using `markAsMayNotExprt`.
 */
export function assertMayExport(exportName: string, value: unknown) {
  if (
    value != null &&
    (typeof value === "function" || typeof value === "object") &&
    MAY_NOT_EXPORT in value
  ) {
    const getReason = value[MAY_NOT_EXPORT] as (exportName: string) => string;
    throw new Error(getReason(exportName));
  }
}
