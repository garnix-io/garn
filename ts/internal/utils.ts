import { NixExpression, nixRaw, nixStrLit } from "../nix.ts";

export const GARN_TS_LIB_VERSION = "v0.0.13";

export const nixSource = (src: string): NixExpression => nixRaw`
  (let
    lib = pkgs.lib;
    lastSafe = list :
      if lib.lists.length list == 0
        then null
        else lib.lists.last list;
  in
  builtins.path
    {
      path = ./${nixRaw(src)};
      name = "source";
      filter = path: type:
        let
          fileName = lastSafe (lib.strings.splitString "/" path);
        in
         fileName != "flake.nix" &&
         fileName != "garn.ts";
    })
`;

export const dbg = <A>(a: A): A => {
  console.error("DBG =>", a);
  return a;
};

export const hasTag = (x: unknown, tag: unknown): boolean =>
  typeof x === "object" && x != null && "tag" in x && x.tag === tag;

export const mapKeys = <T>(
  f: (i: string) => string,
  x: Record<string, T>,
): Record<string, T> => {
  const result: Record<string, T> = {};
  for (const [key, value] of Object.entries(x)) {
    result[f(key)] = value;
  }
  return result;
};

/**
 * Maps an object's values. Typescript currently does not keep track of the
 * object structure, but this could easily be accomplished in the future in a
 * backwards-compatible way.
 */
export const mapValues = <T, R>(
  f: (i: T) => R,
  x: Record<string, T>,
): Record<string, R> => {
  const result: Record<string, R> = {};
  for (const [key, value] of Object.entries(x)) {
    result[key] = f(value);
  }
  return result;
};

export const checkExhaustiveness = (x: never): never => {
  throw new Error(`Exhaustiveness check failed: ${x}`);
};

export const writeTextFile = (
  name: string,
  text: string,
): NixExpression => nixRaw`
  pkgs.writeTextFile {
    name = ${nixStrLit`${name}`};
    text = ${nixStrLit`${text}`};
  }
`;
