import { NixExpression, nixRaw, nixStrLit } from "../nix.ts";
import { z } from "https://deno.land/x/zod@v3.22.4/mod.ts";

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

export const mapValues = <Obj extends Record<string, unknown>, FnResult>(
  f: (i: Obj[keyof Obj], key: keyof Obj) => FnResult,
  x: Obj,
): { [key in keyof Obj]: FnResult } => {
  const result: Partial<{ [key in keyof Obj]: FnResult }> = {};
  for (const [key, value] of Object.entries(x) as Array<
    [keyof Obj, Obj[keyof Obj]]
  >) {
    result[key] = f(value, key);
  }
  return result as { [key in keyof Obj]: FnResult };
};

export const filterNullValues = <T>(
  x: Record<string, T>,
): Record<string, NonNullable<T>> => {
  const result: Record<string, NonNullable<T>> = {};
  for (const [key, value] of Object.entries(x)) {
    if (value != null) result[key] = value;
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

type Result<T> = { data: T; error?: never } | { error: Error; data?: never };

export const parseJson = <T>(schema: z.Schema<T>, json: string): Result<T> => {
  let parsed: unknown;
  try {
    parsed = JSON.parse(json);
  } catch (error) {
    return { error };
  }
  const result = schema.safeParse(parsed);
  if (!result.success) {
    return { error: result.error };
  }
  return { data: result.data };
};
