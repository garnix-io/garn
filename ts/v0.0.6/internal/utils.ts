export const nixSource = (src: string) => `
  (let
    lib = pkgs.lib;
    lastSafe = list :
      if lib.lists.length list == 0
        then null
        else lib.lists.last list;
  in
  builtins.path
    {
      path = ./${src};
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
  x: Record<string, T>
): Record<string, T> => {
  const result: Record<string, T> = {};
  for (const [key, value] of Object.entries(x)) {
    result[f(key)] = value;
  }
  return result;
};