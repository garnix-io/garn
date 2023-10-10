import * as path from "https://deno.land/std@0.202.0/path/mod.ts";

let DOT_GARN_DIR: string | null = null;

/*
 * Returns the path to the `.garn` directory by walking up the tree looking for
 * it, or creating one as a sibling to `garn.ts` or `.git`
 */
export const getDotGarnDir = () => {
  if (DOT_GARN_DIR) return DOT_GARN_DIR;
  const garnRoot =
    findFirstParentContaining(".garn") ??
    findFirstParentContaining("garn.ts") ??
    findFirstParentContaining(".git") ??
    Deno.cwd();

  const dotGarnDir = path.join(garnRoot, ".garn");
  mkdirp(dotGarnDir);
  return (DOT_GARN_DIR = dotGarnDir);
};

/**
 * Returns a directory within `.garn` for the specified source. For example if
 * a project has source `./foo` this will return `/path/to/.garn/foo` to allow
 * caching of files related to that source.
 */
export const getDotGarnProjectDir = (src: string) => {
  const dir = path.normalize(path.join(getDotGarnDir(), src));
  mkdirp(dir);
  return dir;
};

const findFirstParentContaining = (target: string): string | null => {
  for (
    let dir = Deno.cwd();
    dir !== "/";
    dir = path.normalize(path.join(dir, ".."))
  ) {
    try {
      Deno.statSync(path.join(dir, target));
      return dir;
    } catch (e) {
      if (!(e instanceof Deno.errors.NotFound)) throw e;
    }
  }
  return null;
};

const mkdirp = (dir: string) => {
  try {
    Deno.mkdirSync(dir, { recursive: true });
  } catch (e) {
    if (!(e instanceof Deno.errors.AlreadyExists)) throw e;
  }
};
