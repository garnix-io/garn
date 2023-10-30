import { Initializer } from "../base.ts";
import * as fs from "https://deno.land/std@0.201.0/fs/mod.ts";
import outdent from "https://deno.land/x/outdent@v0.8.0/mod.ts";
import { join } from "https://deno.land/std@0.201.0/path/mod.ts";
import { isObj, parseJson } from "../internal/utils.ts";

export const npmInitializer: Initializer = (dir) => {
  const existsPkgJson = fs.existsSync(join(dir, "package.json"));
  const existsPkgLock = fs.existsSync(join(dir, "package-lock.json"));
  const existsYarnLock = fs.existsSync(join(dir, "yarn.lock"));
  if (!existsPkgJson || (!existsPkgLock && existsYarnLock)) {
    return { tag: "ShouldNotRun" };
  }
  const contents = Deno.readTextFileSync(join(dir, "package.json"));
  const [ok, packageJson] = parseJson(contents);
  if (!ok) {
    return {
      tag: "UnexpectedError",
      reason: `Could not parse package.json: ${packageJson.message}`,
    };
  }
  if (!isObj(packageJson)) {
    return {
      tag: "UnexpectedError",
      reason: "Could not parse package.json: package.json is not an object",
    };
  }
  const scripts =
    "scripts" in packageJson && isObj(packageJson.scripts)
      ? Object.keys(packageJson.scripts)
      : [];
  const otherScripts = scripts.filter((name) => name !== "test");
  return {
    tag: "ShouldRun",
    imports: [],
    makeTarget: () =>
      [
        outdent`
          export const ${packageJson.name || "npmProject"} = mkNpmProject({
            description: "${packageJson.description || "An NPM project"}",
            src: ".",
            nodeVersion: "18",
          })
        `,
        ...(scripts.includes("test")
          ? ['.addCheck("test")`npm run test`']
          : []),
        ...otherScripts.map(
          (name) =>
            `.addExecutable(${JSON.stringify(name)})\`npm run ${name}\``,
        ),
      ].join("\n  "),
  };
};

export const initializers = [npmInitializer];
