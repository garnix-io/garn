import { Initializer } from "../base.ts";
import * as fs from "https://deno.land/std@0.201.0/fs/mod.ts";
import outdent from "https://deno.land/x/outdent@v0.8.0/mod.ts";
import { camelCase } from "https://deno.land/x/case@2.2.0/mod.ts";
import { join } from "https://deno.land/std@0.201.0/path/mod.ts";
import { parseJson } from "../internal/utils.ts";
import { z } from "https://deno.land/x/zod@v3.22.4/mod.ts";

export const npmInitializer: Initializer = (dir) => {
  const existsPkgJson = fs.existsSync(join(dir, "package.json"));
  const existsPkgLock = fs.existsSync(join(dir, "package-lock.json"));
  const existsYarnLock = fs.existsSync(join(dir, "yarn.lock"));
  if (!existsPkgJson || (!existsPkgLock && existsYarnLock)) {
    return { tag: "ShouldNotRun" };
  }
  const contents = Deno.readTextFileSync(join(dir, "package.json"));
  const packageJsonSchema = z.object({
    name: z.string().optional(),
    description: z.string().optional(),
    scripts: z.record(z.string()).optional(),
  });
  const result = parseJson(packageJsonSchema, contents);
  if (result.error) {
    return {
      tag: "UnexpectedError",
      reason: `Could not parse package.json: ${result.error.message}`,
    };
  }
  const scripts = Object.keys(result.data.scripts || {});
  const nonTestScripts = scripts.filter((name) => name !== "test");
  return {
    tag: "ShouldRun",
    imports: [],
    makeTarget: () =>
      [
        outdent`
          export const ${camelCase(
            result.data.name || "npmProject",
          )} = garn.javascript.mkNpmProject({
            description: "${result.data.description || "An NPM project"}",
            src: ".",
            nodeVersion: "18",
          })
        `,
        ...(scripts.includes("test")
          ? ['.addCheck("test", "npm run test")']
          : []),
        ...nonTestScripts.map(
          (name) =>
            `.addExecutable(${JSON.stringify(name)}, ${JSON.stringify(
              `npm run ${name}`,
            )})`,
        ),
      ].join("\n  "),
  };
};

export const initializers = [npmInitializer];
