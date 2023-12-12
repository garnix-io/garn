import { Initializer } from "../base.ts";
import * as fs from "https://deno.land/std@0.201.0/fs/mod.ts";
import outdent from "https://deno.land/x/outdent@v0.8.0/mod.ts";
import { camelCase } from "https://deno.land/x/case@2.2.0/mod.ts";
import { join } from "https://deno.land/std@0.201.0/path/mod.ts";
import { parseToml } from "../internal/utils.ts";
import { z } from "https://deno.land/x/zod@v3.22.4/mod.ts";
import { parsePyprojectToml } from "./utils.ts";

export const pythonInitializer: Initializer = (dir) => {
  const existPyprojetToml = fs.existsSync(join(dir, "pyproject.toml"));
  if (!existPyprojetToml) {
    return { tag: "ShouldNotRun" };
  }
  const contents = Deno.readTextFileSync(join(dir, "pyproject.toml"));
  const result = parsePyprojectToml(contents);
  if (result.error) {
    return {
      tag: "UnexpectedError",
      reason: `Could not parse pyproject.toml: ${result.error.message}`,
    };
  }
  return {
    tag: "ShouldRun",
    imports: [],
    makeTarget: () =>
      [
        outdent`
          export const ${camelCase(
            result.data.project.name || "npmProject",
          )} = garn.python.mkPythonProject({
            src: ".",
            pythonInterpreter: garn.python.interpreters.python310,
          })
        `,
      ].join("\n  ") + ";",
  };
};

export const initializers = [pythonInitializer];
