import * as fs from "https://deno.land/std@0.201.0/fs/mod.ts";
import outdent from "https://deno.land/x/outdent@v0.8.0/mod.ts";
import { Initializer } from "../base.ts";
import { camelCase } from "https://deno.land/x/case@2.2.0/mod.ts";
import { join } from "https://deno.land/std@0.201.0/path/mod.ts";

function parseGoMod(goModContents: string): {
  moduleName: string;
  goVersion: string;
} {
  let moduleName: string | null = null;
  let goVersion: string | null = null;
  for (const line of goModContents.split("\n")) {
    const [keyword, rest] = line.split(" ");
    switch (keyword) {
      case "module":
        moduleName = rest.slice(rest.lastIndexOf("/") + 1);
        break;
      case "go":
        goVersion = rest;
        break;
    }
    if (moduleName && goVersion) return { moduleName, goVersion };
  }
  throw new Error("go.mod missing module name or go version");
}

const goModuleInitializer: Initializer = (dir) => {
  if (!fs.existsSync(join(dir, "go.mod"))) {
    return { tag: "ShouldNotRun" };
  }
  try {
    const { goVersion, moduleName } = parseGoMod(
      Deno.readTextFileSync(join(dir, "go.mod")),
    );
    return {
      tag: "ShouldRun",
      makeTarget: () =>
        outdent`
          export const ${camelCase(moduleName)} = garn.go.mkGoProject({
            description: "My go project",
            src: ".",
            goVersion: ${JSON.stringify(goVersion)},
          });
        `,
    };
  } catch (e) {
    if (e instanceof Error) {
      return {
        tag: "ShouldNotRun",
        reason: e.message,
      };
    }
    throw e;
  }
};

export const initializers = [goModuleInitializer];
