import * as fs from "https://deno.land/std@0.201.0/fs/mod.ts";
import { assertEquals } from "https://deno.land/std@0.201.0/assert/mod.ts";

export type ShouldNotRun =
  | { tag: "ShouldNotRun" }
  | { tag: "UnexpectedError"; reason: string };

export type Initializer = (
  path: string
) =>
  | { tag: "ShouldRun"; imports: string; makeTarget: () => string }
  | { tag: "ShouldNotRun" }
  | { tag: "UnexpectedError"; reason: string };

const getDirs = (): string[] => {
  // const cmd = new Deno.Command("bash", { args : ["-c", "git ls-files | xargs dirname | sort | uniq"] })
  // console.log(cmd.outputSync().stderr)
  const dirs: string[] = [
    ...fs.walkSync(".", { includeFiles: false, includeDirs: true }),
  ].map((x) => x.path);
  return dirs;
};

export const initialize = (initializers: Initializer[]) => {
  console.error("[garner] Creating a garner.ts file");
  const dirs = getDirs();

  let imports = "";
  let body = "";

  for (const init of initializers) {
    for (const dir of dirs) {
      const result = init(dir);
      switch (result.tag) {
        case "UnexpectedError":
          console.error("[garner] " + result.reason);
          break;
        case "ShouldNotRun":
          break;
        case "ShouldRun":
          imports += result.imports;
          body += result.makeTarget();
          break;
      }
    }
  }

  Deno.writeTextFileSync("garner.ts", imports + "\n\n" + body);
};

// Tests

const testInitializer: Initializer = (path: string) => {
  if (fs.existsSync("shouldrun")) {
    return {
      tag: "ShouldRun",
      imports: "",
      makeTarget: () => `${path}\n`,
    };
  } else {
    return {
      tag: "ShouldNotRun",
    };
  }
};

Deno.test("Recurses into subdirectories", () => {
  const tempDir = Deno.makeTempDirSync();
  Deno.chdir(tempDir);
  Deno.mkdir(tempDir + "/foo");
  Deno.writeTextFileSync("shouldrun", "");
  initialize([testInitializer]);
  const result = Deno.readTextFileSync("garner.ts");
  assertEquals(result, "\n\n.\nfoo\n");
});

Deno.test("Ignores gitignored directories", () => {
  assertEquals(
    "Need to get off this plane to pick a lib that does this for me",
    ""
  );
});
