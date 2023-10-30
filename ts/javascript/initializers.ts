import { Initializer } from "../base.ts";
import * as fs from "https://deno.land/std@0.201.0/fs/mod.ts";
import {
  assertEquals,
  assert,
} from "https://deno.land/std@0.201.0/assert/mod.ts";
import outdent from "https://deno.land/x/outdent@v0.8.0/mod.ts";
import { join } from "https://deno.land/std@0.201.0/path/mod.ts";
import { isObj, parseJson } from "../internal/utils.ts";

const npmInitializer: Initializer = (dir) => {
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

Deno.test(
  "NPM initializer does not run when package.json is not present",
  () => {
    const tempDir = Deno.makeTempDirSync();
    const result = npmInitializer(tempDir);
    assertEquals(result.tag, "ShouldNotRun");
  },
);

Deno.test(
  "NPM initializer does not run when package-lock.json is not present but yarn.lock is present",
  () => {
    const tempDir = Deno.makeTempDirSync();
    Deno.writeTextFileSync(join(tempDir, "package.json"), "{}");
    Deno.writeTextFileSync(join(tempDir, "yarn.lock"), "");
    const result = npmInitializer(tempDir);
    assertEquals(result.tag, "ShouldNotRun");
  },
);

Deno.test("NPM initializer errors if package.json is unparseable", () => {
  const tempDir = Deno.makeTempDirSync();
  Deno.writeTextFileSync(
    join(tempDir, "package.json"),
    `
      some invalid: json
    `,
  );
  const result = npmInitializer(tempDir);
  if (result.tag !== "UnexpectedError") {
    throw new Error("expected an UnexpectedError");
  }
  assert(result.reason.startsWith("Could not parse package.json"));
});

Deno.test("NPM initializer returns the code to be generated", () => {
  const tempDir = Deno.makeTempDirSync();
  Deno.writeTextFileSync(
    join(tempDir, "package.json"),
    JSON.stringify({
      name: "somepackage",
      description: "just some package",
    }),
  );
  const result = npmInitializer(tempDir);
  if (result.tag !== "ShouldRun") {
    throw new Error("Expected initializer to run");
  }
  assertEquals(
    result.makeTarget(),
    outdent`
      export const somepackage = mkNpmProject({
        description: "just some package",
        src: ".",
        nodeVersion: "18",
      })
    `,
  );
});

Deno.test(
  "NPM initializer has sensible defaults if name and description are missing",
  () => {
    const tempDir = Deno.makeTempDirSync();
    Deno.writeTextFileSync(join(tempDir, "package.json"), "{}");
    const result = npmInitializer(tempDir);
    assertEquals(result.tag, "ShouldRun");
    if (result.tag === "ShouldRun") {
      assertEquals(
        result.makeTarget(),
        outdent`
          export const npmProject = mkNpmProject({
            description: "An NPM project",
            src: ".",
            nodeVersion: "18",
          })
        `,
      );
    }
  },
);

Deno.test(
  "NPM initializer templates out scripts as checks and executables",
  () => {
    const tempDir = Deno.makeTempDirSync();
    Deno.writeTextFileSync(
      join(tempDir, "package.json"),
      JSON.stringify({
        name: "somepackage",
        description: "just some package",
        scripts: {
          test: "jest",
          start: "vite --port 3000",
          build: "vite build",
        },
      }),
    );
    const result = npmInitializer(tempDir);
    assertEquals(result.tag, "ShouldRun");
    if (result.tag === "ShouldRun") {
      assertEquals(
        result.makeTarget(),
        outdent`
          export const somepackage = mkNpmProject({
            description: "just some package",
            src: ".",
            nodeVersion: "18",
          })
            .addCheck("test")\`npm run test\`
            .addExecutable("start")\`npm run start\`
            .addExecutable("build")\`npm run build\`
        `,
      );
    }
  },
);
