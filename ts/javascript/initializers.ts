import { Initializer } from "../base.ts";
import * as fs from "https://deno.land/std@0.201.0/fs/mod.ts";
import { assertEquals } from "https://deno.land/std@0.201.0/assert/mod.ts";
import outdent from "https://deno.land/x/outdent@v0.8.0/mod.ts";
import { mapValues } from "../internal/utils.ts";

const npmInitializer: Initializer = () => {
  const existsPkgJson = fs.existsSync("package.json");
  const existsPkgLock = fs.existsSync("package-lock.json");
  const existsYarnLock = fs.existsSync("yarn.lock");
  if (!existsPkgJson || (!existsPkgLock && existsYarnLock)) {
    return { tag: "ShouldNotRun" };
  }
  const contents = Deno.readTextFileSync("package.json");
  try {
    const packageJson = JSON.parse(contents);
    const scripts = mapValues(
      (script: string) => script.replaceAll("`", "\\`"),
      packageJson.scripts ?? {}
    );
    const testScript: string | undefined = scripts.test;
    const otherScripts = Object.entries(scripts).filter(
      ([name]) => name !== "test"
    );
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
          })`,
          ...(testScript ? [`.addCheck("test")\`${testScript}\``] : []),
          ...otherScripts.map(
            ([name, script]) =>
              `.addExecutable(${JSON.stringify(name)})\`${script}\``
          ),
        ].join("\n  "),
    };
  } catch (_e) {
    return {
      tag: "UnexpectedError",
      reason: "Could not parse package.json",
    };
  }
};

export const initializers = [npmInitializer];

Deno.test(
  "NPM initializer does not run when package.json is not present",
  () => {
    const tempDir = Deno.makeTempDirSync();
    Deno.chdir(tempDir);
    const result = npmInitializer();
    assertEquals(result.tag, "ShouldNotRun");
  }
);

Deno.test(
  "NPM initializer does not run when package-lock.json is not present but yarn.lock is present",
  () => {
    const tempDir = Deno.makeTempDirSync();
    Deno.chdir(tempDir);
    Deno.writeTextFileSync("./package.json", "{}");
    Deno.writeTextFileSync("./yarn.lock", "");
    const result = npmInitializer();
    assertEquals(result.tag, "ShouldNotRun");
  }
);

Deno.test("NPM initializer errors if package.json is unparseable", () => {
  const tempDir = Deno.makeTempDirSync();
  Deno.chdir(tempDir);
  Deno.writeTextFileSync(
    "./package.json",
    `
    name: foo
  `
  );
  const result = npmInitializer();
  assertEquals(result.tag, "UnexpectedError");
  if (result.tag === "UnexpectedError") {
    assertEquals(result.reason, "Could not parse package.json");
  }
});

Deno.test("NPM initializer returns the code to be generated", () => {
  const tempDir = Deno.makeTempDirSync();
  Deno.chdir(tempDir);
  Deno.writeTextFileSync(
    "./package.json",
    JSON.stringify({
      name: "somepackage",
      description: "just some package",
    })
  );
  const result = npmInitializer();
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
      `
    );
  }
});

Deno.test(
  "NPM initializer has sensible defaults if name and description are missing",
  () => {
    const tempDir = Deno.makeTempDirSync();
    Deno.chdir(tempDir);
    Deno.writeTextFileSync("./package.json", "{}");
    const result = npmInitializer();
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
        `
      );
    }
  }
);

Deno.test(
  "NPM initializer templates out scripts as checks and executables",
  () => {
    const tempDir = Deno.makeTempDirSync();
    Deno.chdir(tempDir);
    Deno.writeTextFileSync(
      "./package.json",
      JSON.stringify({
        name: "somepackage",
        description: "just some package",
        scripts: {
          test: "jest",
          start: "vite --port 3000",
          build: "vite build",
        },
      })
    );
    const result = npmInitializer();
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
            .addCheck("test")\`jest\`
            .addExecutable("start")\`vite --port 3000\`
            .addExecutable("build")\`vite build\`
        `
      );
    }
  }
);

Deno.test("NPM initializer escapes scripts as checks and executables", () => {
  const tempDir = Deno.makeTempDirSync();
  Deno.chdir(tempDir);
  Deno.writeTextFileSync(
    "./package.json",
    JSON.stringify({
      name: "somepackage",
      description: "just some package",
      scripts: {
        test: "echo `echo test`",
        start: "echo `echo start`",
      },
    })
  );
  const result = npmInitializer();
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
            .addCheck("test")\`echo \\\`echo test\\\`\`
            .addExecutable("start")\`echo \\\`echo start\\\`\`
        `
    );
  }
});
