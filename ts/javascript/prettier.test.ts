import {
  afterEach,
  beforeEach,
  describe,
  it,
} from "https://deno.land/std@0.206.0/testing/bdd.ts";
import * as garn from "../mod.ts";
import {
  assertFailure,
  assertStderrContains,
  assertSuccess,
  runCheck,
  runExecutable,
} from "../testUtils.ts";
import { assertEquals } from "https://deno.land/std@0.206.0/assert/assert_equals.ts";

describe("prettier plugin", () => {
  let tmpDir: string;

  beforeEach(() => {
    tmpDir = Deno.makeTempDirSync({ prefix: "garn-test" });
  });

  afterEach(() => {
    Deno.removeSync(tmpDir, { recursive: true });
  });

  describe("format", () => {
    it("formats all formattable files in the current working directory", () => {
      const project = mkSampleNpmProject().add(garn.javascript.prettier());
      Deno.writeTextFileSync(
        `${tmpDir}/needs-format.js`,
        "   console.log(\n'needs format'    )",
      );
      assertSuccess(runExecutable(project.format, { cwd: tmpDir }));
      assertEquals(
        Deno.readTextFileSync(`${tmpDir}/needs-format.js`),
        'console.log("needs format");\n',
      );
    });

    it("allows specifying a manual glob to format", () => {
      const project = mkSampleNpmProject().add(
        garn.javascript.prettier({ glob: "*.fmt.js" }),
      );
      const needsFormat = "   console.log(\n'needs format'    )";
      Deno.writeTextFileSync(`${tmpDir}/not-modified.js`, needsFormat);
      Deno.writeTextFileSync(`${tmpDir}/modified.fmt.js`, needsFormat);
      assertSuccess(runExecutable(project.format, { cwd: tmpDir }));
      assertEquals(
        Deno.readTextFileSync(`${tmpDir}/not-modified.js`),
        needsFormat,
      );
      assertEquals(
        Deno.readTextFileSync(`${tmpDir}/modified.fmt.js`),
        'console.log("needs format");\n',
      );
    });

    it("falls back to using nixpkgs prettier if prettier isn't defined in package.json", () => {
      const project = mkSampleNpmProject(false).add(garn.javascript.prettier());
      Deno.writeTextFileSync(
        `${tmpDir}/needs-format.js`,
        "   console.log(\n'needs format'    )",
      );
      assertSuccess(runExecutable(project.format, { cwd: tmpDir }));
      assertEquals(
        Deno.readTextFileSync(`${tmpDir}/needs-format.js`),
        'console.log("needs format");\n',
      );
    });

    it("allows specifying manual configs", () => {
      const project = mkSampleNpmProject().add(
        garn.javascript.prettier({
          config: { singleQuote: true },
        }),
      );
      Deno.writeTextFileSync(
        `${tmpDir}/needs-format.js`,
        "   console.log(\n'needs format, but leave single quotes'    )",
      );
      assertSuccess(runExecutable(project.format, { cwd: tmpDir }));
      assertEquals(
        Deno.readTextFileSync(`${tmpDir}/needs-format.js`),
        "console.log('needs format, but leave single quotes');\n",
      );
    });
  });

  describe("checkPrettier", () => {
    it("fails if there are files that need formatting", () => {
      const project = mkSampleNpmProject().add(garn.javascript.prettier());
      Deno.writeTextFileSync(
        `${tmpDir}/needs-format.js`,
        "   console.log(\n'needs format'    )",
      );
      const output = assertFailure(
        runCheck(project.checkPrettier, { dir: tmpDir }),
      );
      assertStderrContains(
        output,
        [
          "check> [warn] needs-format.js",
          "check> [warn] Code style issues found in the above file. Run Prettier to fix.",
        ].join("\n"),
      );
    });

    it("succeeds if everything is properly formatted", () => {
      const project = mkSampleNpmProject().add(garn.javascript.prettier());
      Deno.writeTextFileSync(
        `${tmpDir}/format-ok.js`,
        'console.log("needs format");\n',
      );
      assertSuccess(runCheck(project.checkPrettier, { dir: tmpDir }));
    });

    it("only checks files specified by glob", () => {
      const project = mkSampleNpmProject().add(
        garn.javascript.prettier({ glob: "format-ok.js" }),
      );
      Deno.writeTextFileSync(
        `${tmpDir}/needs-format.js`,
        "   console.log(\n'needs format'    )",
      );
      Deno.writeTextFileSync(
        `${tmpDir}/format-ok.js`,
        'console.log("format is good");\n',
      );
      assertSuccess(runCheck(project.checkPrettier, { dir: tmpDir }));
    });
  });

  const prettierLock = {
    "node_modules/prettier": {
      version: "3.1.0",
      resolved: "https://registry.npmjs.org/prettier/-/prettier-3.1.0.tgz",
      integrity:
        "sha512-TQLvXjq5IAibjh8EpBIkNKxO749UEWABoiIZehEPiY4GNpVdhaFKqSTu+QrlU6D2dPAfubRmtJTi4K4YkQ5eXw==",
      bin: { prettier: "bin/prettier.cjs" },
      engines: { node: ">=14" },
      funding: { url: "https://github.com/prettier/prettier?sponsor=1" },
    },
  };

  const mkSampleNpmProject = (includePrettier = true) => {
    const packageJson = {
      name: "sample-project",
      version: "0.0.1",
      dependencies: includePrettier ? { prettier: "^3.1.0" } : {},
    };
    const packageLock = {
      name: packageJson.name,
      version: packageJson.version,
      lockfileVersion: 3,
      requires: true,
      packages: {
        "": packageJson,
        ...(includePrettier ? prettierLock : {}),
      },
    };
    Deno.writeTextFileSync(
      `${tmpDir}/package.json`,
      JSON.stringify(packageJson, null, 2) + "\n",
    );
    Deno.writeTextFileSync(
      `${tmpDir}/package-lock.json`,
      JSON.stringify(packageLock, null, 2) + "\n",
    );
    const project = garn.javascript.mkNpmProject({
      description: "",
      nodeVersion: "18",
      src: ".",
    });
    return project;
  };
});
