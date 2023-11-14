import {
  assertEquals,
  assert,
} from "https://deno.land/std@0.201.0/assert/mod.ts";
import outdent from "https://deno.land/x/outdent@v0.8.0/mod.ts";
import { join } from "https://deno.land/std@0.201.0/path/mod.ts";
import { npmInitializer } from "./initializers.ts";

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
      export const somepackage = garn.javascript.mkNpmProject({
        description: "just some package",
        src: ".",
        nodeVersion: "18",
      });
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
          export const npmProject = garn.javascript.mkNpmProject({
            description: "An NPM project",
            src: ".",
            nodeVersion: "18",
          });
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
          export const somepackage = garn.javascript.mkNpmProject({
            description: "just some package",
            src: ".",
            nodeVersion: "18",
          })
            .addCheck("test", "npm run test")
            .addExecutable("start", "npm run start")
            .addExecutable("build", "npm run build");
        `,
      );
    }
  },
);

Deno.test("adds the vite plugin if vite is in the devDependencies", () => {
  const tempDir = Deno.makeTempDirSync();
  Deno.writeTextFileSync(
    join(tempDir, "package.json"),
    JSON.stringify({
      name: "somepackage",
      description: "just some package",
      devDependencies: {
        vite: "*",
      },
    }),
  );
  const result = npmInitializer(tempDir);
  assertEquals(result.tag, "ShouldRun");
  if (result.tag === "ShouldRun") {
    assertEquals(
      result.makeTarget(),
      outdent`
        export const somepackage = garn.javascript.mkNpmProject({
          description: "just some package",
          src: ".",
          nodeVersion: "18",
        })
          .add(garn.javascript.vite);
      `,
    );
  }
});

Deno.test(
  "doesn't add known vite scripts if superseeded by vite plugin",
  () => {
    const tempDir = Deno.makeTempDirSync();
    Deno.writeTextFileSync(
      join(tempDir, "package.json"),
      JSON.stringify({
        name: "somepackage",
        description: "just some package",
        scripts: {
          build: "vite build",
          dev: "vite",
          foo: "test script",
        },
        devDependencies: {
          vite: "*",
        },
      }),
    );
    const result = npmInitializer(tempDir);
    assertEquals(result.tag, "ShouldRun");
    if (result.tag === "ShouldRun") {
      assertEquals(
        result.makeTarget(),
        outdent`
          export const somepackage = garn.javascript.mkNpmProject({
            description: "just some package",
            src: ".",
            nodeVersion: "18",
          })
            .add(garn.javascript.vite)
            .addExecutable("foo", "npm run foo");
        `,
      );
    }
  },
);

Deno.test("doesn't remove executables when not a vite plugin", () => {
  const tempDir = Deno.makeTempDirSync();
  Deno.writeTextFileSync(
    join(tempDir, "package.json"),
    JSON.stringify({
      name: "somepackage",
      description: "just some package",
      scripts: {
        build: "vite build",
        dev: "vite",
        foo: "test script",
      },
      devDependencies: {},
    }),
  );
  const result = npmInitializer(tempDir);
  assertEquals(result.tag, "ShouldRun");
  if (result.tag === "ShouldRun") {
    assertEquals(
      result.makeTarget(),
      outdent`
        export const somepackage = garn.javascript.mkNpmProject({
          description: "just some package",
          src: ".",
          nodeVersion: "18",
        })
          .addExecutable("build", "npm run build")
          .addExecutable("dev", "npm run dev")
          .addExecutable("foo", "npm run foo");
      `,
    );
  }
});
