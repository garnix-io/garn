import {
  afterEach,
  beforeEach,
  describe,
  it,
} from "https://deno.land/std@0.206.0/testing/bdd.ts";
import * as garn from "./mod.ts";
import { buildPackage, runInDevShell } from "../testUtils.ts";
import { assert } from "https://deno.land/std@0.206.0/assert/mod.ts";
import { existsSync } from "https://deno.land/std@0.201.0/fs/exists.ts";

const packageJsonText = {
  name: "my-npm-project",
  version: "1.0.0",
  description: "My npm project",
  dependencies: {
    typescript: "*",
  },
};

const packageLockJsonText = {
  name: "my-npm-project",
  version: "1.0.0",
  lockfileVersion: 3,
  requires: true,
  packages: {
    "": {
      name: "my-npm-project",
      version: "1.0.0",
      dependencies: {
        typescript: "*",
      },
    },
    "node_modules/typescript": {
      version: "5.3.2",
      resolved: "https://registry.npmjs.org/typescript/-/typescript-5.3.2.tgz",
      integrity:
        "sha512-6l+RyNy7oAHDfxC4FzSJcz9vnjTKxrLpDG5M2Vu4SHRVNg6xzqZp6LYSR9zjqQTu8DU/f5xwxUdADOkbrIX2gQ==",
      bin: {
        tsc: "bin/tsc",
        tsserver: "bin/tsserver",
      },
      engines: {
        node: ">=14.17",
      },
    },
  },
};

describe("mkNpmProject", () => {
  let tempDir: string;
  beforeEach(() => {
    tempDir = Deno.makeTempDirSync({ prefix: "garn-test" });
    const packageJson = JSON.stringify(packageJsonText);
    Deno.writeTextFileSync(`${tempDir}/package.json`, packageJson);
    const packageLockJson = JSON.stringify(packageLockJsonText);
    Deno.writeTextFileSync(`${tempDir}/package-lock.json`, packageLockJson);
  });
  afterEach(() => {
    Deno.removeSync(tempDir, { recursive: true });
  });
  it("produces correct store path structure", () => {
    const project = garn.mkNpmProject({
      src: "./.",
      description: "An NPM project",
      nodeVersion: "18",
    });
    const output = buildPackage(project.node_modules, { dir: tempDir });
    assert(
      existsSync(`${output}/node_modules/typescript`),
      `/node_modules/typescript not found in ${output}`,
    );
    assert(existsSync(`${output}/bin/tsc`), `/bin/tsc not found in ${output}`);
  });

  it("generates a devShell that can be entered successfully", () => {
    const project = garn.mkNpmProject({
      src: "./.",
      description: "An NPM project",
      nodeVersion: "18",
    });
    if (project.defaultEnvironment == null) {
      throw new Error(
        `defaultEnvironment should not be null for ${project.description}`,
      );
    }
    runInDevShell(project.defaultEnvironment, { dir: tempDir, cmd: "true" });
  });
});
