import {
  afterEach,
  beforeEach,
  describe,
  it,
} from "https://deno.land/std@0.206.0/testing/bdd.ts";
import {
  assertStderrContains,
  assertStdout,
  assertSuccess,
  buildPackage,
  runInDevShell,
  runCheck,
  runCommand,
  runExecutable,
} from "./testUtils.ts";
import { importFlake } from "./importFlake.ts";
import * as garn from "./mod.ts";
import { existsSync } from "https://deno.land/std@0.201.0/fs/mod.ts";
import { assertEquals } from "https://deno.land/std@0.206.0/assert/mod.ts";

describe("importFlake", () => {
  let tempDir: string;

  beforeEach(() => {
    tempDir = Deno.makeTempDirSync({ prefix: "garn-test" });
  });

  afterEach(() => {
    Deno.removeSync(tempDir, { recursive: true });
  });

  const writeFlakeToImport = (contents: string) => {
    Deno.mkdirSync(`${tempDir}/to-import`, { recursive: true });
    Deno.writeTextFileSync(
      `${tempDir}/to-import/flake.nix`,
      `
        {
          inputs.nixpkgs.url = "github:NixOS/nixpkgs/6fc7203e423bbf1c8f84cccf1c4818d097612566";
          inputs.flake-utils.url = "github:numtide/flake-utils/ff7b65b44d01cf9ba6a71320833626af21126384";
          outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system: 
            let pkgs = import "\${nixpkgs}" { inherit system; }; in ${contents}
          );
        }
      `,
    );
    if (!existsSync(`${tempDir}/to-import/flake.lock`)) {
      assertSuccess(
        runCommand(
          new Deno.Command("nix", {
            args: ["flake", "lock"],
            cwd: `${tempDir}/to-import`,
          }),
        ),
      );
    }
  };

  describe("getPackage", () => {
    it("returns the specified package from the flake file", () => {
      writeFlakeToImport(`{
        packages = {
          main_pkg = pkgs.runCommand "create-some-files" {} ''
            mkdir $out
            touch $out/foo $out/bar
          '';
        };
      }`);
      const flake = importFlake("./to-import");
      const pkg = buildPackage(flake.getPackage("main_pkg"), { dir: tempDir });
      assertEquals(getDirEntryNames(pkg), ["bar", "foo"]);
    });

    it("reflects changes when changing the flake source after importing", () => {
      writeFlakeToImport(`{
        packages = {
          main_pkg = pkgs.runCommand "create-some-files" {} ''
            mkdir $out
            touch $out/original
          '';
        };
      }`);
      const flake = importFlake(`./to-import`);
      buildPackage(flake.getPackage("main_pkg"), { dir: tempDir });
      writeFlakeToImport(`{
        packages = {
          main_pkg = pkgs.runCommand "create-some-files" {} ''
            mkdir $out
            touch $out/modified
          '';
        };
      }`);
      const pkg = buildPackage(flake.getPackage("main_pkg"), { dir: tempDir });
      assertEquals(getDirEntryNames(pkg), ["modified"]);
    });

    it("allows importing relative sources in packages", () => {
      Deno.writeTextFileSync(
        `${tempDir}/foo.js`,
        "console.log('Hello from a js file in tempDir!')",
      );
      writeFlakeToImport(`{
        packages = {
          foo = pkgs.writeScript "foo" ''
            \${pkgs.nodejs}/bin/node \${../foo.js}
          '';
        };
      }`);
      const flake = importFlake("./to-import");
      const exe = garn.shell`${flake.getPackage("foo")}`;
      const output = assertSuccess(runExecutable(exe, { cwd: tempDir }));
      assertStdout(output, "Hello from a js file in tempDir!\n");
    });

    it("displays a helpful error if the specified package does not exist", () => {
      writeFlakeToImport(`{ packages = { }; }`);
      const flake = importFlake("./to-import");
      const exe = garn.shell`${flake.getPackage("foo")}`;
      const output = runExecutable(exe, { cwd: tempDir });
      assertStderrContains(
        output,
        'error: The package "foo" was not found in ./to-import',
      );
    });
  });

  describe("getApp", () => {
    it("returns the specified executable from the flake file apps", () => {
      writeFlakeToImport(`{
        apps = {
          hello = {
            type = "app";
            program = builtins.toString (pkgs.writeScript "hello" ''
              echo hello from flake file
            '');
          };
        };
      }`);
      const flake = importFlake("./to-import");
      const exe = flake.getApp("hello");
      const output = assertSuccess(runExecutable(exe, { cwd: tempDir }));
      assertStdout(output, "hello from flake file\n");
    });

    it("displays a helpful error if the specified app does not exist", () => {
      writeFlakeToImport(`{ apps = { }; }`);
      const flake = importFlake("./to-import");
      const exe = garn.shell`${flake.getApp("foo")}`;
      const output = runExecutable(exe, { cwd: tempDir });
      assertStderrContains(
        output,
        'error: The app "foo" was not found in ./to-import',
      );
    });
  });

  describe("getCheck", () => {
    it("returns the specified check from the flake file", () => {
      writeFlakeToImport(`{
        checks = {
          some-check = pkgs.runCommand "my-check" {} ''
            # ${Date.now()}
            touch $out
            echo running my-check!
          '';
        };
      }`);
      const flake = importFlake("./to-import");
      const check = flake.getCheck("some-check");
      const output = assertSuccess(runCheck(check, { dir: tempDir }));
      assertStderrContains(output, "running my-check!");
    });

    it("displays a helpful error if the specified check does not exist", () => {
      writeFlakeToImport(`{ checks = { }; }`);
      const flake = importFlake("./to-import");
      const exe = garn.shell`${flake.getCheck("foo")}`;
      const output = runExecutable(exe, { cwd: tempDir });
      assertStderrContains(
        output,
        'error: The check "foo" was not found in ./to-import',
      );
    });
  });

  describe("getDevShell", () => {
    it("returns the specified environment from the flake file", () => {
      writeFlakeToImport(`{
        devShells = {
          some-shell = pkgs.mkShell {
            nativeBuildInputs = [ pkgs.hello ];
          };
        };
      }`);
      const flake = importFlake("./to-import");
      const env = flake.getDevShell("some-shell");
      const output = runInDevShell(env, { cmd: "hello", dir: tempDir });
      assertStdout(output, "Hello, world!\n");
    });

    it("displays a helpful error if the specified devShell does not exist", () => {
      writeFlakeToImport(`{ devShells = { }; }`);
      const flake = importFlake("./to-import");
      const exe = garn.shell`${flake.getDevShell("foo")}`;
      const output = runExecutable(exe, { cwd: tempDir });
      assertStderrContains(
        output,
        'error: The devShell "foo" was not found in ./to-import',
      );
    });
  });

  describe("allPackages", () => {
    it("returns a package with symlinks to all found packages", () => {
      writeFlakeToImport(`{
        packages = {
          foo = pkgs.runCommand "foo" {} "echo foo-pkg > $out";
          bar = pkgs.runCommand "bar" {} "echo bar-pkg > $out";
        };
      }`);
      const flake = importFlake("./to-import");
      const pkg = buildPackage(flake.allPackages, { dir: tempDir });
      assertEquals(getDirEntryNames(pkg), ["bar", "foo"]);
      assertEquals(Deno.readTextFileSync(`${pkg}/foo`), "foo-pkg\n");
      assertEquals(Deno.readTextFileSync(`${pkg}/bar`), "bar-pkg\n");
    });
  });

  describe("getAllChecks", () => {
    it("returns a check that composes all found checks", () => {
      writeFlakeToImport(`{
        checks = {
          foo = pkgs.runCommand "check-foo" {} ''
            # ${Date.now()}
            touch $out
            echo running check foo
          '';
          bar = pkgs.runCommand "check-bar" {} ''
            # ${Date.now()}
            touch $out
            echo running check bar
          '';
        };
      }`);
      const flake = importFlake("./to-import");
      const output = assertSuccess(runCheck(flake.allChecks, { dir: tempDir }));
      assertStderrContains(output, "running check foo");
      assertStderrContains(output, "running check bar");
    });
  });

  it("allows importing flakes from url", () => {
    const flake = importFlake(
      "github:garnix-io/debug-tools/8a4026fa6ccbfec070f96d458ffa96e7fb6112e8",
    );
    const exe = flake.getPackage("main_pkg").bin("debug-args");
    const output = assertSuccess(runExecutable(exe));
    assertStdout(output, "");
    assertStderrContains(output, "[]");
  });
});

function getDirEntryNames(path: string) {
  return [...Deno.readDirSync(path)].map((entry) => entry.name);
}
