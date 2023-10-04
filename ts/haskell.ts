import { assertEquals } from "https://deno.land/std@0.201.0/assert/mod.ts";
import * as fs from "https://deno.land/std@0.201.0/fs/mod.ts";
import outdent from "https://deno.land/x/outdent@v0.8.0/mod.ts";
import { Initializer, mkPackage, Package } from "./base.ts";
import { Environment, packageToEnvironment } from "./environment.ts";
import { mkNewPackage, NewPackage } from "./package.ts";
import { mkProject, ProjectWithDefaultEnvironment } from "./project.ts";
import { nixSource } from "./utils.ts";
import { Executable, shell } from "./executable.ts";

type MkHaskellArgs = {
  description: string;
  executable: string;
  compiler: string;
  src: string;
};

export const mkHaskell = (
  args: MkHaskellArgs
): ProjectWithDefaultEnvironment & {
  pkg: NewPackage;
  devShell: Environment;
} => {
  const pkg: NewPackage = mkNewPackage(`
    (pkgs.haskell.packages.${args.compiler}.callCabal2nix
      "garner-pkg"
      ${nixSource(args.src)}
      { })
      // {
        meta.mainProgram = "${args.executable}";
      }
  `);
  const devShell: Environment = packageToEnvironment(pkg);
  const main: Executable = shell`${pkg}/bin/${args.executable}`;
  return mkProject(
    {
      pkg,
      devShell,
      main,
    },
    {
      defaults: {
        environment: "devShell",
        executable: "main",
      },
    }
  );
};

// Initializer

// Currently only works if there's a single cabal file, in the current directory
const mkHaskellInitializer: Initializer = () => {
  const cabalFiles: fs.WalkEntry[] = [...fs.expandGlobSync("*.cabal")];
  if (cabalFiles.length === 0) {
    return { tag: "ShouldNotRun" };
  }
  if (cabalFiles.length > 1) {
    return {
      tag: "UnexpectedError",
      reason: "More than one cabal files found",
    };
  }
  const cmd = new Deno.Command("cabal2json", {
    args: [cabalFiles[0].path],
  });
  const decoder = new TextDecoder();
  const jsonParseResult = cmd.outputSync();
  if (jsonParseResult.code !== 0) {
    return {
      tag: "UnexpectedError",
      reason: "Found but could not parse cabal file",
    };
  }
  const parsedCabal = JSON.parse(decoder.decode(jsonParseResult.stdout));

  return {
    tag: "ShouldRun",
    imports: 'import * as garner from "http://localhost:8777/mod.ts"',
    makeTarget: () =>
      outdent`
      export const ${
        parsedCabal.description.package.name
      } = garner.haskell.mkHaskell({
        description: "${
          parsedCabal.description.synopsis ||
          parsedCabal.description.description ||
          ""
        }",
        executable: "",
        compiler: "ghc94",
        src: "."
      })`,
  };
};

export const initializers = [mkHaskellInitializer];

// Tests

Deno.test("Initializer does not run when no cabal file is present", () => {
  const tempDir = Deno.makeTempDirSync();
  Deno.chdir(tempDir);
  const result = mkHaskellInitializer();
  assertEquals(result.tag, "ShouldNotRun");
});

Deno.test("Initializer errors if the cabal file is unparseable", () => {
  const tempDir = Deno.makeTempDirSync();
  Deno.chdir(tempDir);
  Deno.writeTextFileSync(
    "./foo.cabal",
    `
    name: foo
  `
  );
  const result = mkHaskellInitializer();
  assertEquals(result.tag, "UnexpectedError");
  if (result.tag === "UnexpectedError") {
    assertEquals(result.reason, "Found but could not parse cabal file");
  }
});

Deno.test("Initializer returns a simple string if a cabal file exists", () => {
  const tempDir = Deno.makeTempDirSync();
  Deno.chdir(tempDir);
  Deno.writeTextFileSync(
    "./foo.cabal",
    `
    name: foo
    version: 0.0.1
  `
  );
  const result = mkHaskellInitializer();
  assertEquals(result.tag, "ShouldRun");
  if (result.tag === "ShouldRun") {
    assertEquals(
      result.makeTarget(),
      outdent`
          export const foo = garner.haskell.mkHaskell({
            description: "",
            executable: "",
            compiler: "ghc94",
            src: "."
          })`
    );
  }
});
