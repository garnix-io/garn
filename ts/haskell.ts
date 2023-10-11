import { assertEquals } from "https://deno.land/std@0.201.0/assert/mod.ts";
import * as fs from "https://deno.land/std@0.201.0/fs/mod.ts";
import outdent from "https://deno.land/x/outdent@v0.8.0/mod.ts";
import { Initializer } from "./base.ts";
import { packageToEnvironment, shell } from "./environment.ts";
import { mkPackage, Package } from "./package.ts";
import { mkProject, Project } from "./project.ts";
import { nixSource } from "./internal/utils.ts";

type MkHaskellArgs = {
  description: string;
  executable: string;
  compiler: string;
  src: string;
};

export const mkHaskell = (args: MkHaskellArgs): Project & { pkg: Package } => {
  const pkg: Package = mkPackage(`
    (pkgs.haskell.packages.${args.compiler}.callCabal2nix
      "garn-pkg"
      ${nixSource(args.src)}
      { })
      // {
        meta.mainProgram = "${args.executable}";
      }
  `);
  return mkProject(
    {
      description: args.description,
      defaultEnvironment: packageToEnvironment(pkg, args.src),
      defaultExecutable: shell`${pkg}/bin/${args.executable}`,
    },
    {
      pkg,
    }
  ).mkPackage(`pkgs.haskell.packages.${args.compiler}.cabal-install`);
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
    imports: 'import * as garn from "http://localhost:8777/mod.ts"',
    makeTarget: () =>
      outdent`
      export const ${
        parsedCabal.description.package.name
      } = garn.haskell.mkHaskell({
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
          export const foo = garn.haskell.mkHaskell({
            description: "",
            executable: "",
            compiler: "ghc94",
            src: "."
          })`
    );
  }
});
