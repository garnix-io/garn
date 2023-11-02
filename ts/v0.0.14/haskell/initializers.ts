import * as fs from "https://deno.land/std@0.201.0/fs/mod.ts";
import outdent from "https://deno.land/x/outdent@v0.8.0/mod.ts";
import { Initializer } from "../base.ts";

// Currently only works if there's a single cabal file, in the current directory
export const mkHaskellProjectInitializer: Initializer = (dir) => {
  const cabalFiles: fs.WalkEntry[] = [
    ...fs.expandGlobSync("*.cabal", { root: dir }),
  ];
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
    makeTarget: () =>
      outdent`
      export const ${parsedCabal.name} = garn.haskell.mkHaskellProject({
        description: "${parsedCabal.synopsis || parsedCabal.description || ""}",
        executable: "",
        compiler: "ghc94",
        src: "."
      })`,
  };
};

export const initializers = [mkHaskellProjectInitializer];
