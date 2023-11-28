// This is the dependency file of the repo deployed at doc.deno.land. We import
// deno_doc's `doc` here to check if our modules are parsable by the version of
// deno_doc used by docland.
import { doc } from "https://raw.githubusercontent.com/denoland/docland/main/deps.ts";
import { describe, it } from "https://deno.land/std@0.206.0/testing/bdd.ts";

describe("docland", () => {
  it("is able to generate documentation for mod.ts", async () => {
    await doc(new URL("../mod.ts", import.meta.url).toString());
  });
});
