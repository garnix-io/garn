import { initializers as haskellInitializers } from "./haskell.ts";

let imports = "";
let body = "";

const initializers = haskellInitializers;

console.error("[garner] Creating a garner.ts file");

for (const init of initializers) {
  const result = init();
  switch (result.tag) {
    case "UnexpectedError":
      console.error("[garner] " + result.reason);
      break;
    case "ShouldNotRun":
      break;
    case "ShouldRun":
      imports += result.imports;
      body += result.makeTarget();
      break;
  }
}

Deno.writeTextFileSync("garner.ts", imports + "\n\n" + body);
