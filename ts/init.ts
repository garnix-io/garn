import * as haskell from "./haskell.ts";

let imports = "";
let body = "";

const initializers = haskell.initializers;

console.error("[garner] Creating a garn.ts file");

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

Deno.writeTextFileSync("garn.ts", imports + "\n\n" + body);
