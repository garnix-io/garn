import * as haskell from "./haskell.ts";
import * as typescript from "./typescript.ts";

let imports = "";
let body = "";

const initializers = Array.prototype.concat(
  haskell.initializers,
  typescript.initializers
);

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
