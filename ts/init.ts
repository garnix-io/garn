import { initializers } from "./base.ts";
import { } from "./haskell.ts";

let imports = ""
let body = ""

for (const init of initializers) {
    const result = init()
    switch (result.tag)  {
      case "UnexpectedError":
        console.log(result.reason)
        break
      case "ShouldNotRun":
        break
      case "ShouldRun":
        imports += result.imports
        body += result.makeTarget()
        break
    }
}

Deno.writeTextFileSync("garner.ts", imports + "\n\n" + body)
