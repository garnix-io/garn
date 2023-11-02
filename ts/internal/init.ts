import * as go from "../go/initializers.ts";
import * as haskell from "../haskell/initializers.ts";
import * as javascript from "../javascript/initializers.ts";
import outdent from "https://deno.land/x/outdent@v0.8.0/mod.ts";
import { GARN_TS_LIB_VERSION } from "./utils.ts";

const imports = [
  `import * as garn from "https://garn.io/ts/${GARN_TS_LIB_VERSION}/mod.ts";`,
  `import * as pkgs from "https://garn.io/ts/${GARN_TS_LIB_VERSION}/nixpkgs.ts";`,
];
const initializedSections = [];

const initializers = [
  ...go.initializers,
  ...haskell.initializers,
  ...javascript.initializers,
];

for (const init of initializers) {
  const result = init(Deno.cwd());
  switch (result.tag) {
    case "UnexpectedError":
      console.error("[garn] " + result.reason);
      break;
    case "ShouldNotRun":
      break;
    case "ShouldRun":
      if (result.imports) imports.push(...result.imports);
      initializedSections.push(result.makeTarget().trim());
      break;
  }
}

if (initializedSections.length === 0) {
  console.error(
    "[garn] Cannot detect any project toolchains, sorry! Creating example garn.ts file",
  );
  initializedSections.push(outdent`
    // Welcome to garn! \`garn init\` was unable to find any existing supported
    // projects, but it is easy to get started!


    // Check out the language helper functions under garn.go, garn.haskell, and
    // garn.javascript.
    //
    // For example:

    // export const myGoProject = garn.go.mkGoProject({
    //   description: "My go project",
    //   src: "./my-go-project",
    //   goVersion: "1.20",
    // });

    // export const myHaskellProject = garn.haskell.mkHaskellProject({
    //   description: "My haskell project",
    //   executable: "server",
    //   compiler: "ghc94",
    //   src: "./my-haskell-project",
    // });

    // export const myNodeProject = garn.javascript.mkNpmProject({
    //   description: "My node project",
    //   src: "./my-node-project",
    //   nodeVersion: "18",
    // });


    // You can also manually create environments and projects. For example
    // uncomment this block and you can run \`garn enter myProject\` to be put into a
    // shell with cowsay installed, and \`garn run myProject\` to execute the default
    // executable for this project.

    // const myProjectEnvironment = garn.mkEnvironment().withDevTools([pkgs.cowsay]);

    // export const myProject = garn.mkProject({
    //   description: "My project",
    //   defaultEnvironment: myProjectEnvironment,
    //   defaultExecutable: myProjectEnvironment.shell\`cowsay "Hello from garn!"\`,
    // }, {});


    // In order to get an isolated vscodium that ships with Deno and is
    // pre-configured to use Deno's LSP, you can use the following:

    // Run with \`garn run edit\`.
    // export const edit = garn.editGarnConfig;
  `);
} else {
  console.error("[garn] Creating a garn.ts file");
}

Deno.writeTextFileSync(
  "garn.ts",
  `
${imports.join("\n")}

${initializedSections.join("\n")}
`.trim() + "\n",
);
