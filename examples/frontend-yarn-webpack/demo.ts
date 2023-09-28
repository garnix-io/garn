import * as g from "./deps.ts";
import * as config from "./garner.ts";

const configs = Object.entries(config);

function runnableToNix(name: string, r: g.Runnable) {
  return `
    pkgs.writeScriptBin ${JSON.stringify(name)} ''
      ${r.path ? `export PATH=${g.nixInterpolatedToNixExpr(r.path)}:$PATH` : ""}
      ${g.nixInterpolatedToNixExpr(r.cmd)}
    ''
  `;
}

const pub = configs.reduce(
  (acc: Record<string, string>, [name, packageOrRunnable]) => ({
    ...acc,
    ...(packageOrRunnable.type === "package"
      ? {
        [name]: g.nixInterpolatedToNixExpr(packageOrRunnable.nixExpr),
        ...Object.entries(packageOrRunnable.tasks).reduce(
          (acc: Record<string, string>, [taskName, task]) => ({
            ...acc,
            [`${name}__${taskName}`]: runnableToNix(taskName, task),
          }),
          {},
        ),
      }
      : {
        [name]: runnableToNix(name, packageOrRunnable),
      }),
  }),
  {},
);

const priv: Record<string, string> = {};

Deno.writeTextFileSync(
  "flake.nix",
  `
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/841889913dfd06a70ffb39f603e29e46f45f0c1a";

  inputs.npmlock2nix-repo = {
    url = "github:nix-community/npmlock2nix?rev=9197bbf397d76059a76310523d45df10d2e4ca81";
    flake = false;
  };

  outputs = { self, nixpkgs, npmlock2nix-repo }:
    let
      systems = [ "x86_64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = import "\${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
          ${
    Object.entries(priv).map(([k, v]) => `${k} = ${v.trim()};`).join("\n")
  }
        in
        rec {
          ${
    Object.entries(pub).map(([k, v]) => `${k} = ${v.trim()};`).join("\n")
  }
        });
    };
}
`,
);

// @ts-ignore -
const selectedCfg = config[Deno.args[1]] as
  | undefined
  | (typeof config)[keyof typeof config];
if (Deno.args[0] === "run" && selectedCfg?.type === "package") {
  const tasks = Object.keys(selectedCfg.tasks);
  if (tasks.length === 0) {
    console.log(`${Deno.args[1]} has no tasks to run`);
  } else {
    console.log("Which task?");
    tasks.forEach((t) => console.log(`${Deno.args[1]}.${t}`));
  }
} else {
  const cmd = new Deno.Command("nix", {
    args: [Deno.args[0], `.#${Deno.args[1].replace(".", "__")}`],
    stdout: "piped",
  }).spawn();
  cmd.stdout.pipeTo(Deno.stdout.writable);
  await cmd.status;
}
