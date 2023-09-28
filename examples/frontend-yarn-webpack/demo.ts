import * as g from "./deps.ts";
import * as config from "./garner.ts";

const configs = Object.entries(config);

let c = 0;
function mkName() {
  return `unknown${c}`;
}

const pub = configs.reduce(
  (acc: Record<string, string>, [name, packageOrRunnable]) => ({
    ...acc,
    [name]: packageOrRunnable.type === "package" ? packageOrRunnable.nixExpr : `
      pkgs.writeScriptBin ${JSON.stringify(name)} ''
        ${g.nixInterpolatedToNixExpr(packageOrRunnable.cmd)}
      ''
    `,
  }),
  {},
);

const priv: Record<string, string> = {
  // devDependencies: `123`,
};

console.log(pub);

// const pub = {
//   foo: `
//     pkgs.runCommand "foo" {} ''
//       mkdir -p $out/bin
//       echo ${
//     JSON.stringify(JSON.stringify({
//     }))
//   } > $out/pc.yml
//       echo '\${pkgs."process-compose"}/bin/process-compose -f "$(dirname $0)/../pc.yml"' > $out/bin/foo
//       chmod +x $out/bin/foo
//     ''
//   `,
// };

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

const cmd = new Deno.Command("nix", {
  args: ["run", `.#${Deno.args[0]}`],
  stdout: "piped",
}).spawn();
cmd.stdout.pipeTo(Deno.stdout.writable);
await cmd.status;
