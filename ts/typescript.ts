import { Package, mkPackage } from "./base.ts";
import { Initializer } from "./initializer.ts";
import * as fs from "https://deno.land/std@0.201.0/fs/mod.ts";
import { nixSource } from "./utils.ts";
import outdent from "http://deno.land/x/outdent/mod.ts";
import { assertEquals } from "https://deno.land/std@0.201.0/assert/mod.ts";

const nodeVersions = {
  "14": {
    pkg: "nodejs-14_x",
    permittedInsecurePackages: ["nodejs-14.21.3", "openssl-1.1.1v"],
  },
  "16": {
    pkg: "nodejs-16_x",
    permittedInsecurePackages: ["nodejs-16.20.2"],
  },
  "18": {
    pkg: "nodejs-18_x",
    permittedInsecurePackages: [],
  },
} satisfies Record<
  string,
  { pkg: string; permittedInsecurePackages: Array<string> }
>;

type NodeVersion = keyof typeof nodeVersions;

const fromNodeVersion = (version: NodeVersion) => {
  const { pkg, permittedInsecurePackages } = nodeVersions[version];
  return {
    pkgs: `
      import "\${nixpkgs}" {
        config.permittedInsecurePackages = [${permittedInsecurePackages
          .map((x) => JSON.stringify(x))
          .join(" ")}];
        inherit system;
      }
    `.trim(),
    nodejs: `pkgs.${pkg}`,
  };
};

export const mkNpmFrontend = (args: {
  description: string;
  src: string;
  nodeVersion: NodeVersion;
  testCommand: string;
}): Package => {
  const { pkgs, nodejs } = fromNodeVersion(args.nodeVersion);
  return mkPackage({
    description: args.description,
    expression: `
      let
        npmlock2nix = import npmlock2nix-repo {
          inherit pkgs;
        };
        pkgs = ${pkgs};
      in
      npmlock2nix.v2.build
        {
          src = ${nixSource(args.src)};
          buildCommands = [ ${JSON.stringify(args.testCommand)} "mkdir $out" ];
          installPhase = "true";
          node_modules_attrs = {
            nodejs = ${nodejs};
          };
        }
  `,
  }).setStartCommand(["npm", "run", "start"]);
};

export const mkYarnFrontend = (args: {
  description: string;
  src: string;
  nodeVersion: keyof typeof nodeVersions;
  testCommand: string;
  serverStartCommand: string;
}): Package => {
  const { pkgs, nodejs } = fromNodeVersion(args.nodeVersion);
  return mkPackage({
    description: args.description,
    expression: `
      let
          pkgs = ${pkgs};
          packageJson = pkgs.lib.importJSON ./package.json;
          yarnPackage = pkgs.yarn2nix-moretea.mkYarnPackage {
            nodejs = ${nodejs};
            yarn = pkgs.yarn;
            src = ${nixSource(args.src)};
            buildPhase = ${JSON.stringify(args.testCommand)};
          };
      in
        (pkgs.writeScriptBin "start-server" ''
          #!/usr/bin/env bash

          set -eu

          export PATH=\${pkgs.yarn}/bin:$PATH
          export PATH=\${yarnPackage}/libexec/\${packageJson.name}/node_modules/.bin:$PATH
          yarn --version
          ${args.serverStartCommand}
        '')
    `,
  });
};

// Initializers

const mkNpmFrontendInitializer: Initializer = (path : string) => {
  const existsPkgJson = fs.existsSync("package.json");
  const existsPkgLock = fs.existsSync("package-lock.json");
  if (!existsPkgJson || !existsPkgLock) {
    return { tag: "ShouldNotRun" };
  }
  const contents = Deno.readTextFileSync("package.json");
  try {
    const packageJson = JSON.parse(contents);
    return {
      tag: "ShouldRun",
      imports:
        'import { mkHaskell } from "http://localhost:8777/typescript.ts"',
      makeTarget: () =>
        outdent`
          export const ${packageJson.name || "frontend"} = mkNpmFrontend({
              description: "${packageJson.description || "An NPM frontend"}",
              src: "${path}",
              nodeVersion: "18",
              testCommand: ""
          })
        `,
    };
  } catch (_e) {
    return {
      tag: "UnexpectedError",
      reason: "Could not parse package.json",
    };
  }
};

export const initializers = [mkNpmFrontendInitializer];

// Tests

Deno.test(
  "NPM initializer does not run when no package.json is present",
  () => {
    const tempDir = Deno.makeTempDirSync();
    Deno.chdir(tempDir);
    const result = mkNpmFrontendInitializer(".");
    assertEquals(result.tag, "ShouldNotRun");
  }
);

Deno.test(
  "NPM initializer does not run when no package-lock.json is present",
  () => {
    const tempDir = Deno.makeTempDirSync();
    Deno.chdir(tempDir);
    Deno.writeTextFileSync("./package.json", "{}");
    const result = mkNpmFrontendInitializer(".");
    assertEquals(result.tag, "ShouldNotRun");
  }
);

Deno.test("NPM initializer errors if package.json is unparseable", () => {
  const tempDir = Deno.makeTempDirSync();
  Deno.chdir(tempDir);
  Deno.writeTextFileSync("./package-lock.json", "{}");
  Deno.writeTextFileSync(
    "./package.json",
    `
    name: foo
  `
  );
  const result = mkNpmFrontendInitializer(".");
  assertEquals(result.tag, "UnexpectedError");
  if (result.tag === "UnexpectedError") {
    assertEquals(result.reason, "Could not parse package.json");
  }
});

Deno.test("NPM initializer returns the code to be generated", () => {
  const tempDir = Deno.makeTempDirSync();
  Deno.chdir(tempDir);
  Deno.writeTextFileSync("./package-lock.json", "{}");
  Deno.writeTextFileSync(
    "./package.json",
    `
    {
        "name": "somepackage",
        "description": "just some package"
    }
  `
  );
  const result = mkNpmFrontendInitializer("./somewhere");
  assertEquals(result.tag, "ShouldRun");
  if (result.tag === "ShouldRun") {
    assertEquals(
      result.makeTarget(),
      outdent`
          export const somepackage = mkNpmFrontend({
              description: "just some package",
              src: "./somewhere",
              nodeVersion: "18",
              testCommand: ""
          })
        `
    );
  }
});

Deno.test(
  "NPM initializer has sensible defaults if name and description are missing",
  () => {
    const tempDir = Deno.makeTempDirSync();
    Deno.chdir(tempDir);
    Deno.writeTextFileSync("./package-lock.json", "{}");
    Deno.writeTextFileSync(
      "./package.json",
      `
    {
    }
  `
    );
    const result = mkNpmFrontendInitializer(".");
    assertEquals(result.tag, "ShouldRun");
    if (result.tag === "ShouldRun") {
      assertEquals(
        result.makeTarget(),
        outdent`
          export const frontend = mkNpmFrontend({
              description: "An NPM frontend",
              src: ".",
              nodeVersion: "18",
              testCommand: ""
          })
        `
      );
    }
  }
);
