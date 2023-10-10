import { Environment, packageToEnvironment, shell } from "./environment.ts";
import { Executable } from "./executable.ts";
import { nixStrLit } from "./nix.ts";
import { mkPackage, Package } from "./package.ts";
import { mkProject, Project } from "./project.ts";
import * as path from "https://deno.land/std@0.202.0/path/mod.ts";
import { getDotGarnProjectDir } from "./internals/garn_dir.ts";

export const GOMOD2NIX_REPO =
  "github:nix-community/gomod2nix?rev=f95720e89af6165c8c0aa77f180461fe786f3c21";

const getGoModNixToml = (src: string) => {
  const gen = new Deno.Command("nix", {
    args: [
      "run",
      GOMOD2NIX_REPO,
      "--",
      "--dir",
      src,
      "--outdir",
      getDotGarnProjectDir(src),
    ],
  }).outputSync();
  if (!gen.success) {
    throw Error(
      [
        "Failed to generate gomod2nix.toml:",
        "",
        "Stdout:",
        new TextDecoder().decode(gen.stdout),
        "",
        "Stderr:",
        new TextDecoder().decode(gen.stderr),
      ].join("\n")
    );
  }
  return Deno.readTextFileSync(
    path.join(getDotGarnProjectDir(src), "gomod2nix.toml")
  );
};

/**
 * Create a go-based garn Project.
 */
export const mkGoProject = (args: {
  description: string;
  moduleName: string;
  src: string;
}): Project & {
  pkg: Package;
  devShell: Environment;
  main: Executable;
} => {
  const pkg = mkPackage(
    `
      let
        gomod2nix = gomod2nix-repo.legacyPackages.\${system};
        gomod2nix-toml = pkgs.writeText "gomod2nix-toml" ${
          nixStrLit`${getGoModNixToml(args.src)}`.nixExpression
        };
      in
        gomod2nix.buildGoApplication {
          pname = ${nixStrLit`${args.moduleName}`.nixExpression};
          version = "0.1";
          src = ${args.src};
          modules = gomod2nix-toml;
        }
    `
  );

  return mkProject(
    args.description,
    {
      pkg,
      devShell: packageToEnvironment(pkg, args.src).withDevTools([
        mkPackage("pkgs.gopls"),
      ]),
      main: shell`${pkg}/bin/${args.moduleName}`,
    },
    {
      defaults: {
        environment: "devShell",
        executable: "main",
      },
    }
  );
};
