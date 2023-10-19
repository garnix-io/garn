import { packageToEnvironment, shell } from "../environment.ts";
import { NixExpression, nixRaw, nixStrLit } from "../nix.ts";
import { mkPackage, Package } from "../package.ts";
import { mkProject, Project } from "../project.ts";
import * as path from "https://deno.land/std@0.202.0/path/mod.ts";
import { getDotGarnProjectDir } from "../internal/garn_dir.ts";
import { nixSource } from "../internal/utils.ts";
import { GOMOD2NIX_REPO } from "./consts.ts";

const getGoModNixToml = (src: string): NixExpression => {
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
  return nixStrLit(
    Deno.readTextFileSync(
      path.join(getDotGarnProjectDir(src), "gomod2nix.toml")
    )
  );
};

const GO_VERSION_TO_NIXPKG_NAME = {
  "1.18": "go_1_18",
  "1.19": "go_1_19",
  "1.20": "go_1_20",
};

/**
 * Create a go-based garn Project.
 */
export const mkGoProject = (args: {
  description: string;
  moduleName: string;
  src: string;
  goVersion?: keyof typeof GO_VERSION_TO_NIXPKG_NAME;
}): Project & {
  pkg: Package;
} => {
  const pkg = mkPackage(
    nixRaw`
      let
        gomod2nix = gomod2nix-repo.legacyPackages.\${system};
        gomod2nix-toml = pkgs.writeText "gomod2nix-toml" ${getGoModNixToml(
          args.src
        )};
      in
        gomod2nix.buildGoApplication {
          pname = ${nixStrLit(args.moduleName)};
          version = "0.1";
          go = pkgs.${nixRaw(
            GO_VERSION_TO_NIXPKG_NAME[args.goVersion ?? "1.20"]
          )};
          src = ${nixSource(args.src)};
          modules = gomod2nix-toml;
        }
    `
  );

  return mkProject(
    {
      description: args.description,
      defaultEnvironment: packageToEnvironment(pkg, args.src).withDevTools([
        mkPackage(nixRaw`pkgs.gopls`),
      ]),
      defaultExecutable: shell`${pkg}/bin/${args.moduleName}`,
    },
    {
      pkg,
    }
  );
};
