type Arg = string | { nixExpr: NixInterpolated };

function argToNix(a: Arg): string {
  if (typeof a === "string") return a;
  return `\${${nixInterpolatedToNixExpr(a.nixExpr)}}`;
}

export type NixInterpolated = string | {
  parts: Array<string>;
  interpolations: Array<Arg>;
};

export function nixInterpolatedToNixExpr(nixInterpolated: NixInterpolated) {
  if (typeof nixInterpolated === "string") return nixInterpolated;
  return nixInterpolated.parts.reduce(
    (acc, part, idx) =>
      idx === 0
        ? part
        : acc + argToNix(nixInterpolated.interpolations[idx - 1]) + part,
    "",
  );
}

export type Runnable = {
  type: "runnable";
  path?: NixInterpolated;
  cmd: NixInterpolated;
};

export function nix(
  parts: TemplateStringsArray,
  ...interpolations: Array<Arg>
): NixInterpolated {
  return { parts: [...parts], interpolations };
}

export function shell(
  parts: TemplateStringsArray,
  ...interpolations: Array<Arg>
): Runnable {
  return {
    type: "runnable",
    cmd: nix(parts, ...interpolations),
  };
}

type PackageData<Tasks extends string> = {
  nixExpr: NixInterpolated;
  tasks: Record<Tasks, Runnable>;
};

type PackageHelpers<Tasks extends string> = {
  type: "package";
  shell(parts: TemplateStringsArray, ...args: Array<Arg>): Runnable;
  addTasks<T extends string>(extend: Record<T, Runnable>): Package<Tasks | T>;
};

export type Package<Tasks extends string> =
  & PackageData<Tasks>
  & PackageHelpers<Tasks>;

function mkPackage<T extends string>(
  pkg: PackageData<T>,
  devShellPath?: NixInterpolated,
): Package<T> {
  const self: Package<T> = {
    ...pkg,
    type: "package",
    shell(parts, ...interpolations) {
      return {
        type: "runnable",
        path: devShellPath,
        cmd: { parts: [...parts], interpolations },
      };
    },
    addTasks(extend) {
      return { ...self, tasks: { ...self.tasks, ...extend } };
    },
  };
  return self;
}

export function nixPkg(nixPkgName: string) {
  return mkPackage({ nixExpr: `pkgs.${nixPkgName}`, tasks: {} });
}

// function binSymlink() {
//   return `
//     pkgs.runCommand "" {} ''
//       mkdir $out
//       ln -s \${devDependencies}/libexec/\${packageJson.name}/node_modules/.bin $out/bin
//     ''
//   `;
// }

export function mkYarnPackage(opts: {
  buildCmd: string;
  artifacts: Array<string>;
}): Package<"develop"> {
  const projectName = {
    nixExpr: `(pkgs.lib.importJSON ./package.json).name`,
  };
  const yarnDependencies = {
    nixExpr: `
      pkgs.yarn2nix-moretea.mkYarnPackage {
        nodejs = pkgs.nodejs-18_x;
        yarn = pkgs.yarn;
        src = (
          let
          lib = pkgs.lib;
          lastSafe = list:
            if lib.lists.length list == 0
          then null
          else lib.lists.last list;
          in
          builtins.path
          {
            path = ./.;
            filter = path: type:
              let
            fileName = lastSafe (lib.strings.splitString "/" path);
            in
            fileName == "package.json" || fileName == "yarn.lock";
          }
        );
      }
    `,
  };
  const nodeModuleBins = {
    nixExpr: nix`
      pkgs.runCommand "node-bins" {} ''
        mkdir $out
        ln -s ${yarnDependencies}/libexec/${projectName}/node_modules/.bin $out/bin
      ''
    `,
  };
  const buildPackage = {
    tasks: {},
    nixExpr: nix`
      pkgs.stdenv.mkDerivation {
        name = "${projectName}";
        src = ./.;
        nativeBuildInputs = [ pkgs.yarn (${
      nixInterpolatedToNixExpr(nodeModuleBins.nixExpr)
    }) ];
        buildPhase = ''
          ln -s ${yarnDependencies}/libexec/${projectName}/node_modules node_modules
          ${opts.buildCmd}
        '';
        installPhase = ''
          mkdir $out
          mv ${opts.artifacts.join(" ")} $out
        '';
      }
    `,
  };
  const pkg = mkPackage(
    buildPackage,
    nix`${
      nixPkg("yarn")
    }/bin:${yarnDependencies}/libexec/${projectName}/node_modules/.bin`,
  );
  return pkg.addTasks({ develop: pkg.shell`yarn start` });
}

export function processCompose(runnables: Record<string, Runnable>): Runnable {
  const processComposeConfigPkg = mkPackage({
    tasks: {},
    nixExpr: `
      pkgs.writeText "process-compose.yml" (builtins.toJSON {
        version = "0.5";
        processes = {${
      Object.entries(runnables).reduce(
        (acc: Array<string>, [name, runnable]) => [
          ...acc,
          `
            ${name} = {
              command = "${nixInterpolatedToNixExpr(runnable.cmd)}";
              ${
            runnable.path
              ? `environment = [ "PATH=${
                nixInterpolatedToNixExpr(runnable.path)
              }" ];`
              : ""
          }
              availability = { restart = "always"; };
            };
          `,
        ],
        [],
      ).join("\n")
    }};
      })
    `,
  });
  return shell`${
    nixPkg("process-compose")
  }/bin/process-compose -f ${processComposeConfigPkg}`;
  // return shell`cat ${processComposeConfigPkg}`;
}

export function mkFooBackend(): Package<"server" | "format"> {
  return mkPackage({
    nixExpr: "1",
    tasks: {
      server:
        shell`echo listening on port 8000\nwhile true; do\necho -e 'HTTP/1.1 200 OK\\n\\nHello from backend' | nc -w 0 -l 8000\ndone`,
      format: shell`echo formatting...`,
    },
  });
}
