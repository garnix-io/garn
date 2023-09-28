type Arg = { nixExpr: string };

export function argToNix(a: Arg): string {
  return a.nixExpr;
}

export type NixInterpolated = {
  parts: Array<string>;
  interpolations: Array<Arg>;
};

export function nixInterpolatedToNixExpr(nixInterpolated: NixInterpolated) {
  return nixInterpolated.parts.reduce(
    (acc, part, idx) =>
      idx === 0
        ? part
        : `${acc}\${${
          argToNix(nixInterpolated.interpolations[idx - 1])
        }}${part}`,
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

type PackageData = {
  nixExpr: string;
};

type PackageHelpers = {
  type: "package";
  shell(parts: TemplateStringsArray, ...args: Array<Arg>): Runnable;
};

export type Package = PackageData & PackageHelpers;

function mkPackage(pkg: PackageData, devShellPath?: NixInterpolated): Package {
  return {
    ...pkg,
    type: "package",
    shell(parts, ...interpolations) {
      return {
        type: "runnable",
        path: devShellPath,
        cmd: { parts: [...parts], interpolations },
      };
      // if (!devShellPath) {
      //   return {
      //     type: "runnable",
      //     cmd: { parts: [...parts], interpolations },
      //   };
      // }
      // return {
      //   type: "runnable",
      //   cmd: {
      //     parts: [
      //       `export PATH=${devShellPath.parts[0]}`,
      //       ...devShellPath.parts.slice(1, -1),
      //       `${
      //         devShellPath.parts.length > 1
      //           ? devShellPath.parts[devShellPath.parts.length - 1]
      //           : ""
      //       }:$PATH\n\n${parts[0]}`,
      //       ...parts.slice(1),
      //     ],
      //     interpolations: [
      //       ...devShellPath.interpolations,
      //       ...interpolations,
      //     ],
      //   },
      // };
    },
  };
}

export function nixPkg(nixPkgName: string) {
  return mkPackage({ nixExpr: `pkgs.${nixPkgName}` });
}

// function binSymlink() {
//   return `
//     pkgs.runCommand "" {} ''
//       mkdir $out
//       ln -s \${devDependencies}/libexec/\${packageJson.name}/node_modules/.bin $out/bin
//     ''
//   `;
// }

export function mkYarnPackage(): Package {
  const nixExpr = {
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
     # buildPhase = "yarn mocha";
    }
  `,
  };
  const projectName = {
    nixExpr: `(pkgs.lib.importJSON ./package.json).name`,
  };
  return mkPackage(
    nixExpr,
    nix`${
      nixPkg("yarn")
    }/bin:${nixExpr}/libexec/${projectName}/node_modules/.bin`,
  );
}

export function processCompose(runnables: Record<string, Runnable>): Runnable {
  const processComposeConfigPkg = mkPackage({
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
