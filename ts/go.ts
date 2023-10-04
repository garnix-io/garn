import { mkProject } from "http://localhost:8777/project.ts";
import { Package, mkPackage } from "./package.ts";
import { ProjectWithDefaultEnvironment } from "./project.ts";
import { Executable, shell } from "./executable.ts";
import { Environment, packageToEnvironment } from "./environment.ts";

export const mkGoProject = (args: {
  description: string;
  moduleName: string;
  src: string;
}): ProjectWithDefaultEnvironment & {
  pkg: Package;
  devShell: Environment;
  main: Executable;
} => {
  const pkg = mkPackage(
    `
    pkgs.buildGoModule {
      name = "go-project";
      src = ${args.src};
      vendorHash = null;
    }
  `
  );

  return mkProject(
    args.description,
    {
      pkg,
      devShell: packageToEnvironment(pkg),
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
