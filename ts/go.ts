import { Environment, packageToEnvironment, shell } from "./environment.ts";
import { Executable } from "./executable.ts";
import { Package, mkPackage } from "./package.ts";
import { ProjectWithDefaultEnvironment, mkProject } from "./project.ts";

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
      devShell: packageToEnvironment(pkg, args.src),
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
