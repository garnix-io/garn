import { Package, mkPackage } from "./base.ts";

export const mkGoProject = (args: {
  description: string;
  src: string;
}): Package =>
  mkPackage({
    expression: `
      pkgs.buildGoModule {
        name = "go-project";
        src = ${args.src};
        vendorHash = null;
      }
    `,
    description: args.description,
  });
