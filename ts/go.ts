import { OldPackage, mkOldPackage } from "./base.ts";

export const mkGoProject = (args: {
  description: string;
  src: string;
}): OldPackage =>
  mkOldPackage({
    expression: `
      pkgs.buildGoModule {
        name = "go-project";
        src = ${args.src};
        vendorHash = null;
      }
    `,
    description: args.description,
  });
