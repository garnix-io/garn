import { Schema, z } from "https://deno.land/x/zod@v3.22.2/mod.ts";

type ProjectSettings = {
  defaults?: {
    executable?: string;
    environment?: string;
  };
};

export type Project = ProjectSettings & {
  // shell(
  //   _s: TemplateStringsArray,
  //   ..._args: Array<Package | string>
  // ): Executable;
  // check(
  //   _s: TemplateStringsArray,
  //   ..._args: Array<Package | string>
  // ): Check;
};

export const projectSchema: Schema<Project> = z
  .object({
    defaults: z
      .object({
        executable: z.string().optional(),
        environment: z.string().optional(),
      })
      .optional(),
  })
  .passthrough();

export const mkProject = <T extends ProjectSettings>(
  settings: T
): T & Project => {
  return settings;
};

export type Package = {
  tag: "package";
  description: string;
  nixExpression: string;
  envExpression: (nixExpression: string) => string;
  addDevTools: (extraDevTools: Array<Package>) => Package;
  extraDevTools: Array<Package>;
  setStartCommand: (startCommand: Array<string>) => Package;
  startCommand: Array<string> | null;
};

export const mkPackage = (args: {
  expression: string;
  description: string;
}): Package => ({
  tag: "package",
  description: args.description,
  nixExpression: args.expression,
  envExpression(nixRef): string {
    const devToolsOverride =
      this.extraDevTools.length == 0
        ? ""
        : `.overrideAttrs (finalAttrs: previousAttrs: {
              nativeBuildInputs =
                previousAttrs.nativeBuildInputs
                ++
                [ ${this.extraDevTools.map((p) => p.nixExpression).join(" ")} ];
          })`;
    return ` let expr = ${nixRef};
        in
          (if expr ? env
            then expr.env
            else pkgs.mkShell {inputsFrom = [ expr ];}
          )${devToolsOverride}`;
  },
  addDevTools(this: Package, extraDevTools) {
    return {
      ...this,
      extraDevTools: [...this.extraDevTools, ...extraDevTools],
    };
  },
  extraDevTools: [],
  setStartCommand(this: Package, startCommand) {
    return {
      ...this,
      startCommand,
    };
  },
  startCommand: null,
});

export type ShouldNotRun =
  | { tag: "ShouldNotRun" }
  | { tag: "UnexpectedError"; reason: string };

export type Initializer = () =>
  | { tag: "ShouldRun"; imports: string; makeTarget: () => string }
  | { tag: "ShouldNotRun" }
  | { tag: "UnexpectedError"; reason: string };
