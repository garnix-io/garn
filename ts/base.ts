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

export const initializers: Initializer[] = [];

export const addInitializer = (args: Initializer): void => {
  initializers.push(args);
};
