export type OldPackage = {
  tag: "oldPackage";
  description: string;
  nixExpression: string;
  envExpression: (nixExpression: string) => string;
  addDevTools: (extraDevTools: Array<OldPackage>) => OldPackage;
  extraDevTools: Array<OldPackage>;
  setStartCommand: (startCommand: Array<string>) => OldPackage;
  startCommand: Array<string> | null;
};

export const mkOldPackage = (args: {
  expression: string;
  description: string;
}): OldPackage => ({
  tag: "oldPackage",
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
  addDevTools(this: OldPackage, extraDevTools) {
    return {
      ...this,
      extraDevTools: [...this.extraDevTools, ...extraDevTools],
    };
  },
  extraDevTools: [],
  setStartCommand(this: OldPackage, startCommand) {
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
