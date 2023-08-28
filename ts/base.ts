export type Package = {
  tag: "package";
  nixExpression: string;
  envExpression: (nixExpression: string) => string;
  addDevTools: (extraDevTools: Array<Package>) => Package;
  extraDevTools: Array<Package>;
};

export const mkPackage = (args: { expression: string }): Package => ({
  tag: "package",
  nixExpression: args.expression,
  envExpression(nixRef): string {
    return ` let expr = ${nixRef};
        in
          (if expr ? env
            then expr.env
            else pkgs.mkShell {inputsFrom = [ expr ];}
          ).overrideAttrs (finalAttrs: previousAttrs: {
              nativeBuildInputs =
                previousAttrs.nativeBuildInputs
                ++
                [ ${this.extraDevTools.map((p) => p.nixExpression).join(" ")} ];
          })`;
  },
  addDevTools(this: Package, extraDevTools) {
    return {
      ...this,
      extraDevTools: [...this.extraDevTools, ...extraDevTools],
    };
  },
  extraDevTools: [],
});
