export type Package = {
  tag: "package";
  nixExpression: string;
  envExpression: (nixExpression: string) => string;
  addDevTools: (devTools: Array<Package>) => Package;
  devTools: Array<Package>;
};

export const mkPackage = (args: { expression: string }): Package => ({
  tag: "package",
  nixExpression: args.expression,
  envExpression(nixRef): string {
    return (
      ` let expr = ${nixRef};
        in
          (if expr ? env
            then expr.env
            else pkgs.mkShell {inputsFrom = [ expr ];}
          ).overrideAttrs (finalAttrs: previousAttrs: {
              nativeBuildInputs =
                previousAttrs.nativeBuildInputs
                ++
                ${this.devTools.map((p) => p.nixExpression).join()};
          })`
    );
  },
  addDevTools(this: Package, devTools) {
    this.devTools = this.devTools.concat(devTools); return this;
  },
  devTools: [],
});
