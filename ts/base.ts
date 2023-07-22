export type Package = {
  tag: "package";
  nixExpression: string;
  envExpression: (nixExpression: string) => string;
};

export const mkPackage = (args: { expression: string }): Package => ({
  tag: "package",
  nixExpression: args.expression,
  envExpression: (nixRef) =>
    ` let expr = ${nixRef};
      in if expr ? env
         then expr.env
         else pkgs.mkShell { inputsFrom = [ expr ]; }`,
});
