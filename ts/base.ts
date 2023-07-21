export type Package = {
  tag: "package";
  nixExpression: string;
  envExpression: (nixExpression: string) => string;
};

export const mkPackage = (args: { expression: string }): Package => ({
  tag: "package",
  nixExpression: args.expression,
  envExpression: (nixExpression) =>
    ` let expr = ${nixExpression};
      in if ${nixExpression} ? env
         then ${nixExpression}.env
         else pkgs.mkShell({
           inputsFrom = [ ${nixExpression} ];
         })`,
});
