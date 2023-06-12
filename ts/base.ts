export type Package = {
  tag: "package";
  nixExpression: string;
  envExpression: (nixExpression: string) => string;
};

export const mkPackage = (args: {
  attribute: string;
  env?: (nixExpression: string) => string;
}): Package => ({
  tag: "package",
  nixExpression: args.attribute,
  envExpression: args.env
    ? args.env
    : (nixExpression) =>
    ` let expr = ${nixExpression};
      in if ${nixExpression} ? env
         then ${nixExpression}.env
         else pkgs.mkShell({
           inputsFrom = [ ${nixExpression} ];
         })`,
});
