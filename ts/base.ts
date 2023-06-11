export type Package = {
  tag: "package";
  nixExpression: string;
  envExpression: (nixExpression: string) => string;
};

export const mkPackage = (args: {
  attribute: string;
  env?: (x: string) => string;
}): Package => ({
  tag: "package",
  nixExpression: args.attribute,
  envExpression: args.env
    ? args.env
    : (x) =>
        `pkgs.mkShell({
         inputsFrom = [ ${x} ];
        })`,
});
