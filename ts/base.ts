export type Package = {
  tag: "package";
  nixExpression: string;
};

export const mkPackage = (args: { attribute: string }): Package => ({
  tag: "package",
  nixExpression: args.attribute,
});
