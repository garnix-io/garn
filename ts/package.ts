export type NewPackage = {
  nixExpression: string;

  // disableCheck(this: Package): Package;
};

export const mkNewPackage = (nixExpression: string): NewPackage => ({
  nixExpression,
});
