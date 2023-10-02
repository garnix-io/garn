let emptyEnvironment: Environment;

type Environment = {
  nixExpr?: string;
};

type Executable = {
  environment: Environment;
  exec: string;
  argv: Array<string>;
};

type Check = {
  nixExpr: string;
};

type Package = {
  nixExpr: string;

  disableCheck(this: Package): Package;
};

type ProjectSettings = {
  defaults: {
    executable?: string;
    environment?: string;
  };
};

type Project = ProjectSettings & {
  shell(
    _s: TemplateStringsArray,
    ..._args: Array<Package | string>
  ): Executable;
  check(
    _s: TemplateStringsArray,
    ..._args: Array<Package | string>
  ): Check;
};
