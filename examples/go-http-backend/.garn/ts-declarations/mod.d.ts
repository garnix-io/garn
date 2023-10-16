export declare type Package = {
  tag: "package";
  nixExpression: NixExpression;
  description?: string;
};

export declare type NixExpression = { rawNixExpressionString: string };

export declare type Check = {
  tag: "check";
  nixExpression: NixExpression;
};

export declare type Executable = {
  tag: "executable";
  description: string;
  nixExpression: NixExpression;
};

export declare type Project = {
  /**
   * Returns a new Project with the provided devtools added to the default
   * Environment.
   */
  withDevTools<T extends Project>(this: T, devTools: Array<Package>): T;
  /**
   * A tagged template literal that runs the given command inside the Project's
   * default Environment.
   *
   * Example:
   * ```typescript
   * const myExecutable = myProject.shell`echo "hello world"`;
   * ```
   */
  shell(
    this: Project,
    _s: TemplateStringsArray,
    ..._args: Array<string>
  ): Executable;
  /**
   * Returns a check that runs in a *pure* version of the Project's default
   * Environment.
   */
  check(
    this: Project,
    _s: TemplateStringsArray,
    ..._args: Array<string>
  ): Check;
  /**
   * Adds a Check with the given name to the Project that runs in a *pure*
   * version of the Project's default Environment.
   *
   * Example:
   * ```typescript
   * myProject.addCheck("noTodos")`! grep -r TODO .`
   * ```
   */
  addCheck<T extends Project, Name extends string>(
    name: Name
  ): (
    _s: TemplateStringsArray,
    ..._args: Array<string>
  ) => T & Record<Name, Check>;
};

export declare namespace go {
  export const mkGoProject: (args: {
    description: string;
    moduleName: string;
    src: string;
    goVersion?: "1.18" | "1.19" | "1.20";
  }) => Project & { pkg: Package; };
}
