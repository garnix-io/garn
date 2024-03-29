export type Initializer = (
  directory: string,
) =>
  | { tag: "ShouldRun"; imports?: Array<string>; makeTarget: () => string }
  | { tag: "ShouldNotRun" }
  | { tag: "UnexpectedError"; reason: string };
