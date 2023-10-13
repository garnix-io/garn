export type ShouldNotRun =
  | { tag: "ShouldNotRun" }
  | { tag: "UnexpectedError"; reason: string };

export type Initializer = () =>
  | { tag: "ShouldRun"; imports?: Array<string>; makeTarget: () => string }
  | { tag: "ShouldNotRun" }
  | { tag: "UnexpectedError"; reason: string };
