import { Environment, EnvironmentHelpers } from "./environment.ts";
import { hasTag } from "./utils.ts";

export type Project = {
  tag: "project";
  settings: ProjectSettings;
};

type ProjectSettings = {
  defaults?: {
    executable?: string;
    environment?: string;
  };
};

// In the future we plan on adding Project, Check, etc..
type Nestable = Environment;

type ProjectWithEnvironmentHelpers<Settings> = Settings extends
  { defaults: { environment: string } } ? EnvironmentHelpers
  : Record<string, unknown>;

function proxyEnvironmentHelpers(): EnvironmentHelpers {
  return {
    shell() {
      throw 1;
    },
    check() {
      throw 1;
    },
    withDevTools() {
      throw 1;
    }
  };
}

export const mkProject = <
  Deps extends Record<string, Nestable>,
  Settings extends ProjectSettings,
>(
  deps: Deps,
  settings: Settings,
):
  & Deps
  & Project
  & ProjectWithEnvironmentHelpers<Settings> => {
  const helpers =
    (settings.defaults?.environment != null
      ? proxyEnvironmentHelpers()
      : {}) as ProjectWithEnvironmentHelpers<Settings>;

  return {
    ...deps,
    ...helpers,
    tag: "project",
    settings,
  };
};

export function isProject(p: unknown): p is Project {
  return hasTag(p, "project");
}
