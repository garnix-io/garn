import { Environment, isEnvironment } from "./environment.ts";
import { NewPackage } from "./package.ts";
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

type ProjectWithEnvironmentHelpers<Settings> = Settings extends {
  defaults: { environment: string };
}
  ? {
      withDevTools<
        T extends Project & { settings: { defaults: { environment: string } } }
      >(
        this: T,
        devTools: Array<NewPackage>
      ): T;
    }
  : Record<string, unknown>;

function proxyEnvironmentHelpers(environment: Environment) {
  return {
    shell() {
      throw 1;
    },
    check() {
      throw 1;
    },
    withDevTools<
      T extends Project & { settings: { defaults: { environment: string } } }
    >(this: T, devTools: Array<NewPackage>): T {
      const newEnvironment = environment.withDevTools(devTools);
      return {
        ...this,
        [this.settings.defaults.environment]: newEnvironment,
      };
    },
  };
}

export const mkProject = <
  Deps extends Record<string, Nestable>,
  Settings extends ProjectSettings
>(
  deps: Deps,
  settings: Settings
): Deps & Project & ProjectWithEnvironmentHelpers<Settings> => {
  const environment = getDefaultEnvironment(deps, settings);
  const helpers = (
    environment != null ? proxyEnvironmentHelpers(environment) : {}
  ) as ProjectWithEnvironmentHelpers<Settings>;

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

export const projectDefaultEnvironment = (
  project: Project
): Environment | undefined => {
  return getDefaultEnvironment(project, project.settings);
};

const getDefaultEnvironment = (
  project: Record<string, unknown>,
  settings: ProjectSettings
): Environment | undefined => {
  if (settings.defaults?.environment == null) {
    return undefined;
  }
  if (!(settings.defaults?.environment in project)) {
    throw new Error(
      `defaults.environment points to a non-existing field: ${settings.defaults.environment}`
    );
  }
  const environment: unknown =
    project[settings.defaults.environment as keyof typeof project];
  if (!isEnvironment(environment)) {
    throw new Error(
      `defaults.environment points to a non-environment: ${settings.defaults.environment}`
    );
  }
  if (!environment.nixExpr) {
    throw new Error(`TODO: Handle empty environment`);
  }
  return environment;
};
