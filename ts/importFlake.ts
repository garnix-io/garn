import { Check } from "./check.ts";
import { Environment, mkEnvironment } from "./environment.ts";
import { Executable, mkExecutable } from "./executable.ts";
import { filterNullValues } from "./internal/utils.ts";
import {
  getPathOrError,
  NixExpression,
  nixFlakeDep,
  nixRaw,
  nixStrLit,
} from "./nix.ts";
import { mkPackage, Package } from "./package.ts";

/**
 * Options for importing flake files from GitHub/GitLab/SourceHut.
 */
export type HostedGitServiceOptions = {
  /**
   * The path to a subdirectory in the repository containing the flake file to
   * import.
   */
  dir?: string;

  /**
   * The name of a branch/tag, or a commit hash to import.
   */
  revOrRef?: string;

  /**
   * A host to fetch from other than the default for that hosted service
   * (useful if pulling from a self-hosted GitHub/GitLab/Sourcehut instance).
   */
  host?: string;
};

/**
 * Methods for obtaining apps, checks, dev-shells, and packages from an
 * imported flakefile to include in your garn project.
 */
export type ImportedFlake = {
  /**
   * A check that composes all checks found in the imported flake file.
   */
  allChecks: Check;

  /**
   * A package that composes all packages found in the imported flake file.
   *
   * When building this package the result will be a directory of symlinks to
   * all packages in the flake file.
   */
  allPackages: Package;

  /**
   * Obtain a specific `Executable` from the `apps` section of the imported
   * flake file.
   *
   * Throws an error if the specified `appName` does not exist.
   */
  getApp: (appName: string) => Executable;

  /**
   * Obtain a specific `Check` from the `checks` section of the imported flake
   * file.
   *
   * Throws an error if the specified `checkName` does not exist.
   */
  getCheck: (checkName: string) => Check;

  /**
   * Obtain a specific `Environment` from the `devShells` section of the
   * imported flake file.
   *
   * Throws an error if the specified `devShellName` does not exist.
   */
  getDevShell: (devShellName: string) => Environment;

  /**
   * Obtain a specific `Package` from the `packages` section of the imported
   * flake file.
   *
   * Throws an error if the specified `packageName` does not exist.
   */
  getPackage: (packageName: string) => Package;
};

/**
 * Imports a flake file from a GitHub repository.
 *
 * See `importFlake` for details.
 */
export function importFromGithub(
  repo: string,
  options: HostedGitServiceOptions = {},
): ImportedFlake {
  return importFlakeFromHostedGitService("github", repo, options);
}

/**
 * Imports a flake file from a GitLab repository.
 *
 * See `importFlake` for details.
 */
export function importFromGitlab(
  repo: string,
  options: HostedGitServiceOptions = {},
): ImportedFlake {
  return importFlakeFromHostedGitService("gitlab", repo, options);
}

/**
 * Imports a flake file from a SourceHut repository.
 *
 * See `importFlake` for details.
 */
export function importFromSourcehut(
  repo: string,
  options: HostedGitServiceOptions = {},
): ImportedFlake {
  return importFlakeFromHostedGitService("sourcehut", repo, options);
}

/**
 * Gets apps, checks, dev-shells, and packages from an existing flake file by
 * adding it as a flake input to your project.
 *
 * This is most useful for bringing in flake files from the internet. The hash
 * of the flake file and its referenced sources will be included in your
 * project's `flake.lock`. If you want to import a flake file within your
 * current project, consider using `callFlake` instead which does not update
 * the hash in your flake.lock.
 *
 * See https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
 * for information on how to format `flakeUrl`. You may also want to use
 * `importFromGithub`, `importFromGitlab` or `importFromSourcehut` helpers
 * instead which provide a more ergonomic API when importing from these
 * services.
 *
 * Example:
 * ```typescript
 * const myFlake = importFlake("git+https://example.com/some-repo.git?ref=main");
 *
 * const myProject = getGarnProjectSomehow()
 *   .add(() => ({
 *     format: myFlake.getApp("format"),
 *     env: myFlake.getDevShell("default"),
 *     bundle: myFlake.getPackage("bundle"),
 *     allChecks: myFlake.allChecks,
 *   }));
 * ```
 */
export function importFlake(flakeUrl: string): ImportedFlake {
  return processFlake(
    nixFlakeDep("importedFlake-" + flakeUrl.replaceAll(/[^a-zA-Z0-9]/g, "_"), {
      url: flakeUrl,
    }),
    flakeUrl,
  );
}

/**
 * Gets apps, checks, dev-shells, and packages from an existing flake file
 * within your current project.
 *
 * @param flakeDir The relative path to the directory containing a `flake.nix`
 *
 * Absolute `flakeDir`s and paths outside of the garn project are not
 * supported. Consider using `importFlake` instead if you want to import an
 * absolute flake path or one from a URL.
 *
 * This can be used to migrate an existing flake project to use garn.
 *
 * Example:
 * ```typescript
 * const myFlake = callFlake("./path/to/dir");
 *
 * const myProject = getGarnProjectSomehow()
 *   .add(() => ({
 *     format: myFlake.getApp("format"),
 *     env: myFlake.getDevShell("default"),
 *     bundle: myFlake.getPackage("bundle"),
 *     allChecks: myFlake.allChecks,
 *   }));
 * ```
 */
export function callFlake(flakeDir: string): ImportedFlake {
  const callFlake = nixFlakeDep("call-flake", {
    url: "github:divnix/call-flake",
  });
  return processFlake(nixRaw`(${callFlake} ${nixRaw(flakeDir)})`, flakeDir);
}

function importFlakeFromHostedGitService(
  serviceName: string,
  repo: string,
  opts: HostedGitServiceOptions,
): ImportedFlake {
  if (!repo.match(/^[^/]+\/[^/]+$/)) {
    throw new Error(
      "The `repo` of a hosted git service should match <owner>/<repo>",
    );
  }
  const query = new URLSearchParams(
    filterNullValues({
      dir: opts.dir,
      host: opts.host,
    }),
  ).toString();
  return importFlake(
    serviceName +
      ":" +
      repo +
      (opts.revOrRef ? `/${opts.revOrRef}` : "") +
      (query ? `?${query}` : ""),
  );
}

function processFlake(flake: NixExpression, location: string): ImportedFlake {
  return {
    allChecks: {
      tag: "check",
      nixExpression: nixRaw`pkgs.linkFarm "all-checks" ${flake}.checks.\${system}`,
    },

    allPackages: mkPackage(
      nixRaw`pkgs.linkFarm "all-packages" ${flake}.packages.\${system}`,
      `All packages from flake.nix in ${flake}`,
    ),

    getApp: (appName) =>
      mkExecutable(
        getPathOrError(
          flake,
          ["apps", nixRaw`\${system}`, appName, "program"],
          nixStrLit`The app "${appName}" was not found in ${location}`,
        ),
        `Execute ${appName} from flake.nix in ${flake}`,
      ),

    getCheck: (checkName) => ({
      tag: "check",
      nixExpression: getPathOrError(
        flake,
        ["checks", nixRaw`\${system}`, checkName],
        nixStrLit`The check "${checkName}" was not found in ${location}`,
      ),
    }),

    getDevShell: (devShellName) =>
      mkEnvironment({
        nixExpression: getPathOrError(
          flake,
          ["devShells", nixRaw`\${system}`, devShellName],
          nixStrLit`The devShell "${devShellName}" was not found in ${location}`,
        ),
      }),

    getPackage: (packageName) =>
      mkPackage(
        getPathOrError(
          flake,
          ["packages", nixRaw`\${system}`, packageName],
          nixStrLit`The package "${packageName}" was not found in ${location}`,
        ),
        `${packageName} from flake.nix in ${flake}`,
      ),
  };
}
