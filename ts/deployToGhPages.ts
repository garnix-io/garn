import { Executable } from "./executable.ts";
import { Package } from "./package.ts";
import { Plugin } from "./project.ts";

const ansiBold = "\\e[0;1m";
const ansiReset = "\\e[0m";
const ansiRedBold = "\\e[31;1m";

/**
 * A garn plugin that allows easy deployment of a package to [GitHub
 * pages](https://pages.github.com/).
 *
 * @param pkg - The `Package` whose artifacts will be committed to the
 * `gh-pages` branch. For convenience, this can also be a function that takes
 * in a reference to the project and returns the `Package`.
 *
 * Example:
 * ```typescript
 * export const myProject = mkNpmProject({ ... })
 *   .addPackage("genStaticSite", "npm run build && mv dist/* $out")
 *   .add(garn.deployToGhPages(project => project.genStaticSite));
 * ```
 *
 * Then running `garn run myProject.deployToGhPages` will create the `gh-pages`
 * branch for you if it doesn't already exist, and add a commit containing the
 * artifacts from the specified project (`genStaticSite` in the example above)
 * to the branch.
 *
 * At this point all you need to do to deploy is push the branch to GitHub with
 * `git push <remote> gh-pages:gh-pages`
 */
export function deployToGhPages<T>(
  pkg: Package | ((t: T) => Package),
): Plugin<{ deployToGhPages: Executable }, T> {
  return (p) => {
    if (p.defaultEnvironment == null) {
      throw new Error(
        `'deployToGhPages' can only be added to projects with a default environment`,
      );
    }
    return {
      deployToGhPages: p.defaultEnvironment!.shell`
        set -eu

        REPO_DIR=$(git rev-parse --show-toplevel)
        TMP_DIR=$(mktemp -d)
        TMP_SRC="$TMP_DIR/src"
        TMP_DST="$TMP_DIR/dst"
        VERSION_NAME=$(git describe --tags --dirty --always)

        function cleanup() {
          rm -rf "$TMP_DIR"
        }
        trap cleanup EXIT

        if [ "$(git rev-parse --abbrev-ref HEAD)" = gh-pages ]; then
          >&2 echo -e '${ansiRedBold}error:${ansiBold} deployToGhPages cannot run if gh-pages is currently checked out. Please change branches first.${ansiReset}'
          exit 1
        fi

        git clone --quiet "$REPO_DIR" "$TMP_SRC"
        git -C "$TMP_SRC" checkout gh-pages 2>/dev/null || git -C "$TMP_SRC" checkout --quiet --orphan gh-pages
        cp -rv ${typeof pkg === "function" ? pkg(p) : pkg} "$TMP_DST"
        chmod -R +w "$TMP_DST"
        mv "$TMP_SRC/.git" "$TMP_DST"
        git -C "$TMP_DST" add .
        git -C "$TMP_DST" commit -m "Deploy $VERSION_NAME to gh-pages"
        git fetch --quiet "$TMP_DST" gh-pages:gh-pages
        >&2 echo -e 'Created commit to "gh-pages" branch, but it has not been pushed yet'
        >&2 echo -e 'Run ${ansiBold}git push <remote> gh-pages:gh-pages${ansiReset} to deploy'
      `.setDescription(
        "Builds this project and commits the output to your local `gh-pages` branch",
      ),
    };
  };
}
