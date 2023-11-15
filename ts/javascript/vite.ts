import { Executable } from "../executable.ts";
import { Package } from "../package.ts";
import { Plugin } from "../project.ts";

/**
 * `Plugin` for [vite](https://vitejs.dev/) projects. This will add to your
 * `Project`:
 *
 * - a `Package` called `build` that will bundle your vite project,
 * - an `Executable` called `dev` that will run your dev server,
 * - an `Executable` called `preview` that will run a server to preview your
 *   production build.
 *
 * This plugin requires a field `node_modules` of type `Package` to work.
 * Usually that is created with `garn.javascript.mkNpmProject`.
 *
 * Here's an example:
 *
 * ```typescript
 * import * as garn from "https://garn.io/ts/v0.0.15/mod.ts";
 *
 * export const frontend = garn.javascript
 *   .mkNpmProject({
 *     description: "An NPM project",
 *     src: ".",
 *     nodeVersion: "18",
 *   })
 *   .add(garn.javascript.vite);
 * ```
 *
 * After running `garn run frontend.build` you can then access your built
 * frontend bundle in `./result`.
 */
export const plugin: Plugin<
  { build: Package; dev: Executable; preview: Executable },
  { node_modules: Package }
> = (base) => {
  if (base.defaultEnvironment == null) {
    throw new Error(
      `The 'garn.javascript.vite' plugin can only be added to projects with a default environment.`,
    );
  }
  return {
    build: base.defaultEnvironment.build`
      export PATH=${base.node_modules}/bin:$PATH
      vite build --outDir $out
    `,
    dev: base.defaultEnvironment.shell`
      export PATH=${base.node_modules}/bin:$PATH
      vite`,
    preview: base.defaultEnvironment.shell`
      export PATH=${base.node_modules}/bin:$PATH
      vite preview`,
  };
};
