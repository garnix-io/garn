import { Executable } from "../executable.ts";
import { Package } from "../package.ts";
import { Plugin } from "../project.ts";

export const plugin: Plugin<
  { build: Package; dev: Executable; preview: Executable },
  { node_modules: Package }
> = (base) => {
  if (base.defaultEnvironment == null) {
    throw new Error(
      `'vite.addBuilder' can only be added to projects with a default environment`,
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
