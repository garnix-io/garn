import { GarnConfig, toGarnConfig } from "./runner.ts";

type InternalLibrary = {
  toGarnConfig: (
    nixpkgsInput: string,
    garnExports: Record<string, unknown>
  ) => GarnConfig;
};

const garnGetInternalLib = (): InternalLibrary => ({
  toGarnConfig,
});

// deno-lint-ignore no-explicit-any
if ((window as any).__garnGetInternalLib != null) {
  throw new Error(
    "Registering __garnGetInternalLib twice, using two different garn library versions is not supported."
  );
}
// deno-lint-ignore no-explicit-any
(window as any).__garnGetInternalLib = garnGetInternalLib;
