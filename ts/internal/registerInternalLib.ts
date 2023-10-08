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
(window as any).__garnGetInternalLib = garnGetInternalLib;
