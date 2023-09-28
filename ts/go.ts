import {
  DevServer,
  Formattable,
  ProdServer,
  Project,
  mkCheck,
  mkProject,
} from "./base.ts";

export const mkGoProject = (_args: {
  description: string;
  src: string;
  projectConfigFiles?: Array<string>;
}): Project & DevServer & ProdServer & Formattable => ({
  ...mkProject(),
  checks: { "go-tests": mkCheck() },
});
