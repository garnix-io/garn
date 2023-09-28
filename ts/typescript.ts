import {
  DevServer,
  Formattable,
  ProdServer,
  Project,
  mkCheck,
  mkProject,
} from "./base.ts";

export const mkYarnFrontend = (_args: {
  description: string;
  src: string;
}): Project & DevServer & ProdServer & Formattable => ({
  ...mkProject(),
  checks: { tests: mkCheck(), typecheck: mkCheck() },
});
