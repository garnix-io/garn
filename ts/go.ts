import {
  DevServer,
  Formattable,
  ProdServer,
  Project,
  mkProject,
} from "./base.ts";

export const mkGoProject = (_args: {
  description: string;
  src: string;
}): Project & DevServer & ProdServer & Formattable => mkProject();
