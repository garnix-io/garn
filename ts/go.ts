import { DevServer, Formattable, ProdServer, Project } from "./base.ts";

export const mkGoProject = (_args: {
  description: string;
  src: string;
}): Project & DevServer & ProdServer & Formattable =>
  (() => {
    throw new Error(`bottom`);
  })();
