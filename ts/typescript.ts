import { DevServer, Formattable, ProdServer, Project } from "./base.ts";

export const mkYarnFrontend = (_args: {
  description: string;
  src: string;
}): Project & DevServer & ProdServer & Formattable =>
  (() => {
    throw new Error(`bottom`);
  })();
