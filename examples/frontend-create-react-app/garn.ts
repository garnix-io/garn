import * as garn from "http://localhost:8777/mod.ts";

export const main = garn.javascript.mkNpmProject({
  description: "frontend test app created by create-react-app",
  src: ".",
  nodeVersion: "18",
});

export const start = main.shell`npm install && npm start`;

export const bundle: garn.Project = garn.mkProject(
  {
    description: "website bundle",
  },
  {
    package: main.build`
      npm run build
      cp -rv build/* $out
    `,
  }
);
