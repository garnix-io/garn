# garn

garn is a build tool and environment manager. You configure your project with a
`garn.ts` file that declaratively describes your project and its dependencies.

For example, with this `garn.ts` file:

```typescript
import * as garn from "https://garn.io/ts/v0.0.7/mod.ts";

export const frontend = garn.javascript.mkNpmProject({
  description: "My project frontend",
  src: "frontend",
  nodeVersion: "18",
});

export const backend = garn.go.mkGoProject({
  description: "My project backend",
  src: "backend",
  moduleName: "backend",
  goVersion: "1.20",
});

export const startAll = garn.processCompose({
  frontend: frontend.defaultExecutable!,
  backend: backend.defaultExecutable!,
});
```

Anyone can run your frontend with `garn run frontend`, backend with `garn run
backend`, or both with `garn run startAll`. All without needing to worry about
having the correct version of go, node, or anything else installed.

garn is powered by [Nix](https://nixos.org/), so you get portability without
the performance penalties and other pain points from doing your local
development in docker.

## Getting started

### Install garn

```bash
curl --proto '=https' --tlsv1.2 -sSf https://garn.io/install.sh | sh
```

`garn` needs [`nix`](https://nixos.org/) to be installed, so -- if you don't
have nix already -- the above installer will install nix first, after asking
for confirmation.

### `garn init`

Running `garn init` in a directory without a `garn.ts` file will try to
automatically detect what kind of project you have and generate a `garn.ts`
file for you.

## Commands

### `garn enter`

`garn enter [project]` will put you in a development shell with all needed
dependencies available in your `$PATH`.

### `garn build`

`garn build [project]` will build the specified project and create a symlink
named `result` which links to the resulting build artifacts.

### `garn run`

`garn run [project]` will run the default executable for the specified project.

### `garn check`

`garn check [check]` will run all checks for the specified project. These
checks run in a sandbox.
