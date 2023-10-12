# garn

garn is a build tool and environment manager. You configure your project with a
`garn.ts` file that declaratively describes your project and its dependencies.

For example, with this `garn.ts` file:

```typescript
import * as garn from "https://garn.io/ts/v0.0.5/mod.ts";

export const frontend = garn.mkNpmFrontend({
  description: "My project frontend",
  src: "frontend",
  nodeVersion: "18",
});

export const backend = garn.mkGoProject({
  description: "My project backend",
  src: "backend",
  goVersion: "1.20",
});

export const compose = garn.processCompose([frontend, backend]);
```

Anyone can run your frontend with `garn run frontend`, backend with `garn run
backend`, or both with `garn run compose`. All without needing to worry about
having the correct version of go, node, or anything else installed.

garn is powered by [Nix](https://nixos.org/), so you get portability without
the performance penalties and other pain points from doing your local
development in docker.

## Getting started

### Install garn

TODO

### `garn init`

Running `garn init` in a directory without a `garn.ts` file will try to
automatically detect what kind of project you have and generate a `garn.ts`
file for you.

## Commands

### `garn enter`

`garn enter [project]` will put you in a development shell with all
needed dependencies available in your `$PATH`.

### `garn build`

`garn build [project]` will build the specified project and create a symlink
named `result` which links to the resulting build artifacts.

### `garn run`

`garn run [project|executable]` will run the specified executable or default
executable for a project. You can create custom executables within the scope of
a project using `shell`. For example:

```typescript
export const migrate = backend.shell`go run migrate.go`;
```

will run your migrations when running `garn run migrate`.

### `garn check`

`garn run [project|check]` will run the specified check, or all checks for the
specified project. These checks run in a sandbox. You can create custom checks
within the scope of a project using `check`. For example:

```typescript
export const featureTests = frontend.check`npm run cypress`;
```

will run your feature tests when running `garn check featureTests`.
