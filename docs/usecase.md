# `garn` use cases

This is an aspirational document about how `garn` could work in the future. Details are going to change.

## installing `garn`

For now:

Install `nix` and `garn`:

```sh
nix profile install github:garnix-io/garn
```

## getting started from scratch

```sh
garn intro
```

It'll allow you to multi-select which language presets to use:

```
[ ] go backend
[X] haskell backend
[ ] idris backend
[ ] java backend
[ ] rust backend
[ ] typescript backend
[X] typescript frontend
name for your haskell backend: backend
name for your typescript frontend: frontend

You can now try:
$ garn start # this will spin up the haskell backend and the typescript frontend
$ garn test # run all tests
  $ garn test backend # run all backend tests
  $ garn test frontend # run all frontend tests
$ garn help # show the garn help
```

That creates `garn.ts`:

```ts
import {
  haskell,
  typescriptFrontend,
  nixpkgs,
} from "https://garn.dev/lib@0.0.1/main.ts";

export const backend = haskell.mkProject({
  src: "./backend",
});

export const frontend = typescriptFrontend.mkProject({
  src: "./frontend",
});
```

Also, it creates

- `backend/backend.cabal`
- `backend/package.yaml`
- `backend/src/Main.hs`
- `backend/test/Spec.hs`
- `backend/test/MainSpec.hs`
- `frontend/package.json`
- `frontend/src/index.tsx`
- `frontend/src/index.test.tsx`
- `frontend/yarn.lock`

```sh
$ garn start
backend running on $BACKEND_URL
frontend running on $FRONTEND_URL
$ curl $BACKEND_URL
$ curl $FRONTEND_URL
```

```sh
$ garn edit # opens an editor where the deno extension is set up
$ garn init-vscode # makes deno work well for an existing vscode installation
```

## adding garn to an existing project

```sh
$ garn init
Detected a haskell project in ./backend
Detected a typescript frontend project in ./frontend
```

This would autodetect a starting point for a `garn.ts`:

```ts
import {
  haskell,
  typescriptFrontend,
  nixpkgs,
} from "https://garn.dev/lib@0.0.1/main.ts";

export const backend = haskell.mkProject({
  src: "./backend",
});

export const frontend = typescriptFrontend.mkProject({
  src: "./frontend",
});
```

It seems really difficult to reliably detect project configurations for anything but the simplest projects.
It seems much more important to have a good, documented and very accessible API to support non-standard project setups.
For example we could _try_ to autodetect

```ts
export const frontend = typescriptFrontend.mkProject({
  src: "./frontend",
  testCommand: "npm run vitest run",
});
```

But it seems _much_ more important that `testCommand` is as accessible as possible.

## checkout out a repo with an existing `garn.ts` config

```
$ git clone $WHATEVER
$ cd whatever
$ garn help
garn -- a tool for project configurations

subcommands:
  garn help -- show this help
  garn test -- run all tests
    garn test backend -- run all backend tests
    garn test frontend -- run all backend tests
  garn start -- starts the haskell backend and the typescript frontend
```
