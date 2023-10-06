# `garner` use cases

This is an aspirational document about how `garner` could work in the future. Details are going to change.

## installing `garner`

For now:

Install `nix` and `garner`:

```sh
nix profile install github:garnix-io/garner
```

## getting started from scratch

```sh
garner intro
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
$ garner start # this will spin up the haskell backend and the typescript frontend
$ garner test # run all tests
  $ garner test backend # run all backend tests
  $ garner test frontend # run all frontend tests
$ garner help # show the garner help
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
$ garner start
backend running on $BACKEND_URL
frontend running on $FRONTEND_URL
$ curl $BACKEND_URL
$ curl $FRONTEND_URL
```

```sh
$ garner edit # opens an editor where the deno extension is set up
$ garner init-vscode # makes deno work well for an existing vscode installation
```

## adding garner to an existing project

```sh
$ garner init
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
$ garner help
garner -- a tool for project configurations

subcommands:
  garner help -- show this help
  garner test -- run all tests
    garner test backend -- run all backend tests
    garner test frontend -- run all backend tests
  garner start -- starts the haskell backend and the typescript frontend
```
