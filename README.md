<a href="https://garn.io">
  <img src="./garn.png">
</a>

[![built with garnix](https://img.shields.io/endpoint.svg?url=https%3A%2F%2Fgarnix.io%2Fapi%2Fbadges%2Fgarnix-io%2Fgarn)](https://garnix.io)
[![](https://dcbadge.vercel.app/api/server/q2Fptb7My4?style=flat)](https://discord.gg/q2Fptb7My4)
[![](https://img.shields.io/badge/library-v0.0.18-black)](https://doc.deno.land/https://garn.io/ts/v0.0.18/mod.ts)
[![License](https://img.shields.io/github/license/garnix-io/garn)](LICENSE)


garn is a build tool and development environment manager. You configure your
project with a `garn.ts` file that declaratively describes your project and its
dependencies.

For example, with this `garn.ts` file:

```typescript
import * as garn from "https://garn.io/ts/v0.0.18/mod.ts";

export const frontend = garn.javascript.mkNpmProject({
  description: "My project frontend",
  src: "frontend",
  nodeVersion: "18",
})
  .addExecutable("run", "cd frontend && npm install && npm start");

export const backend = garn.go.mkGoProject({
  description: "My project backend",
  src: "backend",
  goVersion: "1.21",
})
  .addExecutable("run", "cd backend && go run ./main.go");

export const startAll = garn.processCompose({
  frontend: frontend.run,
  backend: backend.run,
});
```

Anyone can run your frontend with `garn run frontend`, backend with `garn run
backend`, or both with `garn run startAll`. All without needing to worry about
having the correct version of `go`, `nodejs`, or anything else installed.

**garn** is powered by [Nix](https://nixos.org/), so you get portable and
reproducible builds and development environments.

## Getting started

### Install garn

```bash
sh <(curl --proto '=https' --tlsv1.2 -sSf https://garn.io/install.sh)
```

`garn` needs [`nix`](https://nixos.org/) to be installed, so -- if you don't
have nix already -- the above installer will install nix first, after asking
for confirmation.

### Create your first `garn.ts` file

You can use `garn init` to template out an initial configuration.

Check out the [getting started guide](https://garn.io/docs/getting_started) for
more information on how to write and modify your `garn.ts` file.

## Commands

### `garn init`

Running `garn init` in a directory without a `garn.ts` file will try to
automatically detect what kind of project you have and generate a `garn.ts`
file for you.

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
checks run in a sandbox. The downside of sandboxing is that these checks won't
have access to the internet. The upside is that they'll be (almost) perfectly
reproducible.

### `garn edit`

`garn edit` will start (and, if necessary, download) VSCodium, with Deno LSP configured for you. It will open `garn.ts` in the current directory. This won't clobber any of your existing VSCode/VSCodium configuration and data.

## Languages and Stacks

We currently have support for the Npm, Go, and Haskell projects.

### Npm

The

### Go

### Haskell

## Core concepts

* **Projects** are
* **Environments**
* **Checks**
* **Packages**
* **Executables**

###

## Plugins

Plugins allow mixing in functionality

## Typescript API

You can find documentation for the **garn** Deno library [here](https://doc.deno.land/https://garn.io/ts/v0.0.18/mod.ts).

## Similar tools

There

## How it works
