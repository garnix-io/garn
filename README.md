
<h1 align="center">
<a href="https://garn.io">
  <img src="./garn.png">
</a>
</h1>

<p align="center">
<i align="center">The Builders' Lingua Franca</i>
</p>

<p align="center">

[![built with garnix](https://img.shields.io/endpoint.svg?url=https%3A%2F%2Fgarnix.io%2Fapi%2Fbadges%2Fgarnix-io%2Fgarn)](https://garnix.io)
[![](https://dcbadge.vercel.app/api/server/q2Fptb7My4?style=flat)](https://discord.gg/q2Fptb7My4)
[![](https://img.shields.io/badge/library-v0.0.20-black)](https://doc.deno.land/https://garn.io/ts/v0.0.20/mod.ts)
[![License](https://img.shields.io/github/license/garnix-io/garn)](LICENSE)

</p>


**garn** is a build tool and development environment manager. You configure your
project with a `garn.ts` file that declaratively describes your project and its
dependencies.

You can think of it as coding up your README. Users, collaborators, and
coworkers no longer need to manually follow install steps, or figure out what
commands they can run, but instead can just use **garn** directly. And if it
works for you, it works for everyone.[^1]


For example, with this `garn.ts` file:

```typescript
import * as garn from "https://garn.io/ts/v0.0.20/mod.ts";

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

You may have noticed the use of import URLs in the file above. This is supported
by Deno, which we use under the hood. But you don't need to install that either.
In fact, **garn** might just be one of the last things you have to manually
install.[^2]

**garn** is powered by [Nix](https://nixos.org/), so you get portable and
reproducible builds and development environments.


---

## Table of contents

- [Getting Started](#getting-started)
- [Commands](#commands)
- [Core Concepts](#core-concepts)
- [Languages and stacks](#languages-and-stacks)
- [How it works](#how-it-works)
- [Comparison to other tools](#comparison-to-other-tools)
- [Typescript API](#typescript-api)

## Getting started

1) **Install garn**:

   ```bash
   sh <(curl --proto '=https' --tlsv1.2 -sSf https://garn.io/install.sh)
   ```

   `garn` needs [`nix`](https://nixos.org/) to be installed, so if you don't
   have nix already the above installer will install nix first, after asking
   for confirmation.

   <details>
     <summary>Manual install (Optional)</summary>
     If you prefer to know exactly what <b>garn</b> is doing, or if you have a
     special setup not covered by our installation script, you can see
     <a href="https://garn.io/docs/getting_started">this page</a> for manual installation
     steps.
   </details>

   <details>
     <summary>Installing shell completion (Optional)</summary>
     You can also install shell completion scripts for the most popular shells.
     See <a href="https://garn.io/docs/getting_started">this page</a> for more information.
   </details>

   <details>
     <summary>Deno LSP for garn files (Optional)</summary>
     <b>garn</b> is much nicer with an IDE-like experience. You can use
     <code>garn edit</code> for that or, if you prefer, you can set up your own editor.
     See <a href="https://garn.io/docs/getting_started">this page</a> for more information
   </details>

2) **Create your first `garn.ts` file**:

   You can use `garn init` to template out an initial configuration. You can
   use `garn edit` to start a text editor.

   Check out the [getting started guide](https://garn.io/docs/getting_started) for
   more information on how to write and modify your `garn.ts` file.

3) **Run some things**: Use the **garn** [commands](#commands) to run some
   tests, build packages, or run executables.

## Commands

### `garn init`

Running `garn init` in a directory without a `garn.ts` file will try to
automatically detect what kind of project you have and generate a `garn.ts`
file for you.

### `garn enter`

`garn enter [project or environment]` will put you in a development shell with
all needed dependencies available in your `$PATH`.

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

`garn edit` will start (and, if necessary, download) VSCodium, with Deno LSP
configured for you. It will open `garn.ts` in the current directory. This won't
clobber any of your existing VSCode/VSCodium configuration and data.

## Core concepts

* **Environments** (also known as devshells, devenvs, virtualenvs) are the context in which a command runs. That includes: packages that are installed, shell variables that are set, PATH elements. Used both for a devshell experience (think Python virtualenvs) and for giving context to `Executable`s.
* **Packages** are recipes for building artifacts (such as binaries or JS bundles). These recipes run in an isolated environment. They also refer to the artifacts themselves.
* **Checks** are like `Package`s, but where we don't care about the artifacts as much as the exit code.
* **Executables** are what you already know: bash scripts or binaries that you can run. They have their own `Environment`, so often can rely on dependencies that are not available "outside".
* **Projects** are collections of the above.
* **Plugins** are functions that add functionality to a Project, potentially modifying existing `Checks`, `Packages`, `Executable`s and `Environment`s, or just adding new ones.


## Languages and Stacks

  <img src="./lingua-franca.png">

<br/>
<br/>

We currently have support for the Npm, Yarn, Go, and Haskell projects, with
more languages coming soon. If you really want to see something, make your
voice heard in our issue tracker!

Note also our [examples](https://github.com/garnix-io/garn/tree/main/examples)
directory, which contains tested examples of using **garn** in various ways.

All examples below require the two following imports:

```typescript
import * as garn from "https://garn.io/ts/v0.0.20/mod.ts";
import * as pkgs from "https://garn.io/ts/v0.0.20/nixpkgs.ts";
```

All `garn.ts` files require the first import, and often you also need the
second. Besides that, you rarely need other imports.

### Npm

The basic workhorse of Npm projects is `mkNpmProject`. An example:

```typescript
export const frontend = garn.javascript.mkNpmProject({
  description: "My project frontend",
  src: ".",
  nodeVersion: "18",
})
  .withDevTools([pkgs.jq])
  .addExecutable("run", "npm install && npm start")
  .addCheck("test", "npm install && npm run test");
```

This creates a Project containing:

- A reproducible, CI-ready Check (run it with `garn check frontend.test`)
- A no-installation-needed Executable (run it with `garn check frontend.test`).
- An Environment or devshell you can enter with `garn enter frontend` with all
  dependencies available.

The right version of `node`, `npm`, `jq`, etc. will be used for all these.
You can see a full project [here](https://github.com/garnix-io/garn/tree/main/examples/npm-project).

### Go

Go projects are usually built with `mkGoProject`:

```typescript
export const server = garn.go.mkGoProject({
    description: "example backend server in go",
    src: ".",
    goVersion: "1.21",
  })
  .addExecutable("migrate", "go run ./scripts/migrate.go")
  .addExecutable("dev", "go run ./main.go");
```

### Haskell

For creating Haskell projects, there's `mkHaskellProject`. An example:

```typescript
export const project = garn.haskell.mkHaskellProject({
  description: "My project",
  src: ".",
  executables: ["server", "openapi-generation"]
  ghcVersion: "ghc94"
})
```

Assuming you have a `cabal` file in `.`, this creates a Project containing:

- A reproducible, CI-ready Package (run it with `garn build project`) that
  includes the test-suites of your project.
- Two executables, that run the corresponding executables from the cabal file.
  Run them with `garn run project.server` and `garn run project.openapi-generation`.
- An Environment or devshell you can enter with `garn enter project` with all
  dependencies available.

The right version of `node`, `npm`, `jq`, etc. will be used for all these.
You can see a full project [here](https://github.com/garnix-io/garn/tree/main/examples/npm-project).

## How it works

The essential idea is to generate Nix code corresponding to your projects, and
then run Nix behind the scenes. In [here](https://github.com/garnix-io/garn/blob/main/ts/nix.ts),
you can see the simple but low-level Nix AST we use. Objects such as `Check`s
have a `nixExpression` field that contains their corresponding Nix expression.

**garn** then generates a `flake.nix` that, for every exported variable of the `garn.ts` file,
has a corresponding `package`, `check`, `devshell` or `app`.

This means that **garn** integrates with Nix (and the Nix ecosystem) *both ways*:
you can write pure Nix expressions and embed them in `garn.ts` files, or import Nix projects, but
you can also from Nix call things generated by **garn**.

## Typescript API

You can find documentation for the **garn** Deno library [here](https://doc.deno.land/https://garn.io/ts/v0.0.20/mod.ts).


## Comparison to other tools

- **Nix**: Nix is a brilliant idea with an immense amount of volunteer effort
  behind it, and the giant's shoulders on which **garn** stands. It has a rather
  complex CLI, and the Nix language is a whole new thing to learn. That said
  it is a much more mature project with support for just about every imaginable
  stack. For complex projects, you might be better off picking Nix right now.
  For simpler ones, or ones that include collaborators who don't feel comfortable
  with Nix, **garn** might be a better option.

  But the choice isn't exclusive
  or binary: **garn** both allows including
  Nix expressions or dependencies within it, and generates a `flake.nix` file that
  can be consumed by other Nix expressions, so that you can combine Nix and
  **garn** quite well.
- **just**, **taskfile**, and **make**: These are build tools or command runners
  that help you write and organize the scripts or commands of your project.
  In that regard, they overlap with **garn**.
  They are much easier to install and get started with than **garn**. But they
  don't manage packages and environments at all, so every user still needs to
  figure that out themselves. **garn** also makes it easy to share your "recipes"
  as a library. (make also does incremental builds, which **garn** does not.)
- **virtualenv**, **rvm**, **nvm**, etc.: These are *language-specific* tools;
  very helpful if you need a particular python package or version of python,
  not at all if you need a system package.
- **devbox** and **devenv**: Both of these projects are more mature than **garn**;
  you are likely to find fewer rough corners with them.
  Other than that, **garn** differs from them in three major ways: it supports
  writing your own packages and checks which run in isolated environments (such as CI)
  and can be cached or consumed by downstream users; it allows configuration in
  Typescript rather than YAML and/or Nix; and it allows easily writing and importing
  libraries with reusable functionality.
- **docker**: Docker isolates processes, and is often used to distribute dev
  environments. **garn** overlaps with this feature. Both tools make it relatively certain that if it works once
  it works everywhere. But with Docker that comes at the price of harder
  integration with your development process.

  Besides development environments, Docker is also used for other purposes, such
  as deployment. **garn** does not support any of that (yet).


[^1]: This might not always be true between different architectures and
  platforms (e.g. Linux vs. MacOS).
[^2]: That said, the installation of **garn** itself isn't always smooth on MacOS.
  If you encounter a problem, please [let us know](https://github.com/garnix-io/garn/issues).
