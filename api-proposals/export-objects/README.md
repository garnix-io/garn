# Export Objects

The gist of this design is to give users flexibility to define their own
taxonomy for `Project`, `Runnable` `Check`, and `NixPackage`.

We do this by allowing users to export plain javascript objects in any
configuration they feel like, and expose the ability to build, check, run and
enter by specifying javascript-style paths to the objects to act on.

## Types

### `Project`

A project is a wrapper around an environment `NixPackage` which can be built or
provide a devshell for. This wrapper has helper methods to create `Runnable`s,
`Check`s that can run within the scope of the environment.

### `Runnable`

A Runnable is a plain object with a single `runnableTag` symbol property which
contains the base nix package and program to execute

These will either compile into nix apps in the `flake.nix` file or just run on
demand with `nix develop --command` (TODO: decide this)

### `Check`

A check will compile into nix checks to be run locally or on CI

## Commands

### `garner run`

Will traverse the passed path and either run the specified `Runnable` if there is
only one, or give you a menu to select from if there are multiple `Runnable`s.

```typescript
export const foo = {
  bar: {
    fizz: garner.run`echo 1`,
    buzz: garner.run`echo 2`,
  },
  baz: {
    quix: garner.run`echo 3`,
  },
}
```

```shell
$ garner run
    What do you want to run?
      1. foo.bar.fizz
      2. foo.bar.buzz
      3. foo.baz
```

```shell
$ garner run foo
    What do you want to run?
      1. foo.bar.fizz
      2. foo.bar.buzz
```

```shell
$ garner run foo.baz
3
```

### `garner check`

Works similarly to `garner run` but for exported `Check`s instead of
`Runnables`. The major difference is it will run all matching checks instead of
presenting a menu

```typescript
const backend = garner.mkGoBackend();

export const foo = {
    myChecks: [
        backend.check`echo 1`,
        backend.check`echo 2`,
    ],

    otherCheck: backend.check`echo 3`,
}
```

```shell
$ garner check
1
2
3
```

```shell
$ garner check foo.myChecks
1
2
```

### `garner enter`

Works similarly to `garner run` and `garner check` but puts you in a devshell
with access to all packages specified by all found `NixPackage`s


## Final thoughts

All of the above examples treat the default export the same as the root (not
sure what the precedence should be, but also could error if there are
conflicts).

```typescript
export const goBackend = garner.mkGoProject()
export const start = goBackend.run`go run main.go`;
```

```typescript
// also works
const goBackend = garner.mkGoProject()

export default {
    backend: {
        goBackend,
        run: goBackend.run`go run main.go`,
    }
}
```

```typescript
// also works
export * as backend from './my-backend-garner-config.ts'
```

The largest advantage to this is it pushes a lot of the questions we have on
API design onto the typescript library which allows for non-breaking changes in
the future in the form of new typescript helper functions. It also means we can
create very ergonomic project factory functions that provide a lot of stuff out
of the box. The users have full control to use the defaults provided by these
functions, or pick and choose the wanted functionality

```typescript
function mkGoBackend(): Project & {
  startDev: Runnable;
  startProd: Runnable;
  format: Runnable & Check;
} {
  ...
}

function mkBashProject(): Project & Runnable & { shellCheck: Check } {
  ...
}
```
