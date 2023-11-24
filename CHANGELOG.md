# Changelog

## v0.0.17 (unreleased)

- Fix locale warnings on MacOS

- For git repositories, add untracked files to build sandboxes for `Check`s and
  `Package`s.

- Tweak the API for creating `Environment`s, making it easier to create
  `Environment`s that have a source directory

- Fix needing to use `--` twice in `garn run` in order to pass arguments to
  executables.

- Require explicit passing of arguments via `"$@"` in `shell`. This allows users
  to have much more control over the behavior of their shell commands, and
  in particular allows multiline scripts to handle arguments (in exactly the
  same way as normal bash scripts).

## v0.0.16

- Allow to build packages that are nested within projects with `garn build projectName.packageName`.
- Allow to build top-level packages with `garn build packageName`.
- Allow adding packages to projects with `.addPackage("packageName", "{build script writing to $out}")`.
- Add `Project.add`, a function to apply so-called `Plugin`s to a project. This
  provides a nice way to bundle up more complex project modifications into a
  single declaration. It also allows to use `Plugin`s from other sources,
  including third-party libraries.
- Expose some useful nested nix packages in the garn `nixpkgs.ts` package.
- Add `garn.javascript.vite`, a `Plugin` that adds fields for bundling vite projects into a `Package`.
- Add `garn.deployToGhPages` plugin to help publish `Packages` to github pages.

## v0.0.15

- Added a `--version` flag
- Added simpler overloads for `Project.addCheck`, `Project.addExecutable`,
  `Project.check`, `Project.shell`, etc.. So you don't have to use the unusual
  backtick syntax.
- Added `golang` version `1.21`.
- `garn run` handles non-zero exitcodes better:
  - Exit codes of child-processes are forwarded by `garn`.
  - `garn` doesn't output a confusing error message about a failed child-process.
- Don't include unused flake inputs in the generated flake files.
