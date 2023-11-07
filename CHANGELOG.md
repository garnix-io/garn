# Changelog

## v0.0.16 (unreleased)

- Allow to build packages that are nested within projects with `garn build projectName.packageName`.
- Allow to build top-level packages with `garn build packageName.
- Allow adding packages to projects with `.addPackage("packageName", "{script to output to $out}")`.

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
