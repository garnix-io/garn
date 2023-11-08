# Changelog

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