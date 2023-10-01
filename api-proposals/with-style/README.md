# with-style

This proposal was one of our initial spikes. The idea is we have a concept of a
`Project` which encapsulates all configuration for a given project in a
codebase. By chaining `.with*` helper methods on the `Project` type a user can
describe all related tasks, checks, and build targets related to this project.

The typescript type of the Project is aware of what tasks exist at compile time
which allows LSP completion and type checking for tasks.
