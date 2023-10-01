# Runtime TS

This idea is just related to `garner run` and is compatible with the other two
proposals.

It attempts to solve the problem of users potentially wanting to specify CLI
arguments, and also that we may not want to have every runnable end up in the
resulting `flake.nix`.

Originally we had discussed about handling CLI arguments as an exported
function (either with schema specified with something like Zod):

```typescript
export const runTests = runnable({ browser: z.boolean(), filter: z.string() }, (argv) => {
  if (argv.browser) return garnix.bash`cypress`;
  return garnix.bash`jest ${argv.filter}`;
});
```

The problem with this is if garner.ts only exists to template out a `flake.nix`
file the above example is not possible to implement. Instead this proposal
suggests using `garner.ts` to not only generate the flake file, but also at
runtime to execute runnables as pure javascript functions. Then helpers like
`garnix.bash` actually just shell out to `nix develop --command ...` rather
than generating nix packages.
