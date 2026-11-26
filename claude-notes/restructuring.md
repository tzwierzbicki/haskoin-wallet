# Restructuring

Splitting a large module into several, and moving a module to another
directory, as `Commands/` and `Keys/` were made.

## Splitting a module

- An import cycle between the parts: before writing a file, list for each
  top-level name the parts that use it, by a word match on the code without
  its comments and strings. A constructor or a field counts for its type.
- The imports of each part: give it the whole import block of the old module,
  build, and remove what `-Wunused-imports` names. One build after that is
  clean. In a warning, `T(x)` is the item `x` of the list and not `T (..)`,
  and `-j` puts a "Compiling" line between its first line and the rest.
- The export list of a part: the names another part imports, and those the old
  module exported, a type with `(..)`. Check at the end that each export is
  imported somewhere.
- Proof that only code moved: the lines of code of the old file (`git show
  HEAD:`), without comments, imports and blank lines, are the lines of the new
  files, counted as a multiset.

## Moving a module

- One regular expression renames the module and its spec everywhere:
  `\bHaskoin\.Wallet\.(Old|…)(Spec)?\b`, over `src`, `test`, `app`,
  `package.yaml` and `haskell-dev.toml` (its `generated-modules` names the
  schema modules). A name that is a prefix takes its directory with it.
- The spec moves with its module, to keep `test/` as `src/` is: a plain `mv`,
  which leaves the index alone. `git diff` then shows a moved file as deleted.
  Compare it with `git show HEAD:old/path` to prove that only its `module`
  line, its imports and the comments that name a module changed.
- The READMEs of `test/data` name spec files by their path.

## Moving a helper to a lower module

- A module with no export list (`Database/TxInfo.hs`, `Util.hs`) gives the
  name to every module that imports it whole. Search the name with `grep -rw`
  first: `contactAt` is also a query of `Database/Contact.hs`. A new module
  with an export list keeps such a name to itself, as `Tx/Addresses.hs` does.
- The layers to keep: no module of `Report/` reaches a module of `Cli/`, and
  `Export/` reaches `Cli/Response.hs` alone. Follow the imports to check it.

## After the move

- The links of the comments: `haskell-dev docs` names each `'name'` that is
  now out of scope, and each one in the comment of a function that is now
  exported, when the name it links is not.
- The tests import `Haskoin.Wallet.Commands`, which exports the commands of
  `Commands/` again: a move inside `Commands/` changes no test.
- The notes that name a file: `grep -rn "Old.hs" CLAUDE.md claude-notes`.
