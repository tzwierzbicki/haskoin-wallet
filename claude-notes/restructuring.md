# Restructuring

Splitting a large module into several, and moving a module to another
directory. `Commands.hs` was split into the modules of `Commands/` this way.

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

## After the move

- The links of the comments: `haskell-dev docs` names each `'name'` that is
  now out of scope, and each one in the comment of a function that is now
  exported, when the name it links is not.
- The tests import `Haskoin.Wallet.Commands`, which exports the commands of
  `Commands/` again: a move inside `Commands/` changes no test.
- The notes that name a file: `grep -rn "Old.hs" CLAUDE.md claude-notes`.
