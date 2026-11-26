# Restructuring

Splitting a large module into several, and moving a module to another
directory, as `Commands/`, `Cli/Parser/`, `Cli/PrettyPrinter/` and `Keys/`
were made.

## Splitting a module

- An import cycle between the parts: before writing a file, list for each
  top-level name the parts that use it, by a word match on the code without
  its comments and strings. A constructor counts for its type. A field too,
  unless types share field names: it then links the wrong type, so leave the
  fields out, as for the parts of `Cli/Parser.hs`.
- The imports of each part: give it the whole import block of the old module,
  build, and remove what `-Wunused-imports` names. One build after that is
  clean. In a warning, `T(x)` is the item `x` of the list and not `T (..)`,
  and `-j` puts a "Compiling" line between its first line and the rest.
- The pragmas of a part: those whose syntax it uses, and `DuplicateRecordFields`
  where it builds a record with a field name that two types in scope share
  (`name` of `CommandSpec` and of `Program`), else "Ambiguous occurrence".
  `NoFieldSelectors` goes where a record is defined.
- "This binding for ‘status’ shadows" in a part alone: `RecordWildCards` of
  the old module kept the warning silent for a field name. Import by a list
  the module that brings the field (`Sync` in `Cli/PrettyPrinter/NextSteps.hs`).
- A comment block at column 0 with a blank line after it introduces a section
  and belongs to no declaration: put it under the marker of its section, or it
  follows the declaration before it to another part.
- Parts with links in both directions: the shared helpers go to modules
  below the parts (`Base`, `Place`, `Table` of `Cli/PrettyPrinter/`), by
  declaration and not by section. Order the parts, and list each use of a
  later part: one function to move breaks each cycle.
- The export list of a part: the names another part imports, and those the old
  module exported, a type with `(..)`. Check at the end that each export is
  imported somewhere. A type in the signature of an export is exported too,
  and a record whose fields nothing reads by name, else `-Wunused-top-binds`.
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

- A build with no warning proves nothing of a file it did not compile again:
  a part written twice with the same text is not. `haskell-dev docs` compiles
  in its own directory: `grep "warning: \[\|is out of scope"` its log, which
  showed 21 unused imports of `Cli/Command.hs` after the fact.
- The links of the comments: `haskell-dev docs` names each `'name'` that is
  now out of scope, and each one in the comment of a function that is now
  exported, when the name it links is not.
- The tests import `Haskoin.Wallet.Commands`, which exports the commands of
  `Commands/` again: a move inside `Commands/` changes no test. The same holds
  for `Cli/Parser.hs`, which exports `Cli/Command.hs` again, and for
  `Cli/PrettyPrinter.hs`, which exports every module of `Cli/PrettyPrinter/`.
- The notes that name a file: `grep -rn "Old.hs" CLAUDE.md claude-notes`.
