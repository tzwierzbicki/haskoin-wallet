# haskoin-wallet

`hw` is a command-line BIP44 and BIP84 wallet for Bitcoin and Bitcoin Cash. It keeps
its accounts in SQLite and asks a haskoin-store server for the chain. Signing
can happen on an offline machine.

## How this file is kept

This file is loaded into every session, so each line costs tokens every time.
It has three parts:

- **The rules**: everything above `## Feature notes`. Only the maintainer
  changes them. If a change makes a rule out of date, say so in your report
  and leave the rule alone.
- **The feature notes**: the index of the files of `claude-notes/`, one file
  for each kind of feature. A file costs tokens only in the sessions that
  work on its feature.
- **The working notes**: the last section. It holds only what applies to
  every session, whatever the feature.

Claude owns the working notes, the files of `claude-notes/` and their index,
and keeps them without asking, by the rules below.

### Rules for the notes

The notes save the next session the research this session paid for.

- **Before the work.** Read each file of `claude-notes/` that the index names
  for the task, before you read the code.
- **When to add.** Something about this repository took real effort to find
  out (several searches, a failed build or test run, a wrong first approach),
  and it would take that effort again. At the end of a task, ask what would
  have saved the most effort, and write that down, or nothing.
- **Where it goes.** What helps on one kind of feature goes to the file of
  that feature. What helps whatever the feature goes to the working notes:
  the build, the tests, the layout of the code, a trap that any change can
  fall into. A fact has one home: another file points to it.
- **A new file.** When no file fits, create one, named after the feature. Add
  its line to the index in the same change: the file, then when to read it,
  in one or two lines. A file starts with a title and what it covers. Delete
  a file, and its line, when nothing is left in it.
- **What to leave out.** What one `grep` or the module names show. The story
  of a task. What matters to one task only. The maintainer's decisions and
  preferences, which go to memory. What the rules, the README or a module
  header already say: point to it instead. When a note covers what a memory
  says about the repository, trim that memory to what is left.
- **Only what was checked.** An entry states what this session saw in the
  code or in a run. Never a guess.
- **Form.** The task or the symptom first, then what to do. Name files and
  functions, never line numbers. An entry of the working notes is one or two
  lines. An entry of a feature file is four lines at most. Put it under the
  heading it fits, and add a heading only when none fits.
- **Upkeep.** Update an entry instead of adding a second one. Fix or delete
  an entry in the same change that makes it wrong, and at once when you find
  one wrong.
- **Size.** 80 lines at most for the working notes, and 120 lines at most for
  a file of `claude-notes/`. When one is full, merge entries or drop the
  least useful one before adding. A feature file that stays full is split in
  two, by the parts of its feature.
- **Report.** Name each change to the notes in one line of your report.

## Build and test

- Build: `scripts/dev build`
- All tests: `scripts/dev test`
- Regtests: `scripts/dev test Regtest`. After any change to `regtest/Containerfile` 
  or `regtest/rootfs/`, run `regtest/build.sh services`
- One spec: `scripts/dev test Database.Coin` (a word of the module name;
  `--match "/Haskoin.Wallet.Database.Coin/group/"` narrows it)
- `stack build` and `stack test` are the release build. Do not mix them
  with `scripts/dev` in one session: each switch compiles everything again.
- Build the documentation with `scripts/dev docs` and make sure it has no 
  warnings about broken markup.

## Database

  * Version 0.9 is no longer supported (no backward compatibility and no migration).
  * All database schema changes happen in the current database version until an 
    official release. Only then will a migration step be considered.

## Documentation

A change is not finished until the documentation it affects is updated, in
the same change. Check each of these:

- `README.md`: search it for every command, option and message the change
  touches, and update each section concerned. A new section also goes in the
  table of contents. In the `console` blocks, each command must still run and
  each output must be what the new `hw` prints, word for word: check it
  against the test that pins it, or against a run of the new `hw` with `HOME`
  set to a scratch folder. Version numbers name the version in
  `package.yaml`, even before it is released.
- `CHANGELOG.md`, under `[Unreleased]`: meaningful high-level features only,
  not small details or housekeeping.
- Haddock: for every new declaration, and for each module header and comment
  the change made wrong.

Documentation explains the purpose of the code, not only its behaviour: what
problem a module, type or function solves, why it is designed this way, the
trade-offs and the alternatives it rejects, when to use it and when not. New
README material goes in the section where a reader would look for it.

If a change needs no documentation update, say so in your report, with the
reason.

## Feature notes

Read a file before any work on what its line names.

- `claude-notes/tags.md`: tags, the tags of a change address, a tag that
  locks its coins, a `--tag` filter on a report or an export.
- `claude-notes/payments.md`: building a payment, a pending transaction, its
  change address and the coins it holds.
- `claude-notes/transaction-view.md`: what `hw` prints for a transaction, on
  the screen, in a spreadsheet, or before it signs or sends.
- `claude-notes/transaction-files.md`: reading, signing or sending a
  transaction file; the network and the portfolio of a file.
- `claude-notes/sweeps.md`: `preparesweep` and `signsweep`.
- `claude-notes/database.md`: a change of the schema, or a new query.
- `claude-notes/coins.md`: what `sync` does to a coin, `lockcoin` and its
  lock, a payment that its sender replaces.

## Working notes

### Where things are

- `Cli/Parser.hs`, `Commands.hs` and `Cli/PrettyPrinter.hs` have 4000 to 5600
  lines each: search them for the command name and read that range only.
- A command lives in four files of `src/Haskoin/Wallet`: `Cli/Parser.hs` (its
  `Command` constructor, and its `CommandSpec` with the help text),
  `Commands.hs` (the dispatch, its `cmd…` function, and a line in each
  `case` over all commands), `Cli/Response.hs` (its `Response` constructor)
  and `Cli/PrettyPrinter.hs` (what it prints).
- Its tests, under `test/Haskoin/Wallet`: `Arbitrary.hs`, `Cli/ParserSpec.hs`,
  `Cli/PrettyPrinterSpec.hs`, `CommandsSpec.hs` and a spec of `Workflow/`.
- A new module, stashed: the next build writes `haskoin-wallet.cabal` again
  and blocks `git stash pop`. Run `git checkout -- haskoin-wallet.cabal` first.
- Database work of a command goes through `catchDatabaseFailure cfg`
  (`DatabaseFailure.hs`): a failure of SQLite is then a sentence on stdout.

### Test helpers

- `TestUtils.hs`: what every spec shares, the database helpers too.
  `HwTestUtils.hs`: the `hw` of the build against a fake store, in a home
  with no portfolio before `synced` or `newTestPortfolio`.
  `RegtestUtils.hs`: the live regtest chains, in Podman.
- `scripts/dev test WORD` matches the name hspec gives a spec, which has no
  `Spec` at its end: `Workflow.Tag` runs `Workflow/TagSpec.hs`. `Workflow.TagSpec`
  matches nothing, and the run still ends with 0 failures. With `--match`, the
  words are left aside: only the match runs.
- `hw` by hand in a scratch `HOME` needs `.hw/config.toml` with the keys
  `useStore` of `HwTestUtils.hs` writes, and holds no coin. For the output of
  a funded wallet, as a README block needs, print `hw home args` from a
  temporary test of a `Workflow` spec, and run it as the next entry says.
- `hw` wraps at 80 columns a `sentence` of a report and every error, question
  and warning (`wrapText`, `Cli/Wrap.hs`). In a test, `printed` and `unwrapped`
  read through the breaks, which move with the paths. Each prompt ends its line.
- The `hw` of the build, to run by hand: `.stack-work/install` holds some
  twenty old ones. Take the path `scripts/dev build` prints after "Installing
  executable hw in", or the newest of `.stack-work/install/*/*/*/bin/hw`.
- A port podman published, with nothing behind it in the container: a request
  sent under 1 ms after the connection gets a reset, and a later one a plain
  close. A regtest of it takes both sentences of `unreachableStore`.
- Keep the output of `scripts/dev test` in a file: a `grep` for the totals
  loses the name of a test that fails once.
- `scripts/dev test` hides what a test prints. To read it, as for a temporary
  timing, run the test binary itself with `HOME` and `TMPDIR` in the
  scratchpad: `.stack-work/dist/*/*/build/spec/spec --match "/Module/"`.
- `scripts/dev docs` warns only for the modules it builds again. Some 140 link
  warnings (`out of scope`, `ambiguous`) are old: search for the names you added.
- A Haddock link warns as `ambiguous` for a type named like its constructor, and
  as `out of scope` for a name its module does not import: link a function it imports.
