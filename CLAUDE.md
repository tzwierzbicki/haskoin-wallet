# haskoin-wallet

`hw` is a command-line BIP44 and BIP84 wallet for Bitcoin and Bitcoin Cash. It keeps
its accounts in SQLite and asks a haskoin-store server for the chain. Signing
can happen on an offline machine.

## How this file is kept

This file is loaded into every session, so each line costs tokens every time.
It has two parts:

- **The rules**: everything above `## Working notes`. Only the maintainer
  changes them. If a change makes a rule out of date, say so in your report
  and leave the rule alone.
- **The working notes**: the last section. Claude keeps it, without asking,
  by the rules below.

### Rules for the working notes

The notes save the next session the research this session paid for.

- **When to add.** Something about this repository took real effort to find
  out (several searches, a failed build or test run, a wrong first approach),
  and it would take that effort again on another feature. At the end of a
  task, ask what would have saved the most effort, and write that down, or
  nothing.
- **What to leave out.** What one `grep` or the module names show. The story
  of a task. What matters to one feature only. The maintainer's decisions and
  preferences, which go to memory. What the rules, the README or a module
  header already say: point to it instead. A fact has one home: when a note
  here covers what a memory says about the repository, trim that memory to
  what is left.
- **Only what was checked.** An entry states what this session saw in the
  code or in a run. Never a guess.
- **Form.** One entry is one or two lines: the task or the symptom first,
  then what to do. Name files and functions, never line numbers. Put it under
  the heading it fits, and add a heading only when none fits.
- **Upkeep.** Update an entry instead of adding a second one. Fix or delete
  an entry in the same change that makes it wrong, and at once when you find
  one wrong.
- **Size.** 80 lines at most. When they are full, merge entries or drop the
  least useful one before adding.
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
- The change address of a payment: no row says it is taken. `changeAddress` in
  `Database/PendingTx.hs` gives the lowest one that has no transaction and
  that no pending transaction pays. `insertPendingTx` takes one for every
  new pending transaction, and `removePendingTx` gives it back.
- The tags of a change address, never put unasked: `changeTakesTags` in
  `Database/PendingTx.hs` (`--tag-change`), `retagChangeChain` in
  `Database/ChangeChain.hs` (`tag --with-change`), `untagFreedChange`.
- A tag that locks its coins: a column of the tag row, nothing per coin.
  `userLocks` in `Database/PendingTx.hs` reads the lockcoin and tag locks
  (`Database/Coin.hs` imports that module). A payment opens its
  `--spend-from-tag` tags: `spendableCoinsOpening`, `buildTxSignData`, then
  `importPreparedTx`, which keeps them in the pending row. `refuseLockedCoins`
  asks again at signtx and broadcast. A command that may lock: `tellingStopped`.
- The coins a pending transaction holds, in `Database/PendingTx.hs`: those
  it spends (`lockedOutpoints`), those of payments it replaces (`replacedOutpoints`).
- A question about the transaction a payment builds: ask between the build
  and `keepPreparedLeaving`, which writes the `--output` file. `txInfoOf pool
  tsd` gives its summary, with the tags of its addresses, before anything is
  kept, as `confirmMixing` in `Commands.hs` does.
- The addresses the view of a transaction lists: `spentFrom` and `paidTo` in
  `Cli/PrettyPrinter.hs`, shared with the sheet; all when `txInPreparation`.
- The transactions at an address: `txidsAtQuery` in `Database/ChangeChain.hs`,
  then `liveSummary`. A summary alone tells spending from paying.
- `touchedBy` names as change the internal addresses a transaction pays, and
  no other: change `--change-back` sent to a receiving address is not in it.
  It can also be the address the payment spends from, so addresses loop.
- A filter of tags on what `wholeHistory` reads (a statement, the journal):
  its rows have no tags of own addresses. `txSelection` in `Commands.hs` asks
  the database (`txidsKept`), the pure modules ask `keepsTx` of
  `TxSelection.hs`; for addresses, `addressSelection` and `keepsAddress`.
- A new module, stashed: the next build writes `haskoin-wallet.cabal` again
  and blocks `git stash pop`. Run `git checkout -- haskoin-wallet.cabal` first.
- Database work of a command goes through `catchDatabaseFailure cfg`
  (`DatabaseFailure.hs`): a failure of SQLite is then a sentence on stdout.
- A column added to V5 reaches no database of an earlier build: the tables
  are made in a file without any only (`createDatabase` in `Migration.hs`).

### Database queries

- A join inside an `exists` with no constant on its second table: SQLite may
  read it the other way round, the whole second table for each row. Pin the
  plan in `DatabaseSpec` (`queryPlanOf`), and nest a second `exists` in place
  of the join to settle the order, as `txKept` in `Database/Tag.hs` does.

### Test helpers

- `TestUtils.hs`: what every spec shares. `HwTestUtils.hs`: the `hw` of the
  build against a fake store, in a home with no portfolio before `synced` or
  `newTestPortfolio`. `RegtestUtils.hs`: the live regtest chains, in Podman.
- A payment shuffles its outputs: a workflow test finds the change by its
  address among `inMempool`, as `changeOn` in `Workflow/BumpFeeSpec.hs` does.
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
- `scripts/dev test` hides what a test prints. To read it, as for a temporary
  timing, run the test binary itself with `HOME` and `TMPDIR` in the
  scratchpad: `.stack-work/dist/*/*/build/spec/spec --match "/Module/"`.
- `scripts/dev docs` warns only for the modules it builds again. Some 140 link
  warnings (`out of scope`, `ambiguous`) are old: search for the names you added.
