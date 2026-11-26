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

- Build: `haskell-dev build`
- All tests: `haskell-dev test`
- Regtests: `haskell-dev test Regtest`. After any change to `regtest/Containerfile`
  or `regtest/rootfs/`, run `regtest/build.sh services`
- One spec: `haskell-dev test Database.Coin` (a word of the module name;
  `--match "/Haskoin.Wallet.Database.Coin/group/"` narrows it)
- `haskell-dev` is a script of [jprupp/tools](https://github.com/jprupp/tools),
  shared with the other packages, and not a file of this repository. It must
  be on the `PATH`. Its settings for this package are in `haskell-dev.toml`.
  A change to the script itself is a change to jprupp/tools.
- `stack build` and `stack test` are the release build. Do not mix them
  with `haskell-dev` in one session: each switch compiles everything again.
- Build the documentation with `haskell-dev docs` and make sure it has no
  warnings about broken markup.

## Database

  - Version 0.9 is no longer supported (no backward compatibility and no migration).
  - All database schema changes happen in the current database version until an 
    official release. Only then will a migration step be considered.

## General
  - Do not recommend searching addresses on the offline computer if that only works
    for small wallets and large wallets would miss the address.
  - Keep the stackage resolver aligned with the one in haskoin-core.
  - We only support the latest version of haskoin-store and haskoin-store-data, 
    the one pinned in stack.yaml. We don't write any code to keep 
    backwards-compatibility with older versions. 
  - Before asking the user to make a decision, consider, in order:
    * Any relevant official specifications
    * How other wallets or bitcoin-core implement the feature (if it makes sense)
    * The conservative or safest option for the end-user

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
- `claude-notes/sweeps.md`: `sweep`, `preparesweep` and `signsweep`.
- `claude-notes/wallet-files.md`: `importwallet`, `exportwallet`, `show FILE`
  of a file of a wallet; a record of BIP 329 and the keys of `hw` on it.
- `claude-notes/database.md`: a change of the schema, or a new query.
- `claude-notes/coins.md`: what `sync` does to a coin, `lockcoin` and its
  lock, a payment that its sender replaces.
- `claude-notes/address-gap.md`: the gap of `receive`, the scan of `sync`
  and `recover` for used addresses, and its tests.
- `claude-notes/sync-chain.md`: the blocks `sync` keeps of the chain, the
  server it refuses for being on another chain, `sync --new-chain`.
- `claude-notes/balance-views.md`: the amounts of a portfolio in `portfolios`,
  `sync`, `balances` and below the txid of a sending; those of an address in
  `addresses`.
- `claude-notes/hidden-networks.md`: a network that `shown-networks` leaves
  out: what an answer, a refusal, `recover` or `verifymsg` does with it.
- `claude-notes/mnemonic-sheets.md`: `mnemonicsheet`, the sheet `newportfolio`
  writes in `~/.hw/mnemonic-sheets`, a PDF page laid out by hand.
- `claude-notes/restructuring.md`: splitting a large module into several, or
  moving a module to another directory.

## Working notes

### Where things are

- `Cli/Parser.hs` and `Cli/PrettyPrinter.hs` have 4800 to 6900 lines each:
  search them for the command name and read that range only.
- A command lives in five files of `src/Haskoin/Wallet`: `Cli/Parser.hs` (its
  `Command` constructor, and its `CommandSpec` with the help text),
  `Commands.hs` (the dispatch; its header names the module of `Commands/` with
  each `cmd…` function), `Cli/Response.hs` (its `Response` constructor) and
  `Cli/PrettyPrinter.hs` (what it prints).
- Its tests, under `test/Haskoin/Wallet`: `Arbitrary.hs`, `Cli/ParserSpec.hs`,
  `Cli/PrettyPrinterSpec.hs`, `CommandsSpec.hs` and a spec of `Workflow/`.
- A new module, stashed: the next build writes `haskoin-wallet.cabal` again
  and blocks `git stash pop`. Run `git checkout -- haskoin-wallet.cabal` first.
- A new record with a field named like a common variable (`coin`, `value`):
  give its module `NoFieldSelectors`, or each module that imports it warns of
  shadowing. The fields still read with a dot.
- Testnet3: `btcTest` of haskoin-core warns as deprecated. Use `btcTestnet3` of
  `Haskoin.Wallet.Testnet3`, in tests too. Bitcoin Cash's is `bchTest3`, not deprecated.
- A local function that reads a field with a dot (`row.field`) needs a type
  signature or `FlexibleContexts`: else `HasField`, "Non type-variable argument".
- A listing reaches the screen through `printListing` of `Main.hs` and
  `chunkDoc`. The `listing` case of `responsePrinter` serves the tests only.
- Database work of a command goes through `catchDatabaseFailure cfg`
  (`Database/Failure.hs`): a failure of SQLite is then a sentence on stdout.

### Test helpers

- `TestUtils.hs`: what every spec shares, the database helpers too.
  `HwTestUtils.hs`: the `hw` of the build against a fake store, in a home
  with no portfolio before `synced` or `newTestPortfolio`.
  `RegtestUtils.hs`: the live regtest chains, in Podman.
- `haskell-dev test WORD` matches the name hspec gives a spec, which has no
  `Spec` at its end: `Workflow.Tag` runs `Workflow/TagSpec.hs`. `Workflow.TagSpec`
  matches nothing: the run says `No spec module has` and fails. With `--match`,
  the words are left aside: only the match runs.
- `hw` by hand in a scratch `HOME` needs `.hw/config.toml` with the keys
  `useStore` of `HwTestUtils.hs` writes, and holds no coin. For the output of
  a funded wallet, as a README block needs, print `hw home args` from a
  temporary test of a `Workflow` spec, and run it as the next entry says.
- `hw` wraps at 80 columns a `sentence` of a report and every error, question
  and warning (`wrapText`, `Cli/Wrap.hs`). In a test, `printed` and `unwrapped`
  read through the breaks, which move with the paths. Each prompt ends its line.
  Not on a run of `hwOnScreen`, whose lines end with `\r\n`: compare the words
  there, `Text.unwords (Text.words (runOut run))`.
- A secret asked on a terminal: `hw` discards what is still typed ahead when
  it ends (`forgetTypedSecrets`, `Cli/Prompt.hs`). `hwThenShell` gives the
  line a shell would read after `hw`, to test what a command leaves there.
- The `hw` of the build, to run by hand: `.stack-work/install` holds some
  twenty old ones. Take the path `haskell-dev build` prints after "Installing
  executable hw in", or the newest of `.stack-work/install/*/*/*/bin/hw`.
- A port podman published, with nothing behind it in the container: a request
  sent under 1 ms after the connection gets a reset, and a later one a plain
  close. A regtest of it takes both sentences of `unreachableStore`.
- Keep the output of `haskell-dev test` in a file: a `grep` for the totals
  loses the name of a test that fails once.
- `haskell-dev test` hides what a test prints. To read it, as for a temporary
  timing, run the test binary itself with `HOME` and `TMPDIR` in the
  scratchpad: `.stack-work/dist/*/*/build/spec/spec --match "/Module/"`.
- A regtest spec run that way is all pending, "not built": podman keeps its
  images under the real `HOME`. Run it with `haskell-dev test --match`.
- The regtest containers are pods of podman-test (`Test.Podman`). What a killed
  run left: `podman pod ps --filter label=podman-test.run`, then `podman pod rm`.
- Regtests all fail with "The store of btc is not in step with its node": the
  store did not start, as with an option its version dropped (`regtest/README.md`).
- Run all tests with `haskell-dev test --jobs 14`: about 4 minutes, most of it
  the regtests. The default of jprupp/tools is 8 processes.
- A whole run ends with "8 pending" or "110 pending": regtest pods did not
  start, as when memory ran out. Run `haskell-dev test Regtest --jobs 4`.
- `MemorySpec` failed once in a whole run, on `waitForProcess: does not exist
  (No child processes)`, and passed alone and in the next whole run.
- `haskell-dev docs` runs Haddock on every module each time, in about 40
  seconds, and ends with a check that must say `nothing to report`.
- A link in a Haddock comment: `'name'` when the module imports or exports the
  name, `'Module.name'` for an exported name of another module, `@name@` otherwise.
- A type named like its constructor is `t'Name'`, read after a space only: write
  `(the type t'Name')`. A type of another module is `t'Module.Name'`.
- In a Haddock comment, a slash is `\/` and a capitalized word in double quotes
  is `\"Total\"`: `/a/` is italics, and `"Total"` a link to a module.
