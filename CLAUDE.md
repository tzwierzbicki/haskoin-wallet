# haskoin-wallet

`hw` is a command-line BIP44 and BIP84 wallet for Bitcoin and Bitcoin Cash. It keeps
its accounts in SQLite and asks a haskoin-store server for the chain. Signing
can happen on an offline machine.

## Do not change this file

Never edit `CLAUDE.md`. Only the maintainer changes it. This holds even when a
change makes something here out of date. Say so in your report instead.

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

## Keep the documentation up to date with every change

Every change to this repository must update all the documentation it affects,
in the same commit or pull request. A change is not finished until its
documentation is. Before you finish, check each of these and update what the
change touches:

- `README.md`: each section the change touches. The sections are the feature
  list at the top, Install, Quick start, Keep your funds safe, Using `hw`,
  Commands, Transaction files, Configuration, Shell completion, and Upgrading
  from an earlier version. A new section also goes in the table of contents.
- The examples in the `console` blocks of the README. Some blocks list
  commands, each with a `#` comment. Others show a session: a `$ hw` line,
  then what `hw` prints, with `...` for the lines left out. Search the README
  for every command, option and message the change touches. Each command shown
  must still run. Each output shown must be what the new `hw` prints, word for
  word. Check an output against the test that pins it, or against a run of the
  new `hw` with `HOME` set to a scratch folder.
- Version numbers in the README. They name the version in `package.yaml`,
  even before that version is released.
- `CHANGELOG.md`: describe the change under `[Unreleased]`. Only meaningful
  high-level features are described in the CHANGELOG, not small details or
  housekeeping edits.
- Haddock in `src`.
- Module headers and declaration comments: add Haddock for every new
  declaration, and fix comments that the change made wrong.

If a change needs no documentation update, say so explicitly when you report
the change, with the reason.

## How to write documentation here

Documentation explains the purpose and spirit of the code, not only its
behaviour. For every module and every significant type or function, say what
problem it solves and why it is designed the way it is, including the trade-offs
and the alternatives it rejects. A reader should come away knowing when to use
it and when not to. Keep to the structure the README already has, and put new
material in the section where a reader would look for it.

## Testing conventions

A test states what the code should do, in words that make sense without
reading the code. Its name says the behaviour in plain words. Its expected
values are literals: amounts, addresses, lines of output, JSON. A short comment
gives any arithmetic, such as `-- 40000 paid + 1000 of fee = 41000`. A test
never computes its expectation with the code under test. Each test checks one
rule.

Write each test from what the code should do, by your own reasoning. Do not
write a test only to pin what the code does now. Before you write a test, say
in one sentence what should happen in that case. Take the expectation from
that sentence, not from the code.

If the code disagrees with the expectation, keep the expectation. Report the
mismatch to the maintainer, with the failing test. Never change a test to make
it pass without saying so. If a test would only run a code path and has no
sound expectation, say so instead of writing it.

