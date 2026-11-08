# haskoin-wallet

`hw` is a command-line BIP44 and BIP84 wallet for Bitcoin and Bitcoin Cash. It keeps
its accounts in SQLite and asks a haskoin-store server for the chain. Signing
can happen on an offline machine.

## Do not change this file

Never edit `CLAUDE.md`. Only the maintainer changes it. This holds even when a
change makes something here out of date. Say so in your report instead.

## Build and test

- Build: `stack build`
- All tests: `stack test`
- One spec: `stack test --ta '--match "/Haskoin.Wallet.Database.Coin/"'`
  (the spec's module name without `Spec`; add `/group/` parts to narrow it)
- Warnings are on (`-Wall`). Keep the build free of them.
- The specs under `test/Haskoin/Wallet/Workflow/` run the `hw` binary of
  this build in a home of its own, against a fake store over HTTP
  (`test/Haskoin/Wallet/HwTestUtils.hs`).

## Keep the documentation up to date with every change

Every change to this repository must update all the documentation it affects,
in the same commit or pull request. A change is not finished until its
documentation is. Before you finish, check each of these and update what the
change touches:

- `README.md`: features, installation steps, examples, configuration,
  tutorials, module overview and caveats. Code examples must still compile
  against the new code, and version numbers in examples must name the current
  release.
- `CHANGELOG.md`: describe the change under the next version.
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

Always write the tests based on your expectation and logical deduction of
what the code should do. Don't write tests merely to pin the code. If there are
any discrepancies between your expections and the actual code, fix whichever
you think makes the most sense and report it.

## Checks

Build the documentation with `stack haddock --no-haddock-deps` and make sure it
has no warnings about broken markup. CI builds it too, both with the OS
libraries and with `stack --nix`.
