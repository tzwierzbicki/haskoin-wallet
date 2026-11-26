# Questions about the settings

The questions hw asks about the keys of `config.toml`: the setup of a first
run (four questions) and `setdefault` without an option (twelve). The memory
`setdefault-questions` has the decisions of the maintainer.

## Where things are

- `Cli/Question.hs` asks (`askSeveral`, `askOne`, `askEntry`), with menus on a
  terminal (`Cli/Question/Menu.hs`) and typed lines elsewhere. The question of
  each setting is a value of `Cli/Question/Settings.hs`, without a number, on
  the default of hw. `Cli/Setup.hs` and `Cli/SetDefault.hs` give the numbers.
- `setdefault` without an option is `noDefaults`. `defaultsAsked` of
  `Commands/Defaults.hs` turns it into the `SetDefault` of the answers that
  differ (`changedDefaults`), in `commandResponse`, before the command runs:
  `journalAfter` and `cmdSetDefault` then see options, and nothing else.

## A new setting of the file

- It needs: its `Setting` in `configSettings` and its reader in `configOfText`
  (`Config.hs`), a question in `Cli/Question/Settings.hs`, a line of
  `askSettings` and of `changedDefaults` (`Cli/SetDefault.hs`), a field of
  `SetDefault` and of `noDefaults` (`Cli/Command.hs`, and a `pure Nothing` or an
  option in `Cli/Parser/Defaults.hs`), and a line of `defaultChanges`.
- The count of the questions is written out: "Twelve" in `introDoc` of
  `Cli/SetDefault.hs`, "twelve" in the help of `setdefault` and in the README,
  the table of the README, and `keepAll` and `answering` of
  `Workflow/SetDefaultSpec.hs`, which type twelve lines.

## Tests

- A typed prompt longer than 80 columns is wrapped, and a test that keeps the
  lines ending with `] ` loses its start: the home of `withHome` shows eight
  networks, so call `setShownNetworks home ["btc", "bch"]` first.
- On a terminal (`hwAtKeyboard`), a menu is ready at "Enter confirms." and a
  written answer at the start of its prompt, as in "Gap, from 1 to 1000 [".
  A failure of `setdefault` is an answer: `Error: ...` on the output, not on
  the terminal, where the failure of the setup goes.
- What a user sees on a terminal, for a README block: run the `hw` of the
  build under Python `pty.fork()` with `NO_COLOR=1`, write the keys of each
  question after a short wait, and read the escape codes out of what comes.
