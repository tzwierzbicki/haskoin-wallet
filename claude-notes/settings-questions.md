# Questions about the settings

The questions hw asks about the keys of `config.toml`: the setup of a first
run (four questions) and `setconfig` without an option (twelve). The memory
`setconfig-questions` has the decisions of the maintainer.

## Where things are

- `Cli/Question.hs` asks (`askSeveral`, `askOne`, `askEntry`), with menus on a
  terminal (`Cli/Question/Menu.hs`) and typed lines elsewhere. The question of
  each setting is a value of `Cli/Question/Settings.hs`, without a number, on
  the default of hw. `Cli/Setup.hs` and `Cli/SetConfig.hs` give the numbers.
- `setconfig` without an option is `noSettings`. `setConfigAsked` of
  `Commands/Config.hs` turns it into the `SetConfig` of the answers that
  differ (`changedSettings`), in `commandResponse`, before the command runs:
  `journalAfter` and `cmdSetConfig` then see options, and nothing else.

## A new setting of the file

- To read and ask it: its `Setting` in `configSettings` and its reader in
  `configOfText` (`Config.hs`), a question in `Cli/Question/Settings.hs`, a
  line of `askSettings` and of `changedSettings` (`Cli/SetConfig.hs`).
- To write and show it: a field of `SetConfig` and of `noSettings`
  (`Cli/Command.hs`), a `pure Nothing` or an option in `Cli/Parser/Config.hs`,
  a line of `settingChanges` and of `cmdShowConfig` (`Commands/Config.hs`).
- The count of the questions is written out: "Twelve" in `introDoc` of
  `Cli/SetConfig.hs`, "twelve" in the help of `setconfig` and in the README,
  the table of the README, and `keepAll` and `answering` of
  `Workflow/SetConfigSpec.hs`, which type twelve lines.

## Tests

- A typed prompt longer than 80 columns is wrapped, and a test that keeps the
  lines ending with `] ` loses its start: the home of `withHome` shows eight
  networks, so call `setShownNetworks home ["btc", "bch"]` first.
- On a terminal (`hwAtKeyboard`), a menu is ready at "Enter confirms." and a
  written answer at the start of its prompt, as in "Gap, from 1 to 1000 [".
  A failure of `setconfig` is an answer: `Error: ...` on the output, not on
  the terminal, where the failure of the setup goes.
- What a user sees on a terminal, for a README block: run the `hw` of the
  build under Python `pty.fork()` with `NO_COLOR=1`, write the keys of each
  question after a short wait, and read the escape codes out of what comes.
