# Files of a wallet

`importwallet`, `exportwallet` and `show FILE` of a BIP 329 file: the records
(`Bip329.hs`) and what the import does with them (`Labels.hs`).

## The import and its view

- A new rule of the import needs no code in `show FILE`: `fileShown` runs the
  whole import (`importTelling`) in a savepoint and undoes it. A refusal of
  `importwallet` is a refusal of `show FILE`, without ". Nothing was changed".
- What `show` says of a record comes from the import too: `importTelling`
  gives the report and a `FileRecord` for each record that `show` lists, with
  the labels it replaced and its `RecordFate`. `recordFate` reads the fate
  from the counts the record moved: a new count needs a line there.
- The last lines of `show FILE` are those of `importwallet`
  (`importCountsDoc`), from `shownReport`. `FileShown` holds no count of its own.
- A refusal of the keys starts with the lines that were not read
  (`linesNotRead` in `importKeys`): a key that `readWalletKey` refuses, and a
  line that is no record. `fileKeys` keeps the line number of each key.
- A file from which no key is taken is refused when a key that names a
  network was left aside (`noKeyImported`). A key record without `network` is
  of another wallet, and is left aside with a word, in any database.

## Tests

- A test of a key left aside needs a file from which another key is taken:
  `leftAside` of `LabelsSpec.hs` adds the test wallet to the lines given.
- `WalletImport`, `PortfolioImport` and `ImportReport` are built by position
  in some thirty literals of `LabelsSpec.hs`, `PrettyPrinterSpec.hs` and
  `Arbitrary.hs`: a new field is a regex over them. `nothingDone` and
  `importedWith` cover the reports with record updates.
- A printer test of `show FILE` builds its file with `fileOf` of
  `PrettyPrinterSpec.hs` and record updates. Its view ends with a blank line
  and "Labels set: none", which each literal of a whole view carries.
- `Record` is built by position in `lineParser` and `arbitraryRecord` only:
  a new key of hw is a field there, in `record` and in `recordFields`.
- A contact with a known date, for a literal line: `addContactDated` with
  `fixtureTime`. `addContact` dates at the time of the run.
