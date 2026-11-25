# Mnemonic sheets

The blank PDF sheet to write a mnemonic or the shares of a backup on by hand:
`mnemonicsheet`, and the sheet `newportfolio` writes for a new wallet in
`~/.hw/mnemonic-sheets`. The memory `mnemonic-backup-sheet` has the decisions.

## Where it is

- The page is pure, in `Report/MnemonicSheet.hs`: `sheetKindOf` reads the
  options as `newportfolio` does, `sheetPages` gives the texts of each page,
  `pageMarks` places them. `Commands.hs` has the IO only: `cmdMnemonicSheet`,
  `writeWalletSheet` and `removeWalletSheet`.
- A page laid out by hand takes its title block, footer, notes, colours and
  margins from `Report/Layout.hs` (`titleMarks`, `footerMarks`, `notesMarks`,
  `notesHeight`): never copy them.
- The number of words comes from `mnemonicWordCount` (`Entropy.hs`) and
  `shareWordCount` (`Shares.hs`), which `newMnemonic` and `newShares` check
  their entropy with too: one refusal for both commands.

## The layout

- A change of the page: the sheet of 12 words must stay the approved one,
  `~/Templates/mnemonic-backup-sheet.pdf`. Compare 80 dpi images of both
  (`pdftoppm -r 80 -png`, then `cmp`): they were identical on 2026-11-25.
- The words take the room between the fields and the notes, so a line gets
  lower with more words. The number of a word and the gap above its rule
  scale with the height of a row in `wordLineMarks`: with a fixed gap, the
  first line of 24 or 33 words runs into the rule of the heading.
- To look at a page: a `Main` in the scratchpad that writes `sheetPdf` to a
  file, run with `stack exec -- runghc FILE` from the repository after a
  build, then `pdftoppm -r 80 -png` and read the image.

## The sheet of a wallet

- `newportfolio` writes it as the last step of `newPortfolioAnswer`, for a
  new secret only. A failure is a `warn` on stderr and no sixth field in
  `ResponseNewPortfolio`: the words are made by then, and the wallet stays.
- No box of the passphrase is ticked, and "Wallet" stays blank: a tick would
  keep on disk whether the wallet has a passphrase.

## Tests

- `pdfTexts` (`TestUtils.hs`) reads the texts of a written file, in the order
  drawn: the kind, the title, the subtitle, the fields, then the numbers.
  `textsOf` and `marksOf` (`Report/TestUtils.hs`) read a page before it is a
  file.
- The mnemonic of `newportfolio` is random: a test takes the fingerprint and
  the file from the answer (`ResponseNewPortfolio`), never from a fixture.
