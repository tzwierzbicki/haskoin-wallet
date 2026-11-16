# The fonts of the reports

`hw report` writes PDF files that carry their fonts. The two files here are
compiled into `hw` (`Haskoin.Wallet.Report.Typefaces`), and a report embeds the
glyphs it uses.

| File | Font |
|---|---|
| `LiberationSans-Regular.ttf` | Liberation Sans, version 2.1.5 |
| `LiberationSans-Bold.ttf` | Liberation Sans Bold, version 2.1.5 |

They come unchanged from the Liberation Fonts project,
<https://github.com/liberationfonts/liberation-fonts>.

    sha256  075e71be4c58beda172ecb574a799308f04c3d054c6a5b7434954d3b0f1b28d8  LiberationSans-Regular.ttf
    sha256  b72c65e551d653b54217ffad0d1454ce0657e4e8d4a28bcc79e0acd2598f4bf3  LiberationSans-Bold.ttf

## Licence

The fonts are under the SIL Open Font License, version 1.1, whose text is in
`LICENSE`. It lets anyone bundle the fonts with software and embed them in
documents. The licence of `hw` itself does not cover them.

    Digitized data copyright (c) 2010 Google Corporation
      with Reserved Font Arimo, Tinos and Cousine.
    Copyright (c) 2012 Red Hat, Inc.
      with Reserved Font Name Liberation.

## Why this font

Liberation Sans has the widths of Helvetica and Arial, so a statement looks
as readers expect one to look. It has the letters of Latin, Greek and
Cyrillic. It has no Chinese, Japanese, Korean or Arabic: a character it does
not have is written as a question mark.

To change the font, replace the two files and the names in
`Haskoin.Wallet.Report.Typefaces`. The font must be a TrueType font with
outlines (a `glyf` table), not an OpenType font with PostScript outlines.
