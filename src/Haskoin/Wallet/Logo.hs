-- | The logo of hw: its name, as a computer stores it.
--
-- The name is two letters, and a computer keeps a letter as a byte. The
-- byte of h is 0x68, and the byte of w is 0x77. The logo draws each byte
-- as a row of eight squares, a square for each bit, the high bit first.
-- A set bit is a strong square and a clear bit is a pale one. The name
-- stands after the last row, for a reader who does not read bits.
--
-- = Where it is drawn
--
-- The logo is drawn in two places, each with what the place has:
--
-- * At the top of the help of the program, in block characters
--   ("Haskoin.Wallet.Cli.Parser.Help").
--
-- * At the top of the first page of a report, in filled boxes
--   ("Haskoin.Wallet.Report.Layout").
--
-- This module holds what the two drawings share: the name, and its
-- bits. Neither drawing spells the bits out by hand, so the two cannot
-- come to differ.
--
-- = Why this logo
--
-- The logo was designed for a terminal first. It needs one character
-- for a square, which every monospace font has, and it takes two lines
-- above the help. A name drawn in large letters of block characters
-- takes four lines or more, and looks different in every font.
--
-- The same squares make the report version. The PDF writer of the
-- reports draws text, rules and filled boxes, and nothing else. A logo
-- with curves or with an image would ask more of it.
module Haskoin.Wallet.Logo
  ( logoName,
    nameBits,
  )
where

import Data.Bits (testBit)
import Data.Char (ord)

-- | The name the logo spells
logoName :: String
logoName = "hw"

-- | The bits of each letter of a name, a row for each letter. A row has
-- the eight bits of the byte of its letter, the high bit first, and True
-- for a set bit. The name is plain letters, each of one byte: only the
-- low eight bits of a character are read.
nameBits :: String -> [[Bool]]
nameBits name = [[testBit (ord letter) bit | bit <- [7, 6 .. 0]] | letter <- name]
