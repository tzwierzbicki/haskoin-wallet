{-# LANGUAGE OverloadedStrings #-}

-- | A command line of hw in an answer: a line the user can copy and run
-- ('hwLine'), and the words of one, each in the colour the help gives it.
-- A command takes its colour from the list of the commands
-- ("Haskoin.Wallet.Cli.Parser"), so this is the one module of the answers
-- that needs the parser.
module Haskoin.Wallet.Cli.PrettyPrinter.CommandLine
  ( hwLine,
    commandWord,
    optionWord,
    metavarWord,
    pendingHashWord,
    fileWord,
    shellWord,
  )
where

import Data.Char (isAlphaNum)
import Haskoin.Wallet.Cli.Parser (program)
import Haskoin.Wallet.Cli.Parser.Help (commandName)
import Haskoin.Wallet.Cli.Parser.Run (commandNamed)
import Haskoin.Wallet.Cli.PrettyPrinter.Base (styled)
import Haskoin.Wallet.Cli.Style
import Prettyprinter (Doc, hsep)

-- | A command line of hw, "hw signtx NOSIGHASH", the command in the colour
-- of its kind as the help shows it
hwLine :: String -> [Doc Style] -> Doc Style
hwLine name args = hsep ("hw" : commandWord name : args)

-- | A command of hw, in the colour of its kind, as the list of commands
-- shows it
commandWord :: String -> Doc Style
commandWord name = maybe (styled Command name) commandName (commandNamed program name)

-- | An option, in the colour the help shows the options in
optionWord :: String -> Doc Style
optionWord = styled Option

-- | What the user has to fill in, as the help shows it
metavarWord :: String -> Doc Style
metavarWord = styled Metavar

-- | The hash that names a pending transaction
pendingHashWord :: String -> Doc Style
pendingHashWord = styled PendingHash

-- | The name of a file in a command the user may copy, quoted when the
-- shell needs it ('shellWord')
fileWord :: String -> Doc Style
fileWord = styled UserText . shellWord

-- | A word of a command typed in a shell, quoted when the shell would read
-- it as more than one word or change it: a file name with a space, for
-- instance
shellWord :: String -> String
shellWord word
  | not (null word) && all plain word = word
  | otherwise = "'" <> concatMap quoted word <> "'"
  where
    plain c = isAlphaNum c || c `elem` ("-_./:@%+=," :: String)
    quoted '\'' = "'\\''"
    quoted c = [c]
