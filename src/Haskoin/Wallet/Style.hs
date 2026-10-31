{-# LANGUAGE LambdaCase #-}

-- | How hw styles what it prints on a terminal, in its help and in the
-- answers of its commands. A document names what each of its parts is,
-- such as a command or an amount, with the 'Style' annotations of
-- prettyprinter. The theme here says how each one looks. Off a terminal,
-- or with NO_COLOR set, the document is printed plain.
--
-- A colour means one thing everywhere. Cyan is a command, and magenta is
-- secret: a command that asks for a mnemonic or private keys, or the
-- mnemonic itself. Green is an option, an amount coming in, or a state as
-- it should be. Red is an amount going out, or something wrong. Yellow
-- waits for something. Faint is secondary, such as the name of a field,
-- and bold is a title or a name. The text itself keeps the colour of the
-- terminal.
--
-- The colours are the dull ANSI ones. Most terminal themes make them
-- readable on a light background as well as on a dark one, which the vivid
-- ones are not.
module Haskoin.Wallet.Style
  ( Style (..),
    Output (..),
    outputOf,
    offlineColourName,
    putDoc,
    renderPlain,
  )
where

import qualified Data.Text.IO as Text
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import qualified Prettyprinter.Render.Text as Plain
import System.Console.ANSI
import System.Environment (lookupEnv)
import System.IO (Handle, hPutChar, hPutStr)

-- | What a part of a document is
data Style
  = -- | The title of a help: what the command does
    Title
  | -- | The heading of a section, such as Options:
    Heading
  | -- | A heading inside a section, such as one over some of the options
    Subheading
  | -- | A command of hw
    Command
  | -- | A command that asks for a mnemonic or private keys, to run on an
    -- offline computer if one can
    OfflineCommand
  | -- | What the help says of such a command
    OfflineNote
  | -- | An option, such as --account
    Option
  | -- | What stands for a value to type, such as FILENAME
    Metavar
  | -- | What is there for completeness, such as a default
    Muted
  | -- | What went wrong: an error, or a state such as a transaction in
    -- conflict
    Failure
  | -- | The name of a field of an answer, such as Fee:
    Field
  | -- | An account, by its name
    Account
  | -- | The words of a mnemonic. They are secret, as the commands that ask
    -- for them are offline.
    Secret
  | -- | The hash by which the commands name a pending transaction: its
    -- nosigHash, or its txid once it is signed
    PendingHash
  | -- | What the user wrote or chose: a label, a file
    UserText
  | -- | An amount the wallet gets or holds
    Incoming
  | -- | An amount the wallet pays
    Outgoing
  | -- | A state as it should be: signed, spendable, valid
    Good
  | -- | A state that waits for something, often the user: not signed,
    -- locked, not confirmed yet
    Waiting
  deriving (Eq, Show)

-- | How each part looks on a terminal
sgrOf :: Style -> [SGR]
sgrOf = \case
  Title -> [bold]
  Heading -> [bold, SetUnderlining SingleUnderline]
  Subheading -> [bold]
  Command -> [foreground Cyan]
  OfflineCommand -> [foreground Magenta]
  OfflineNote -> [foreground Magenta]
  Option -> [foreground Green]
  -- Faint dims the colour of the text, whatever the theme: light grey on
  -- a dark background, dark grey on a light one. A metavar stands out less
  -- than the text around it. Underlined, it differs from a muted part.
  Metavar -> [SetConsoleIntensity FaintIntensity, SetUnderlining SingleUnderline]
  Muted -> [faint]
  Failure -> [foreground Red]
  -- The name is less than its value, as a metavar is less than the text
  Field -> [faint]
  Account -> [bold]
  Secret -> [bold, foreground Magenta]
  PendingHash -> [bold]
  UserText -> [SetItalicized True]
  Incoming -> [foreground Green]
  Outgoing -> [foreground Red]
  Good -> [foreground Green]
  Waiting -> [foreground Yellow]
  where
    faint = SetConsoleIntensity FaintIntensity
    bold = SetConsoleIntensity BoldIntensity
    foreground = SetColor Foreground Dull

-- | The colour of 'OfflineCommand', as the help names it to the user
offlineColourName :: String
offlineColourName = "magenta"

-- | Whether a document goes out styled or plain
data Output = Styled | Plain
  deriving (Eq, Show)

-- | How to print on the handle: styled on a terminal, unless NO_COLOR is
-- set to anything but the empty string (https://no-color.org)
outputOf :: Handle -> IO Output
outputOf h = do
  noColour <- maybe False (not . null) <$> lookupEnv "NO_COLOR"
  terminal <- hSupportsANSI h
  return $ if terminal && not noColour then Styled else Plain

-- | Print the document on the handle, 80 columns wide, then a line break
putDoc :: Handle -> Output -> Doc Style -> IO ()
putDoc h output doc = do
  let stream = layoutPretty (LayoutOptions (AvailablePerLine 80 1.0)) doc
  case output of
    Styled -> renderStyled h stream
    Plain -> Plain.renderIO h stream
  hPutChar h '\n'

-- | Print a laid out document with the codes of its styles. A part inside
-- another one has the styles of both, such as a bold command.
renderStyled :: Handle -> SimpleDocStream Style -> IO ()
renderStyled h = go []
  where
    go :: [Style] -> SimpleDocStream Style -> IO ()
    go styles = \case
      SEmpty -> return ()
      SChar c rest -> hPutChar h c >> go styles rest
      SText _ t rest -> Text.hPutStr h t >> go styles rest
      SLine i rest -> hPutStr h ('\n' : replicate i ' ') >> go styles rest
      SAnnPush style rest -> switchTo (style : styles) >> go (style : styles) rest
      SAnnPop rest -> switchTo (drop 1 styles) >> go (drop 1 styles) rest
      -- layoutPretty never gives up on a layout
      SFail -> error "renderStyled: a layout that failed"
    -- The codes of the styles open, after a reset of those of the part
    -- that ends
    switchTo styles = hPutStr h (setSGRCode (Reset : concatMap sgrOf (reverse styles)))

-- | The document as plain text, on one line as long as it needs
renderPlain :: Doc Style -> String
renderPlain = renderString . layoutPretty (LayoutOptions Unbounded)
