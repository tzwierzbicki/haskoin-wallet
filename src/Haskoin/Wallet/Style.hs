{-# LANGUAGE LambdaCase #-}

-- | How hw styles what it prints on a terminal. A document names what each
-- of its parts is, such as a command or an option, with the 'Style'
-- annotations of prettyprinter. The theme here says how each one looks.
-- Off a terminal, or with NO_COLOR set, the document is printed plain.
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
  | -- | What is there for completeness: a default, or the brackets of a
    -- usage line
    Muted
  | -- | What went wrong
    Failure
  deriving (Eq, Show)

-- | How each part looks on a terminal
sgrOf :: Style -> [SGR]
sgrOf = \case
  Title -> [bold]
  Heading -> [bold, SetUnderlining SingleUnderline]
  Command -> [foreground Cyan]
  OfflineCommand -> [foreground Magenta]
  OfflineNote -> [foreground Magenta]
  Option -> [foreground Green]
  Metavar -> [SetUnderlining SingleUnderline]
  Muted -> [SetConsoleIntensity FaintIntensity]
  Failure -> [foreground Red]
  where
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
