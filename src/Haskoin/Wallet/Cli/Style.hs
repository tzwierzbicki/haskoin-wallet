{-# LANGUAGE LambdaCase #-}

-- | How hw styles what it prints on a terminal, in its help and in the
-- answers of its commands. A document names what each of its parts is,
-- such as a command or an amount, with the 'Style' annotations of
-- prettyprinter. The theme here says how each one looks. Off a terminal,
-- or with NO_COLOR set, the document is printed plain.
--
-- A colour means one thing everywhere. Cyan is a command, and magenta is
-- a command to run on an offline computer. Green is an option, an amount
-- coming in, or a state as it should be. Red is an amount going out, or
-- something wrong. Yellow asks for a second look: something waits, or
-- an answer is about another portfolio than the active one. Blue is an
-- address of the wallet, with its label, which sets it apart from the
-- addresses of others in a transaction. An underline is an address of the
-- address book, one of somebody else that the user knows. Faint is
-- secondary, such as the name of a field, and bold is a title, a name,
-- the words of a mnemonic or the row of a menu the cursor is on.
-- The text itself keeps the colour of the terminal.
--
-- The logo at the top of the help is cyan too, since it spells the name
-- of the program, which is what one types before any command.
--
-- The colours are the dull ANSI ones. Most terminal themes make them
-- readable on a light background as well as on a dark one, which the vivid
-- ones are not. Only a QR code sets its own colours: see 'QrCode'.
--
-- Whatever a document holds, the terminal receives no order from its
-- text: every control or format character in it is printed as an escape
-- ('visibleStream'), styled or plain. The only codes on the terminal are
-- those of the styles.
module Haskoin.Wallet.Cli.Style
  ( Style (..),
    Output (..),
    outputOf,
    offlineColourName,
    errorDoc,
    wrappedAfter,
    putDoc,
    renderPlain,
    visibleStream,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Haskoin.Wallet.Cli.Wrap (screenColumns, wrapText)
import Haskoin.Wallet.Tags (TagColor (..))
import Haskoin.Wallet.Util (hidingSecKeys, isHidden, visibleText)
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
  | -- | A command that asks for a mnemonic or private keys and needs no
    -- server, to run on an offline computer if one can
    OfflineCommand
  | -- | What the help says of such a command
    OfflineNote
  | -- | An option, such as --portfolio
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
  | -- | The words of a mnemonic or of a share. They keep the colour of the
    -- terminal, which reads well on any theme.
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
    -- locked, not confirmed yet. A replacement too, with the transactions
    -- it replaces: the network keeps one of them, and the payment waits in
    -- two places until it mines one.
    Waiting
  | -- | The note that an answer about one thing shows it from another
    -- portfolio than the active one. Its paths and its labels are of that
    -- portfolio, which a user who switched may take for the active one.
    OtherPortfolio
  | -- | An address of the wallet, or the derivation path that names it,
    -- and its label, in a transaction or a coin. They share a colour, so
    -- that a label below its address is seen to go with it, and so that
    -- the addresses of the wallet stand apart from those of others.
    OwnAddress
  | -- | An address of the address book in a transaction: one of somebody
    -- else, which the user added as a contact
    -- ("Haskoin.Wallet.Contacts"). It is underlined, and keeps the colour
    -- of the text.
    --
    -- No colour was left for it. The six dull colours each mean one
    -- thing already, and a tag may be shown in any of them. Italic is
    -- what the user wrote, a label or a file. An underline is used
    -- nowhere else in an answer: only the help underlines, for its
    -- headings and for the values to type. It also suits what it says.
    -- The address is not the wallet's, so it is not blue, but it is not a
    -- stranger's either: it is a known one, marked as a word one has
    -- looked up.
    BookAddress
  | -- | The row of a menu the cursor is on, in the setup. The row also
    -- starts with a @>@, so the menu reads the same without styles.
    Selected
  | -- | A QR code. It is black on white whatever the theme: a scanner
    -- finds dark modules on a light background, and not all of them read
    -- the opposite.
    QrCode
  | -- | A tag, in the colour the user gave it. This is the one colour
    -- that means nothing of its own: the user chose it, to tell the tags
    -- apart at a glance. A tag is always in a place that says it is one,
    -- between brackets or in a column of tags, so its colour is not taken
    -- for the red of an amount going out or the green of one coming in.
    TagColour !TagColor
  | -- | A square of the logo that stands for a set bit
    -- ("Haskoin.Wallet.Logo")
    LogoSetBit
  | -- | A square of the logo that stands for a clear bit. It is the same
    -- square in a weaker shade, like a lamp that is off beside one that
    -- is on.
    LogoClearBit
  | -- | The name of the program, after the squares of the logo
    LogoName
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
  Secret -> [bold]
  PendingHash -> [bold]
  UserText -> [SetItalicized True]
  Incoming -> [foreground Green]
  Outgoing -> [foreground Red]
  Good -> [foreground Green]
  Waiting -> [foreground Yellow]
  OtherPortfolio -> [foreground Yellow]
  OwnAddress -> [foreground Blue]
  BookAddress -> [SetUnderlining SingleUnderline]
  Selected -> [bold]
  -- The colours of the palette's cube, which themes leave as they are,
  -- unlike the sixteen named ones
  QrCode -> [SetPaletteColor Foreground (xterm6LevelRGB 0 0 0), SetPaletteColor Background (xterm6LevelRGB 5 5 5)]
  TagColour color -> [foreground (ansiColourOf color)]
  LogoSetBit -> [foreground Cyan]
  -- Faint follows the theme, as it does for a metavar: the square is
  -- weaker than a set one on a dark background and on a light one. A
  -- fixed grey would vanish on one of the two.
  LogoClearBit -> [faint, foreground Cyan]
  LogoName -> [bold, foreground Cyan]
  where
    faint = SetConsoleIntensity FaintIntensity
    bold = SetConsoleIntensity BoldIntensity
    foreground = SetColor Foreground Dull

-- | The colour of the terminal a colour of a tag stands for
ansiColourOf :: TagColor -> Color
ansiColourOf = \case
  TagRed -> Red
  TagGreen -> Green
  TagYellow -> Yellow
  TagBlue -> Blue
  TagMagenta -> Magenta
  TagCyan -> Cyan

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

-- | A failure, as an answer, a listing or a command line that cannot be
-- parsed reports it. Every error on the screen is this line, so this is
-- where a private key the error quotes is taken out ('hidingSecKeys').
--
-- The message is wrapped between words ('wrappedAfter'), with its first
-- line counted from the word Error. A long message was one line before,
-- which the terminal cut in the middle of a word.
errorDoc :: Text.Text -> Doc Style
errorDoc err = wrappedAfter (annotate Failure (pretty label)) label (hidingSecKeys err)
  where
    label = "Error:"

-- | A message after a label, wrapped between words ('wrapText'): its
-- first line is counted from the label, which is given as it is printed
-- and as its plain text. A command to copy that the message gives on a
-- line of its own stays whole.
wrappedAfter :: Doc Style -> String -> Text.Text -> Doc Style
wrappedAfter labelDoc label message =
  labelDoc <> pretty (Text.pack (drop (length label) (wrapText (label <> " " <> Text.unpack message))))

-- | Print the document on the handle, 'screenColumns' wide, then a line
-- break
putDoc :: Handle -> Output -> Doc Style -> IO ()
putDoc h output doc = do
  let stream = visibleStream $ layoutPretty (LayoutOptions (AvailablePerLine screenColumns 1.0)) doc
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
renderPlain = renderString . visibleStream . layoutPretty (LayoutOptions Unbounded)

-- | A laid out document whose texts hold no order to the terminal: each
-- control or format character is written as an escape ('visibleText').
-- A label of a file that reached a document as it is can then neither
-- move the cursor nor rewrite what hw printed. A line break is the
-- layout's own and stays one: a text that may hold one must be passed
-- through 'visibleText' before it becomes a document. The layout is done
-- before, so a line that holds an escape is a little longer than the
-- layout counted.
visibleStream :: SimpleDocStream ann -> SimpleDocStream ann
visibleStream stream = case stream of
  SEmpty -> SEmpty
  SFail -> SFail
  SChar c rest
    | isHidden c -> visible (Text.singleton c) rest
    | otherwise -> SChar c (visibleStream rest)
  SText _ t rest -> visible t rest
  SLine i rest -> SLine i (visibleStream rest)
  SAnnPush ann rest -> SAnnPush ann (visibleStream rest)
  SAnnPop rest -> SAnnPop (visibleStream rest)
  where
    visible t rest = let shown = visibleText t in SText (Text.length shown) shown (visibleStream rest)
