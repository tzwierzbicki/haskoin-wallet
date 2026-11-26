{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | The building blocks of the command line: the options and arguments of
-- a command, each made once, as the row of the help that describes it and
-- the optparse-applicative parser that reads it. The two cannot drift
-- apart.
--
-- The builders read like those of optparse-applicative: 'option',
-- 'switch', 'flag', 'argument', with the modifiers 'short', 'long',
-- 'metavar', 'help', 'value' and 'showConfig'. They make an t'Args'
-- instead of a parser. An t'Args' holds the usage and the help rows next to
-- the parser, and '<*>', '<|>', 'optional', 'many' and 'some' combine all
-- three at once. Only the parser goes to optparse-applicative.
-- "Haskoin.Wallet.Cli.Parser.Help" lays out the rest.
module Haskoin.Wallet.Cli.Parser.Args
  ( -- * The options of a command
    Args (..),
    Usage (..),
    OptDoc (..),
    Name (..),
    nameText,

    -- * Building an option, a flag or an argument
    Mod,
    OptionFields,
    option,
    strOption,
    switch,
    flag,
    flag',
    argument,
    strArgument,
    secondLeftToCommand,
    short,
    long,
    metavar,
    help,
    value,
    showConfig,
    showConfigWith,
    completeWith,
    completer,
    HasCompleter,
    optionGroup,

    -- * Reading a value, from optparse-applicative
    ReadM,
    str,
    maybeReader,
    eitherReader,
    listIOCompleter,
    mkCompleter,

    -- * A command, and the program
    CommandSpec (..),
    CommandGroup (..),
    Program (..),
  )
where

import Control.Applicative (Alternative (..), optional)
import Data.Maybe (isJust)
import Data.String (IsString)
import Options.Applicative (HasCompleter, OptionFields, ReadM, eitherReader, listIOCompleter, maybeReader, mkCompleter, str)
import qualified Options.Applicative as O

{- The options of a command -}

-- | The options and arguments of a command: the shape of its usage line,
-- the rows of its help, and the parser that reads them. '<*>' puts two
-- one after the other, '<|>' makes them alternatives, and 'optional',
-- 'many' and 'some' apply to the three at once.
data Args a = Args
  { usage :: Usage,
    rows :: [OptDoc],
    parser :: O.Parser a
  }

instance Functor Args where
  fmap f args = args {parser = fmap f args.parser}

instance Applicative Args where
  pure x = Args NoUsage [] (pure x)
  f <*> x = Args (Sequence f.usage x.usage) (f.rows <> x.rows) (f.parser <*> x.parser)

instance Alternative Args where
  empty = Args NoUsage [] empty
  a <|> b = Args (choice a.usage b.usage) (a.rows <> b.rows) (a.parser <|> b.parser)

  -- The default many and some are defined by each other, and would build
  -- the usage and the rows without end. Repeated or not, an option has one
  -- row.
  many a = Args (Optional (Repeated a.usage)) a.rows (many a.parser)
  some a = Args (Repeated a.usage) a.rows (some a.parser)

-- | One usage or the other. A branch with nothing to type, a pure value,
-- makes the other optional: that is what 'optional' builds.
choice :: Usage -> Usage -> Usage
choice NoUsage u = Optional u
choice u NoUsage = Optional u
choice a b = Choice a b

-- | The shape of the usage line of a command: how its options and
-- arguments are combined
data Usage
  = -- | Nothing to type: a pure value
    NoUsage
  | -- | One option, flag or argument
    Item OptDoc
  | -- | One after the other
    Sequence Usage Usage
  | -- | One or the other
    Choice Usage Usage
  | -- | Left out, or given
    Optional Usage
  | -- | Given once or more
    Repeated Usage
  deriving (Eq, Show)

-- | What the help says of one option, flag or argument
data OptDoc = OptDoc
  { -- | The names it is typed by, none for an argument
    names :: [Name],
    -- | The name of its value in the help, when it takes one
    valueName :: Maybe String,
    -- | What the help says it does
    helpText :: Maybe String,
    -- | Its default, as the help shows it
    defaultShown :: Maybe String,
    -- | The subheading it is listed under among the options, when it has
    -- one: see 'optionGroup'
    subheading :: Maybe String
  }
  deriving (Eq, Show)

-- | A later description completes an earlier one: the names add up, and
-- the rest is the last given
instance Semigroup OptDoc where
  a <> b =
    OptDoc
      { names = a.names <> b.names,
        valueName = b.valueName <|> a.valueName,
        helpText = b.helpText <|> a.helpText,
        defaultShown = b.defaultShown <|> a.defaultShown,
        subheading = b.subheading <|> a.subheading
      }

instance Monoid OptDoc where
  mempty = OptDoc {names = [], valueName = Nothing, helpText = Nothing, defaultShown = Nothing, subheading = Nothing}

-- | A name of an option: -p, or --portfolio
data Name = Short Char | Long String
  deriving (Eq, Show)

-- | The name as it is typed
nameText :: Name -> String
nameText (Short c) = ['-', c]
nameText (Long s) = "--" <> s

{- Building an option, a flag or an argument -}

-- | What describes an option, a flag or an argument, gathered from the
-- modifiers with '<>': its names, its metavar, its help and its default,
-- and the same for optparse-applicative, with the shell completion. @f@
-- is the kind of field optparse-applicative gives it, which keeps a name
-- off an argument.
data Mod f a = Mod
  { doc :: OptDoc,
    defaultValue :: Maybe a,
    -- | How the default is shown in the help, when it is
    showsDefault :: Maybe (a -> String),
    -- | What optparse-applicative gets
    optparse :: O.Mod f a
  }

instance Semigroup (Mod f a) where
  a <> b =
    Mod
      { doc = a.doc <> b.doc,
        defaultValue = b.defaultValue <|> a.defaultValue,
        showsDefault = b.showsDefault <|> a.showsDefault,
        optparse = a.optparse <> b.optparse
      }

instance Monoid (Mod f a) where
  mempty = Mod {doc = mempty, defaultValue = Nothing, showsDefault = Nothing, optparse = mempty}

-- | An option that takes a value, read by the reader
option :: ReadM a -> Mod O.OptionFields a -> Args a
option rdr m = one m (isJust m.defaultValue) (O.option rdr m.optparse)

-- | An option that takes a value as it is typed, with no reader that could
-- refuse it
strOption :: (IsString s) => Mod O.OptionFields s -> Args s
strOption = option str

-- | A flag that is False unless typed
switch :: Mod O.FlagFields Bool -> Args Bool
switch = flag False True

-- | A flag with its value when left out, and its value when typed
flag :: a -> a -> Mod O.FlagFields a -> Args a
flag off on m = one m True (O.flag off on m.optparse)

-- | A flag with no value when left out: for one of several alternatives
flag' :: a -> Mod O.FlagFields a -> Args a
flag' on m = one m False (O.flag' on m.optparse)

-- | An argument, read by the reader
argument :: ReadM a -> Mod O.ArgumentFields a -> Args a
argument rdr m = one m (isJust m.defaultValue) (O.argument rdr m.optparse)

-- | An argument taken as it is typed, with no reader that could refuse it
strArgument :: (IsString s) => Mod O.ArgumentFields s -> Args s
strArgument = argument str

-- | Two arguments the usage line asks for, of which the parser lets the
-- second be left out, as in label ADDRESS LABEL. The command then refuses
-- the first word alone with a sentence that says what to type, where
-- optparse-applicative would only say that LABEL is missing. Both are
-- read as they are typed.
secondLeftToCommand :: Mod O.ArgumentFields String -> Mod O.ArgumentFields String -> Args (String, Maybe String)
secondLeftToCommand first second =
  Args
    { usage = Sequence (Item first.doc) (Item second.doc),
      rows = [first.doc, second.doc],
      parser = (,) <$> O.strArgument first.optparse <*> optional (O.strArgument second.optparse)
    }

-- | One option, flag or argument: its row of the help, its place in the
-- usage line, optional when it has a default, and its parser
one :: Mod f a -> Bool -> O.Parser a -> Args a
one m hasDefault parser = Args {usage = usage, rows = [doc], parser = parser}
  where
    doc = m.doc {defaultShown = m.showsDefault <*> m.defaultValue}
    usage
      | hasDefault = Optional (Item doc)
      | otherwise = Item doc

-- | The one-letter name of an option or a flag, typed after one dash
short :: (O.HasName f) => Char -> Mod f a
short c = mempty {doc = mempty {names = [Short c]}, optparse = O.short c}

-- | The long name of an option or a flag, typed after two dashes
long :: (O.HasName f) => String -> Mod f a
long s = mempty {doc = mempty {names = [Long s]}, optparse = O.long s}

-- | The name the help gives the value of an option, or an argument, such
-- as TXID
metavar :: (O.HasMetavar f) => String -> Mod f a
metavar v = mempty {doc = mempty {valueName = Just v}, optparse = O.metavar v}

-- | What the help says the option, flag or argument does. Only the help of
-- hw shows it, so optparse-applicative gets nothing of it.
help :: String -> Mod f a
help s = mempty {doc = mempty {helpText = Just s}}

-- | The value when the option is left out. 'showConfig' shows it in the
-- help.
value :: (O.HasValue f) => a -> Mod f a
value x = mempty {defaultValue = Just x, optparse = O.value x}

-- | Show the default of the option in the help, as 'show' writes it
showConfig :: (Show a) => Mod f a
showConfig = showConfigWith show

-- | Show the default of the option in the help, as the function writes
-- it: for a value whose 'show' is not what the user types
showConfigWith :: (a -> String) -> Mod f a
showConfigWith f = mempty {showsDefault = Just f}

-- | The values the shell completes the option with
completeWith :: (O.HasCompleter f) => [String] -> Mod f a
completeWith xs = mempty {optparse = O.completeWith xs}

-- | The action that gives the values the shell completes the option with,
-- for values that are known only when the user types, such as file names
completer :: (O.HasCompleter f) => O.Completer -> Mod f a
completer c = mempty {optparse = O.completer c}

{- Subheadings -}

-- | List the options under a subheading of the help, after the options
-- without one. An option under a subheading already stays under it.
optionGroup :: String -> Args a -> Args a
optionGroup title args = args {rows = map under args.rows}
  where
    under doc = doc {subheading = doc.subheading <|> Just title}

{- A command, and the program -}

-- | A command of the program: what its help says, and its options
data CommandSpec a = CommandSpec
  { name :: String,
    -- | Whether to run it on an offline computer: it asks for a mnemonic
    -- or private keys and needs no server. The help marks such a command
    -- with !. A command that needs the server cannot run there, so it is
    -- not marked even when it takes a secret. recover always takes one,
    -- and preparesweep does with --key-file. Their details warn of it.
    offline :: Bool,
    -- | What it does, in a line: shown in the list of commands and at the
    -- top of its help
    summary :: String,
    -- | What its help explains under the options, in paragraphs separated
    -- by blank lines. A paragraph whose lines all start with a space, such
    -- as a list or an example, is kept as written. The others are
    -- reflowed. Empty for none.
    details :: String,
    args :: Args a
  }

-- | The commands of one kind, listed together in the help
data CommandGroup a = CommandGroup
  { title :: String,
    commands :: [CommandSpec a]
  }

-- | The program: its commands by group, the options that go with any
-- command, and what its help says. A run gives the command and those
-- options.
data Program cmd opts = Program
  { name :: String,
    -- | What --version prints, in lines separated by newlines
    version :: String,
    -- | What stands for the command in the usage line
    commandUsage :: String,
    -- | What the program does, at the top of its help
    intro :: String,
    -- | The commands whose name is also an everyday word, such as coins.
    -- The help takes such a word for the command only in backticks:
    -- \`coins\`. It takes the name of another command for the command
    -- wherever it is.
    everydayNames :: [String],
    -- | The paragraphs under the list of commands, as the details of a
    -- command
    details :: String,
    -- | The options every command takes, before or after it
    globals :: Args opts,
    -- | The long name of the one of these options that turns the pager
    -- off, without its dashes: no-pager. A help is shown before the
    -- options are parsed, so the runner looks for this name itself.
    noPagerName :: String,
    groups :: [CommandGroup cmd]
  }
