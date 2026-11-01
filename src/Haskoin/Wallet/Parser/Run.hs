{-# LANGUAGE OverloadedRecordDot #-}

-- | Running the command line of a program: show its help when asked, or
-- have optparse-applicative parse the arguments, and report what is wrong
-- with them. The help is shown before anything is parsed. -h or --help
-- anywhere before a -- shows the help of the command named first, or of
-- the program when none is. --version before a -- shows the version of
-- the program, when no command is named.
module Haskoin.Wallet.Parser.Run
  ( runProgram,
    Request (..),
    requestOf,
    commandWord,
    commandNamed,
  )
where

import Control.Applicative (empty, (<|>))
import Control.Monad (guard)
import Data.List (find, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Haskoin.Wallet.Parser.Args
import Haskoin.Wallet.Parser.Help
import Haskoin.Wallet.Style (Output, Style, outputOf, putDoc)
import qualified Options.Applicative as O
import Prettyprinter (Doc, pretty)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (Handle, stderr, stdout)
import Text.Read (readMaybe)

-- | Parse the command line of the program: the command, and the options
-- that go with any command. A request for help or for the version, a
-- command typed alone that needs arguments, and a command line that
-- cannot be parsed print what they have to, and leave.
runProgram :: Program cmd opts -> IO (cmd, opts)
runProgram program = do
  args <- getArgs
  case requestOf (programValueOptions program) args of
    -- As optparse-applicative did: the help, as a failure
    NothingTyped -> leave stderr (ExitFailure 1) (`programHelp` program)
    HelpOf word -> leave stdout ExitSuccess (helpOf word)
    Version -> leave stdout ExitSuccess (const (pretty program.version))
    Parse -> parse args
  where
    helpOf word output = case word >>= commandNamed program of
      Just c -> commandHelp program c
      Nothing -> programHelp output program

    parse args =
      case O.execParserPure parserPrefs (programInfo program) parsedArgs of
        O.Success result -> return result
        -- The shell asked what completes a word: optparse-applicative
        -- answers, and hw leaves
        O.CompletionInvoked completion -> do
          offered <- O.execCompletion completion =<< getProgName
          putStr (withValueOption offered)
          exitSuccess
        O.Failure failure -> do
          let (parserHelp, code, _) = O.execFailure failure program.name
              command = commandWord (programValueOptions program) args >>= commandNamed program
          case command of
            -- As optparse-applicative did: a command typed alone that
            -- needs arguments gets its help, as a failure
            Just c | typedAlone c args -> leave stderr code (const (commandHelp program c))
            _ -> leave stderr code (const (usageError parserHelp program command))
      where
        (parsedArgs, valueOption) = splitAttachedValue program args
        -- Each word offered for a value typed against its short option
        -- replaces the option and the value both
        withValueOption offered
          | null valueOption = offered
          | otherwise = unlines (map (valueOption <>) (lines offered))

    typedAlone :: CommandSpec cmd -> [String] -> Bool
    typedAlone c args = dropWhile (/= c.name) args == [c.name]

-- | Print the document on the handle, styled or plain as the handle
-- takes it, and leave with the code. The document may differ between the
-- two, as the list of commands does.
leave :: Handle -> ExitCode -> (Output -> Doc Style) -> IO a
leave h code doc = do
  output <- outputOf h
  putDoc h output (doc output)
  exitWith code

-- | What the arguments ask for, before they are parsed
data Request
  = -- | The help of the program, for a command line without arguments
    NothingTyped
  | -- | The help of the command named, or of the program when none is
    HelpOf (Maybe String)
  | -- | The version of the program
    Version
  | -- | A run: the arguments are parsed
    Parse
  deriving (Eq, Show)

-- | What the arguments ask for. -h or --help before a -- asks for help.
-- --version before a -- asks for the version, when no command is named.
-- It is an option of the program, and no command takes it. The first
-- argument takes the value of the options that take one, to tell the
-- command named from such a value.
requestOf :: [String] -> [String] -> Request
requestOf valueOptions args
  | null args = NothingTyped
  -- The shell completes the words typed so far, --help among them maybe
  | any isCompletion args = Parse
  | typed ["-h", "--help"] = HelpOf command
  | typed ["--version"] && isNothing command = Version
  | otherwise = Parse
  where
    typed names = any (`elem` names) (takeWhile (/= "--") args)
    command = commandWord valueOptions args

-- | Whether the argument is one the completion script of
-- optparse-applicative passes
isCompletion :: String -> Bool
isCompletion arg = "--bash-completion-" `isPrefixOf` arg || "-completion-script" `isSuffixOf` arg

-- | The arguments of a shell completion, with a value typed against its
-- short option made a word of its own: -amai becomes -a and mai.
-- optparse-applicative completes a value only as a word of its own. Also
-- the option, to put back before each word offered: -a, or nothing when no
-- value was split. The arguments of anything else are left as they are.
splitAttachedValue :: Program cmd opts -> [String] -> ([String], String)
splitAttachedValue program args = fromMaybe (args, "") $ do
  index <- readMaybe =<< indexM
  '-' : letter : typedValue@(_ : _) <- listToMaybe (drop index typed)
  -- The command named before the word, without the name of the program
  let command = commandWord (programValueOptions program) (drop 1 (take index typed)) >>= commandNamed program
  guard (letter `elem` valueLetters program command)
  let split = take index typed <> [['-', letter], typedValue] <> drop (index + 1) typed
  return (others <> completionArgs (index + 1) split, ['-', letter])
  where
    (indexM, typed, others) = completionParts args

-- | The arguments of a shell completion, apart: the index of the word to
-- complete, the words typed, in order, and the other arguments
completionParts :: [String] -> (Maybe String, [String], [String])
completionParts ("--bash-completion-index" : i : rest) = (Just i, ws, os)
  where
    (_, ws, os) = completionParts rest
completionParts ("--bash-completion-word" : w : rest) = (i, w : ws, os)
  where
    (i, ws, os) = completionParts rest
completionParts (a : rest) = (i, ws, a : os)
  where
    (i, ws, os) = completionParts rest
completionParts [] = (Nothing, [], [])

-- | The arguments that ask to complete the word of the index among the
-- words typed
completionArgs :: Int -> [String] -> [String]
completionArgs index typed = ["--bash-completion-index", show index] <> concatMap (\w -> ["--bash-completion-word", w]) typed

-- | The short names of the options that take a value: those of the
-- options of the program, and those of the command, when one is named
valueLetters :: Program cmd opts -> Maybe (CommandSpec cmd) -> [Char]
valueLetters program command = [c | row <- rows, Just _ <- [row.metavar], Short c <- row.names]
  where
    rows = program.globals.rows <> maybe [] (.args.rows) command

-- | The command named by the arguments: the first word before -- that is
-- neither an option nor the value of one of the options that take one
commandWord :: [String] -> [String] -> Maybe String
commandWord valueOptions = go . takeWhile (/= "--")
  where
    go (opt : _ : rest) | opt `elem` valueOptions = go rest
    go (word : rest)
      | "-" `isPrefixOf` word = go rest
      | otherwise = Just word
    go [] = Nothing

-- | The command of the program with the name
commandNamed :: Program cmd opts -> String -> Maybe (CommandSpec cmd)
commandNamed program word = find ((== word) . (.name)) (concatMap (.commands) program.groups)

-- | The options of the program that take a value: the word after them
programValueOptions :: Program cmd opts -> [String]
programValueOptions program = [nameText n | row <- program.globals.rows, Just _ <- [row.metavar], n <- row.names]

-- | How optparse-applicative parses. After the command, it reads the
-- options of the command and the options that go with any command
-- together. The shell then completes both kinds there. A command that
-- misses an argument also says so when such an option follows it.
parserPrefs :: O.ParserPrefs
parserPrefs = O.prefs O.subparserInline

-- | The parser optparse-applicative runs: the command, by its name, and
-- the options that go with any command, before or after it.
--
-- It also knows -h, --help and --version, only so that the shell
-- completes them. 'requestOf' finds them before anything is parsed. They
-- are hidden, so that no error of the parser names them.
programInfo :: Program cmd opts -> O.ParserInfo (cmd, opts)
programInfo program = O.info (run <|> versionAlone) mempty
  where
    run = (,) <$> commands <*> program.globals.parser <* helpFlag
    commands =
      O.subparser $
        mconcat [O.command c.name (O.info c.args.parser mempty) | group <- program.groups, c <- group.commands]
          <> O.metavar program.commandUsage
    -- Never read: -h and --help before -- are found first, and after --
    -- they are arguments
    helpFlag = O.switch (O.short 'h' <> O.long "help" <> O.hidden)
    -- An alternative to the command and the other options. The parser
    -- drops it once one of them is typed, so the shell completes
    -- --version only right after hw. The parser reads it only before a
    -- command, and then fails.
    versionAlone = O.flag' () (O.long "version" <> O.hidden) *> empty
