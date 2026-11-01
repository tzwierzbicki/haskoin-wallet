-- | The shell completion of a program, around the one of
-- optparse-applicative: the scripts of bash and fish, the words of a
-- request as the shell typed them, and the completion of a file name.
--
-- The script of zsh is the one of optparse-applicative. The scripts of
-- bash and fish are hw's own. The ones of optparse-applicative added a /
-- to a value that is also the name of a directory, an account named src
-- for instance: bash's registered the program with -o filenames, and
-- fish's tested each value. The one of bash also expanded the words typed
-- as patterns, so that s* became the files of the directory. zsh adds a
-- / to such a value too, and takes it out when the command runs.
module Haskoin.Wallet.Cli.Parser.Completion
  ( bashScript,
    bashScriptName,
    fishScript,
    fishScriptName,
    completionParts,
    completionArgs,
    enrichedRequest,
    escapedForBash,
    completeFile,
  )
where

import Control.Monad (forM)
import Data.List (isPrefixOf, sort)
import qualified System.Directory as D
import Text.Read (readMaybe)
import UnliftIO.Exception (handleAny)

{- The script of bash -}

-- | The script that has bash complete the program, printed for
-- --bash-completion-script. At each Tab, it runs the program by the name
-- given, with the index of the word to complete and the words typed, and
-- offers the lines the program prints. The function is named after the
-- program, as optparse-applicative names it.
bashScript :: String -> String -> String
bashScript program name =
  unlines
    [ function <> "()",
      "{",
      "    local request=(--bash-completion-index \"$COMP_CWORD\")",
      "    local word",
      "    for word in \"${COMP_WORDS[@]}\"; do",
      "        request+=(--bash-completion-word \"$word\")",
      "    done",
      "    COMPREPLY=()",
      "    while IFS= read -r word; do",
      "        COMPREPLY+=(\"$word\")",
      "    done < <(" <> name <> " \"${request[@]}\")",
      "    # A directory alone gets no space after its /, so that the path",
      "    # typed goes on into it",
      "    if [[ ${#COMPREPLY[@]} -eq 1 && ${COMPREPLY[0]} == */ ]]; then",
      "        compopt -o nospace 2> /dev/null",
      "    fi",
      "}",
      "",
      "complete -F " <> function <> " " <> program
    ]
  where
    function = "_" <> program

-- | The name the arguments ask the script of bash to run the program by,
-- when they ask for the script
bashScriptName :: [String] -> Maybe String
bashScriptName = scriptNameAfter "--bash-completion-script"

{- The script of fish -}

-- | The script that has fish complete the program, printed for
-- --fish-completion-script. As the one of bash, it runs the program by
-- the name given at each Tab, with the index of the word to complete and
-- the words typed, and offers the lines the program prints. fish asks
-- for a description with each word, as it shows one after a tab.
fishScript :: String -> String -> String
fishScript program name =
  unlines
    [ "function " <> function,
      "    set -l words (commandline --tokenize --current-process)",
      "    # The words before the cursor, counted: the index of the one to complete",
      "    set -l index (count (commandline --tokenize --cut-at-cursor --current-process))",
      "    set -l request --bash-completion-enriched --bash-completion-index $index",
      "    for word in $words",
      "        set -a request --bash-completion-word $word",
      "    end",
      "    " <> name <> " $request",
      "end",
      "",
      "complete --no-files --command " <> program <> " --arguments '(" <> function <> ")'"
    ]
  where
    function = "_" <> program

-- | The name the arguments ask the script of fish to run the program by,
-- when they ask for the script
fishScriptName :: [String] -> Maybe String
fishScriptName = scriptNameAfter "--fish-completion-script"

-- | The name after the option that asks for a script, when the arguments
-- hold it
scriptNameAfter :: String -> [String] -> Maybe String
scriptNameAfter option (word : name : _) | word == option = Just name
scriptNameAfter option (_ : rest) = scriptNameAfter option rest
scriptNameAfter _ [] = Nothing

{- The words of a request -}

-- | The arguments of a shell completion, apart: the index of the word to
-- complete, the words typed, in order, and the other arguments.
--
-- The shell passes each word as typed, with the backslashes that escape
-- its characters: my\ savings, or TXID\: once bash has completed a coin
-- up to its colon. They are taken out, as the shell takes them out of
-- the command it runs.
--
-- bash breaks a word at its =, so that --account=main comes as --account,
-- = and main. The = is taken out: the value follows the option as a word
-- of its own, which optparse-applicative reads the same, and the index
-- moves with the words. An = that is the word to complete stands for an
-- empty value, since bash inserts what completes it after the =.
completionParts :: [String] -> (Maybe Int, [String], [String])
completionParts args = (index', words', others)
  where
    (indexM, typed, others) = partsOf args
    (words', index') = withoutEquals (readMaybe =<< indexM) (map unescaped typed)
    partsOf ("--bash-completion-index" : i : rest) = (Just i, ws, os)
      where
        (_, ws, os) = partsOf rest
    partsOf ("--bash-completion-word" : w : rest) = (i, w : ws, os)
      where
        (i, ws, os) = partsOf rest
    partsOf (a : rest) = (i, ws, a : os)
      where
        (i, ws, os) = partsOf rest
    partsOf [] = (Nothing, [], [])

-- | The words without the = bash broke a word at, after an option, with
-- the index of the word to complete moved along: see 'completionParts'
withoutEquals :: Maybe Int -> [String] -> ([String], Maybe Int)
withoutEquals index = go 0
  where
    go position (option : "=" : rest)
      | "--" `isPrefixOf` option =
          let (ws, indexLeft) = go (position + 2) rest
           in case index of
                -- The = is the word to complete: an empty value
                Just i | i == position + 1 -> (option : "" : ws, indexLeft)
                -- The = is before the word to complete, which moves up
                Just i | i > position + 1 -> (option : ws, subtract 1 <$> indexLeft)
                _ -> (option : ws, indexLeft)
    go position (w : rest) = let (ws, indexLeft) = go (position + 1) rest in (w : ws, indexLeft)
    go _ [] = ([], index)

-- | A word as the shell typed it, without the backslashes that escape its
-- characters: my\ savings is my savings
unescaped :: String -> String
unescaped ('\\' : c : rest) = c : unescaped rest
unescaped (c : rest) = c : unescaped rest
unescaped [] = []

-- | The arguments that ask to complete the word of the index among the
-- words typed
completionArgs :: Int -> [String] -> [String]
completionArgs index typed = ["--bash-completion-index", show index] <> concatMap (\w -> ["--bash-completion-word", w]) typed

-- | Whether the arguments of a shell completion ask for a description
-- with each word, as the scripts of zsh and fish do. The script of bash
-- does not.
enrichedRequest :: [String] -> Bool
enrichedRequest = elem "--bash-completion-enriched"

-- | A word as bash inserts it in the command line, which takes it as it
-- is: a backslash before each character bash would take for the end of
-- the word, a quote or a pattern. These are the characters bash escapes
-- in a file name. zsh and fish quote a word themselves.
escapedForBash :: String -> String
escapedForBash = concatMap escape
  where
    escape c
      | c `elem` special = ['\\', c]
      | otherwise = [c]
    special = " \t\\\"'@<>=;|&()#$`?*[!:{~" :: String

{- Files -}

-- | The files and directories whose path starts with the word typed, for
-- a FILENAME argument, as the shell lists them for a command of its own.
-- A hidden file is offered only to a word that starts with a dot, as the
-- shell does. A directory ends with a /. The script of bash puts no space
-- after it, so that the path goes on into it.
completeFile :: String -> IO [String]
completeFile typed = handleAny (const (return [])) $ do
  entries <- D.listDirectory (if null dir then "." else dir)
  let names = [name | name <- entries, start `isPrefixOf` name, hidden start || not (hidden name)]
  forM (sort names) $ \name -> do
    isDirectory <- D.doesDirectoryExist (dir <> name)
    return $ dir <> name <> (if isDirectory then "/" else "")
  where
    -- The word up to its last slash, and the start of a name after it
    (dir, start) = let (revStart, revDir) = break (== '/') (reverse typed) in (reverse revDir, reverse revStart)
    hidden = ("." `isPrefixOf`)
