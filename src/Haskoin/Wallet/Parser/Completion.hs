-- | The shell completion of a program, around the one of
-- optparse-applicative: the script of bash, the words of a request as the
-- shell typed them, and the completion of a file name.
--
-- The scripts of zsh and fish are those of optparse-applicative. The
-- script of bash is hw's own. The one of optparse-applicative expands the
-- words typed as patterns, so that s* became the files of the directory,
-- and registers the program with -o filenames, which adds a / to a value
-- that is also the name of a directory, an account named src for
-- instance.
module Haskoin.Wallet.Parser.Completion
  ( bashScript,
    bashScriptName,
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
import System.Environment (getArgs)
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
bashScriptName ("--bash-completion-script" : name : _) = Just name
bashScriptName (_ : rest) = bashScriptName rest
bashScriptName [] = Nothing

{- The words of a request -}

-- | The arguments of a shell completion, apart: the index of the word to
-- complete, the words typed, in order, and the other arguments.
--
-- The shell passes each word as typed, with the backslashes that escape
-- its characters: my\ savings, or TXID\: once bash has completed a coin
-- up to its colon. They are taken out, as the shell takes them out of
-- the command it runs.
completionParts :: [String] -> (Maybe String, [String], [String])
completionParts ("--bash-completion-index" : i : rest) = (Just i, ws, os)
  where
    (_, ws, os) = completionParts rest
completionParts ("--bash-completion-word" : w : rest) = (i, unescaped w : ws, os)
  where
    (i, ws, os) = completionParts rest
completionParts (a : rest) = (i, ws, a : os)
  where
    (i, ws, os) = completionParts rest
completionParts [] = (Nothing, [], [])

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
-- shell does. For bash, a directory ends with a /, and the script of hw
-- puts no space after it, so that the path goes on into it. zsh and fish
-- mark a directory themselves.
completeFile :: String -> IO [String]
completeFile typed = handleAny (const (return [])) $ do
  enriched <- enrichedRequest <$> getArgs
  entries <- D.listDirectory (if null dir then "." else dir)
  let names = [name | name <- entries, start `isPrefixOf` name, hidden start || not (hidden name)]
  forM (sort names) $ \name -> do
    isDirectory <- D.doesDirectoryExist (dir <> name)
    return $ dir <> name <> (if isDirectory && not enriched then "/" else "")
  where
    -- The word up to its last slash, and the start of a name after it
    (dir, start) = let (revStart, revDir) = break (== '/') (reverse typed) in (reverse revDir, reverse revStart)
    hidden = ("." `isPrefixOf`)
