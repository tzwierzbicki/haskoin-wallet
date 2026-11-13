-- | The shell completion of a program, around the one of
-- optparse-applicative: the scripts of bash and fish, the words of a
-- request as the shell typed them, and the completion of a file name.
--
-- The script of zsh is the one of optparse-applicative. The scripts of
-- bash and fish are hw's own. The ones of optparse-applicative added a /
-- to a value that is also the name of a directory, the network btc in a
-- directory that holds a directory btc for instance: bash's registered the program with -o filenames, and
-- fish's tested each value. The one of bash also expanded the words typed
-- as patterns, so that s* became the files of the directory. zsh adds a
-- / to such a value too, and takes it out when the command runs.
module Haskoin.Wallet.Cli.Parser.Completion
  ( bashScript,
    bashScriptName,
    fishScript,
    fishScriptName,
    completionParts,
    keptByBash,
    completionArgs,
    enrichedRequest,
    escapedForBash,
    completeFile,
  )
where

import Control.Monad (forM)
import Data.List (isPrefixOf, sort, stripPrefix)
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
-- its characters: my\ wallet.ods, or TXID\: once bash has completed a coin
-- up to its colon. They are taken out, as the shell takes them out of
-- the command it runs.
--
-- bash breaks a word at a colon typed as it is, so that TXID:0 comes as
-- TXID, : and 0. The pieces are joined again, and the index moves with
-- the words.
--
-- bash breaks a word at its =, so that --network=btc comes as --network,
-- = and btc. The = is taken out: the value follows the option as a word
-- of its own, which optparse-applicative reads the same, and the index
-- moves with the words. An = that is the word to complete stands for an
-- empty value, since bash inserts what completes it after the =.
completionParts :: [String] -> (Maybe Int, [String], [String])
completionParts args = (index', words', others)
  where
    (_, _, others) = requestParts args
    (joined, joinedIndex, _) = bashWords args
    (words', index') = withoutEquals joinedIndex (map unescaped joined)

-- | What bash keeps of the word to complete: the part up to the last
-- colon it broke the word at, which it leaves as it is. It replaces only
-- what follows. So a value offered for TXID: must leave out the TXID:
-- the word starts with, which hw matched it on. It is empty for a word
-- bash did not break, and for zsh and fish.
keptByBash :: [String] -> String
keptByBash args = unescaped kept
  where
    (_, _, kept) = bashWords args

-- | The words of a request as the shell typed them, with the index of the
-- word to complete and what bash keeps of it. The words bash broke at a
-- colon are joined again. zsh and fish do not break a word at a colon,
-- and their words are as they come.
bashWords :: [String] -> ([String], Maybe Int, String)
bashWords args
  | enrichedRequest args = (typed, index, "")
  | otherwise = joinedAtColons index typed
  where
    (indexM, typed, _) = requestParts args
    index = readMaybe =<< indexM

-- | The arguments of a request, apart: the index of the word to complete,
-- as typed, the words, and the other arguments
requestParts :: [String] -> (Maybe String, [String], [String])
requestParts ("--bash-completion-index" : i : rest) = (Just i, ws, os)
  where
    (_, ws, os) = requestParts rest
requestParts ("--bash-completion-word" : w : rest) = (i, w : ws, os)
  where
    (i, ws, os) = requestParts rest
requestParts (a : rest) = (i, ws, a : os)
  where
    (i, ws, os) = requestParts rest
requestParts [] = (Nothing, [], [])

-- | The words with those bash broke at a colon joined again, the index of
-- the word to complete moved along, and what bash keeps of that word
-- ('keptByBash').
--
-- bash breaks a word at a colon typed as it is: TXID:0 comes as TXID, :
-- and 0, and TXID: as TXID and :, with the colon as the word to complete.
-- Joined, the word is matched whole, as zsh and fish pass it. A colon
-- bash escaped, as in TXID\:, breaks nothing. hw takes no word that is a
-- colon alone, so each colon among the words is one bash broke a word at.
-- The script could send the whole line, to tell a colon typed alone from
-- the others. That was turned down: it adds a loop over the line to the
-- script, for a case no command has. Only the words up to the one to
-- complete are joined. Those after it play no part in its completion.
joinedAtColons :: Maybe Int -> [String] -> ([String], Maybe Int, String)
joinedAtColons indexM typed = case indexM of
  Just index
    | index >= 0 && index < length typed ->
        let (upToCursor, after) = splitAt (index + 1) typed
            groups = piecesOf upToCursor
         in (map concat groups <> after, Just (length groups - 1), keptOf (last groups))
  _ -> (typed, indexM, "")
  where
    -- The pieces of each word, in order: a colon and the piece after it
    -- go with the word before them
    piecesOf = reverse . map reverse . foldl glue []
    glue (current@(previous : _) : done) piece
      | piece == ":" || previous == ":" = (piece : current) : done
    glue done piece = [piece] : done
    -- bash keeps the pieces before the one under the cursor, and the
    -- colon itself when the cursor is on it
    keptOf pieces = case reverse pieces of
      ":" : _ -> concat pieces
      _ : before -> concat (reverse before)
      [] -> ""

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
--
-- A ~/ at the start of the word stays as it is. It is a path from the
-- home directory, as the user typed it ('completeFile'), and bash expands
-- it when the command runs. Escaped, it would name a directory ~ in the
-- working directory instead. No other value hw offers starts with ~/.
escapedForBash :: String -> String
escapedForBash word = case stripPrefix "~/" word of
  Just rest -> "~/" <> concatMap escape rest
  Nothing -> concatMap escape word
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
--
-- A word that starts with ~/ is a path from the home directory, which the
-- shell expands when the command runs ('inHome'). The shell passes the
-- word as typed, so hw expands the ~ itself to list the directory. Each
-- path is offered with the ~/ the user typed, as the shell offers it for
-- a command of its own.
completeFile :: String -> IO [String]
completeFile typed = handleAny (const (return [])) $ do
  listed <- inHome dir
  entries <- D.listDirectory (if null listed then "." else listed)
  let names = [name | name <- entries, start `isPrefixOf` name, hidden start || not (hidden name)]
  forM (sort names) $ \name -> do
    isDirectory <- D.doesDirectoryExist (listed <> name)
    return $ dir <> name <> (if isDirectory then "/" else "")
  where
    -- The word up to its last slash, and the start of a name after it
    (dir, start) = let (revStart, revDir) = break (== '/') (reverse typed) in (reverse revDir, reverse revStart)
    hidden = ("." `isPrefixOf`)

-- | The path with a ~/ at its start read as the home directory, as the
-- shell reads it: ~/docs/ is /home/alice/docs/ for instance. The home is
-- the one of HOME, as bash expands it. Any other path is left as it is.
-- The ~ of a user named after it, as in ~alice/, is left too. The shell
-- reads it as the home of that user. hw does not look that user up, and
-- offers nothing for it.
inHome :: FilePath -> IO FilePath
inHome ('~' : '/' : rest) = (\home -> home <> "/" <> rest) <$> D.getHomeDirectory
inHome path = return path
