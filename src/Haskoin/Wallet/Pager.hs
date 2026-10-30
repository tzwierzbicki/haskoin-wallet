{-# LANGUAGE ScopedTypeVariables #-}

-- | The pager a listing goes through on a terminal, as git shows a log:
-- less, unless HW_PAGER, the key pager of config.json or PAGER names
-- another one. less quits by itself when the listing fits on the screen,
-- shows the colours, and leaves the listing on the screen when it quits
-- (-F, -R and -X), unless LESS is set to something else. A listing goes
-- out as it is when stdout is not a terminal, with --no-pager, and when
-- the pager named is empty or cat.
module Haskoin.Wallet.Pager
  ( pagerCommand,
    pagerNamed,
    withPager,
    writeDoc,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (IOException, catch, finally, throwIO, try)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import GHC.IO.Exception (IOErrorType (ResourceVanished))
import Haskoin.Wallet.Config (Config (configPager))
import Haskoin.Wallet.Style (Output, Style, outputOf, putDoc)
import Prettyprinter (Doc)
import System.Directory (findExecutable)
import System.Environment (getEnvironment, lookupEnv)
import System.IO (Handle, hClose, hFlush, hIsTerminalDevice, hPutStrLn, stderr, stdout)
import System.IO.Error (ioeGetErrorType)
import System.Posix.Signals (Handler (Ignore), installHandler, sigINT)
import System.Process (CreateProcess (env, std_in), StdStream (CreatePipe), createProcess, shell, waitForProcess)

-- | The pager to pipe a listing through, or none: with --no-pager, when
-- stdout is not a terminal, or when the pager named is none
pagerCommand :: Bool -> Config -> IO (Maybe String)
pagerCommand noPager cfg = do
  terminal <- hIsTerminalDevice stdout
  hwPager <- lookupEnv "HW_PAGER"
  pager <- lookupEnv "PAGER"
  return $
    if noPager || not terminal
      then Nothing
      else pagerNamed hwPager (configPager cfg) pager

-- | The pager named by HW_PAGER, the key pager of config.json or PAGER,
-- the first of them set, or less when none is. None when the name is
-- empty or cat.
pagerNamed :: Maybe String -> Maybe Text -> Maybe String -> Maybe String
pagerNamed hwPager fromConfig pager =
  case words named of
    [] -> Nothing
    ["cat"] -> Nothing
    _ -> Just named
  where
    named = fromMaybe "less" (hwPager <|> (cs <$> fromConfig) <|> pager)

-- | Run the action with the handle to write the listing on, and whether
-- to style it, as the terminal takes it: the input of the pager, or
-- stdout when there is none. The pager is waited for after the action,
-- as long as the user reads. Ctrl-C goes to the pager, which handles it;
-- hw ignores it meanwhile. A pager that is not installed is reported on
-- stderr, and the listing is printed without one.
withPager :: Maybe String -> (Handle -> Output -> IO a) -> IO a
withPager pagerM action = do
  output <- outputOf stdout
  case pagerM of
    Nothing -> action stdout output
    Just command -> do
      installed <- case words command of
        program : _ -> findExecutable program
        [] -> return Nothing
      case installed of
        Nothing -> do
          hPutStrLn stderr $ "The pager " <> command <> " was not found. The listing is printed without one."
          action stdout output
        Just _ -> pipedThrough command (`action` output)

-- | Run the action on the input of the pager command, which the shell
-- runs, and wait for the pager
pipedThrough :: String -> (Handle -> IO a) -> IO a
pipedThrough command action = do
  environment <- getEnvironment
  -- The options git gives less: quit when the listing fits on the screen
  -- (-F), show the colours (-R), leave the listing on the screen (-X)
  let withLess
        | any ((== "LESS") . fst) environment = environment
        | otherwise = ("LESS", "FRX") : environment
  (inputM, _, _, pager) <- createProcess (shell command) {std_in = CreatePipe, env = Just withLess}
  input <- maybe (ioError (userError "The pager has no input")) return inputM
  previous <- installHandler sigINT Ignore Nothing
  action input `finally` do
    -- The pager may be gone already, with its input closed
    hClose input `catch` \(_ :: IOException) -> return ()
    _ <- waitForProcess pager
    installHandler sigINT previous Nothing

-- | Write the document on the handle, styled or plain, and send it on to
-- the reader. False once the reader is gone: the pager when the user quits
-- it before the end of the listing, or the command the listing is piped
-- to when it stops reading. There is nothing left to write for.
writeDoc :: Handle -> Output -> Doc Style -> IO Bool
writeDoc h output doc = do
  written <- try (putDoc h output doc >> hFlush h) :: IO (Either IOException ())
  case written of
    Right () -> return True
    Left e
      | ioeGetErrorType e == ResourceVanished -> return False
      | otherwise -> throwIO e
