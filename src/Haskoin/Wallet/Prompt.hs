{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What hw asks the user for on the terminal, typed without echo: the
-- mnemonic of a wallet, whole or in parts, and its passphrase, which give
-- the signing key of an account; and the private keys of a sweep, when
-- they are not in a file.
module Haskoin.Wallet.Prompt where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Haskoin
import Haskoin.Wallet.Entropy (mergeMnemonicParts)
import Haskoin.Wallet.Keys
import Numeric.Natural (Natural)
import qualified System.Console.Haskeline as Haskeline
import qualified System.Directory as D
import qualified System.IO as IO

{- The terminal -}

askInputLineHidden :: String -> IO String
askInputLineHidden message = do
  inputM <-
    Haskeline.runInputT Haskeline.defaultSettings $
      Haskeline.getPassword (Just '*') message
  maybe
    (error "No action due to EOF")
    return
    inputM

askInputLine :: String -> IO String
askInputLine message = do
  inputM <-
    Haskeline.runInputT Haskeline.defaultSettings $
      Haskeline.getInputLine message
  maybe
    (error "No action due to EOF")
    return
    inputM

{- The mnemonic and the passphrase -}

askMnemonicWords :: String -> IO Mnemonic
askMnemonicWords txt = do
  mnm <- askInputLineHidden txt
  case fromMnemonic (cs mnm) of -- validate the mnemonic
    Right _ -> return $ cs mnm
    Left _ -> do
      liftIO $ putStrLn "Invalid mnemonic"
      askMnemonicWords txt

askMnemonic :: (MonadError String m, MonadIO m) => Natural -> m MnemonicPass
askMnemonic splitMnemIn = do
  mnm <-
    if splitMnemIn == 1
      then liftIO $ askMnemonicWords "Enter your mnemonic words: "
      else do
        ms <- forM [1 .. splitMnemIn] $ \n ->
          liftIO $ askMnemonicWords $ "Split mnemonic part #" <> show n <> ": "
        liftEither $ mergeMnemonicParts ms
  return
    MnemonicPass
      { mnemonicWords = mnm,
        mnemonicPass = ""
      }

askMnemonicPass :: (MonadError String m, MonadIO m) => Natural -> m MnemonicPass
askMnemonicPass splitMnemIn = do
  mnem <- askMnemonic splitMnemIn
  passStr <- liftIO askPassword
  return mnem {mnemonicPass = cs passStr}

askPassword :: IO String
askPassword = do
  pass <- askPassword1
  if null pass
    then return pass
    else do
      pass2 <- askInputLineHidden "Repeat your mnemonic passphrase   : "
      if pass == pass2
        then return pass
        else do
          putStrLn "The passphrases did not match"
          askPassword

askPassword1 :: IO String
askPassword1 = askInputLineHidden "Mnemonic passphrase or leave empty: "

-- | The signing key of an account from its mnemonic, with the public side
-- of its seed, asking for the passphrase until the key derived matches the
-- public key of the account. The one place a private key comes out of the
-- mnemonic for a command.
askSigningKey ::
  (MonadIO m) =>
  Network ->
  Ctx ->
  Natural ->
  MnemonicPass ->
  XPubKey ->
  ExceptT String m (PublicSeed, XPrvKey)
askSigningKey net ctx idx mnem@(MnemonicPass m _) accPub = do
  p <- liftIO askPassword1
  master <- liftEither $ masterKey (MnemonicPass m $ cs p)
  let seed = seedOfMaster ctx master
  if seedAccountKey seed net idx == accPub
    then return (seed, accountKey net ctx master idx)
    else do
      liftIO $ putStrLn "The passphrase or the mnemonic did not match the account "
      askSigningKey net ctx idx mnem accPub

{- The private keys of a sweep -}

-- | The private keys of a sweep, from where the user keeps them: a file,
-- or typed on the terminal without echo
readSecKeys :: (MonadIO m) => Network -> KeySource -> ExceptT String m [SecKey]
readSecKeys net (KeysInFile file) = do
  exists <- liftIO $ D.doesFileExist file
  unless exists $ throwError "The private key file does not exist"
  parseKeys net . cs =<< liftIO (IO.readFile file)
readSecKeys net KeysOnTerminal = do
  text <- liftIO $ askInputLineHidden "Private keys (WIF, minikey or hex, separated by spaces): "
  parseKeys net (cs text)

parseKeys :: (Monad m) => Network -> Text -> ExceptT String m [SecKey]
parseKeys net text = do
  keys <- liftEither $ parseSecKeys net text
  when (null keys) $ throwError "No private keys were given"
  return keys
