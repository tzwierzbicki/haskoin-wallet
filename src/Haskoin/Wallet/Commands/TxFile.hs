{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Transaction files, as the commands read and write them. A command reads
-- the file of --input, a PSBT in bytes or in base64 text, or a transaction
-- alone in hexadecimal ('readTxFileIn'), finds the network it is on
-- ('fileNetwork'), and writes a PSBT with the keys and the paths of the
-- accounts of its portfolio ('writeTxFile'). "Haskoin.Wallet.Tx.PsbtFile"
-- says what such a file holds.
--
-- A command given a hash in place of a file works on the pending
-- transaction of the wallet as on a file ('heldTxRead'), so that signing
-- and sending have one way to go on from either.
module Haskoin.Wallet.Commands.TxFile
  ( notInWallet,
    poolFileAccounts,
    TxFileRead (..),
    heldTxRead,
    readTxFileIn,
    TxFileForm (..),
    txFileForm,
    txFileOfBytes,
    fileNetwork,
    writeTxFile,
    writeOutput,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Char (isHexDigit)
import Data.Foldable (for_)
import Data.List (nub)
import Data.String.Conversions (cs)
import Haskoin
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Tx.PsbtFile (FileAccount (..), psbtTxFile, txFilePsbt)
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Signing (labelledTxFile)
import Haskoin.Wallet.Util

-- | The refusal of a transaction that no portfolio of the wallet can keep
-- or sign. An offline computer that made one portfolio of a mnemonic gets
-- this for a file of another portfolio of it, so the refusal says how to
-- add the portfolio.
notInWallet :: String
notInWallet =
  "The transaction does not involve this wallet. It may be of a portfolio this database does not have yet: \
  \newportfolio --existing adds the next portfolio of a mnemonic, and importwallet the portfolios of a file"

-- | The keys of an account, as the derivation records of its files name
-- them: with its path in its portfolio, and the fingerprint of its
-- wallet
fileAccountOf :: Portfolio -> DBAccount -> FileAccount
fileAccountOf portfolio acc =
  FileAccount (accountXPubKey acc) (accountKind acc) (accountPath portfolio acc) (portfolioFingerprint portfolio) (accountNetwork acc)

-- | The keys of the accounts of a pool, as its files name them
poolFileAccounts :: Pool -> ExceptT String (DB IO) [FileAccount]
poolFileAccounts pool = do
  portfolio <- getPortfolio pool.poolPortfolio
  map (fileAccountOf portfolio . snd) <$> lift (poolAccounts pool)

-- | A transaction file as read: the transaction file, and the PSBT it was
-- read from when it was one, for the signed transaction to be written
-- into it
data TxFileRead = TxFileRead !TxFile !(Maybe Psbt)

-- | A pending transaction of the wallet, read as a file that names no
-- network and labels no address, and that comes from no PSBT: what a
-- command given a hash works on, in place of the file of a command given
-- --input
heldTxRead :: PendingTx -> TxFileRead
heldTxRead held = TxFileRead (plainTxFile Nothing held.pendingTxData) Nothing

-- | The transaction file at the path. A PSBT, in bytes or in base64 text,
-- is read with its paths relative to the portfolio of the wallet its
-- derivation records name (see "Haskoin.Wallet.Tx.PsbtFile"). A
-- transaction alone, in hexadecimal, as Bitcoin Core's finalizepsbt gives
-- it, is read with the signing data the wallet holds for it, which it
-- must. Anything else is refused.
--
-- The network given is the one of the command, from --network or from
-- the configuration. It says how the signatures another signer left in a
-- PSBT are verified when neither the file nor a portfolio of the wallet
-- names a network ('psbtTxFile').
readTxFileIn :: Network -> FilePath -> ExceptT String (DB IO) TxFileRead
readTxFileIn fallback file = do
  bytes <- liftEitherIO $ readFileBytes file
  txFileOfBytes fallback file bytes

-- | The forms a transaction file takes, as its first bytes tell them
data TxFileForm
  = -- | A PSBT in bytes, as BIP 174 writes it
    PsbtBytes
  | -- | A PSBT in base64 text
    PsbtText
  | -- | A transaction alone, in hexadecimal
    HexTransaction

-- | The form of a transaction file the bytes are in, if any. The spaces
-- and line breaks around a text do not count. A file of a wallet is in
-- none of these forms: it is JSON, which is not all hexadecimal digits.
txFileForm :: BS.ByteString -> Maybe TxFileForm
txFileForm bytes
  | isPsbt bytes = Just PsbtBytes
  | base64Magic `BS.isPrefixOf` text = Just PsbtText
  | isHexText = Just HexTransaction
  | otherwise = Nothing
  where
    text = C8.strip bytes
    -- "psbt" and 0xff in base64: what the text of any PSBT starts with
    base64Magic = "cHNidP"
    isHexText = not (BS.null text) && even (BS.length text) && C8.all isHexDigit text

-- | The transaction file of the bytes of a file, read as 'readTxFileIn'
-- reads it
txFileOfBytes :: Network -> FilePath -> BS.ByteString -> ExceptT String (DB IO) TxFileRead
txFileOfBytes fallback file bytes =
  case txFileForm bytes of
    Just PsbtBytes -> fromPsbt =<< refused (decodePsbt bytes)
    Just PsbtText -> fromPsbt =<< refused (decodePsbtBase64 stripped)
    Just HexTransaction -> fromRawTx =<< refused (rawTransaction stripped)
    Nothing -> throwError (couldNotRead file "Not a PSBT, in bytes or in base64 text, nor the hexadecimal of a transaction")
  where
    -- The text without the spaces and line breaks around it
    stripped = C8.strip bytes
    refused :: Either String a -> ExceptT String (DB IO) a
    refused = liftEither . first (couldNotRead file)
    fromPsbt psbt = do
      -- The accounts of each pool together: a payment may spend the
      -- coins of both
      pools <- lift allAccountPools
      txFile <- refused $ psbtTxFile (Just fallback) pools psbt
      return $ TxFileRead txFile (Just psbt)
    allAccountPools = do
      accounts <- getAccounts
      let poolsHeld = nub [accountPool acc | (_, _, acc) <- accounts]
      return [[fileAccountOf portfolio acc | (portfolio, _, acc) <- accounts, accountPool acc == pool] | pool <- poolsHeld]
    -- The transaction a hexadecimal text holds, which must be the whole
    -- text: haskoin-core's decodeTx refuses bytes after the transaction,
    -- as Bitcoin Core does. The words of the parser name its internals,
    -- so the refusal is hw's own.
    rawTransaction :: BS.ByteString -> Either String Tx
    rawTransaction text =
      maybe (Left notOneTransaction) (first (const notOneTransaction) . decodeTx) (decodeHex (cs text))
    notOneTransaction = "The hexadecimal text is not one whole transaction"
    fromRawTx tx = do
      heldM <- lookupPendingTx (nosigHash tx)
      held <- liftMaybe "The file holds a transaction alone, which the wallet does not hold as pending: hw needs the whole transaction each of its coins comes from, which a PSBT carries" heldM
      let tsd = held.pendingTxData
      return $ TxFileRead (plainTxFile Nothing tsd {txSignDataTx = tx}) Nothing

-- | The network a file is on, when something names it: the file itself,
-- or --network, which may only repeat what the file names
fileNetwork :: Maybe Network -> Maybe Network -> ExceptT String (DB IO) (Maybe Network)
fileNetwork givenM fileM = case (givenM, fileM) of
  (Just given, Just named)
    | given /= named -> throwError $ "The file is on " <> named.name <> ", not on the --network " <> given.name
  _ -> return (fileM <|> givenM)

-- | Write a transaction file as a PSBT, with the keys of the accounts of
-- its pool when it has one, named by the fingerprint of the mnemonic and
-- their paths, at a path that must be free
writeTxFile :: Maybe Pool -> FilePath -> TxFile -> ExceptT String (DB IO) ()
writeTxFile poolM file txFile = do
  accounts <- maybe (return []) poolFileAccounts poolM
  psbt <- liftEither $ txFilePsbt accounts txFile
  liftEitherIO $ writeFileBytes file (encodePsbt psbt)

-- | Write a transaction of the network to the --output file, when there is
-- one, with the labels of its addresses as its summary has them, and the
-- keys of its pool when it has one
writeOutput :: Maybe Pool -> Network -> Maybe FilePath -> TxInfo -> TxSignData -> ExceptT String (DB IO) ()
writeOutput poolM net output info tsd = for_ output $ \file -> writeTxFile poolM file (labelledTxFile net info tsd)
