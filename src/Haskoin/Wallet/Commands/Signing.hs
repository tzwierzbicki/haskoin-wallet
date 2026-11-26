{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Signing a transaction: signtx, and what signsweep shares with it. The
-- transaction is a pending one of the wallet, named by its hash, or the
-- one of a file ('transactionToSign'). It is shown before the secret is
-- asked for ('showToSign'), so that the user knows what the signature is
-- for. Once signed, it is kept in place of the pending one, and written
-- to its file when there is one ('keepSigned').
module Haskoin.Wallet.Commands.Signing
  ( SignedFile (..),
    transactionToSign,
    signedFileOf,
    refuseForPrivateKeys,
    showToSign,
    writeSigned,
    keepSigned,
    cmdSignTx,
    signWalletTx,
  )
where

import Control.Monad (forM_, unless, void, when)
import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (isJust, listToMaybe)
import Haskoin
import Haskoin.Wallet.Amounts (UnitChoice)
import Haskoin.Wallet.Cli.Command
import Haskoin.Wallet.Cli.PrettyPrinter.Base (renderOn)
import Haskoin.Wallet.Cli.PrettyPrinter.Transaction (txAnswerDoc)
import Haskoin.Wallet.Cli.Prompt
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Commands.Answer (seenFromPlace, txAnswer)
import Haskoin.Wallet.Commands.Run (runDBResponse)
import Haskoin.Wallet.Commands.ShortId (pendingTxNamed)
import Haskoin.Wallet.Commands.TxFile (TxFileRead (..), fileNetwork, heldTxRead, notInWallet, poolFileAccounts, readTxFileIn)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Keys
import Haskoin.Wallet.ShortId
import Haskoin.Wallet.Tx.PsbtFile (signedPsbt, signerChecks, txFilePsbt)
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Signing (SpentCoin, outsideInputs, outsideUnsigned, heldPoolOf, labelledTxFile, labelsFromFile, parseOutsideTxSignData, parseTxSignData, signHeldWalletTx)
import Haskoin.Wallet.Util
import System.IO (stderr)

-- | The file a signed transaction is written to, besides the wallet that
-- holds it
data SignedFile
  = -- | None: the pending transaction signed by its nosigHash is enough
    NoSignedFile
  | -- | The --output file, which must be free
    NewFile FilePath
  | -- | The --input file without --output, which the user agreed to
    -- replace, by an answer or with --yes: the unsigned transaction is of
    -- no use once signed
    ReplacedFile FilePath

-- | The transaction signtx and signsweep sign, as its file holds it, with
-- the network the file names and the labels it carries, and the pool of
-- the wallet it involves, if any, which hw finds by itself
-- ('transactionPool'). A pending transaction given by its nosigHash is as
-- a file that names no network and labels no address. A transaction
-- signed already, or sent, is not signed again, nor is one that spends a
-- coin the user locked
-- ("Haskoin.Wallet.Database.PendingTx".'refuseLockedCoins'), nor a file
-- that the signer of the spec refuses as a whole ('signerChecks'). The
-- command then refuses what its secret does not sign (@refuseForMnemonic@,
-- 'refuseForPrivateKeys') and settles the file to write
-- ('signedFileOf'), all before the user types a secret.
transactionToSign ::
  Config ->
  -- | The --network of the command, for a file that names none
  Maybe Network ->
  Signing ->
  ExceptT String (DB IO) (Maybe Pool, TxFileRead)
transactionToSign cfg givenM signing = do
  TxFileRead txFile@(TxFile netM tsd _ _) sourceM <- case (signing.nosigHash, signing.input) of
    (Nothing, Nothing) ->
      throwError "Give the NOSIGHASH of a pending transaction, or an --input file"
    (Just _, Just _) ->
      throwError "Can not specify both a NOSIGHASH and an --input file"
    (Just named, _) -> do
      held <- pendingTxNamed cfg named
      when held.pendingTxOnline $
        throwError "The transaction was sent already: there is nothing left to sign"
      return (heldTxRead held)
    (_, Just file) -> readTxFileIn (commandNetwork cfg givenM) file
  when (isSigned (txSignDataTx tsd)) $
    throwError "The transaction is already signed"
  named <- fileNetwork givenM netM
  -- The pool found is the one that pays, if any: the keys of a portfolio
  -- that does not pay the transaction sign none of its inputs
  activeM <- lift activePortfolioId
  (poolM, completed) <- transactionPool cfg activeM named tsd
  -- A transaction of no pool spends no coin of the wallet
  forM_ poolM $ \pool -> refuseLockedCoins pool.poolNetwork (txSignDataTx tsd)
  -- A PSBT is checked on the network it is signed on: that of its pool,
  -- or else the one named, or the default
  forM_ sourceM $ liftEither . signerChecks (maybe (commandNetwork cfg named) (.poolNetwork) poolM)
  return (poolM, TxFileRead txFile {txFileData = completed} sourceM)

-- | The file a command writes its signed transaction to: the --output
-- file, which must be free, or the --input file, which is replaced only
-- if the user agrees: by an answer, or in advance with --yes, which lets
-- a script sign a file in place. Asked once nothing else can refuse the
-- transaction, and before the user types a secret. An --output file
-- leaves the --input file as it is, with --yes too.
signedFileOf :: Signing -> ExceptT String (DB IO) SignedFile
signedFileOf signing = case (signing.input, signing.output) of
  (_, Just file) -> NewFile file <$ checkFileToWrite file
  (Just file, Nothing) -> do
    replacing <- if signing.yes then return True else askYes terminal (ScriptOption "--yes or --output") (replaceQuestion file)
    unless replacing $ throwError "The transaction was not signed: give an --output file to write it to"
    return (ReplacedFile file)
  (Nothing, Nothing) -> return NoSignedFile
  where
    replaceQuestion file = "Replace " <> file <> " with the signed transaction? Give an --output file to write it elsewhere."

-- | The transaction to sign as the user named it, for a refusal to give
-- the command that signs it: its hash as typed, or its --input file
signingNamed :: Signing -> String
signingNamed signing = case signing.input of
  Just file -> "--input " <> file
  Nothing -> maybe "NOSIGHASH" hashRefText signing.nosigHash

-- | Refuse a transaction of a pool that the mnemonic does not sign,
-- before signtx asks for anything. The mnemonic signs the coins of the
-- wallet. It signs nothing of a transaction that spends none of them: a
-- sweep, which signsweep signs with the private keys of its coins. It
-- cannot finish one with an input from outside the wallet that nobody
-- signed ('outsideUnsigned').
--
-- pendingtxs lists payments and sweeps together, so the wrong command is
-- an easy mistake. Before, signtx asked for the mnemonic of a sweep and
-- then failed: a secret typed for nothing is a secret exposed for
-- nothing.
refuseForMnemonic :: Signing -> Pool -> TxSignData -> ExceptT String (DB IO) ()
refuseForMnemonic signing pool tsd = do
  (summary, outside) <- inputsOutside pool tsd
  unless (paysFrom summary) $
    throwError $
      "The transaction spends no coin of this wallet, so the mnemonic signs nothing in it. \
      \The private keys of its coins sign such a transaction, a sweep: hw signsweep "
        <> signingNamed signing
  forM_ (listToMaybe [(i, coin) | (i, coin, False) <- outside]) $
    throwError . uncurry (outsideUnsigned pool.poolNetwork)

-- | Refuse a transaction of a pool that private keys do not sign, before
-- signsweep asks for anything. The user holds no private key of a coin of
-- the wallet, so a transaction that spends coins of the wallet alone, a
-- payment, is left to signtx. Before, signsweep asked for the keys of a
-- payment and then named a missing key. A transaction that also spends
-- coins from outside the wallet is read on: an older hw prepared such
-- sweeps.
refuseForPrivateKeys :: Signing -> Pool -> TxSignData -> ExceptT String (DB IO) ()
refuseForPrivateKeys signing pool tsd = do
  (_, outside) <- inputsOutside pool tsd
  when (null outside) $
    throwError $
      "Every coin the transaction spends is of this wallet: the mnemonic signs them, not private keys. \
      \Sign the transaction with hw signtx "
        <> signingNamed signing

-- | The summary of a transaction as its pool sees it, and its inputs
-- that spend no coin of the pool ('outsideInputs')
inputsOutside :: Pool -> TxSignData -> ExceptT String (DB IO) (TxSummary, [(Int, SpentCoin, Bool)])
inputsOutside pool tsd = do
  keys <- lift $ poolKeys pool
  liftEither $ do
    summary <- txInfoSummary <$> parseTxSignData pool.poolNetwork keys tsd
    (summary,) <$> outsideInputs pool.poolNetwork summary tsd

-- | Show the transaction about to be signed, before the secret is asked
-- for, as show shows it: with the wallet's labels for a pending
-- transaction, those of the file for a file, and either way with the
-- addresses another portfolio of the database holds named as such, and
-- with the time the wallet created it when it holds it. On stderr, as the
-- questions, to keep the --json answer clean.
showToSign :: UnitChoice -> Signing -> Network -> Maybe Pool -> TxFile -> ExceptT String (DB IO) ()
showToSign choice signing net poolM txFile = do
  let tsd = txFileData txFile
  (info, seenM) <- case poolM of
    Just pool -> do
      keys <- lift $ poolKeys pool
      parsed <- liftEither $ parseTxSignData net keys tsd
      filled <-
        lift $
          if isJust signing.nosigHash
            then fillTxInfo pool parsed
            else fillAddresses pool (labelsFromFile txFile parsed)
      -- A transaction the pool holds is shown with what the database
      -- knows of it, the time it was created first, as pendingtxs shows it
      heldM <- lookupPendingTx (nosigHash (txSignDataTx tsd))
      let info = case heldM of
            Just held | held.pendingTxPool == pool -> filled {txInfoStatus = heldStatus held filled.txInfoStatus}
            _ -> filled
      seen <- seenFromPlace =<< poolPlace pool
      return (info, Just seen)
    Nothing -> (,Nothing) <$> (lift . fillContacts net =<< liftEither (parseOutsideTxSignData net tsd))
  naming <- lift walletNaming
  doc <- liftIO $ txAnswerDoc naming net seenM choice info
  liftIO $ renderOn stderr doc

-- | Write a transaction just signed to its file, if any, with the keys of
-- its pool when it has one: into the PSBT the --input file held, which
-- keeps every field of it, or as a new PSBT for a transaction that came
-- from the wallet
writeSigned :: Maybe Pool -> Maybe Psbt -> SignedFile -> TxFile -> ExceptT String (DB IO) ()
writeSigned poolM sourceM signedFile signed =
  case signedFile of
    NoSignedFile -> return ()
    NewFile file -> write (writeFileBytes file)
    ReplacedFile file -> write (replaceFileBytes file)
  where
    write put = do
      accounts <- maybe (return []) poolFileAccounts poolM
      psbt <- liftEither $ case sourceM of
        Just source -> signedPsbt accounts source signed
        Nothing -> txFilePsbt accounts signed
      liftEitherIO $ put (encodePsbt psbt)

-- | Keep a transaction just signed: in place of the pending one when it was
-- given by its nosigHash, and in its file when there is one. It is given
-- back with its labels, as the answer shows it: the wallet's for a pending
-- transaction, and those the --input file carries for a transaction of a
-- file, which go on to the signed file. The file is what an offline
-- computer has.
keepSigned :: Signing -> SignedFile -> Pool -> TxFileRead -> TxSignData -> TxInfo -> ExceptT String (DB IO) TxInfo
keepSigned signing signedFile pool (TxFileRead file sourceM) signed info = do
  labelled <-
    lift $
      if fromWallet
        then fillTxInfo pool info
        else fillAddresses pool (labelsFromFile file info)
  when fromWallet $ void $ importPendingTx pool signed
  writeSigned (Just pool) sourceM signedFile (labelledTxFile pool.poolNetwork labelled signed)
  return labelled
  where
    fromWallet = isJust signing.nosigHash

-- | Sign a transaction of a portfolio with the keys of its mnemonic, or of
-- the shares of its backup, and answer with it: 'signWalletTx', which
-- send and sendall sign with too. The transaction is shown first, as show shows it.
cmdSignTx :: Config -> UnitChoice -> Signing -> IO Response
cmdSignTx cfg choice signing =
  runDBResponse cfg $ do
    (pool, signed) <- signWalletTx cfg signing $ \pool -> showToSign choice signing pool.poolNetwork (Just pool)
    txAnswer pool signed

-- | Sign a transaction of a portfolio with the keys of its mnemonic, or of
-- the shares of its backup, and keep it signed ('keepSigned'): all that
-- signtx does but its answer, and the second step of send and of
-- sendall. The keys are those of every account the portfolio has on the
-- network of the transaction, since a payment may spend the coins of
-- both. The pool of the transaction is given back with the transaction as
-- signed.
--
-- The transaction is shown first, by the action given, so that the user
-- sees what the mnemonic is typed for: the prompt is the confirmation,
-- and an interrupt stops it. The command says how it shows it. Every
-- refusal comes before that, and so before the user types a secret
-- ('transactionToSign', @refuseForMnemonic@).
signWalletTx ::
  Config ->
  Signing ->
  -- | Show the transaction to sign, given its pool and as its file reads
  (Pool -> TxFile -> ExceptT String (DB IO) ()) ->
  ExceptT String (DB IO) (Pool, TxInfo)
signWalletTx cfg signing showFirst = do
  (poolM, file@(TxFileRead txFile _)) <- transactionToSign cfg Nothing signing
  -- A file that involves no portfolio may be of a portfolio the database
  -- lacks, or a sweep
  pool <- liftMaybe (notInWallet <> ". The private keys of its coins sign a sweep: hw signsweep " <> signingNamed signing) poolM
  refuseForMnemonic signing pool (txFileData txFile)
  signedFile <- signedFileOf signing
  let net = pool.poolNetwork
  portfolio <- getPortfolio pool.poolPortfolio
  accounts <- lift $ poolAccounts pool
  showFirst pool txFile
  keysOfPool <- lift $ poolKeys pool
  (seed, keys) <- askSigningKeys terminal net (portfolioIndex portfolio.portfolioRow) keysOfPool
  -- The signed data names the seed, for the derivation records of its
  -- file
  let named = (txFileData txFile) {txSignDataFingerprint = Just (seedFingerprint seed)}
  (signed, info) <- liftEither $ signHeldWalletTx net (heldPoolOf (snd <$> accounts)) named keys
  (pool,) <$> keepSigned signing signedFile pool file signed info
