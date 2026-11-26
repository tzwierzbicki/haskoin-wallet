{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Sending a transaction: broadcast, and send and sendall, which prepare,
-- sign and send in one command. The two take the steps of preparetx or
-- preparesendall, of signtx and of broadcast, so that a payment made in
-- one command is the payment the three commands would make. Every command
-- that sends ends the same way ('sendTxRead'), with a sync of the
-- portfolios the transaction touches.
module Haskoin.Wallet.Commands.Send
  ( cmdBroadcast,
    sendTxRead,
    cmdSend,
    cmdSendAll,
    sendJustSigned,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT (..), catchError, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (listToMaybe, maybeToList)
import Data.String.Conversions (cs)
import Haskoin
import Haskoin.Wallet.Amounts (UnitChoice)
import Haskoin.Wallet.Cli.Command
import Haskoin.Wallet.Cli.PrettyPrinter.Base (renderOn)
import Haskoin.Wallet.Cli.PrettyPrinter.Payment (preparedDoc)
import Haskoin.Wallet.Cli.PrettyPrinter.Transaction (txAnswerDoc)
import Haskoin.Wallet.Cli.Prompt
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Commands.Answer (seenFromPlace)
import Haskoin.Wallet.Commands.Payment (Prepared (..), preparePayment, prepareSendAll)
import Haskoin.Wallet.Commands.Run (keepSoFar, runDBResponse)
import Haskoin.Wallet.Commands.Scope (shownAmong)
import Haskoin.Wallet.Commands.ShortId (chainTxidOf, pendingTxNamed)
import Haskoin.Wallet.Commands.Signing (signWalletTx)
import Haskoin.Wallet.Commands.Sync (terminalWatcher)
import Haskoin.Wallet.Commands.TxFile (TxFileRead (..), fileNetwork, heldTxRead, readTxFileIn)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.ShortId
import Haskoin.Wallet.Store
import Haskoin.Wallet.Sync
import Haskoin.Wallet.Sync.Status (StatusLine (..), withStatusLine)
import Haskoin.Wallet.Tx.SignData
import System.IO (stderr)
import UnliftIO (withRunInIO)

-- | Send a signed transaction: a pending one of the wallet, by its id or
-- its nosigHash, or the one of a file. One the wallet holds goes on the
-- network of its portfolio, which the file or --network may only name; one
-- it does not hold goes on the network its file names, or on that of
-- --network or of the configuration when the file names none. See
-- 'sendSigned'. The user sees the transaction and says whether to send
-- it, unless --yes.
cmdBroadcast :: Config -> UnitChoice -> Maybe HashRef -> Maybe FilePath -> Bool -> Maybe Network -> IO Response
cmdBroadcast cfg choice namedM fileM yes givenM =
  case (namedM, fileM) of
    (Just named, Nothing) -> sending (heldTxRead <$> pendingToSend named)
    (Nothing, Just file) -> sending (readTxFileIn (commandNetwork cfg givenM) file)
    (Nothing, Nothing) -> return $ ResponseError "Give the TXID of a pending transaction or an --input file"
    (Just _, Just _) -> return $ ResponseError "Can not specify both a TXID and an --input file"
  where
    -- The pending transaction named. A hash that names a transaction of
    -- the history gets an answer of its own. A sync has seen that
    -- transaction on the network and took it off the pending ones, so
    -- "The pending transaction does not exist" left the user of a payment
    -- that stays unconfirmed with nothing to do next.
    pendingToSend named =
      pendingTxNamed cfg named `catchError` \failure -> do
        inHistory <- lift $ runExceptT (chainTxidOf named)
        throwError $ case inHistory of
          Right _ | failure == noPendingTx -> sentAlready named
          _ -> failure
    sentAlready named =
      "The transaction "
        <> hashRefText named
        <> " is not pending: it was sent, and a sync has seen it on the network, so there is nothing to send."
        <> " txs shows where it stands. A payment of Bitcoin that stays unconfirmed is replaced with bumpfee"
    sending readTx =
      runDBResponse cfg $ do
        storeOn <- liftIO $ webStores cfg
        sendTxRead cfg storeOn choice yes givenM =<< readTx

-- | Send a signed transaction, read from the wallet or from a file, and
-- answer with it as sent: all that broadcast does once it has read its
-- transaction, and the last step of send and of sendall. See 'sendSigned'
-- for what is sent where. The user sees the transaction and says whether
-- to send it, unless the command says not to ask. broadcast says so with
-- --yes. send and sendall always do: they showed the transaction before
-- they asked for the mnemonic, and typing the mnemonic was the answer.
--
-- The labels of a file are left aside: the transaction is shown as the
-- wallet holds it.
sendTxRead ::
  Config ->
  -- | The stores of the command
  (Network -> Store) ->
  UnitChoice ->
  -- | Whether to send without showing the transaction and asking
  Bool ->
  -- | The --network of the command, for a file that names none
  Maybe Network ->
  TxFileRead ->
  ExceptT String (DB IO) Response
sendTxRead cfg storeOn choice yes givenM (TxFileRead (TxFile netM tsd _ _) _) = do
  named <- fileNetwork givenM netM
  -- A file of a hidden network, or of a transaction pending on one, is
  -- refused, as the pending transaction named by its hash is
  heldM <- lookupPendingTx (nosigHash (txSignDataTx tsd))
  _ <- shownAmong cfg "The transaction" id (maybeToList named <> [held.pendingTxPool.poolNetwork | Just held <- [heldM]])
  activeM <- lift activePortfolioId
  sent <- sendSigned cfg storeOn beforeSending activeM named tsd
  case sent of
    SentPending place info -> do
      after <- syncAfterSending cfg storeOn (placeNetwork place) (txSignDataTx tsd)
      ResponseSentPending place info after <$> activePortfolioRef
    SentOutside net txid -> ResponseSentTx net txid <$> syncAfterSending cfg storeOn net (txSignDataTx tsd)
  where
    -- On stderr, with the question, to keep the --json answer clean
    beforeSending net placeM info =
      unless yes $ do
        seenM <- mapM seenFromPlace placeM
        naming <- lift walletNaming
        liftIO $ renderOn stderr =<< txAnswerDoc naming net seenM choice info
        confirmed <- askYes terminal (ScriptOption "--yes") "Send this transaction to the network? This cannot be undone."
        unless confirmed $ throwError "The transaction was not sent"

-- | Sync the portfolios that a transaction just sent touches, on its
-- network ('touchedPools'): the last step of every command that sends.
-- The type t'SyncAfter' says why.
--
-- The store lists the transaction by now, which is what the sending
-- waited for. A haskoin-store writes a transaction with the coins and
-- the balances it changes in one step, so this sync finds the change.
-- That was checked on the regtest chains before this was built: at the
-- moment the store first listed a payment, it gave its change as a coin
-- and no longer gave the coin spent, for 40 payments of 40 on Bitcoin
-- and as many on Bitcoin Cash.
--
-- What the wallet recorded of the sending is saved first ('keepSoFar'):
-- the transaction left the computer, so it stays marked as sent
-- whatever comes of the sync. A sync that fails is undone alone, and its
-- failure is a part of the answer. The command does not fail for it,
-- since it did what it was asked: the payment is sent, and a failure
-- here would tell a script, and the user, that it is not.
syncAfterSending :: Config -> (Network -> Store) -> Network -> Tx -> ExceptT String (DB IO) SyncAfter
syncAfterSending cfg storeOn net tx = do
  keepSoFar
  outcome <- lift . runExceptT . undoneIfRefused $ do
    pools <- touchedPools net tx
    if null pools then return Nothing else Just <$> onStatusLine (syncPools cfg storeOn pools Incremental . terminalWatcher)
  return $ case outcome of
    Left failure -> SyncAfter [] (Just (cs failure))
    Right Nothing -> nothingToSyncAfter
    Right (Just synced) -> SyncAfter (syncedPools synced) (cs . snd <$> listToMaybe (syncedDown synced))

-- | Run a sync with the status line of the terminal, which is gone once
-- the sync ends ('withStatusLine')
onStatusLine :: (StatusLine -> ExceptT String (DB IO) a) -> ExceptT String (DB IO) a
onStatusLine action = ExceptT $ withRunInIO (\run -> withStatusLine (\line -> run (runExceptT (action line))))

-- | What send and sendall tell the user after the transaction and before
-- the mnemonic. signtx signs and stops, and broadcast asks before it
-- sends. send and sendall ask nothing after the mnemonic, so the user must
-- know by then that typing it sends the payment.
sendNotice :: String
sendNotice = "Typing the mnemonic signs this transaction and sends it to the network. This cannot be undone."

-- | Make a payment in one command: prepare it as preparetx does, sign it
-- as signtx does, and send it as broadcast does (@prepareSignSend@). The
-- first step is that of preparetx ('preparePayment'), so the payment is
-- the one the three commands make of the same options. The command takes
-- the options of preparetx, without an --output file.
cmdSend :: Config -> UnitChoice -> PrepareTx -> IO Response
cmdSend cfg choice opts =
  prepareSignSend cfg choice $ \storeOn -> preparePayment cfg storeOn choice opts

-- | Send all the coins of the active portfolio in one command: prepare
-- the transaction as preparesendall does, sign it as signtx does, and
-- send it as broadcast does (@prepareSignSend@). The first step is that
-- of preparesendall ('prepareSendAll'), so the coins sent and what each
-- recipient gets are those the three commands make of the same options.
-- The command takes the options of preparesendall, without an --output
-- file. It is to preparesendall what send is to preparetx, and differs
-- from send in nothing but the transaction it prepares.
cmdSendAll :: Config -> UnitChoice -> PrepareSendAll -> IO Response
cmdSendAll cfg choice opts =
  prepareSignSend cfg choice $ \storeOn -> prepareSendAll cfg storeOn choice opts

-- | Prepare a transaction, sign it as signtx does and send it as broadcast
-- does, in one command: all that send and sendall do. They differ only in
-- how they prepare the transaction, which is the action given: that of
-- preparetx for send, and that of preparesendall for sendall. The action
-- is handed the stores of the command, and leaves the transaction pending
-- and unsigned, as its command does. The other two steps are those of
-- signtx and broadcast ('signWalletTx' and 'sendJustSigned'), so the
-- transaction and what the wallet holds of it afterwards are the same as
-- with the three commands run one after the other.
--
-- Such a command is for a wallet whose mnemonic is typed on the online
-- computer anyway. It asks for the mnemonic and needs the store, so it
-- cannot keep the mnemonic offline. The three commands stay for that.
--
-- The transaction is shown once, before the mnemonic is asked, as the
-- command that prepares it answers with it: with the coins it leaves in
-- the portfolio. Nothing of it changes between then and the sending, so
-- it is not shown again, and the answer on the screen is its txid alone.
-- Typing the mnemonic is the confirmation. broadcast asks whether to
-- send, and these commands do not: the user saw the transaction, was told
-- that the mnemonic sends it ('sendNotice'), and typed it.
--
-- What a failure leaves depends on whether the transaction was signed:
--
-- * Before the signature, nothing is kept. A refusal, a no to a question
--   and a mnemonic not typed all undo the pending transaction. The user
--   did not confirm the payment, and an unsigned transaction left behind
--   would lock its coins until a deletetx.
--
-- * After the signature, the signed transaction is kept, as signtx leaves
--   it ('sendJustSigned'). It is in the wallet before it is handed to the
--   network. A sending that fails, or that is interrupted while the store
--   waits for its nodes, may have reached the network all the same. The
--   coins must then stay locked, and broadcast must be able to send the
--   transaction again. The failure says so, and names the transaction.
--
-- The store is asked whether it is well before the mnemonic is asked, so
-- that a store that is down does not cost the user a mnemonic typed for
-- nothing.
prepareSignSend ::
  Config ->
  UnitChoice ->
  -- | Prepare the transaction, with the stores of the command
  ((Network -> Store) -> ExceptT String (DB IO) Prepared) ->
  IO Response
prepareSignSend cfg choice prepare =
  runDBResponse cfg $ do
    storeOn <- liftIO $ webStores cfg
    -- As preparetx or preparesendall: the transaction is pending, and its
    -- coins are locked
    prepared <- prepare storeOn
    -- A store that is down stops the command here, before the mnemonic
    fromStore $ checkHealth (storeOn prepared.preparedPool.poolNetwork)
    -- As signtx NOSIGHASH
    _ <- signWalletTx cfg (signingPending prepared.preparedHash) $ \_ _ -> showBeforeMnemonic prepared
    -- As broadcast TXID --yes
    sendJustSigned cfg storeOn choice prepared.preparedHash
  where
    -- The transaction as the command that prepares it answers with it,
    -- then, after a blank line, what typing the mnemonic does. On stderr,
    -- as the questions, to keep the --json answer clean.
    showBeforeMnemonic :: Prepared -> ExceptT String (DB IO) ()
    showBeforeMnemonic prepared = do
      place <- poolPlace prepared.preparedPool
      activeM <- activePortfolioRef
      naming <- lift walletNaming
      liftIO $ renderOn stderr =<< preparedDoc naming choice place prepared.preparedInfo prepared.preparedLeft activeM
      liftIO $ tell terminal ("\n" <> sendNotice)

-- | Send a pending transaction that the command just signed, as broadcast
-- TXID --yes does: the last step of send, of sendall, and of a sweep that
-- pays a portfolio. The user agreed to the sending before the signature,
-- so nothing is shown or asked here.
--
-- The signed transaction is saved first ('keepSoFar'): it is in the
-- wallet from here on, whatever comes of the sending. A sending that
-- fails says why, then what the wallet holds and what to do with it. The
-- user gave the secret and reads a failure, so the failure says that the
-- signature is not lost. The transaction is named by the short id of its
-- txid, as the next steps name one.
sendJustSigned :: Config -> (Network -> Store) -> UnitChoice -> TxHash -> ExceptT String (DB IO) Response
sendJustSigned cfg storeOn choice pendingHash = do
  keepSoFar
  signed <- requirePendingTx pendingHash
  sendTxRead cfg storeOn choice True Nothing (heldTxRead signed) `catchError` (throwError . withWhatIsKept signed)
  where
    withWhatIsKept :: PendingTx -> String -> String
    withWhatIsKept signed failure =
      let short = shortHashText (txHash (txSignDataTx signed.pendingTxData))
       in failure
            <> ". The transaction is signed and stays pending: broadcast "
            <> short
            <> " sends it again, and deletetx "
            <> short
            <> " deletes it and frees its coins"
