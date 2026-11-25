{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What a command says when it locks a coin that a pending transaction
-- spends. A tag can lock the coins at its addresses, and tag, renametag
-- --merge and importwallet can lock coins that way. The lock then wins
-- over the older payment, and the user is told at once of each payment it
-- stops ('tellingStopped').
module Haskoin.Wallet.Commands.TagLock
  ( tellingStopped,
  )
where

import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Class (liftIO)
import Haskoin
import Haskoin.Wallet.Cli.Prompt
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.ShortId

-- | Run a command that may lock coins, then tell the user of each
-- pending transaction that spends a coin the command locked: tag,
-- renametag --merge and importwallet. On stderr, as the questions, to
-- keep the --json answer clean.
--
-- A lock is what the user asked for last, so it wins over an older
-- payment: the command locks, and signtx and broadcast then refuse the
-- payment ("Haskoin.Wallet.Database.PendingTx".'refuseLockedCoins').
-- Refusing the lock instead was turned down. It would leave every other
-- coin of the tag unlocked until the user had settled the payment, and
-- it would make importwallet or renametag fail for a reason far from
-- what they do. lockcoin does refuse: it is about one coin, and a
-- refusal there leaves nothing else unlocked.
--
-- The user is told at once, since a pending payment is easy to forget,
-- and since this is the moment to delete it (@stoppedNotice@). The
-- transactions are those refused after the command and not before it,
-- so a command says nothing of a lock that was there already.
tellingStopped :: ExceptT String (DB IO) a -> ExceptT String (DB IO) a
tellingStopped command = do
  before <- stopped
  result <- command
  after <- stoppedPendingTxs
  liftIO $ mapM_ (tell terminal . uncurry stoppedNotice) [new | new@(held, _) <- after, held.pendingTxKey `notElem` before]
  return result
  where
    stopped = map ((.pendingTxKey) . fst) <$> stoppedPendingTxs

-- | What a command that locks a coin says of a pending transaction that
-- spends it: the transaction, by the short id pendingtxs shows, the
-- coin, as coins lists it, and what the lock does to the transaction. One
-- that waits to be signed or sent is stopped, and deletetx deletes it.
-- One that was sent is on its way, and no lock of the wallet stops the
-- network.
stoppedNotice :: PendingTx -> OutPoint -> String
stoppedNotice held outpoint =
  ("The pending transaction " <> short <> " spends the coin " <> shortCoinText outpoint <> ", which is locked now. ")
    <> if held.pendingTxOnline
      then "It was sent already, and the lock does not stop it"
      else "It can no longer be signed or sent: deletetx " <> short <> " deletes it"
  where
    short = pendingShortId held
