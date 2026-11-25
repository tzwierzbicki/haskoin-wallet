{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What a hash or a short id names in the wallet: a pending transaction, a
-- transaction on the chain, or a coin.
--
-- A command takes a hash in full or by its short id, its first 8 digits
-- ("Haskoin.Wallet.ShortId"). A short id is looked up among the hashes the
-- command can name, in every portfolio, and the command then goes on as
-- with the full hash: it says what it says of a full hash the portfolio
-- does not hold, for instance. A short id that matches none gets the
-- answer a full hash that matches nothing gets, with the short id in it.
-- One that matches two or more is refused, with each one named, for the
-- user to give the full hash.
module Haskoin.Wallet.Commands.ShortId
  ( pendingHashOf,
    pendingTxNamed,
    chainTxidOf,
    walletTxHashOf,
    walletCoinOf,
    coinNamed,
    shownViews,
  )
where

import Control.Monad.Except (ExceptT (..))
import Control.Monad.Trans.Class (lift)
import Haskoin
import Haskoin.Wallet.Commands.Scope (shownAmong)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.ShortId

-- | The pending transaction a command names, by its nosigHash or its txid
-- once signed, in full or by its short id
pendingHashOf :: HashRef -> ExceptT String (DB IO) TxHash
pendingHashOf = fullHashOf noPendingTx pendingNamed pendingHashesWith

-- | The pending transaction a command names, as 'pendingHashOf' finds it,
-- which must exist, on a network hw shows. One on a hidden network is
-- refused, with the command that shows its network: exporttx, deletetx,
-- signtx, signsweep and broadcast all take a pending transaction by its
-- hash, and none of them works on a hidden network.
pendingTxNamed :: Config -> HashRef -> ExceptT String (DB IO) PendingTx
pendingTxNamed cfg named = do
  held <- requirePendingTx =<< pendingHashOf named
  _ <- shownAmong cfg ("The pending transaction " <> hashRefText named) (.poolNetwork) [held.pendingTxPool]
  return held

-- | The transaction on the chain that txs --after names, by its txid in
-- full or by its short id, among those of every portfolio. The listing
-- then refuses one of another portfolio.
chainTxidOf :: HashRef -> ExceptT String (DB IO) TxHash
chainTxidOf named = fullHashOf none transactionsNamed (lift . walletTxidsWith) named
  where
    none = "The transaction " <> hashRefText named <> " is not among those listed"

-- | The transaction a command names, pending or on the chain, by its
-- txid, or by its nosigHash while it is pending, in full or by its short
-- id: what label and bumpfee take
walletTxHashOf :: HashRef -> ExceptT String (DB IO) TxHash
walletTxHashOf named = fullHashOf (noTransaction named) walletTxsNamed walletHashesWith named

-- | The answer to a hash that names no transaction of the wallet
noTransaction :: HashRef -> String
noTransaction named = "The wallet has no transaction " <> hashRefText named

-- | The coin a command names, by its txid in full or by its short id, and
-- its index, among the coins of every portfolio: what lockcoin,
-- unlockcoin and --coin take
walletCoinOf :: CoinRef -> ExceptT String (DB IO) OutPoint
walletCoinOf named = coinNamed ("The wallet has no coin " <> coinRefText named) named

-- | The coin a command names, as 'walletCoinOf' finds it, with the answer
-- to a short id that matches no coin
coinNamed :: String -> CoinRef -> ExceptT String (DB IO) OutPoint
coinNamed none = \case
  CoinRef (FullHash hash) index -> return (OutPoint hash index)
  CoinRef (ShortHash short) index -> liftEither . pickCoin none short index =<< lift (coinsWith short index)

-- | The views of a transaction the user named, among those of every
-- portfolio that holds it: those on the networks hw shows ('shownAmong')
shownViews :: Config -> HashRef -> [(Pool, Place, TxInfo)] -> ExceptT String (DB IO) [(Pool, Place, TxInfo)]
shownViews cfg named = shownAmong cfg ("The transaction " <> hashRefText named) (\(pool, _, _) -> pool.poolNetwork)
