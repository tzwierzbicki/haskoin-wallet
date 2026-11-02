{-# LANGUAGE OverloadedRecordDot #-}

-- | The version, the lock time and the input sequences of the transactions
-- hw builds, set as Bitcoin Core and Electrum set theirs, so that a
-- transaction of hw looks like one of theirs.
--
-- The lock time is the height of the best block the wallet knows. It keeps
-- the transaction out of every block below that height, so a miner cannot
-- take its fee by mining it into a fork of the chain ("fee sniping"). One
-- time in ten, the lock time is up to 99 blocks lower. The time between
-- preparing a transaction and sending it, long when it is signed offline,
-- then does not show in the gap between its lock time and its block. A
-- wallet behind the chain, whose best block was mined more than eight
-- hours ago, writes a lock time of 0 instead, as Core and Electrum do: a
-- stale height would single it out.
--
-- A lock time applies only when the sequence of an input is below the
-- final 0xffffffff. On Bitcoin, 0xfffffffd also marks the transaction
-- replaceable (BIP 125). Bitcoin Cash replaces no transaction, and its
-- wallets write 0xfffffffe, the highest sequence that is not final.
-- Version 2 is what all those wallets write. It matters only to the
-- relative lock times of BIP 68, which both sequences turn off.
module Haskoin.Wallet.Tx.LockTime
  ( txVersion,
    inputSequence,
    withHeader,
    lockTimeFor,
    staleAfter,
    isStale,
  )
where

import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word32)
import Haskoin
import System.Random (RandomGen, uniformR)

-- | The version of every transaction hw builds
txVersion :: Word32
txVersion = 2

-- | The sequence of every input of the transactions hw builds: on Bitcoin,
-- 0xfffffffd, which marks the transaction replaceable (BIP 125); on Bitcoin
-- Cash, 0xfffffffe, which does not. Both let the lock time apply.
inputSequence :: Network -> Word32
inputSequence net
  | net.replaceByFee = 0xfffffffd
  | otherwise = 0xfffffffe

-- | The transaction with the version, the lock time and the sequences of
-- hw, everything else kept: the builder of haskoin writes version 1, final
-- sequences and no lock time
withHeader :: Network -> Word32 -> Tx -> Tx
withHeader net lockTime (Tx _ inputs outputs witness _) =
  Tx txVersion [TxIn input.outpoint input.script (inputSequence net) | input <- inputs] outputs witness lockTime

-- | The lock time of a new transaction, drawn from the height of the best
-- block: the height itself, or one time in ten a height up to 99 blocks
-- below it, never below 0, as Electrum draws it. Without a height, 0.
lockTimeFor :: (RandomGen g) => Maybe BlockHeight -> g -> (Word32, g)
lockTimeFor Nothing gen = (0, gen)
lockTimeFor (Just height) gen
  | draw == 0 = let (back, gen'') = uniformR (0, 99) gen' in (height - min height back, gen'')
  | otherwise = (height, gen')
  where
    (draw, gen') = uniformR (0 :: Int, 9) gen

-- | How old the best block may be before the wallet counts as behind the
-- chain: eight hours, as in Bitcoin Core and Electrum
staleAfter :: NominalDiffTime
staleAfter = 8 * 60 * 60

-- | Whether the wallet is behind the chain at the time given: its best
-- block was mined, by its header, more than 'staleAfter' before, or at a
-- time the wallet does not know
isStale :: UTCTime -> Maybe Word32 -> Bool
isStale _ Nothing = True
isStale now (Just blockTime) = addUTCTime staleAfter (posixSecondsToUTCTime (fromIntegral blockTime)) < now
