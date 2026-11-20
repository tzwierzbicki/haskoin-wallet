{-# LANGUAGE OverloadedRecordDot #-}

-- | Coins of Bitcoin Cash that hold tokens (CashTokens), which hw shows
-- and never spends.
--
-- Since May 2023 an output of Bitcoin Cash may hold tokens. They are
-- written in front of the script that locks the coin, in the script field
-- of the output: the token prefix (@Haskoin.Script.CashToken@ of
-- haskoin-core). A transaction that spends such a coin decides where its
-- tokens go. One that says nothing of them destroys them.
--
-- hw knows nothing of tokens: it has no command that shows them or sends
-- them, and a payment it builds names none. So hw must never spend a coin
-- that holds tokens. The coin is still a coin of the wallet, with real
-- satoshis: haskoin-store lists it at the plain address of its script,
-- and counts it in the balance and the history of that address. Leaving
-- it out would make the totals of hw disagree with those of the store.
-- So hw keeps the coin, with a lock that nothing opens
-- ('Haskoin.Wallet.Database.Coin.LockedByTokens'), and the user moves it
-- with a wallet that knows tokens, which the same mnemonic opens.
--
-- This module is the one place that says what such a coin is. Three
-- places ask it, each a barrier of its own: sync, which stores the coin
-- as locked ("Haskoin.Wallet.Database.Coin"), a sweep, which leaves the
-- coin where it is ("Haskoin.Wallet.Tx.Sweep"), and signing, which
-- refuses a transaction that spends one ("Haskoin.Wallet.Tx.Signing").
--
-- Bitcoin has no tokens. The byte that starts a token prefix is an
-- undefined opcode there, so nothing here applies to a network of
-- Bitcoin: 'holdsTokens' is false on it for every script.
module Haskoin.Wallet.CashTokens
  ( holdsTokens,
    lockingScript,
    tokensRefusal,
  )
where

import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Haskoin (Network (..), hasTokenPrefix, splitTokenData)

-- | Whether the script field of an output of the network holds tokens: the
-- network is one of Bitcoin Cash, and the field starts with the byte of a
-- token prefix. The rest of the prefix is not read: a field that starts
-- with the byte and has no valid prefix is in no block, and hw would not
-- spend it either.
holdsTokens :: Network -> ByteString -> Bool
holdsTokens net script = isJust net.cashAddrPrefix && hasTokenPrefix script

-- | The script that locks a coin of the network: its script field, without
-- the token prefix when it has one. This is the script that the address
-- of the coin stands for. 'Nothing' for a prefix that does not read.
lockingScript :: Network -> ByteString -> Maybe ByteString
lockingScript net script
  | holdsTokens net script = either (const Nothing) (Just . snd) (splitTokenData script)
  | otherwise = Just script

-- | Why hw does not sign a transaction that spends a coin with tokens. The
-- sentence says what to do with the coin, since hw offers no way.
tokensRefusal :: String
tokensRefusal =
  "The transaction spends a coin that holds tokens (CashTokens). hw does not handle tokens and never spends such a coin: \
  \the tokens could be destroyed. Move the coin with a wallet that handles the tokens of Bitcoin Cash"
