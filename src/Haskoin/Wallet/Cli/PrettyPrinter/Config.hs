{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What setconfig and showconfig answer with: the keys of the
-- configuration file with their values, what setconfig did to the journal
-- hw keeps, and the active portfolio, which showconfig shows.
module Haskoin.Wallet.Cli.PrettyPrinter.Config
  ( keyRowsDoc,
    journalSwitchDoc,
    activeShownDoc,
  )
where

import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin.Wallet.Cli.PrettyPrinter.Base (field, sentence, styled, text, wordsOf)
import Haskoin.Wallet.Cli.PrettyPrinter.Place (refText)
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Database.Portfolio
import Prettyprinter (Doc, pretty, (<+>))

-- | The keys of the configuration file with their values, a key on each
-- line, padded to the longest one, as setconfig and showconfig answer.
-- The keys are the names of the file, so that the user finds them there.
keyRowsDoc :: [(Text, Text)] -> [Doc Style]
keyRowsDoc settings = [field pad key <> pretty value | (key, value) <- settings]
  where
    pad = fromIntegral (maximum (0 : (Text.length . fst <$> settings)))

-- | What setconfig says of the journal hw keeps for ledger, once it
-- turned it on or off: where the file is, for the user to include it in
-- a journal of their own, or that hw removes it. A journal left behind
-- would go on being read, and would say less and less of the wallet.
journalSwitchDoc :: JournalSwitch -> Doc Style
journalSwitchDoc switched
  | journalKept switched = "hw now keeps a journal for ledger at" <+> styled UserText (journalPath switched)
  | otherwise = "hw no longer keeps a journal, and removes" <+> styled UserText (journalPath switched)

-- | What showconfig says of the active portfolio, in a sentence. A
-- sentence always names the wallet, as "portfolio1 of wallet0", whatever
-- the number of wallets: the user asked which one it is. Nothing when
-- neither --portfolio nor --wallet asked for it.
activeShownDoc :: ActiveShown -> Maybe (Doc Style)
activeShownDoc = \case
  ActiveNotAsked -> Nothing
  ActivePortfolioShown (Just ref) -> Just (text ("The active portfolio is " <> refText ref <> "."))
  ActiveWalletShown (Just index) -> Just (text ("The active wallet is " <> cs (walletName index) <> "."))
  ActivePortfolioShown Nothing -> Just noPortfolioDoc
  ActiveWalletShown Nothing -> Just noPortfolioDoc
  where
    -- The refusal of the commands that need a portfolio says how to make
    -- one, which the user who has none needs to know here too
    noPortfolioDoc = sentence (wordsOf (noWallets <> "."))
