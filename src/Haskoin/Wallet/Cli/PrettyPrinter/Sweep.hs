{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The answers about sweeps: the sweep prepared, the coins it leaves where
-- they are, with the reason, and its dust coins.
module Haskoin.Wallet.Cli.PrettyPrinter.Sweep
  ( sweepDoc,
    unsweptDoc,
    sweepDustReportDoc,
  )
where

import Data.List (intersperse)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Cli.PrettyPrinter.Base (counted, neutralAmount, sentence, text, wordsOf)
import Haskoin.Wallet.Cli.PrettyPrinter.Payment (dustCoinsSaid, dustReportWith)
import Haskoin.Wallet.Cli.PrettyPrinter.Place (SeenFrom (..))
import Haskoin.Wallet.Cli.PrettyPrinter.Transaction (txAnswerDoc)
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Tx.Dust (DustReport (..), SizedCoin (..))
import Haskoin.Wallet.Tx.Sweep (UnsweptCoin (..), UnsweptReason (..))
import Haskoin.Wallet.Util
import Prettyprinter (Doc, indent, vcat, (<+>))

-- | A sweep just prepared, as preparesweep answers with it: the
-- transaction with every detail ('txAnswerDoc'), as its portfolio sees it
-- when it pays one, then what the sweep leaves out, if anything, below it
-- and apart from it: the addresses given to sweep from that hold no coin,
-- the coins that hold tokens, and the coins worth less than their fee
-- ('unsweptDoc'). The user gave them to be swept: without a word, a sweep
-- that left one out reads as one that swept it.
sweepDoc :: WalletNaming -> UnitChoice -> Maybe SeenFrom -> Network -> TxInfo -> [Address] -> [UnsweptCoin] -> IO (Doc Style)
sweepDoc naming choice seenM net info empty unswept = do
  txDoc <- txAnswerDoc naming net seenM choice info
  return . vcat . intersperse mempty $
    txDoc
      : [emptyNote | not (null empty)]
        <> [unsweptDoc net (unitOn net choice) reason left | reason <- [HoldsTokens, WorthLessThanItsFee], let left = filter ((== reason) . (.reason)) unswept, not (null left)]
  where
    emptyNote =
      vcat $
        text (counted (fromIntegral (length empty)) "address given holds" "addresses given hold" <> " no coin and " <> isOrAre <> " not in the sweep:")
          : [indent 2 (text (addrToString net addr)) | addr <- empty]
    isOrAre = if length empty == 1 then "is" else "are" :: String

-- | The coins a sweep leaves at their addresses for a reason, as
-- preparesweep names them below the sweep: each as @sweptCoinDoc@ shows
-- it. The coins that hold tokens end with what to do with them, since no
-- command of hw moves them.
unsweptDoc :: Network -> AmountUnit -> UnsweptReason -> [UnsweptCoin] -> Doc Style
unsweptDoc net unit reason unswept =
  vcat $
    text (counted (fromIntegral (length unswept)) one many <> " not in the sweep:")
      : [indent 2 (sweptCoinDoc net unit left) | left <- unswept]
        <> [sentence (wordsOf "hw does not handle tokens and never spends such a coin. Move it with a wallet that handles the tokens of Bitcoin Cash.") | reason == HoldsTokens]
  where
    (one, many) = case reason of
      WorthLessThanItsFee -> ("coin is worth less than its fee and is", "coins are worth less than their fee and are") :: (String, String)
      HoldsTokens -> ("coin holds tokens (CashTokens) and is", "coins hold tokens (CashTokens) and are")

-- | A coin from outside the wallet in a report of preparesweep: its
-- outpoint with the txid in full, as --coin of preparesweep takes it, then
-- its address and its amount on the line below, as a From line of the
-- sweep gives them. The wallet does not hold the coin, so it has no short
-- id, no path and no time to show, as a coin of the wallet has
-- ('Haskoin.Wallet.Cli.PrettyPrinter.Payment.reportCoinDoc').
sweptCoinDoc :: Network -> AmountUnit -> UnsweptCoin -> Doc Style
sweptCoinDoc net unit left =
  vcat
    [ text (coinText left.outpoint),
      indent 2 ("at" <+> text (addrToString net left.address) <> "  " <> neutralAmount unit (fromIntegral left.value))
    ]

-- | What preparesweep says of the dust coins of a sweep, before it asks
-- whether to spend them too: the report of
-- 'Haskoin.Wallet.Cli.PrettyPrinter.Payment.dustReportDoc', on coins
-- the wallet does not hold. Each is shown as the sweep names it
-- (@sweptCoinDoc@).
sweepDustReportDoc :: Network -> AmountUnit -> DustReport -> Doc Style
sweepDustReportDoc net unit report =
  dustReportWith
    unit
    report
    [ vcat (sweptCoinDoc net unit left : (indent 2 <$> said))
      | (sized, said) <- dustCoinsSaid unit report,
        Just addr <- [sized.coin.address],
        let left = UnsweptCoin sized.coin.outpoint addr (fromIntegral sized.coin.value) WorthLessThanItsFee
    ]
