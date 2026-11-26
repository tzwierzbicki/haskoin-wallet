{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What the commands that prepare a payment say before they ask: the
-- payment prepared, the coins left out, the dust coins and the tags a
-- payment would mix. And the fee rates that fees shows.
module Haskoin.Wallet.Cli.PrettyPrinter.Payment
  ( preparedDoc,
    reportCoinDoc,
    dustReportDoc,
    dustCoinsSaid,
    dustReportWith,
    mixingDoc,
    feeRatesDoc,
  )
where

import Control.Monad
import Data.List (genericLength, intersperse, nub)
import qualified Data.Map.Strict as Map
import Data.String.Conversions (cs)
import Data.Time (UTCTime, diffUTCTime, utcToLocalZonedTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Cli.PrettyPrinter.Base (agoText, cellsLine, commaList, counted, neutralAmount, rateDoc, rblock, sentence, shownWidth, styled, tagMarks, text, valueText, wordsOf)
import Haskoin.Wallet.Cli.PrettyPrinter.Coin (compactCoinDoc)
import Haskoin.Wallet.Cli.PrettyPrinter.Place (PathNaming (..), SeenFrom (..), ownPathsAlone, pathsSeenFrom)
import Haskoin.Wallet.Cli.PrettyPrinter.Transaction (txAnswerDoc)
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Tags (ShownTag (..), tagText)
import Haskoin.Wallet.Tx.Dust (DustPlace (..), DustReport (..), SizedCoin (..), inputFee, valueOf, worthSpendingUpTo)
import Haskoin.Wallet.Tx.FeeRates (FeeRates (..), levelName, rateAt)
import Haskoin.Wallet.Tx.Mixing (SpentGroup (..))
import Prettyprinter (Doc, hsep, indent, vcat, (<+>))

{- The questions of the commands that prepare a transaction -}

-- The reports the commands show on stderr before they ask. A sentence
-- wraps at the width of the screen, as the help does. A coin is shown as
-- coins shows it, then what the report says of it ('reportCoinDoc'). The
-- amounts are in the unit asked for.

-- | A transaction of a portfolio just prepared, as preparetx,
-- preparesendall and bumpfee answer with it: the transaction with every
-- detail ('txAnswerDoc'), then the coins it leaves in the portfolio, if
-- any, below it and apart from it (@leftCoinsDoc@). send and sendall show
-- the same before they ask for the mnemonic, so that the user reads there
-- what preparetx or preparesendall would have answered. The last argument
-- is the active portfolio, which may be another one than that of the
-- transaction.
preparedDoc :: WalletNaming -> UnitChoice -> Place -> TxInfo -> [LeftCoin] -> Maybe PortfolioRef -> IO (Doc Style)
preparedDoc naming choice place info left activeM = do
  let seen = SeenFrom (placePortfolio place) activeM
      net = placeNetwork place
      unit = unitOn net choice
  txDoc <- txAnswerDoc naming net (Just seen) choice info
  leftDocs <-
    if null left
      then return []
      else (\leftDoc -> [mempty, leftDoc]) <$> leftCoinsDoc (pathsSeenFrom naming seen) net unit left
  return (vcat (txDoc : leftDocs))

-- | The coins a transaction just prepared leaves in the portfolio on its
-- network, each with why it stays ('reportCoinDoc'), a blank line between
-- them. Their paths are written as those of the transaction above them
-- are.
leftCoinsDoc :: PathNaming -> Network -> AmountUnit -> [LeftCoin] -> IO (Doc Style)
leftCoinsDoc paths net unit left = do
  coins <- forM left $ \coin -> reportCoinDoc paths net unit [text (leftCoinWhy coin)] coin.leftCoin
  return . vcat . intersperse mempty $
    text (counted (fromIntegral (length left)) "coin stays" "coins stay" <> " in the portfolio:") : coins

-- | A coin in a report, before a question or after a transaction just
-- prepared: as the compact view of coins shows it, then what the report
-- says of it, a line each. A coin looks the same wherever hw shows it, and
-- the user tells it by its short id, its time, its value and its address,
-- as coins lists it. Its path names its portfolio when the report is on
-- another portfolio than the active one (the type t'PathNaming'), as a
-- replacement by bumpfee may be.
reportCoinDoc :: PathNaming -> Network -> AmountUnit -> [Doc Style] -> JsonCoin -> IO (Doc Style)
reportCoinDoc paths net unit said coin = do
  time <- traverse utcToLocalZonedTime (jsonCoinTime coin)
  return $ vcat (compactCoinDoc paths net unit time coin : (indent 2 <$> said))

-- | The report on the coins worth less than their fee, as preparesendall
-- shows it before its question: the coins, each as coins shows it among
-- the views given, with the rates it is worth spending at, then what
-- spending them and leaving them does. preparesendall works in the active
-- portfolio, so the paths are alone ('ownPathsAlone').
dustReportDoc :: Network -> AmountUnit -> [JsonCoin] -> DustReport -> IO (Doc Style)
dustReportDoc net unit views report =
  dustReportWith unit report
    <$> sequence [reportCoinDoc ownPathsAlone net unit said view | (sized, said) <- dustCoinsSaid unit report, Just view <- [Map.lookup sized.coin.outpoint byOutpoint]]
  where
    byOutpoint = Map.fromList [(jsonCoinOutpoint view, view) | view <- views]

-- | The dust coins of a report, each with what the report says of it: the
-- fee of its input when the dust coins do not all cost the same, since the
-- header then gives none, and the fee rates at which it is worth spending
dustCoinsSaid :: AmountUnit -> DustReport -> [(SizedCoin, [Doc Style])]
dustCoinsSaid unit report = [(sized, [hsep (ownFee sized <> [worthAt sized])]) | sized <- report.dustCoins]
  where
    severalFees = length (nub (inputFee report.rate <$> report.dustCoins)) > 1
    ownFee sized = ["costs" <+> neutralAmount unit (fromIntegral (inputFee report.rate sized)) <> "," | severalFees]
    worthAt sized = case worthSpendingUpTo sized of
      0 -> "worth less than its fee at any rate"
      upTo -> "worth spending at" <+> rateDoc upTo <+> "or less"

-- | The report on the dust coins around the coins as their command shows
-- them: how many they are and what each costs, the coins, then what
-- spending them and leaving them does, with what remains for the
-- recipients either way. Where the coins are (the type t'DustPlace') words
-- what the two choices leave behind.
dustReportWith :: AmountUnit -> DustReport -> [Doc Style] -> Doc Style
dustReportWith unit report coins =
  vcat . intersperse mempty $ [header] <> coins <> [vcat [spent, left]]
  where
    count = length report.dustCoins
    one = count == 1
    them = if one then "it" else "them"
    their = if one then "its address" else "their addresses"
    amount v = neutralAmount unit (fromIntegral v)
    -- The fee of an input depends on the kind of its coin. The header
    -- gives it when the dust coins all cost the same, and each coin
    -- gives its own otherwise.
    header = case nub (inputFee report.rate <$> report.dustCoins) of
      [fee]
        | one -> sentence $ wordsOf "1 coin costs more to spend than it is worth at" <> [rateDoc report.rate <> ",", amount fee <> ":"]
        | otherwise -> sentence $ wordsOf (show count <> " coins cost more to spend than they are worth at") <> [rateDoc report.rate <> ",", amount fee] <> wordsOf "each:"
      _ -> sentence $ wordsOf (show count <> " coins cost more to spend than they are worth at") <> [rateDoc report.rate <> ":"]
    dustValue = valueOf report.dustCoins
    spent =
      sentence $
        wordsOf ("Spending " <> them <> " adds")
          <> [amount report.addedFee]
          <> wordsOf "of fee for"
          <> [amount dustValue]
          <> wordsOf "of coins. What remains for the addresses without an amount is"
          <> [amount report.withDust]
          <> wordsOf "instead of"
          <> [amount report.withoutDust <> ",", amount (report.withoutDust - report.withDust)]
          <> wordsOf ("less" <> emptied)
    -- A portfolio that spends its dust coins too may have no coin left
    emptied = case report.place of
      InPortfolio False -> ", and the portfolio has no coin left on the network."
      _ -> "."
    left = sentence $ kept <> wordsOf "Once the other coins are gone," <> later
    kept = case report.place of
      InPortfolio _ -> wordsOf ("Leaving " <> them <> ", the portfolio keeps") <> [amount dustValue <> "."]
      AtSweptAddresses -> wordsOf ("Leaving " <> them <> " keeps") <> [amount dustValue] <> wordsOf ("at " <> their <> ".")
    later = case report.laterRate of
      Just upTo
        | one -> wordsOf "it can only be spent at" <> [rateDoc upTo] <> wordsOf "or less."
        | otherwise -> wordsOf "they can only be spent together, at" <> [rateDoc upTo] <> wordsOf "or less."
      Nothing -> wordsOf ((if one then "it" else "they") <> " can never be spent: any fee would leave less than") <> [amount report.dustLimit <> "."]

-- | What a command says of a transaction that joins groups of coins,
-- before it asks whether to go on ("Haskoin.Wallet.Tx.Mixing"): each
-- group on a line, with its tags, how many of its coins the transaction
-- spends and what they are worth, then why it matters. The coins at
-- addresses without a tag are a group too, named "no tag". The values are
-- right-aligned, so that their points line up.
--
-- The groups are told and not the coins: the transaction is shown after
-- the question, with the tags of each address it spends from under it.
mixingDoc :: AmountUnit -> [SpentGroup] -> Doc Style
mixingDoc unit groups =
  vcat $
    [sentence (wordsOf "The transaction spends coins that do not all have the same tags:")]
      <> [indent 2 (cellsLine (cells spent)) | spent <- groups]
      <> [sentence (wordsOf "A transaction shows that its coins have one owner, so it joins these groups.")]
  where
    cells spent =
      [ (widest groupName, groupName spent, nameDoc spent),
        (widest groupCount, groupCount spent, text (groupCount spent)),
        (widest groupWorth, worth, text worth)
      ]
      where
        worth = rblock (fromIntegral (widest groupWorth)) (groupWorth spent)
    -- The text of the tags is what 'tagMarks' shows, without its colours
    groupName spent
      | null spent.spentGroupTags = "no tag"
      | otherwise = unwords ["[" <> cs (tagText (shownTag shown)) <> "]" | shown <- spent.spentGroupTags]
    nameDoc spent
      | null spent.spentGroupTags = styled Muted "no tag"
      | otherwise = hsep (tagMarks spent.spentGroupTags)
    groupCount spent = counted (genericLength spent.spentGroupCoins) "coin" "coins"
    groupWorth spent = valueText unit spent.spentGroupValue
    widest :: (SpentGroup -> String) -> Int
    widest cell = maximum (0 : map (shownWidth . cell) groups)

{- Fee rates -}

-- | The fee rates recent blocks paid on the network, with how fresh they
-- are: the age of the latest of those blocks, which also tells when the
-- network is slow. A latest block mined an hour ago or more waits: its
-- rates may be too low, as the help of fees says.
feeRatesDoc :: Network -> UTCTime -> FeeRates -> Doc Style
feeRatesDoc net now rates =
  vcat
    [ text ("Fee rates of " <> net.name <> ", from " <> blocks),
      indent 2 $ commaList [text (cs (levelName level)) <+> text (show (rateAt level rates)) | level <- [minBound .. maxBound]] <+> "sat/vB",
      indent 2 $ "the latest block was mined" <+> age
    ]
  where
    latest = posixSecondsToUTCTime (fromIntegral rates.ratesLatestTime)
    age
      | diffUTCTime now latest >= 3600 = styled Waiting (agoText now latest)
      | otherwise = text (agoText now latest)
    blocks
      | rates.ratesFromHeight == rates.ratesToHeight = "block " <> show rates.ratesToHeight
      | otherwise = "blocks " <> show rates.ratesFromHeight <> " to " <> show rates.ratesToHeight
