{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What the statements of the command report share: the balance
-- statement ("Haskoin.Wallet.Report.BalanceStatement") and the account
-- statement ("Haskoin.Wallet.Report.AccountStatement").
--
-- The two statements answer different questions. The first says what
-- the wallets held on a day. The second says what happened to them over
-- a period. They must still agree: the closing balance of an account
-- statement is the balance a balance statement gives for the same day.
-- So they count the transactions by one rule, which is kept here, and
-- neither has a copy of its own.
--
-- They also say the same things about themselves: who prepared them,
-- that the wallets are self-custody wallets, what they cover, which
-- blocks they were worked out from. A reader who has seen one statement
-- finds the same facts at the same place in the other.
--
-- = What is here
--
-- * The date a statement stops at (t'StatementDate'), and the rule that
--   says which transactions count up to it ('lastBlockCounted'). The
--   rule is explained in "Haskoin.Wallet.Report.BalanceStatement".
--
-- * What a statement is made from: the history of each portfolio
--   (t'PortfolioHistory') and the block the last sync reached
--   (t'SyncedTo').
--
-- * The prices the user gives (t'Valuation'), and the check that no
--   asset is left without one ('checkPrices').
--
-- * The reference of a statement ('referenceOf').
--
-- * The facts and the notes both statements state in the same words.
--
-- What a statement shows of the balances or of the transactions is not
-- here. Each statement has its own module for that.
module Haskoin.Wallet.Report.Statement
  ( -- * What a statement is made from
    StatementDate (..),
    PortfolioHistory (..),
    SyncedTo (..),
    BlockNamed (..),
    Valuation (..),
    walletsOf,
    statementTimes,

    -- * Which transactions count
    refuseStale,
    dayAt,
    confirmedIn,
    lastBlockCounted,
    unconfirmedLeftOut,
    syncedBlock,
    countedBlock,
    withBlockHash,

    -- * Prices
    checkPrices,
    priceOf,

    -- * The reference
    referenceOf,
    dateStated,
    valuationStated,

    -- * Naming things
    assetName,
    assetUnit,
    assetTitle,
    longDate,
    longTime,
    counted,
    inZone,

    -- * What both statements say of themselves
    holderFacts,
    preparationFacts,
    coverageFacts,
    valuationFacts,
    syncFacts,
    blockLines,
    preparationNotes,
    leftOutSentence,
    blockNote,
  )
where

import Control.Monad (forM_, unless)
import Crypto.Hash (SHA256 (..), hashWith)
import Data.Containers.ListUtils (nubOrd)
import Data.List (nub, (\\))
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (Day, LocalTime (..), TimeZone, UTCTime, ZonedTime (..), defaultTimeLocale, formatTime, utcToLocalTime)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Cli.Journal (satoshisText)
import Haskoin.Wallet.Cli.Sheet (ZoneAt, offsetText)
import Haskoin.Wallet.Database.Portfolio (PortfolioRef (..), WalletNaming (..), walletName)
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Report.Layout (Fact (..))
import Haskoin.Wallet.Report.Price (Price, priceText)
import Numeric.Natural (Natural)

{- What a statement is made from -}

-- | The date a statement stops at: the end of a day that is over, on the
-- clock of the computer, or the moment the statement is made. The end of
-- a day comes with the zone the clock has at that midnight, so that the
-- statement can say which midnight it means.
data StatementDate = AtEndOf !Day !TimeZone | AtPreparation
  deriving (Eq, Show)

-- | A portfolio as a statement reads it: how the answers name it, the
-- description of its wallet and its own, and its transactions on each
-- network hw shows, as the history gives them
-- ('Haskoin.Wallet.Database.TxInfo.wholeHistory')
data PortfolioHistory = PortfolioHistory
  { historyRef :: !PortfolioRef,
    historyWalletDescription :: !Text,
    historyDescription :: !Text,
    historyPools :: ![(Network, [ListedTx])]
  }
  deriving (Eq, Show)

-- | The block the last sync of a network reached, with its hash, and
-- the time of that block when the database has it
data SyncedTo = SyncedTo
  { syncedToHeight :: !BlockHeight,
    syncedToHash :: !BlockHash,
    syncedToTime :: !(Maybe UTCTime)
  }
  deriving (Eq, Show)

-- | The prices a statement values the balances at: the currency, the
-- price of a coin of each network that has one, and where the prices
-- come from, as the user says it, or nothing
data Valuation = Valuation
  { valuationCurrency :: !Text,
    valuationPrices :: ![(Network, Price)],
    valuationSource :: !Text
  }
  deriving (Eq, Show)

-- | A block as a statement names it: its height, its hash when it is
-- known, and the time of the block on the clock of the computer when it
-- is known
data BlockNamed = BlockNamed
  { blockNamedHeight :: !BlockHeight,
    blockNamedHash :: !(Maybe BlockHash),
    blockNamedTime :: !(Maybe LocalTime)
  }
  deriving (Eq, Show)

-- | The wallets of the portfolios, each once, in the order of their
-- indexes: its index, its description, and how many portfolios it has
walletsOf :: [PortfolioHistory] -> [(Natural, Text, Int)]
walletsOf histories =
  [ (index, description, length [() | history <- histories, refWallet (historyRef history) == index])
    | (index, description) <- Map.toAscList (Map.fromList [(refWallet (historyRef history), historyWalletDescription history) | history <- histories])
  ]

-- | Every time a statement reads a day or a time of day from, in UTC:
-- those of the confirmed transactions, and those of the blocks the last
-- syncs reached. The caller asks the computer for the zone at each
-- ('ZoneAt').
statementTimes :: [(Network, Maybe SyncedTo)] -> [PortfolioHistory] -> [UTCTime]
statementTimes networks histories =
  nubOrd $
    [time | (_, Just synced) <- networks, Just time <- [syncedToTime synced]]
      <> [ time
           | history <- histories,
             (_, transactions) <- historyPools history,
             Just (_, Just time) <- confirmedIn . listedTxInfo <$> transactions
         ]

{- Which transactions count -}

-- | Refuse a statement that stops at the end of a day when the last sync
-- of a network reached a block dated on that day or before. The wallet
-- then does not know what happened up to the end of the day, and the
-- statement would leave it out without a word.
refuseStale :: ZoneAt -> StatementDate -> Network -> Maybe SyncedTo -> Either String ()
refuseStale zoneAt date net synced =
  case (date, synced >>= syncedToTime) of
    (AtEndOf day _, Just time) ->
      unless (dayAt zoneAt time > day) . Left $
        "The last sync of "
          <> net.name
          <> " reached a block dated "
          <> cs (longDate (dayAt zoneAt time))
          <> ", and the statement is for the end of "
          <> cs (longDate day)
          <> ": it could miss transactions. Run hw sync first"
    _ -> Right ()

-- | The day a time falls on, on the clock of the computer
dayAt :: ZoneAt -> UTCTime -> Day
dayAt zoneAt time = localDay (utcToLocalTime (zoneAt time) time)

-- | The block of a live confirmed transaction: its height, and the time
-- of the block. Nothing for an unconfirmed transaction, for a dead one
-- and for one in preparation.
confirmedIn :: TxInfo -> Maybe (BlockHeight, Maybe UTCTime)
confirmedIn info = case txInfoStatus info of
  OnChain status
    | not (chainDead status),
      Store.BlockRef height _ <- chainBlock status ->
        Just (height, chainTime status)
  _ -> Nothing

-- | The last block a statement counts on a network, given the
-- transactions of every portfolio there: the highest block that is
-- dated on the day of the statement or before, among those of the
-- confirmed transactions. A statement of now counts every confirmed
-- transaction. Nothing when no transaction counts.
--
-- Every block up to that one counts, even one dated after the day: the
-- dates of the blocks do not always follow their order, and a statement
-- that left such a block out would show a state the chain never had.
lastBlockCounted :: ZoneAt -> StatementDate -> [TxInfo] -> Maybe BlockHeight
lastBlockCounted zoneAt date transactions =
  case [height | (height, timeM) <- mapMaybe confirmedIn transactions, datedInTime timeM] of
    [] -> Nothing
    heights -> Just (maximum heights)
  where
    datedInTime timeM = case date of
      AtPreparation -> True
      -- A block without a time cannot be said to be dated in time. It
      -- still counts when a later block is.
      AtEndOf day _ -> maybe False (\time -> dayAt zoneAt time <= day) timeM

-- | The unconfirmed transactions of a network: how many, each counted
-- once even when two portfolios see it, and what they add up to over
-- the portfolios. A payment from one portfolio to another adds up to
-- its fee.
unconfirmedLeftOut :: [ListedTx] -> (Int, Integer)
unconfirmedLeftOut transactions = (length (nubOrd (fst <$> unconfirmed)), sum (snd <$> unconfirmed))
  where
    unconfirmed =
      [ (chainTxid status, summaryAmount (txInfoSummary info))
        | info <- listedTxInfo <$> transactions,
          OnChain status <- [txInfoStatus info],
          not (chainDead status),
          Store.MemRef _ <- [chainBlock status]
      ]

-- | The block the last sync reached, as a statement names it, with its
-- time on the clock of the computer
syncedBlock :: ZoneAt -> SyncedTo -> BlockNamed
syncedBlock zoneAt to =
  BlockNamed (syncedToHeight to) (Just (syncedToHash to)) ((\time -> utcToLocalTime (zoneAt time) time) <$> syncedToTime to)

-- | A block a statement counts up to, at the height given, as the
-- statement names it. Its time is that of the transactions it holds,
-- among those given. Its hash is known when it is the block the last
-- sync reached, and is otherwise asked of the server ('withBlockHash').
countedBlock :: ZoneAt -> Maybe SyncedTo -> [TxInfo] -> BlockHeight -> BlockNamed
countedBlock zoneAt synced transactions height =
  BlockNamed
    height
    (listToMaybe [syncedToHash to | Just to <- [synced], syncedToHeight to == height])
    (listToMaybe [utcToLocalTime (zoneAt time) time | (confirmedAt, Just time) <- mapMaybe confirmedIn transactions, confirmedAt == height])

-- | A block of a network with the hash given for its network and its
-- height, among the hashes the server gave. A block whose hash is
-- already known keeps it, and one no hash is given for stays without.
withBlockHash :: [(Network, BlockHeight, BlockHash)] -> Network -> BlockNamed -> BlockNamed
withBlockHash hashes net block = case blockNamedHash block of
  Just _ -> block
  Nothing -> block {blockNamedHash = listToMaybe [hash | (hashNet, height, hash) <- hashes, hashNet == net, height == blockNamedHeight block]}

{- Prices -}

-- | Check the prices of a statement against the networks it covers and
-- those the wallets hold coins on. The prices are refused when one
-- names a network the statement does not cover, when a network has two
-- prices, and when an asset the wallets hold has no price: the total
-- value would leave that asset out and still look complete.
checkPrices :: Valuation -> [Network] -> [Network] -> Either String ()
checkPrices valuation covered held = do
  forM_ priced $ \net ->
    unless (net `elem` covered) . Left $
      "The statement does not cover " <> net.name <> ", which --price names: it covers " <> listed covered
  forM_ (priced \\ nub priced) $ \net ->
    Left ("--price names " <> net.name <> " twice: give one price for each network")
  forM_ held $ \net ->
    unless (net `elem` priced) . Left $
      "The wallets hold coins on " <> net.name <> ", which has no price: give one with --price " <> net.name <> "=PRICE, or the total value would leave them out"
  where
    priced = fst <$> valuationPrices valuation
    listed :: [Network] -> String
    listed networks = cs (Text.intercalate ", " [cs net.name | net <- networks])

-- | The price of an asset, when the statement has prices and one for it
priceOf :: Maybe Valuation -> Network -> Maybe Price
priceOf valuation net = listToMaybe [price | Just prices <- [valuation], (priced, price) <- valuationPrices prices, priced == net]

{- The reference -}

-- | The reference of a statement, given what it states as lines of
-- text in a fixed order: eight hexadecimal digits in two groups, as in
-- 3F9A-52C1. It is the start of the SHA-256 hash of the lines.
--
-- Two statements with the same reference state the same. When a tax
-- office asks about a statement, both sides can tell which one it is.
referenceOf :: [Text] -> Text
referenceOf stated = Text.toUpper (Text.take 4 digest <> "-" <> Text.take 4 (Text.drop 4 digest))
  where
    digest = Text.pack (show (hashWith SHA256 (encodeUtf8 (Text.unlines stated))))

-- | The date a statement stops at, as its reference states it. A
-- statement of now, made at the time given, is dated to the minute, so
-- each has a reference of its own.
dateStated :: ZonedTime -> StatementDate -> Text
dateStated prepared date = case date of
  AtEndOf day _ -> "end of " <> Text.pack (show day)
  AtPreparation -> "at " <> Text.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (zonedTimeToLocalTime prepared))

-- | The prices of a statement, as its reference states them: the
-- currency, then each price. No line without prices.
valuationStated :: Maybe Valuation -> [Text]
valuationStated valuation =
  concat
    [ ("currency " <> valuationCurrency prices)
        : [Text.unwords ["price", cs net.name, priceText price] | (net, price) <- valuationPrices prices]
      | Just prices <- [valuation]
    ]

{- Naming things -}

-- | The name of the asset of a network, as a reader outside of hw knows
-- it. A test network has no name of its own, and is named after the
-- word hw has for it.
assetName :: Network -> Text
assetName net = case net.name of
  "btc" -> "Bitcoin"
  "bch" -> "Bitcoin Cash"
  name -> "Test network " <> cs name

-- | The unit of the amounts of a network: its name in capitals, as in
-- BTC and BCH. The journal for ledger names its commodities the same
-- way.
assetUnit :: Network -> Text
assetUnit net = Text.toUpper (cs net.name)

-- | An asset with its unit, as in Bitcoin (BTC)
assetTitle :: Network -> Text
assetTitle net = assetName net <> " (" <> assetUnit net <> ")"

-- | A day written in full, as in 31 December 2025. The month is a word,
-- so that no reader takes the day for the month.
longDate :: Day -> Text
longDate = Text.pack . formatTime defaultTimeLocale "%-d %B %Y"

-- | A time on a clock as a day and a time of day, as in 15 November
-- 2026, 14:32
longTime :: LocalTime -> Text
longTime time = longDate (localDay time) <> ", " <> Text.pack (formatTime defaultTimeLocale "%H:%M" (localTimeOfDay time))

-- | A count with its noun, in the singular for one: 1 portfolio, 2
-- portfolios
counted :: Int -> Text -> Text
counted count noun = Text.pack (show count) <> " " <> noun <> (if count == 1 then "" else "s")

-- | The zone of a time, as a statement writes it after the time: a
-- space, then the offset in brackets, as in (UTC+01:00)
inZone :: TimeZone -> Text
inZone zone = " (" <> offsetText zone <> ")"

{- What both statements say of themselves -}

-- | The fact that names the holder, when the user gave one
holderFacts :: Text -> [Fact]
holderFacts holder = [Fact "Holder" [holder] | not (Text.null holder)]

-- | The facts about how a statement was made: when, by whom, its
-- reference, and that the wallets are self-custody wallets. A statement
-- looks like one a bank sends, and no bank made it, so it says who did.
preparationFacts :: ZonedTime -> Text -> [Fact]
preparationFacts prepared reference =
  [ Fact "Prepared on" [longTime (zonedTimeToLocalTime prepared) <> inZone (zonedTimeZone prepared)],
    Fact "Prepared by" ["The holder, from public blockchain data. Not audited."],
    Fact "Reference" [reference],
    Fact "Custody" ["Self-custody: the holder keeps the keys. No exchange or online wallet."]
  ]

-- | The facts about what a statement covers: its wallets, each with its
-- description and how many portfolios it has, and its assets. A wallet
-- is named only when the statement covers several, as in every view.
coverageFacts :: WalletNaming -> [(Natural, Text, Int)] -> [Network] -> [Fact]
coverageFacts naming wallets networks =
  [ Fact "Wallets" walletLines,
    Fact "Assets" [Text.intercalate ", " (assetTitle <$> networks)]
  ]
  where
    walletLines = case (naming, wallets) of
      (WithoutWallets, [(_, description, portfolios)]) ->
        ("1 wallet, " <> counted portfolios "portfolio") : [description | not (Text.null description)]
      _ ->
        [ walletName index <> ", " <> counted portfolios "portfolio" <> (if Text.null description then "" else ": " <> description)
          | (index, description, portfolios) <- wallets
        ]

-- | The fact about the prices of a statement, when it has some: their
-- currency, that the holder gave them, and where they came from
valuationFacts :: Maybe Valuation -> [Fact]
valuationFacts valuation =
  [ Fact "Valuation" (("In " <> valuationCurrency prices <> ", at the prices the holder gave") : ["Source of the prices: " <> valuationSource prices | not (Text.null (valuationSource prices))])
    | Just prices <- [valuation]
  ]

-- | The fact that says how far each network was synced: the block its
-- last sync reached, with its date, or that it was never synced
syncFacts :: [(Network, Maybe BlockNamed)] -> [Fact]
syncFacts networks = [Fact "Blockchain data" (syncedLine <$> networks)]
  where
    syncedLine (net, synced) = assetName net <> ": " <> case synced of
      Nothing -> "not synchronised"
      Just block -> "synchronised to block " <> shown (blockNamedHeight block) <> maybe "" (\time -> ", dated " <> longTime time) (blockNamedTime block)
    shown = Text.pack . show

-- | A block as the section Blockchain data names it, on two lines: its
-- height and its date, then its hash, or that the server did not give
-- it
blockLines :: BlockNamed -> [Text]
blockLines block =
  [ "Block " <> Text.pack (show (blockNamedHeight block)) <> maybe "" (\time -> ", dated " <> longTime time) (blockNamedTime block),
    maybe "Hash not available: the server did not give it" (\hash -> "Hash " <> blockHashToHex hash) (blockNamedHash block)
  ]

-- | The first notes of a statement: who prepared it, that the wallets
-- are self-custody wallets, and what it covers. A reader must not take
-- a statement for the confirmation of a third party, or its figures for
-- all that the holder owns.
preparationNotes :: [Text]
preparationNotes =
  [ "This statement was prepared by the holder of the wallets, from the data of public blockchains. No third party has audited or confirmed it.",
    "The wallets are self-custody wallets: the holder keeps the keys that control the assets. They are not accounts at an exchange, at a bank or with an online wallet service, and no third party holds the assets.",
    "The statement covers only the wallets it was prepared from. Assets the holder keeps elsewhere, such as at an exchange or in another wallet, are not part of it."
  ]

-- | The sentence of a note that says what the unconfirmed transactions
-- of a network would change, given how many a statement of now left out
-- and what they add up to. Nothing when it left none out.
leftOutSentence :: Network -> (Int, Integer) -> Maybe Text
leftOutSentence net leftOut = case leftOut of
  (0, _) -> Nothing
  (unconfirmed, amount) ->
    Just $
      "On "
        <> assetName net
        <> ", "
        <> counted unconfirmed "unconfirmed transaction"
        <> (if unconfirmed == 1 then " was" else " were")
        <> " left out, which would change the balance by "
        <> (if amount >= 0 then "+" else "")
        <> satoshisText amount
        <> " "
        <> assetUnit net
        <> "."

-- | The note that says how a statement names a block
blockNote :: Text
blockNote = "A block is named by its height and by its hash, which identifies it on the blockchain."
