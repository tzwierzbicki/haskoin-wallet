{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The answers of a sync: what each portfolio holds after it, what is new,
-- the networks that failed or that the server does not serve, and the
-- funds found at addresses of a kind hw cannot spend.
module Haskoin.Wallet.Cli.PrettyPrinter.Sync
  ( syncResultsPrinter,
    syncResultDoc,
    syncedAfterDoc,
    notServedDoc,
    failedNetworksDoc,
    foreignFundsDoc,
    syncAfterDocs,
  )
where

import Data.List (intercalate, intersperse, nub)
import Data.Text (Text)
import Haskoin
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Cli.PrettyPrinter.Balance (shortBalanceFigures)
import Haskoin.Wallet.Cli.PrettyPrinter.Base (amountPrinter, andList, block, commaList, counted, styled, text, withStyle)
import Haskoin.Wallet.Cli.PrettyPrinter.Place (placeLine, portfolioWords)
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Keys (foreignKindDescription)
import Haskoin.Wallet.ShortId (shortCoinText, shortHashText)
import Haskoin.Wallet.Sync
import Prettyprinter (Doc, indent, vcat, (<+>))

-- | Whether a sync is worth a block of its own: the portfolio holds
-- something on the network, or the sync changed something. sync goes over
-- every network of every portfolio, and most of them hold nothing.
syncWorthShowing :: SyncRes -> Bool
syncWorthShowing result =
  syncResCoins result > 0
    || syncResConfirmed result > 0
    || syncResUnconfirmed result > 0
    || syncResTxUpdates result > 0
    || syncResCoinUpdates result > 0
    || syncResRebroadcast result > 0
    || not (null (syncResNotPublished result))
    || syncResConflicted result > 0
    || syncResNewAddrs result > 0

-- | The syncs worth showing, each with its results, a blank line between
-- them, then one line for each portfolio that names its networks where
-- nothing is held and nothing changed. A sync goes over the portfolios of
-- every wallet, and each is named as the views name one
-- ('portfolioWords').
syncResultsPrinter :: WalletNaming -> UnitChoice -> [SyncRes] -> Doc Style
syncResultsPrinter naming choice results =
  vcat . intersperse mempty $ (syncResultDoc naming choice <$> shown) <> [vcat quietLines | not (null quietLines)]
  where
    shown = filter syncWorthShowing results
    quiet = filter (not . syncWorthShowing) results
    quietLines =
      [ (text (portfolioWords naming ref) `withStyle` Account) <> "  " <> styled Muted ("nothing on " <> andList [(placeNetwork (syncResPlace r)).name | r <- quiet, placePortfolio (syncResPlace r) == ref])
        | ref <- nub (placePortfolio . syncResPlace <$> quiet)
      ]

-- | A portfolio synced on a network: where it is, what it holds there,
-- the block it is synced to, and what changed, one kind of change a
-- line. What did not change is left out, and a sync that changed nothing
-- says so.
syncResultDoc :: WalletNaming -> UnitChoice -> SyncRes -> Doc Style
syncResultDoc naming choice result =
  vcat $ placeLine naming (syncResPlace result) : (indent 2 <$> syncHoldsDoc unit result : synced : changes)
  where
    unit = syncedUnit choice result
    synced
      | null changes = "synced at block" <+> height <> ", nothing new"
      | otherwise = "synced at block" <+> height
    height = text (show (syncResBlockHeight result))
    changes = updated <> syncNewsDocs unit result
    updated = case (syncResTxUpdates result, syncResCoinUpdates result) of
      (0, 0) -> []
      (t, 0) -> [text (counted t "transaction" "transactions") <+> "updated"]
      (0, c) -> [text (counted c "coin" "coins") <+> "updated"]
      (t, c) -> [text (counted t "transaction" "transactions" <> " and " <> counted c "coin" "coins") <+> "updated"]

-- | A portfolio as the sync that follows a sending leaves it
-- ('syncAfterDocs'): where it is and what it holds, then what the sync
-- has to tell (@syncNewsDocs@). The block of a sync
-- ('syncResultDoc') has two more lines, the block reached and the count
-- of what was updated. They say that a sync was done, which the user of
-- send did not ask about, and the payment itself always updates
-- something.
--
-- The news stays. A sync tells each of them once: a lock that moved to
-- the coin of a replacement just sent, or a coin with tokens that came
-- since the last sync, would otherwise never be told.
syncedAfterDoc :: WalletNaming -> UnitChoice -> SyncRes -> Doc Style
syncedAfterDoc naming choice result =
  vcat $ placeLine naming (syncResPlace result) : (indent 2 <$> syncHoldsDoc unit result : syncNewsDocs unit result)
  where
    unit = syncedUnit choice result

-- | The unit of the amounts of a portfolio synced on a network: that of
-- its network ('unitOn'). A sync goes over several networks.
syncedUnit :: UnitChoice -> SyncRes -> AmountUnit
syncedUnit choice result = unitOn (placeNetwork (syncResPlace result)) choice

-- | What a portfolio holds on a network once synced: what its coins are
-- worth and what it can spend ('shortBalanceFigures'), and how many
-- coins
syncHoldsDoc :: AmountUnit -> SyncRes -> Doc Style
syncHoldsDoc unit result = commaList (shortBalanceFigures unit (syncResTotal result) (syncResSpendable result) <> [coins])
  where
    coins = text (counted (fromIntegral (syncResCoins result)) "coin" "coins")

-- | What a sync has to tell beyond its counts, a line or two for each:
-- the locks that moved, the coins with tokens that came, the
-- transactions sent again and those that could not be, the pending
-- transactions found in conflict, and the addresses found
syncNewsDocs :: AmountUnit -> SyncRes -> [Doc Style]
syncNewsDocs unit result = locksMoved <> withTokens <> sentAgain <> notPublished <> conflicted <> found
  where
    -- A coin the user locked is gone, its payment replaced, and the coin
    -- of the replacement is locked: both short ids, since unlockcoin now
    -- takes the second
    locksMoved =
      [ text ("lock moved from " <> shortCoinText old <> " to " <> shortCoinText heir <> ", the coin that replaced it")
        | (old, heir) <- syncResLocksMoved result
      ]
    -- A coin that holds tokens of Bitcoin Cash arrived. hw locks it for
    -- good, so the lines say how many came, what they hold, and what
    -- moves them: no command of hw does.
    withTokens = case syncResTokenCoins result of
      [] -> []
      values ->
        [ text (counted (fromIntegral (length values)) "new coin holds" "new coins hold" <> " tokens (CashTokens):")
            <+> amountPrinter unit (sum values)
            <> ", which hw never spends",
          indent 2 . text $ "a wallet that handles tokens moves " <> (if length values == 1 then "it" else "them") <> ", with the same mnemonic"
        ]
    sentAgain = [text (counted r "transaction" "transactions") <+> "sent again" | let r = syncResRebroadcast result, r > 0]
    -- A transaction the store did not publish stays pending: its short
    -- id, to name it in deletetx, then what the store said, whole
    notPublished =
      [ styled Failure (shortHashText txid <> " not sent again") <> ":" <+> text why
        | (txid, why) <- syncResNotPublished result
      ]
    -- A conflicted transaction can no longer be sent
    conflicted = [styled Failure (counted k "pending transaction" "pending transactions" <> " conflicted") | let k = syncResConflicted result, k > 0]
    found = [text (counted n "new address" "new addresses") <+> "found" | let n = syncResNewAddrs result, n > 0]

-- | The networks the host does not serve, in one line: no failure, since
-- no host serves every network
notServedDoc :: [Network] -> Doc Style
notServedDoc nets = styled Muted ("Not served by the store: " <> intercalate ", " ((.name) <$> nets))

-- | The networks a sync or a search left out because their store is down,
-- a line for each with the failure of its store. The word says what was
-- not done there: "not synced", or "left out" for recover, whose
-- network may fail during the search or during the syncs after it.
--
-- A failure is wrapped as an error is ('wrappedAfter'), after the network
-- and the word: its sentences break between words, and a command to copy
-- that it gives on a line of its own stays whole.
failedNetworksDoc :: String -> [(Network, Text)] -> Doc Style
failedNetworksDoc notDone nets =
  vcat [wrappedAfter (text net.name <> "  " <> styled Failure (notDone <> ":")) (net.name <> "  " <> notDone <> ":") failure | (net, failure) <- nets]

-- | The funds of a seed at accounts hw does not hold, as recover shows
-- them: a line for each account, with its network, its path, its kind, how
-- many of its addresses were used and what they hold, then what to do with
-- the funds. The network comes first, as on the line of an account. The
-- path alone does not tell it, since m\/44'\/0' is searched on Bitcoin Cash
-- too.
foreignFundsDoc :: UnitChoice -> [(Network, ForeignFunds)] -> Doc Style
foreignFundsDoc choice elsewhere =
  vcat $
    "The seed was paid at paths hw cannot spend from:"
      : (indent 2 . fundsLine <$> elsewhere)
      <> [indent 2 "A wallet that holds such accounts can move the funds to an address of this wallet."]
  where
    netOf (net, _) = net.name
    pathOf (net, funds) = pathToStr (foreignPath net funds)
    -- The widest of the column, so that the next one lines up
    widthOf shown = fromIntegral $ maximum (0 : (length . shown <$> elsewhere))
    fundsLine entry@(net, funds) =
      text (block (widthOf netOf) (netOf entry))
        <> "  "
        <> text (block (widthOf pathOf) (pathOf entry))
        <> "  "
        <> text (foreignKindDescription (foreignKind funds) <> ": " <> counted (foreignUsed funds) "address" "addresses" <> " used, ")
        <> left net (foreignBalance funds)
    left _ 0 = "nothing left"
    left net balance = amountPrinter (unitOn net choice) balance <+> "left"

-- | What follows the answer of a sending, each part after a blank line:
-- the portfolios the transaction touches as the sync left them
-- ('syncedAfterDoc'), then why the sync did not go through, when it did
-- not. Nothing for a transaction that touches no portfolio.
--
-- The failure starts with what matters most to a user who reads the word
-- \"failed\" below a payment: the transaction is sent.
syncAfterDocs :: WalletNaming -> UnitChoice -> SyncAfter -> [Doc Style]
syncAfterDocs naming choice after =
  (syncedAfterDoc naming choice <$> after.syncedAfter)
    <> [wrappedAfter (text sent) sent failure | Just failure <- [after.notSyncedAfter]]
  where
    sent = "The transaction is sent, and the wallet could not sync after it:"
