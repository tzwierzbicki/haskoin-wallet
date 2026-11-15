{-# LANGUAGE OverloadedStrings #-}

-- | The journal exportjournal writes for the ledger command line tool: an
-- entry for each live transaction of the wallet, whose postings add up to
-- zero, the declarations of what the file uses, and the balances at the
-- end.
module Haskoin.Wallet.Cli.JournalSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (TimeZone (..), UTCTime (..), fromGregorian)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Cli.Journal (commodityOf, journalSafe, journalText, portfolioAccount, satoshisText)
import Haskoin.Wallet.Contacts (Contact (..))
import Haskoin.Wallet.Database.Portfolio (Place (..), PortfolioRef (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Keys (legacyPath)
import Haskoin.Wallet.Tags (ShownTag (..))
import Haskoin.Wallet.TestUtils
import Test.Hspec

-- Every transaction below has the time seenTime, 14 Nov 2023 22:13:20
-- UTC, which a clock of Central Europe in winter reads as 23:13:20 on the
-- same day. The journals are written the next morning, on 15 Nov 2023 at
-- 10:00:00 UTC, unless a test says otherwise. The txid of fakeTxHash n is
-- 62 zeros and n in hexadecimal, so every short id is 00000000. The
-- transactions are those of portfolio0 of wallet0 on btc, mainPlace,
-- unless a test says otherwise.
--
-- The story is that of the shared summaries. The portfolio receives 50000
-- at /0/0 in block 800000. It then spends that coin in block 800001:
-- 40000 to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU, 9000 of change and 1000 of
-- fee, so 41000 leave it and 9000 are left.

-- | The morning after 'seenTime': 15 Nov 2023 10:00:00 UTC
nextMorning :: UTCTime
nextMorning = UTCTime (fromGregorian 2023 11 15) (10 * 3600)

-- | The journal of the rows, written the next morning on a clock of
-- Central Europe in winter
cetJournal :: [(Place, ListedTx)] -> Text
cetJournal = journalText (const cet) nextMorning

-- | The paragraphs of a journal: its lines, apart at each empty line
paragraphs :: Text -> [[Text]]
paragraphs = filter (not . null) . map Text.lines . Text.splitOn "\n\n"

-- | The entries of the transactions of a journal, each as its lines: the
-- paragraphs that start with a date, but the balances at the end
txEntries :: Text -> [[Text]]
txEntries journal =
  [ entry
    | entry@(first : _) <- paragraphs journal,
      Text.isPrefixOf "20" first,
      not (Text.isSuffixOf "Balances of hw" first)
  ]

-- | The entry of the balances of a journal, as its lines
balances :: Text -> [[Text]]
balances journal = [entry | entry@(first : _) <- paragraphs journal, Text.isSuffixOf "Balances of hw" first]

-- | The payment received, confirmed in block 800000, with the label given
received :: Text -> ListedTx
received label = ListedTx (txOnChain paymentSummary 1 (Store.BlockRef 800000 1) 3 False label) (Just 50000)

-- | The payment made, confirmed in block 800001, after which 9000 are left
spent :: ListedTx
spent = ListedTx (txOnChain spendSummary 2 (Store.BlockRef 800001 1) 2 False "") (Just 9000)

spec :: Spec
spec = do
  describe "An amount of the journal" $ do
    it "is a number of coins with eight decimals" $
      satoshisText 50000 `shouldBe` "0.00050000"

    it "has a minus sign in front when it is below zero" $
      -- 120000000 satoshis = 1.2 coins
      satoshisText (-120000000) `shouldBe` "-1.20000000"

    it "is written in full for every coin there will ever be" $
      -- 21 million coins of 100000000 satoshis each
      satoshisText 2100000000000000 `shouldBe` "21000000.00000000"

    it "is zero with all its decimals" $
      satoshisText 0 `shouldBe` "0.00000000"

  describe "The commodity of a network" $ do
    it "is the name of the network in capitals" $
      (commodityOf btc, commodityOf bch, commodityOf btcTest) `shouldBe` ("BTC", "BCH", "BTCTEST")

    it "is in double quotes when the name has a digit, which ledger asks for" $
      commodityOf btcTest4 `shouldBe` "\"BTCTEST4\""

  describe "The account of a portfolio" $
    it "names its wallet and the portfolio by their indexes" $
      portfolioAccount (PortfolioRef 1 2) `shouldBe` "Assets:hw:wallet1:portfolio2"

  describe "A text from outside written in the journal" $ do
    it "stays as it is when it is plain" $
      journalSafe "Rent of March" `shouldBe` "Rent of March"

    it "keeps to one line: a line break becomes a space" $
      journalSafe "Rent\n2023-01-01 * Forged\r\n    Assets:Bank  1000 EUR" `shouldBe` "Rent 2023-01-01 * Forged Assets:Bank 1000 EUR"

    it "has single spaces alone: two spaces or a tab would end the name of a payee" $
      journalSafe "Rent  ; :forged:\tmore" `shouldBe` "Rent ; :forged: more"

    it "has no space at either end" $
      journalSafe "  Rent \n" `shouldBe` "Rent"

    it "writes a control character as an escape" $
      journalSafe "Rent\ESC[2J" `shouldBe` "Rent\\x1b[2J"

  describe "The entry of a transaction" $ do
    -- The payer paid the fee of 1000: the portfolio gains the 50000 it
    -- was paid, which come from someone hw does not know
    it "posts a payment received to the account of the portfolio, from Income:Unknown, without the fee of its payer" $
      txEntries (cetJournal [(mainPlace, received "Salary")])
        `shouldBe` [ [ "2023-11-14 * (00000000) Salary",
                       "    ; txid: 0000000000000000000000000000000000000000000000000000000000000001",
                       "    ; time: 2023-11-14 22:13:20 UTC",
                       "    ; block: 800000",
                       "    Assets:hw:wallet0:portfolio0   0.00050000 BTC",
                       "    Income:Unknown                -0.00050000 BTC"
                     ]
                   ]

    -- 40000 paid + 1000 of fee = 41000 leave the portfolio
    it "posts a payment made to Expenses:Unknown with the address paid, its fee to Expenses:Fees:Mining, and both from the account of the portfolio" $
      txEntries (cetJournal [(mainPlace, spent)])
        `shouldBe` [ [ "2023-11-14 * (00000000) Payment sent",
                       "    ; txid: 0000000000000000000000000000000000000000000000000000000000000002",
                       "    ; time: 2023-11-14 22:13:20 UTC",
                       "    ; block: 800001",
                       "    Expenses:Unknown               0.00040000 BTC",
                       "    ; address: 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                       "    Expenses:Fees:Mining           0.00001000 BTC",
                       "    Assets:hw:wallet0:portfolio0  -0.00041000 BTC"
                     ]
                   ]

    -- The batch pays 60000 and 30000 with 1000 of fee: 91000 leave
    it "gives a posting to each address a payment pays, the largest first" $ do
      let batch = ListedTx (txOnChain batchSummary 2 (Store.BlockRef 800001 1) 2 False "") (Just 9000)
      drop 4 <$> txEntries (cetJournal [(mainPlace, batch)])
        `shouldBe` [ [ "    Expenses:Unknown               0.00060000 BTC",
                       "    ; address: 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                       "    Expenses:Unknown               0.00030000 BTC",
                       "    ; address: 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ",
                       "    Expenses:Fees:Mining           0.00001000 BTC",
                       "    Assets:hw:wallet0:portfolio0  -0.00091000 BTC"
                     ]
                   ]

    -- The portfolio pays itself, and loses the fee of 1000 alone
    it "posts a transaction between the addresses of a portfolio as its fee alone, and calls it an internal transfer" $ do
      let internal = ListedTx (txOnChain internalSummary 2 (Store.BlockRef 800001 1) 2 False "") (Just 49000)
      (take 1 <$> txEntries (cetJournal [(mainPlace, internal)]), drop 4 <$> txEntries (cetJournal [(mainPlace, internal)]))
        `shouldBe` ( [["2023-11-14 * (00000000) Internal transfer"]],
                     [ [ "    Expenses:Fees:Mining           0.00001000 BTC",
                         "    Assets:hw:wallet0:portfolio0  -0.00001000 BTC"
                       ]
                     ]
                   )

    -- portfolio0 pays 40000 to an address of portfolio1 with 1000 of fee.
    -- portfolio0 sees a payment of 41000, and portfolio1 a payment
    -- received of 40000 at its address labelled "Holidays". The wallet
    -- as a whole lost the fee, and nothing else.
    it "is one entry for a payment from a portfolio to another: the money moves between their two accounts, and the fee is the only expense" $ do
      let seenByPortfolio1 =
            paymentSummary
              { summaryAmount = 40000,
                summaryMyOutputs = Map.fromList [(otherAddr 0, MyOutputs 40000 (legacyPath (Deriv :/ 0 :/ 4)) "Holidays")],
                summaryOtherOutputs = Map.fromList [(internalAddr 0, 9000)],
                summaryOtherInputs = Map.fromList [(externalAddr 0, 50000)]
              }
          rows =
            [ (mainPlace, ListedTx paidToPortfolio1 (Just 9000)),
              (Place (PortfolioRef 0 1) btc, ListedTx (txOnChain seenByPortfolio1 2 (Store.BlockRef 800001 1) 2 False "") (Just 40000))
            ]
      txEntries (cetJournal rows)
        `shouldBe` [ [ "2023-11-14 * (00000000) Holidays",
                       "    ; txid: 0000000000000000000000000000000000000000000000000000000000000002",
                       "    ; time: 2023-11-14 22:13:20 UTC",
                       "    ; block: 800001",
                       "    Assets:hw:wallet0:portfolio1   0.00040000 BTC",
                       "    Expenses:Fees:Mining           0.00001000 BTC",
                       "    Assets:hw:wallet0:portfolio0  -0.00041000 BTC"
                     ]
                   ]

    -- Another wallet adds a coin of 10000 to the inputs. hw cannot tell
    -- who paid the fee of 1000, so the 41000 the portfolio lost are all
    -- posted as paid to others.
    it "has no posting for the fee of a transaction made with coins of another wallet: what left the portfolio is posted to Expenses:Unknown" $ do
      let shared = spendSummary {summaryOtherInputs = Map.fromList [(otherAddr 1, 10000)]}
          row = ListedTx (txOnChain shared 2 (Store.BlockRef 800001 1) 2 False "") (Just 9000)
      drop 4 <$> txEntries (cetJournal [(mainPlace, row)])
        `shouldBe` [ [ "    Expenses:Unknown               0.00041000 BTC",
                       "    Assets:hw:wallet0:portfolio0  -0.00041000 BTC"
                     ]
                   ]

    it "is pending, with an exclamation mark and no block, while its transaction is unconfirmed" $ do
      let unconfirmed = ListedTx (txOnChain paymentSummary 1 (Store.MemRef 1700000000) 0 False "") (Just 50000)
      take 4 <$> txEntries (cetJournal [(mainPlace, unconfirmed)])
        `shouldBe` [ [ "2023-11-14 ! (00000000) Payment received",
                       "    ; txid: 0000000000000000000000000000000000000000000000000000000000000001",
                       "    ; time: 2023-11-14 22:13:20 UTC",
                       "    Assets:hw:wallet0:portfolio0   0.00050000 BTC"
                     ]
                   ]

    it "is left out for a dead transaction, whose money never moved" $ do
      let dead = ListedTx (txOnChain spendSummary 2 (Store.MemRef 1700000000) 0 True "") Nothing
      txEntries (cetJournal [(mainPlace, dead), (mainPlace, received "")]) `shouldSatisfy` ((== 1) . length)

    -- 22:13:20 UTC is 00:13:20 of the next day two hours ahead of UTC
    it "has the date of its transaction on the clock of the computer, and the time in UTC in a note" $ do
      let twoHoursAhead = TimeZone 120 False "EET"
      take 3 <$> txEntries (journalText (const twoHoursAhead) nextMorning [(mainPlace, received "")])
        `shouldBe` [ [ "2023-11-15 * (00000000) Payment received",
                       "    ; txid: 0000000000000000000000000000000000000000000000000000000000000001",
                       "    ; time: 2023-11-14 22:13:20 UTC"
                     ]
                   ]

    it "gives the tags of its transaction on a line, between colons, in the order of the alphabet" $ do
      let tagged = (txOnChain spendSummary 2 (Store.BlockRef 800001 1) 2 False "") {txInfoTags = noTxTags {txOwnTags = [ShownTag rentTag Nothing, ShownTag kycTag Nothing]}}
      [line | line <- concat (txEntries (cetJournal [(mainPlace, ListedTx tagged (Just 9000))])), Text.isInfixOf ":kyc:" line]
        `shouldBe` ["    ; :kyc:rent:"]

  describe "The name of an entry" $ do
    let firstLines rows = [first | first : _ <- txEntries (cetJournal rows)]
        landlord = Contact "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" "Landlord" [] seenTime
        toLandlord label = (txOnChain spendSummary 2 (Store.BlockRef 800001 1) 2 False label) {txInfoContacts = Map.fromList [(otherAddr 0, landlord)]}

    it "is the label of the transaction" $
      firstLines [(mainPlace, received "Salary")] `shouldBe` ["2023-11-14 * (00000000) Salary"]

    it "is the label of the contact a payment pays, when the transaction has no label" $
      firstLines [(mainPlace, ListedTx (toLandlord "") (Just 9000))] `shouldBe` ["2023-11-14 * (00000000) Landlord"]

    it "is the label of the transaction rather than that of the contact, when it has both" $
      firstLines [(mainPlace, ListedTx (toLandlord "Rent of March") (Just 9000))] `shouldBe` ["2023-11-14 * (00000000) Rent of March"]

    it "comes with the label of the contact below the posting of what the contact was paid" $
      take 3 . drop 4 <$> txEntries (cetJournal [(mainPlace, ListedTx (toLandlord "Rent of March") (Just 9000))])
        `shouldBe` [ [ "    Expenses:Unknown               0.00040000 BTC",
                       "    ; address: 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                       "    ; contact: Landlord"
                     ]
                   ]

    it "is the label of the address a payment was received at, when the transaction has no label" $
      firstLines [(mainPlace, ListedTx (txOnChain paymentToRent 1 (Store.BlockRef 800000 1) 3 False "") (Just 50000))]
        `shouldBe` ["2023-11-14 * (00000000) Rent"]

    -- The change of the payment comes back to an address labelled
    -- "Savings", which says nothing of whom the payment went to
    it "is not the label of the address the change of a payment comes back to" $ do
      let changeToSavings = spendSummary {summaryMyOutputs = Map.fromList [(externalAddr 1, MyOutputs 9000 (legacyPath (Deriv :/ 0 :/ 1)) "Savings")]}
      firstLines [(mainPlace, ListedTx (txOnChain changeToSavings 2 (Store.BlockRef 800001 1) 2 False "") (Just 9000))]
        `shouldBe` ["2023-11-14 * (00000000) Payment sent"]

    it "says payment received, payment sent or internal transfer when nothing has a label" $
      firstLines
        [ (mainPlace, ListedTx (txOnChain internalSummary 3 (Store.BlockRef 800002 1) 1 False "") (Just 8000)),
          (mainPlace, spent),
          (mainPlace, received "")
        ]
        `shouldBe` [ "2023-11-14 * (00000000) Payment received",
                     "2023-11-14 * (00000000) Payment sent",
                     "2023-11-14 * (00000000) Internal transfer"
                   ]

    -- portfolio0 pays 40000 to an address of portfolio1 that has no
    -- label there, and neither side labelled the transaction
    it "says transfer between portfolios for a payment from a portfolio to another, when nothing has a label" $ do
      let unlabelled = (txOnChain spendSummary 2 (Store.BlockRef 800001 1) 2 False "") {txInfoElsewhere = Map.fromList [(otherAddr 0, HeldElsewhere (PortfolioRef 0 1) (legacyPath (Deriv :/ 0 :/ 4)) "")]}
          seenByPortfolio1 =
            paymentSummary
              { summaryAmount = 40000,
                summaryMyOutputs = Map.fromList [(otherAddr 0, MyOutputs 40000 (legacyPath (Deriv :/ 0 :/ 4)) "")],
                summaryOtherOutputs = Map.fromList [(internalAddr 0, 9000)],
                summaryOtherInputs = Map.fromList [(externalAddr 0, 50000)]
              }
      firstLines
        [ (mainPlace, ListedTx unlabelled (Just 9000)),
          (Place (PortfolioRef 0 1) btc, ListedTx (txOnChain seenByPortfolio1 2 (Store.BlockRef 800001 1) 2 False "") (Just 40000))
        ]
        `shouldBe` ["2023-11-14 * (00000000) Transfer between portfolios"]

    it "keeps a label with a line break on the line of the entry, so that the label adds no line to the journal" $
      firstLines [(mainPlace, received "Salary\n2023-01-01 * Forged")]
        `shouldBe` ["2023-11-14 * (00000000) Salary 2023-01-01 * Forged"]

  describe "The journal of the wallet" $ do
    -- txs lists the newest first: the payment made, then the one received
    it "lists the transactions in the order of the chain, the oldest first" $
      [take 2 entry | entry <- txEntries (cetJournal [(mainPlace, spent), (mainPlace, received "")])]
        `shouldBe` [ ["2023-11-14 * (00000000) Payment received", "    ; txid: 0000000000000000000000000000000000000000000000000000000000000001"],
                     ["2023-11-14 * (00000000) Payment sent", "    ; txid: 0000000000000000000000000000000000000000000000000000000000000002"]
                   ]

    it "starts with a comment that says what wrote it and when, and that it is not to be edited" $
      take 2 (Text.lines (cetJournal [(mainPlace, received "")]))
        `shouldBe` [ "; The transactions of hw on the chain, written by exportjournal on 2023-11-15 10:00:00 UTC.",
                     "; hw writes this file whole each time. Do not edit it: write it again."
                   ]

    it "declares its commodity with eight decimals, its accounts, and the names of its notes and tags, before the first entry" $ do
      let tagged = (txOnChain spendSummary 2 (Store.BlockRef 800001 1) 2 False "") {txInfoTags = noTxTags {txOwnTags = [ShownTag rentTag Nothing]}}
      take 3 (drop 1 (paragraphs (cetJournal [(mainPlace, ListedTx tagged (Just 9000)), (mainPlace, received "")])))
        `shouldBe` [ ["commodity BTC", "    format 1000.00000000 BTC"],
                     ["account Assets:hw:wallet0:portfolio0", "account Expenses:Fees:Mining", "account Expenses:Unknown", "account Income:Unknown"],
                     ["tag txid", "tag time", "tag block", "tag address", "tag rent"]
                   ]

    it "holds the comment alone for a wallet without transactions" $
      paragraphs (cetJournal []) `shouldSatisfy` ((== 1) . length)

    -- A payment of 50000 on Bitcoin Cash, in block 900000
    it "writes the amounts of Bitcoin Cash in BCH, in the account of the same portfolio" $ do
      let onBch = ListedTx (txOnChain paymentSummary 3 (Store.BlockRef 900000 1) 1 False "") (Just 50000)
      drop 4 <$> txEntries (cetJournal [(Place mainPortfolioRef bch, onBch)])
        `shouldBe` [ [ "    Assets:hw:wallet0:portfolio0   0.00050000 BCH",
                       "    Income:Unknown                -0.00050000 BCH"
                     ]
                   ]

  describe "The balances at the end of the journal" $ do
    -- 50000 received - 41000 spent = 9000, the balance txs shows beside
    -- the newest transaction
    it "state what the portfolio holds after its newest transaction, in a posting that moves nothing" $
      balances (cetJournal [(mainPlace, spent), (mainPlace, received "")])
        `shouldBe` [ [ "2023-11-15 * Balances of hw",
                       "    Assets:hw:wallet0:portfolio0   0.00000000 BTC = 0.00009000 BTC"
                     ]
                   ]

    it "state the balance of each network of a portfolio, each in its commodity" $ do
      let onBch = ListedTx (txOnChain paymentSummary 3 (Store.BlockRef 900000 1) 1 False "") (Just 50000)
      drop 1 <$> balances (cetJournal [(mainPlace, spent), (mainPlace, received ""), (Place mainPortfolioRef bch, onBch)])
        `shouldBe` [ [ "    Assets:hw:wallet0:portfolio0   0.00000000 BTC = 0.00009000 BTC",
                       "    Assets:hw:wallet0:portfolio0   0.00000000 BCH = 0.00050000 BCH"
                     ]
                   ]

    it "are dated the day the journal is written" $
      take 1 <$> balances (cetJournal [(mainPlace, received "")]) `shouldBe` [["2023-11-15 * Balances of hw"]]

    -- A journal written on 1 Nov 2023, before the date of its
    -- transaction, as when the clock of the computer is late
    it "are dated as the last entry when that one is dated after the day the journal is written, so that no entry comes after them" $ do
      let earlier = UTCTime (fromGregorian 2023 11 1) 0
      take 1 <$> balances (journalText (const cet) earlier [(mainPlace, received "")]) `shouldBe` [["2023-11-14 * Balances of hw"]]

    it "are left out of a journal without transactions" $
      balances (cetJournal []) `shouldBe` []
