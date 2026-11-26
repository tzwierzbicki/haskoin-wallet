{-# LANGUAGE OverloadedStrings #-}

-- | The journal exportjournal writes for the ledger command line tool: an
-- entry for each live transaction of the wallet, whose postings add up to
-- zero, the accounts that tags file a payment under, the declarations of
-- what the file uses, and the balances at the end. A journal of some
-- transactions holds the entries of those alone, and no balances.
module Haskoin.Wallet.Export.JournalSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (TimeZone (..), UTCTime (..), fromGregorian)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Export.Journal (commodityOf, journalSafe, journalText, portfolioAccount, satoshisText)
import Haskoin.Wallet.Contacts (Contact (..))
import Haskoin.Wallet.Database.Portfolio (Place (..), PortfolioRef (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Config.JournalAccounts (TagAccounts, noTagAccounts)
import Haskoin.Wallet.Keys (legacyPath)
import Haskoin.Wallet.Tags (ShownTag (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Testnet3 (btcTestnet3)
import Haskoin.Wallet.Export.TxSelection (TxSelection (..))
import Test.Hspec

-- Every transaction below has the time seenTime, 14 Nov 2023 22:13:20
-- UTC, which a clock of Central Europe in winter reads as 23:13:20 on the
-- same day, unless a test says otherwise. The txid of fakeTxHash n is
-- 62 zeros and n in hexadecimal, so every short id is 00000000. The
-- transactions are those of portfolio0 of wallet0 on btc, mainPlace,
-- unless a test says otherwise.
--
-- The story is that of the shared summaries. The portfolio receives 50000
-- at /0/0 in block 800000. It then spends that coin in block 800001:
-- 40000 to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU, 9000 of change and 1000 of
-- fee, so 41000 leave it and 9000 are left.

-- | The journal of the rows on a clock of Central Europe in winter, in a
-- configuration that gives no tag an account
cetJournal :: [(Place, ListedTx)] -> Text
cetJournal = journalText noTagAccounts Nothing (const cet)

-- | The accounts the tags of the tests stand for: rent and cold for
-- accounts of expenses, salary for one of income. kyc stands for none.
tagAccounts :: TagAccounts
tagAccounts =
  Map.fromList
    [ (rentTag, "Expenses:Housing:Rent"),
      (coldTag, "Expenses:Housing:Heating"),
      (salaryTag, "Income:Salary")
    ]

-- | The journal of the rows on the same clock, in a configuration that
-- gives these tags their accounts
filedJournal :: [(Place, ListedTx)] -> Text
filedJournal = journalText tagAccounts Nothing (const cet)

-- | The lines of the postings of the entries of the transactions of a
-- journal, without their notes
postings :: Text -> [Text]
postings journal = [line | line <- concat (txEntries journal), Text.isPrefixOf "    " line, not (Text.isPrefixOf "    ;" line)]

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
      (commodityOf btc, commodityOf bch, commodityOf btcTestnet3) `shouldBe` ("BTC", "BCH", "BTCTEST")

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
      let seenByPortfolio1 = spendSeenByPortfolio1 {summaryMyOutputs = Map.fromList [(otherAddr 0, MyOutputs 40000 (legacyPath (Deriv :/ 0 :/ 4)) "Holidays")]}
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
      take 3 <$> txEntries (journalText noTagAccounts Nothing (const twoHoursAhead) [(mainPlace, received "")])
        `shouldBe` [ [ "2023-11-15 * (00000000) Payment received",
                       "    ; txid: 0000000000000000000000000000000000000000000000000000000000000001",
                       "    ; time: 2023-11-14 22:13:20 UTC"
                     ]
                   ]

    it "gives the tags of its transaction on a line, between colons, in the order of the alphabet" $ do
      let tagged = (txOnChain spendSummary 2 (Store.BlockRef 800001 1) 2 False "") {txInfoTags = noTxTags {txOwnTags = [ShownTag rentTag Nothing, ShownTag kycTag Nothing]}}
      [line | line <- concat (txEntries (cetJournal [(mainPlace, ListedTx tagged (Just 9000))])), Text.isInfixOf ":kyc:" line]
        `shouldBe` ["    ; :kyc:rent:"]

  -- The table gives rent the account Expenses:Housing:Rent, cold the
  -- account Expenses:Housing:Heating and salary the account
  -- Income:Salary. The address 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU is the
  -- contact Landlord in the tests that say so.
  describe "A tag that stands for an account" $ do
    let withTags tags info = info {txInfoTags = noTxTags {txOwnTags = [ShownTag tag Nothing | tag <- tags]}}
        landlordWith tags = Contact "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" "Landlord" [ShownTag tag Nothing | tag <- tags] seenTime
        toLandlordWith tags info = info {txInfoContacts = Map.fromList [(otherAddr 0, landlordWith tags)]}
        paid = txOnChain spendSummary 2 (Store.BlockRef 800001 1) 2 False ""
        paidIn = txOnChain paymentSummary 1 (Store.BlockRef 800000 1) 3 False ""

    -- 40000 paid + 1000 of fee = 41000 leave the portfolio
    it "files what a payment with the tag pays under its account, and leaves the fee a mining fee" $
      postings (filedJournal [(mainPlace, ListedTx (withTags [rentTag] paid) (Just 9000))])
        `shouldBe` [ "    Expenses:Housing:Rent          0.00040000 BTC",
                     "    Expenses:Fees:Mining           0.00001000 BTC",
                     "    Assets:hw:wallet0:portfolio0  -0.00041000 BTC"
                   ]

    it "files a payment received with the tag as coming from its account" $
      postings (filedJournal [(mainPlace, ListedTx (withTags [salaryTag] paidIn) (Just 50000))])
        `shouldBe` [ "    Assets:hw:wallet0:portfolio0   0.00050000 BTC",
                     "    Income:Salary                 -0.00050000 BTC"
                   ]

    -- The landlord gives back 50000 of rent, and the rent paid is lower
    -- by as much
    it "is the same account whichever way the money goes: a refund with the tag lowers the expense" $
      postings (filedJournal [(mainPlace, ListedTx (withTags [rentTag] paidIn) (Just 50000))])
        `shouldBe` [ "    Assets:hw:wallet0:portfolio0   0.00050000 BTC",
                     "    Expenses:Housing:Rent         -0.00050000 BTC"
                   ]

    it "leaves a payment in Expenses:Unknown when its tag stands for no account" $
      postings (filedJournal [(mainPlace, ListedTx (withTags [kycTag] paid) (Just 9000))])
        `shouldBe` [ "    Expenses:Unknown               0.00040000 BTC",
                     "    Expenses:Fees:Mining           0.00001000 BTC",
                     "    Assets:hw:wallet0:portfolio0  -0.00041000 BTC"
                   ]

    it "leaves every payment in the unknown accounts when the configuration gives no tag an account" $
      postings (cetJournal [(mainPlace, ListedTx (withTags [rentTag] paid) (Just 9000))])
        `shouldBe` [ "    Expenses:Unknown               0.00040000 BTC",
                     "    Expenses:Fees:Mining           0.00001000 BTC",
                     "    Assets:hw:wallet0:portfolio0  -0.00041000 BTC"
                   ]

    -- The payment itself has no tag
    it "files what a payment pays to a contact with the tag under its account" $
      postings (filedJournal [(mainPlace, ListedTx (toLandlordWith [rentTag] paid) (Just 9000))])
        `shouldBe` [ "    Expenses:Housing:Rent          0.00040000 BTC",
                     "    Expenses:Fees:Mining           0.00001000 BTC",
                     "    Assets:hw:wallet0:portfolio0  -0.00041000 BTC"
                   ]

    -- The landlord is tagged rent, and this payment to the landlord is
    -- tagged cold, for the heating
    it "files a payment under the account of its own tag rather than that of the tag of its contact" $
      postings (filedJournal [(mainPlace, ListedTx (withTags [coldTag] (toLandlordWith [rentTag] paid)) (Just 9000))])
        `shouldBe` [ "    Expenses:Housing:Heating       0.00040000 BTC",
                     "    Expenses:Fees:Mining           0.00001000 BTC",
                     "    Assets:hw:wallet0:portfolio0  -0.00041000 BTC"
                   ]

    -- The batch pays 60000 to the landlord and 30000 to
    -- 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ, which is no contact
    it "files each address a payment pays by the tags of its own contact, and leaves the others in Expenses:Unknown" $ do
      let batch = toLandlordWith [rentTag] (txOnChain batchSummary 2 (Store.BlockRef 800001 1) 2 False "")
      postings (filedJournal [(mainPlace, ListedTx batch (Just 9000))])
        `shouldBe` [ "    Expenses:Housing:Rent          0.00060000 BTC",
                     "    Expenses:Unknown               0.00030000 BTC",
                     "    Expenses:Fees:Mining           0.00001000 BTC",
                     "    Assets:hw:wallet0:portfolio0  -0.00091000 BTC"
                   ]

    -- hw cannot tell whether the payment was the rent or the heating,
    -- nor how much of it was each
    it "leaves a payment in Expenses:Unknown when two of its tags stand for two accounts" $
      postings (filedJournal [(mainPlace, ListedTx (withTags [coldTag, rentTag] paid) (Just 9000))])
        `shouldBe` [ "    Expenses:Unknown               0.00040000 BTC",
                     "    Expenses:Fees:Mining           0.00001000 BTC",
                     "    Assets:hw:wallet0:portfolio0  -0.00041000 BTC"
                   ]

    -- Another wallet adds a coin of 10000 to the inputs, so the 41000
    -- the portfolio lost have one posting, without an address
    it "files all that left the portfolio under its account, in a transaction made with coins of another wallet" $ do
      let shared = spendSummary {summaryOtherInputs = Map.fromList [(otherAddr 1, 10000)]}
          row = ListedTx (withTags [rentTag] (txOnChain shared 2 (Store.BlockRef 800001 1) 2 False "")) (Just 9000)
      postings (filedJournal [(mainPlace, row)])
        `shouldBe` [ "    Expenses:Housing:Rent          0.00041000 BTC",
                     "    Assets:hw:wallet0:portfolio0  -0.00041000 BTC"
                   ]

    -- The portfolio pays itself: nothing goes to others, and the fee of
    -- 1000 is the only expense
    it "changes nothing of a transaction between the addresses of a portfolio, whose fee stays a mining fee" $ do
      let internal = withTags [rentTag] (txOnChain internalSummary 2 (Store.BlockRef 800001 1) 2 False "")
      postings (filedJournal [(mainPlace, ListedTx internal (Just 49000))])
        `shouldBe` [ "    Expenses:Fees:Mining           0.00001000 BTC",
                     "    Assets:hw:wallet0:portfolio0  -0.00001000 BTC"
                   ]

    -- The payment received spends a coin at
    -- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU, which is a contact with the
    -- tag salary: the employer
    it "files a payment received under the account of a tag of the contact it came from" $
      postings (filedJournal [(mainPlace, ListedTx (toLandlordWith [salaryTag] paidIn) (Just 50000))])
        `shouldBe` [ "    Assets:hw:wallet0:portfolio0   0.00050000 BTC",
                     "    Income:Salary                 -0.00050000 BTC"
                   ]

    -- The employer gives back a rent it was paid by mistake. The tag of
    -- the transaction is about this one payment.
    it "files a payment received under the account of its own tag rather than that of the contact it came from" $
      postings (filedJournal [(mainPlace, ListedTx (withTags [rentTag] (toLandlordWith [salaryTag] paidIn)) (Just 50000))])
        `shouldBe` [ "    Assets:hw:wallet0:portfolio0   0.00050000 BTC",
                     "    Expenses:Housing:Rent         -0.00050000 BTC"
                   ]

    -- hw cannot tell which of the two accounts the contact means
    it "leaves a payment received in Income:Unknown when the tags of the contact it came from stand for two accounts" $
      postings (filedJournal [(mainPlace, ListedTx (toLandlordWith [salaryTag, rentTag] paidIn) (Just 50000))])
        `shouldBe` [ "    Assets:hw:wallet0:portfolio0   0.00050000 BTC",
                     "    Income:Unknown                -0.00050000 BTC"
                   ]

    it "leaves a payment received in Income:Unknown when the contact it came from has no tag with an account" $
      postings (filedJournal [(mainPlace, ListedTx (toLandlordWith [kycTag] paidIn) (Just 50000))])
        `shouldBe` [ "    Assets:hw:wallet0:portfolio0   0.00050000 BTC",
                     "    Income:Unknown                -0.00050000 BTC"
                   ]

    it "has its account declared among the accounts of the journal, so that the strict checks of ledger pass" $
      [line | line <- Text.lines (filedJournal [(mainPlace, ListedTx (withTags [rentTag] paid) (Just 9000))]), Text.isPrefixOf "account " line]
        `shouldBe` ["account Assets:hw:wallet0:portfolio0", "account Expenses:Fees:Mining", "account Expenses:Housing:Rent"]

    it "is named with its account in the first comment of the journal, in the order of the alphabet" $
      take 1 (paragraphs (filedJournal [(mainPlace, received "")]))
        `shouldBe` [ [ "; The transactions of every portfolio of hw on the chain, as a journal for ledger.",
                       "; hw writes this file whole each time. Do not edit it: your changes would be lost.",
                       ";",
                       "; Tags that stand for an account, from the configuration of hw:",
                       ";   cold    Expenses:Housing:Heating",
                       ";   rent    Expenses:Housing:Rent",
                       ";   salary  Income:Salary"
                     ]
                   ]

    it "is named in the first comment of a journal without transactions too" $
      Text.lines (filedJournal [])
        `shouldBe` [ "; The transactions of every portfolio of hw on the chain, as a journal for ledger.",
                     "; hw writes this file whole each time. Do not edit it: your changes would be lost.",
                     ";",
                     "; Tags that stand for an account, from the configuration of hw:",
                     ";   cold    Expenses:Housing:Heating",
                     ";   rent    Expenses:Housing:Rent",
                     ";   salary  Income:Salary"
                   ]

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
      firstLines
        [ (mainPlace, ListedTx unlabelled (Just 9000)),
          (Place (PortfolioRef 0 1) btc, ListedTx (txOnChain spendSeenByPortfolio1 2 (Store.BlockRef 800001 1) 2 False "") (Just 40000))
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

    it "starts with a comment that says what the file is, and that it is not to be edited" $
      take 2 (Text.lines (cetJournal [(mainPlace, received "")]))
        `shouldBe` [ "; The transactions of every portfolio of hw on the chain, as a journal for ledger.",
                     "; hw writes this file whole each time. Do not edit it: your changes would be lost."
                   ]

    -- hw writes the journal again after many commands, and leaves a file
    -- alone that did not change. A time of writing would change it.
    it "names no time of writing, so that the same history gives the same file" $
      [line | line <- Text.lines (cetJournal [(mainPlace, spent), (mainPlace, received "")]), Text.isInfixOf "written" line] `shouldBe` []

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
        `shouldBe` [ [ "2023-11-14 * Balances of hw",
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

    -- The payment received is dated 14 Nov 2023. The payment made is
    -- given the time of 20 Nov 2023 at 12:00:00 UTC, 13:00:00 on that
    -- day in Central Europe.
    it "are dated as the newest entry, so that no entry comes after them and the file changes with the history alone" $ do
      let sixDaysLater = UTCTime (fromGregorian 2023 11 20) (12 * 3600)
          madeLater = (listedTxInfo spent) {txInfoStatus = OnChain (chainOf (listedTxInfo spent)) {chainTime = Just sixDaysLater}}
      take 1 <$> balances (cetJournal [(mainPlace, ListedTx madeLater (Just 9000)), (mainPlace, received "")])
        `shouldBe` [["2023-11-20 * Balances of hw"]]

    it "are left out of a journal without transactions" $
      balances (cetJournal []) `shouldBe` []

  describe "A journal of some transactions" $ do
    -- The selection keeps the payment made, the transaction 2, as
    -- portfolio0 sees it on btc
    let rentOnly = TxSelection "tagged rent" (Set.fromList [(mainPortfolioRef, "btc", fakeTxHash 2)])
        rentJournal = journalText noTagAccounts (Just rentOnly) (const cet)

    it "holds the entries of the transactions kept, and no other" $
      [take 2 entry | entry <- txEntries (rentJournal [(mainPlace, spent), (mainPlace, received "")])]
        `shouldBe` [["2023-11-14 * (00000000) Payment sent", "    ; txid: 0000000000000000000000000000000000000000000000000000000000000002"]]

    it "says after its first lines which transactions it holds, and that it states no balances" $
      take 3 (drop 2 (Text.lines (rentJournal [(mainPlace, spent), (mainPlace, received "")])))
        `shouldBe` [ ";",
                     "; Only the transactions tagged rent are in this file.",
                     "; It states no balances at its end: the transactions left out moved money too."
                   ]

    -- The 50000 received are left out, so the 9000 the portfolio holds
    -- are not what the entries add up to
    it "states no balances at its end, which its entries would not add up to" $
      balances (rentJournal [(mainPlace, spent), (mainPlace, received "")]) `shouldBe` []

    -- portfolio0 pays 40000 to portfolio1, with 1000 of fee. The
    -- selection keeps the payment for portfolio0 alone.
    it "keeps the entry of a payment from a portfolio to another whole when it is kept for one of the two" $ do
      let rows =
            [ (mainPlace, ListedTx paidToPortfolio1 (Just 9000)),
              (Place (PortfolioRef 0 1) btc, ListedTx (txOnChain spendSeenByPortfolio1 2 (Store.BlockRef 800001 1) 2 False "") (Just 40000))
            ]
      postings (rentJournal rows)
        `shouldBe` [ "    Assets:hw:wallet0:portfolio1   0.00040000 BTC",
                     "    Expenses:Fees:Mining           0.00001000 BTC",
                     "    Assets:hw:wallet0:portfolio0  -0.00041000 BTC"
                   ]

    -- Bitcoin and Bitcoin Cash share the transactions made before they
    -- split, so an id alone does not name a transaction
    it "leaves out a transaction of another network that has the id of one it keeps" $ do
      let onBch = ListedTx (txOnChain paymentSummary 2 (Store.BlockRef 900000 1) 1 False "") (Just 50000)
      txEntries (rentJournal [(Place mainPortfolioRef bch, onBch)]) `shouldBe` []
