{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The question hw asks about each setting of its configuration file
-- ("Haskoin.Wallet.Config"): what it asks, why the setting matters, and
-- the values it takes. The setup of a first run asks the four that hw
-- cannot choose well for its user ("Haskoin.Wallet.Cli.Setup").
-- setdefault without an option asks all twelve
-- ("Haskoin.Wallet.Cli.SetDefault"). "Haskoin.Wallet.Cli.Question" asks
-- them.
--
-- A question here has no number, and starts on the default of hw: its
-- first row, or the rows checked. The setup and setdefault give each
-- question its number, and setdefault starts it on the value hw has now.
--
-- The table journal-accounts has no question. It is a list of lines of
-- any length, one for each tag, which the user writes in the file.
module Haskoin.Wallet.Cli.Question.Settings
  ( -- * Where the commands work
    networksQuestion,
    networkTitle,
    defaultNetworkQuestion,
    defaultTypeQuestion,

    -- * How the answers look
    unitQuestion,
    listingViewQuestion,
    listingReverseQuestion,

    -- * What hw keeps by itself
    journalQuestion,
    autoBackupDatabaseQuestion,
    autoBackupLabelsQuestion,

    -- * The server
    hostEntry,
    readHost,
    parallelRequestsEntry,
    gapEntry,
    readCount,
  )
where

import Data.Default (def)
import Data.Maybe (listToMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin (Network (..), bch, bchRegTest, bchTest3, bchTest4, btc, btcRegTest, btcTest4)
import Haskoin.Wallet.Amounts (AmountUnit (..), unitName)
import Haskoin.Wallet.Cli.Question (Entry (..), Question (..), orList)
import Haskoin.Wallet.Cli.Question.Menu (Row (..))
import Haskoin.Wallet.Config
import Haskoin.Wallet.Config.View (View (..), viewName)
import Haskoin.Wallet.Keys (AccountKind (..), kindName, portfolioNetworks)
import Haskoin.Wallet.Testnet3 (btcTestnet3, isDeprecatedNetwork)
import Numeric.Natural (Natural)
import Text.Read (readMaybe)

{- Where the commands work -}

-- | The networks hw shows. btc and bch start
-- checked, as in a configuration that does not say
-- ('configShownNetworks').
networksQuestion :: Question Network
networksQuestion =
  Question
    { questionTitle = "Which networks do you work with?",
      questionWhy =
        "A wallet of hw has accounts on eight networks: Bitcoin, Bitcoin Cash, and three test networks of each. \
        \hw shows the networks you check here, and hides the others. \
        \A hidden network is left out of every answer, and no command can name it. \
        \It is not lost. hw keeps it up to date, and you can show it later.",
      questionPrompt = "Networks to show, separated by spaces",
      questionRows = [Row net (networkName net) (networkTitle net) (net `elem` [btc, bch]) | net <- portfolioNetworks],
      questionCursor = 0
    }

-- | What a network is, in a few words, for a user who knows its coin but
-- not its short name. A network that hw still works on and no longer
-- recommends says so ('isDeprecatedNetwork'): this is where a user picks
-- the networks to work with, so it is where the word helps.
networkTitle :: Network -> Text
networkTitle net = maybe "" (deprecated . snd) (listToMaybe [titled | titled@(named, _) <- titles, named.name == net.name])
  where
    deprecated title
      | isDeprecatedNetwork net = title <> " (deprecated)"
      | otherwise = title
    titles =
      [ (btc, "Bitcoin"),
        (bch, "Bitcoin Cash"),
        (btcTestnet3, "Bitcoin, test network 3"),
        (btcTest4, "Bitcoin, test network 4"),
        (btcRegTest, "Bitcoin, a test chain of your own"),
        (bchTest3, "Bitcoin Cash, test network 3"),
        (bchTest4, "Bitcoin Cash, test network 4"),
        (bchRegTest, "Bitcoin Cash, a test chain of your own")
      ]

-- | The default network, among the networks the user just chose to
-- show, which are given. The first of them is the default answer. hw
-- always has a default network, so the question offers no way out of
-- one.
defaultNetworkQuestion :: [Network] -> Question Network
defaultNetworkQuestion shown =
  Question
    { questionTitle = "Which network is the default?",
      questionWhy =
        "Many commands work on one network. receive, for instance, gives out an address of one network. \
        \Such a command takes the default network when you do not give it --network. \
        \The option still names another network for one command.",
      questionPrompt = "Default network: " <> orList (networkName <$> shown),
      questionRows = [Row net (networkName net) (networkTitle net) False | net <- shown],
      questionCursor = 0
    }

-- | The default type of address. bip44 comes first,
-- as in a configuration that does not say ('configDefaultType').
defaultTypeQuestion :: Question AccountKind
defaultTypeQuestion =
  Question
    { questionTitle = "Which type of address is the default?",
      questionWhy =
        "On Bitcoin, hw holds two types of address, in two accounts of the same wallet. \
        \bip44 is the legacy type. Its addresses start with 1, and every service can pay them. \
        \bip84 is native SegWit. Its addresses start with bc1q, and spending from them costs less in fees. \
        \A command that works on one type, such as receive, takes the default type when you do not give it --type. \
        \Bitcoin Cash has bip44 addresses only, whatever you choose here.",
      questionPrompt = "Default type of address: " <> orList (kindName <$> [Bip44, Bip84]),
      questionRows =
        [ Row Bip44 (kindName Bip44) "Legacy addresses, which start with 1 on Bitcoin" False,
          Row Bip84 (kindName Bip84) "Native SegWit addresses, which start with bc1q: lower fees" False
        ],
      questionCursor = 0
    }

{- How the answers look -}

-- | The unit of the amounts. btc comes first, as in a configuration
-- that does not say ('configUnit'). The rows are the four names --unit
-- takes: btc and bch both stand for whole coins, and both are offered,
-- so that the question starts on the one the file has.
unitQuestion :: Question AmountUnit
unitQuestion =
  Question
    { questionTitle = "Which unit do you count in?",
      questionWhy =
        "hw shows every amount in this unit, and reads in it an amount that you type without a unit. \
        \A whole coin is 100 000 000 satoshis, and a bit is 100 satoshis. \
        \btc and bch both stand for whole coins: hw shows them as btc on Bitcoin and as bch on Bitcoin Cash. \
        \--unit still names another unit for one command.",
      questionPrompt = "Unit of the amounts: " <> orList (rowName <$> rows),
      questionRows = rows,
      questionCursor = 0
    }
  where
    rows =
      [ Row UnitBitcoin (unitName UnitBitcoin) "Whole coins, as in 0.01500000" False,
        Row UnitBitcoinCash (unitName UnitBitcoinCash) "Whole coins too: the same as btc" False,
        Row UnitBit (unitName UnitBit) "Bits, as in 15000.00" False,
        Row UnitSatoshi (unitName UnitSatoshi) "Satoshis, as in 1500000" False
      ]

-- | The view of the listings. compact comes first, as in a configuration
-- that does not say ('configListingView').
listingViewQuestion :: Question View
listingViewQuestion =
  Question
    { questionTitle = "How do the listings show their elements?",
      questionWhy =
        "The listings are portfolios, addresses, balances, txs, pendingtxs and coins. \
        \Each one shows its elements in the view you choose here. \
        \--compact, --verbose or --tabular still sets the view of one listing.",
      questionPrompt = "View of the listings: " <> orList (rowName <$> rows),
      questionRows = rows,
      questionCursor = 0
    }
  where
    rows =
      [ Row Compact (viewName Compact) "Each element in a few lines" False,
        Row Verbose (viewName Verbose) "Every detail of each element" False,
        Row Table (viewName Table) "Each element on one row of a table" False
      ]

-- | Whether the listings list their rows in the reverse order. No comes
-- first, as in a configuration that does not say
-- ('configListingReverse').
listingReverseQuestion :: Question Bool
listingReverseQuestion =
  Question
    { questionTitle = "Do the listings list their rows in the reverse order?",
      questionWhy =
        "With yes, addresses, balances, coins, txs and pendingtxs list their rows from the last to the first on the screen. \
        \txs then lists the oldest transaction first, and the newest one is the last line you read. \
        \--reverse or --no-reverse still sets the order of one listing. \
        \With --json or --csv, a listing keeps the usual order.",
      questionPrompt = "Reverse the listings: " <> orList (rowName <$> rows),
      questionRows = rows,
      questionCursor = 0
    }
  where
    rows =
      [ Row False "no" "The usual order" False,
        Row True "yes" "The reverse order: the oldest transaction first" False
      ]

{- What hw keeps by itself -}

-- | Whether hw keeps a journal for ledger. No
-- comes first, as in a configuration that does not say
-- ('configJournal'): the journal tells the whole history in plain text,
-- so hw writes it only for a user who asks.
--
-- The question names ledger with what it is and where it comes from. A
-- user of a wallet may never have met it, and the word alone could be
-- taken for something else. It then says what ledger is for, since the
-- journal is worth keeping only to a user who wants those reports.
journalQuestion :: Question Bool
journalQuestion =
  Question
    { questionTitle = "Should hw keep a journal for ledger, the accounting program?",
      questionWhy =
        "ledger is a free accounting program of the command line (ledger-cli.org). \
        \It keeps books by double entry from a text file you write, the journal. \
        \Its reports say what each account holds, what you earned and what you spent, over any period. \
        \With yes, hw keeps a journal of your transactions in its data directory, and writes it again after each command that changes it. \
        \You include that file in your own journal once, and your books follow your wallet. \
        \With no, hw exportjournal still writes the journal when you ask.",
      questionPrompt = "Keep a journal for ledger: " <> orList (rowName <$> rows),
      questionRows = rows,
      questionCursor = 0
    }
  where
    rows =
      [ Row False "no" "hw keeps no journal" False,
        Row True "yes" "hw keeps a journal for ledger up to date" False
      ]

-- | Whether hw keeps copies of the database. Yes comes first, as in a
-- configuration that does not say ('configAutoBackupDatabase').
autoBackupDatabaseQuestion :: Question Bool
autoBackupDatabaseQuestion =
  Question
    { questionTitle = "Should hw keep copies of the database?",
      questionWhy =
        "With yes, hw copies the database to the directory auto-backups of its data directory after each command that adds a wallet or a portfolio. \
        \It also copies it there before deletewallet, deleteportfolio and restoredatabase, which lose what the database holds. \
        \hw keeps the recent copies and fewer of the older ones. showbackups lists them, and restoredatabase brings one back. \
        \With no, hw makes no copy by itself, and backup still makes one when you ask.",
      questionPrompt = "Keep copies of the database: " <> orList (rowName <$> rows),
      questionRows = rows,
      questionCursor = 0
    }
  where
    rows =
      [ Row True "yes" "hw copies the database by itself" False,
        Row False "no" "hw makes no copy of the database by itself" False
      ]

-- | Whether hw keeps copies of the labels. Yes comes first, as in a
-- configuration that does not say ('configAutoBackupLabels').
autoBackupLabelsQuestion :: Question Bool
autoBackupLabelsQuestion =
  Question
    { questionTitle = "Should hw keep copies of your labels?",
      questionWhy =
        "With yes, hw writes what exportwallet writes to a new file of the directory auto-backups after each command that changes it: \
        \your labels, your tags, your contacts and the public keys of your portfolios. \
        \If you lose the database, importwallet reads that file back, and you type nothing again. \
        \With no, hw writes no such file by itself, and exportwallet still writes one when you ask.",
      questionPrompt = "Keep copies of the labels: " <> orList (rowName <$> rows),
      questionRows = rows,
      questionCursor = 0
    }
  where
    rows =
      [ Row True "yes" "hw copies the labels by itself" False,
        Row False "no" "hw makes no copy of the labels by itself" False
      ]

{- The server -}

-- | The address of the haskoin-store server, starting on the one given.
-- It is written, since no list holds the servers there are.
hostEntry :: Text -> Entry Text
hostEntry now =
  Entry
    { entryTitle = "Which server does hw ask about the chain?",
      entryWhy =
        "hw asks a haskoin-store server which transactions paid your addresses, and sends your transactions through it. \
        \The server sees the addresses hw asks about. \
        \The public server is "
          <> configHost def
          <> ". To ask a haskoin-store of your own instead, type its address, as in http://localhost:3000/.",
      entryPrompt = "Address of the server",
      entryNow = now,
      entryName = id,
      entryRead = readHost
    }

-- | The address of a server as typed: a web address, which starts with
-- http:\/\/ or https:\/\/ and has no space. hw can send a request to no
-- other address, so a name alone, as in localhost:3000, is refused here
-- rather than at the next sync.
readHost :: Text -> Either String Text
readHost typed
  | Text.any (== ' ') typed = Left "The address of a server has no space"
  | any (\scheme -> scheme `Text.isPrefixOf` typed && typed /= scheme) ["http://", "https://"] = Right typed
  | otherwise = Left "Type the address of a server, which starts with http:// or https://"

-- | How many requests go to the server at once, starting on the number
-- given
parallelRequestsEntry :: Natural -> Entry Natural
parallelRequestsEntry now =
  Entry
    { entryTitle = "How many requests does hw send to the server at once?",
      entryWhy =
        "The public server takes one request at a time, so keep 1 unless you run a haskoin-store of your own. \
        \A server of your own may take more, and a sync is then faster.",
      entryPrompt = "Requests at once, from 1 to " <> cs (show mostParallelRequests),
      entryNow = now,
      entryName = cs . show,
      entryRead = readCount mostParallelRequests
    }

-- | The gap, starting on the number given
gapEntry :: Natural -> Entry Natural
gapEntry now =
  Entry
    { entryTitle = "How many unused addresses in a row does hw look at?",
      entryWhy =
        "This is the gap of BIP44. receive gives out this many unused addresses in a row at most, in each account. \
        \sync looks for payments on this many addresses past the last used one. \
        \20 is the gap of most wallets. Keep it unless you give out many addresses that are never paid. \
        \A larger gap makes each sync ask the server about more addresses.",
      entryPrompt = "Gap, from 1 to " <> cs (show mostGap),
      entryNow = now,
      entryName = cs . show,
      entryRead = readCount mostGap
    }

-- | A count as typed, from one to the largest given, as the file takes it
-- ('countWithin'): a whole number in digits
readCount :: Natural -> Text -> Either String Natural
readCount most typed =
  maybe (Left ("Type " <> cs (wholeNumberUpTo most))) Right (countWithin most =<< readMaybe (Text.unpack typed))
