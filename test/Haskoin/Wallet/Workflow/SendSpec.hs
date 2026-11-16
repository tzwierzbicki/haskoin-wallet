{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A payment made with send or with sendall, the two commands that
-- prepare a payment, sign it and send it.
--
-- send must do what preparetx, signtx and broadcast do when they are run
-- one after the other with the same options, and sendall what
-- preparesendall, signtx and broadcast do. So the payments of this spec
-- are made both ways, and each expectation, written out, is held against
-- both ('payments', 'sendAlls'): the two ways agree on every rule tested
-- here, and neither is the measure of the other.
--
-- Then what the one command alone does: the transaction shown once,
-- before the mnemonic, which is the confirmation; what a payment that was
-- not signed leaves in the wallet, which is nothing; and what one that
-- was signed and not sent leaves, which is what signtx leaves.
--
-- sendall must behave as send does in all of this. So every rule is
-- tested for both commands ('inOnes'), each on its own payment, with its
-- own expectation written out.
module Haskoin.Wallet.Workflow.SendSpec (spec) where

import Control.Monad (forM_, void)
import Data.Char (isHexDigit)
import Data.IORef (modifyIORef, readIORef)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Haskoin
import Haskoin.Wallet.Database.Coin (CoinLock (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Store (PublishFailure (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Workflow.TestUtils
import System.Exit (ExitCode (..))
import Test.Hspec

spec :: Spec
spec = do
  bothWaysSpec
  screenSpec
  notSignedSpec
  notSentSpec

{- The same payment, both ways -}

-- | A payment the tests make both ways
data Payment = Payment
  { -- | What the payment shows, in the words of its test
    payRule :: String,
    -- | The chain the store serves
    payChain :: Chain,
    -- | How the home gets its portfolio and its coins
    paySetup :: FilePath -> IO (),
    -- | The options of the payment, as the one command and the command
    -- that prepares it both take them
    payOptions :: [String],
    -- | What the user types for the questions of those options, before the
    -- mnemonic
    payTyped :: [Text],
    -- | What the network must be handed: the coins the transaction spends,
    -- sorted, and what it pays, by amount
    payHanded :: ([OutPoint], [(Word64, Either String Address)])
  }

-- | The payments made both ways, one for each kind of option of
-- preparetx. Every payment is of 20000 satoshis to
-- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU at 10 satoshis a byte, unless its
-- comment says otherwise.
--
-- The sizes are those of the network: 10 bytes for a transaction, 148 for
-- a legacy input and 34 for a legacy output. One coin that pays one
-- recipient and legacy change makes 10 + 148 + 34 + 34 = 226 bytes, so a
-- fee of 2260. A transaction with a native SegWit input has 10.5 virtual
-- bytes of its own, such an input 68, and a native SegWit output 31. The
-- sum is rounded up.
payments :: [Payment]
payments =
  [ -- The coin of 50000 pays 20000 and 2260 of fee: 27740 of change
    Payment
      { payRule = "pays the recipient its amount, and the change to a new change address",
        payChain = paidChain,
        paySetup = synced,
        payOptions = [recipient "0.0002", "--fee-rate", "10"],
        payTyped = [],
        payHanded = paymentSent
      },
    -- One input and three outputs: 10 + 148 + 3 × 34 = 260 bytes, a fee
    -- of 2600, and 50000 - 20000 - 10000 - 2600 = 17400 of change
    Payment
      { payRule = "pays each of several recipients its amount",
        payChain = paidChain,
        paySetup = synced,
        payOptions = [recipient "0.0002", fixedTo (otherAddr 1) "0.0001", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([paidCoin], [(10000, Right (otherAddr 1)), (17400, Right (internalAddr 0)), (20000, Right (otherAddr 0))])
      },
    -- 226 bytes × 20 = 4520 of fee, and 50000 - 20000 - 4520 = 25480 of
    -- change
    Payment
      { payRule = "pays the fee rate of --fee-rate for each byte",
        payChain = paidChain,
        paySetup = synced,
        payOptions = [recipient "0.0002", "--fee-rate", "20"],
        payTyped = [],
        payHanded = ([paidCoin], [(20000, Right (otherAddr 0)), (25480, Right (internalAddr 0))])
      },
    -- The median rate of the chain of fees is 12: 226 × 12 = 2712 of fee,
    -- and 50000 - 20000 - 2712 = 27288 of change. An empty line takes the
    -- median.
    Payment
      { payRule = "pays the median rate of recent blocks when nothing is typed at the question of the fee rate",
        payChain = feesChain,
        paySetup = synced,
        payOptions = [recipient "0.0002"],
        payTyped = [""],
        payHanded = ([paidCoin], [(20000, Right (otherAddr 0)), (27288, Right (internalAddr 0))])
      },
    -- 226 × 101 = 22826 of fee, and 50000 - 20000 - 22826 = 7174 of change
    Payment
      { payRule = "pays a fee rate above 100 once the user answers yes",
        payChain = paidChain,
        paySetup = synced,
        payOptions = [recipient "0.0002", "--fee-rate", "101"],
        payTyped = ["y"],
        payHanded = ([paidCoin], [(7174, Right (internalAddr 0)), (20000, Right (otherAddr 0))])
      },
    Payment
      { payRule = "pays a fee rate above 100 without a question, with --yes",
        payChain = paidChain,
        paySetup = synced,
        payOptions = [recipient "0.0002", "--fee-rate", "101", "--yes"],
        payTyped = [],
        payHanded = ([paidCoin], [(7174, Right (internalAddr 0)), (20000, Right (otherAddr 0))])
      },
    -- The fee of 2260 comes out of the 20000: the recipient gets 17740,
    -- and the change is 50000 - 20000 = 30000
    Payment
      { payRule = "takes the fee from the recipient with --recipients-pay",
        payChain = paidChain,
        paySetup = synced,
        payOptions = [recipient "0.0002", "--recipients-pay", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([paidCoin], [(17740, Right (otherAddr 0)), (30000, Right (internalAddr 0))])
      },
    -- Coins of 50000 and 30000, and either pays 20000 alone. Smallest
    -- first spends the one of 30000, output 1: 30000 - 20000 - 2260 = 7740
    -- of change.
    Payment
      { payRule = "spends the coins --strategy selects",
        payChain = confirmedChain twoCoinsTx,
        paySetup = synced,
        payOptions = [recipient "0.0002", "--strategy", "smallest-first", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf twoCoinsTx 1], [(7740, Right (internalAddr 0)), (20000, Right (otherAddr 0))])
      },
    -- The coin of 30000 has 1 confirmation and the one of 50000 has 3.
    -- Smallest first would spend the one of 30000, which --min-conf 2
    -- leaves out.
    Payment
      { payRule = "spends only the coins with the confirmations --min-conf asks",
        payChain = olderAndNewerChain,
        paySetup = synced,
        payOptions = [recipient "0.0002", "--min-conf", "2", "--strategy", "smallest-first", "--fee-rate", "10"],
        payTyped = [],
        payHanded = paymentSent
      },
    -- The coin of 30000 at /0/1, output 1, named by its short id
    Payment
      { payRule = "spends the coin --coin names",
        payChain = confirmedChain twoAddressesTx,
        paySetup = synced,
        payOptions = [recipient "0.0002", "--coin", shortCoinArg twoAddressesTx 1, "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf twoAddressesTx 1], [(7740, Right (internalAddr 0)), (20000, Right (otherAddr 0))])
      },
    -- Both coins at the address, 80000: two inputs and two outputs make
    -- 10 + 2 × 148 + 2 × 34 = 374 bytes, a fee of 3740, and 80000 - 20000
    -- - 3740 = 56260 of change
    Payment
      { payRule = "spends every coin at the --spend-from address",
        payChain = confirmedChain twoCoinsTx,
        paySetup = synced,
        payOptions = [recipient "0.0002", "--spend-from", textOf (externalAddr 0), "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf twoCoinsTx 0, coinOf twoCoinsTx 1], [(20000, Right (otherAddr 0)), (56260, Right (internalAddr 0))])
      },
    -- Of the two coins at the address, smallest first needs the one of
    -- 30000 alone
    Payment
      { payRule = "spends only what the payment needs among the coins chosen, with --select",
        payChain = confirmedChain twoCoinsTx,
        paySetup = synced,
        payOptions = [recipient "0.0002", "--spend-from", textOf (externalAddr 0), "--select", "--strategy", "smallest-first", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf twoCoinsTx 1], [(7740, Right (internalAddr 0)), (20000, Right (otherAddr 0))])
      },
    -- The bip84 coin of 30000, output 1, with bip84 change: 10.5 + 68 +
    -- 34 + 31 = 143.5, so 144 virtual bytes, a fee of 1440, and 30000 -
    -- 20000 - 1440 = 8560 of change
    Payment
      { payRule = "spends only the coins of the type --coin-type names",
        payChain = confirmedChain bothTypesTx,
        paySetup = syncedBothTypes,
        payOptions = [recipient "0.0002", "--coin-type", "bip84", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf bothTypesTx 1], [(8560, Right (segwitInternalAddr 0)), (20000, Right (otherAddr 0))])
      },
    -- 60000 needs both coins, 80000, with bip44 change: 10.5 + 148 + 68 +
    -- 34 + 34 = 294.5, so 295 virtual bytes, a fee of 2950, and 80000 -
    -- 60000 - 2950 = 17050 of change
    Payment
      { payRule = "spends bip44 and bip84 coins together, signed for both from the mnemonic typed once",
        payChain = confirmedChain bothTypesTx,
        paySetup = syncedBothTypes,
        payOptions = [recipient "0.0006", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf bothTypesTx 0, coinOf bothTypesTx 1], [(17050, Right (internalAddr 0)), (60000, Right (otherAddr 0))])
      },
    -- A legacy coin with bip84 change: 10 + 148 + 34 + 31 = 223 bytes, a
    -- fee of 2230, and 50000 - 20000 - 2230 = 27770 of change
    Payment
      { payRule = "sends the change to an address of the type --change-type names",
        payChain = paidChain,
        paySetup = synced,
        payOptions = [recipient "0.0002", "--change-type", "bip84", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([paidCoin], [(20000, Right (otherAddr 0)), (27770, Right (segwitInternalAddr 0))])
      },
    -- The change of 27740 goes back to /0/0, which the coin spent is at
    Payment
      { payRule = "sends the change back to the address --change-back names",
        payChain = paidChain,
        paySetup = synced,
        payOptions = [recipient "0.0002", "--change-back", textOf (externalAddr 0), "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([paidCoin], [(20000, Right (otherAddr 0)), (27740, Right (externalAddr 0))])
      },
    -- The coins of 50000 at /0/0 and 30000 at /0/1 pay 60000 together:
    -- 374 bytes, a fee of 3740, and 80000 - 60000 - 3740 = 16260 of
    -- change, back to /0/0, where the larger coin is
    Payment
      { payRule = "sends the change back to the address of the largest coin spent, with --change-back largest",
        payChain = confirmedChain twoAddressesTx,
        paySetup = synced,
        payOptions = [recipient "0.0006", "--change-back", "largest", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf twoAddressesTx 0, coinOf twoAddressesTx 1], [(16260, Right (externalAddr 0)), (60000, Right (otherAddr 0))])
      },
    -- The coin of 50000 of the portfolio on Bitcoin Cash, and its first
    -- change address there
    Payment
      { payRule = "pays on the --network given, from the coins of the portfolio there",
        payChain = confirmedChain cashCoinTx,
        paySetup = cashSynced,
        payOptions = [recipient "0.0002", "--network", "bch", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf cashCoinTx 0], [(20000, Right (otherAddr 0)), (27740, Right cashChangeAddr)])
      }
  ]
  where
    recipient = fixedTo (otherAddr 0)
    coinOf tx = OutPoint (txHash tx)
    -- The coin of 50000 of the paid chain, and of the chain of fees
    paidCoin = coinOf payment 0

-- | The payments of all the coins made both ways, one for each kind of
-- option of preparesendall. Every one goes to
-- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU at 10 satoshis a byte, with no
-- change, unless its comment says otherwise. The address pays the fee out
-- of the coins.
--
-- The sizes are those of 'payments'. The coins of 50000 and 30000 to one
-- legacy address make 10 + 2 × 148 + 34 = 340 bytes, so a fee of 3400,
-- and 80000 - 3400 = 76600 for the address. The coin of 50000 alone makes
-- 10 + 148 + 34 = 192 bytes, a fee of 1920, and 48080 for the address.
sendAlls :: [Payment]
sendAlls =
  [ Payment
      { payRule = "sends every coin to the address, which pays the fee, with no change",
        payChain = confirmedChain twoCoinsTx,
        paySetup = synced,
        payOptions = [everything, "--fee-rate", "10"],
        payTyped = [],
        payHanded = (bothCoins, [(76600, Right (otherAddr 0))])
      },
    -- Two outputs make 374 bytes, a fee of 3740. The first address gets
    -- its 30000, and the second 80000 - 30000 - 3740 = 46260.
    Payment
      { payRule = "pays a recipient its fixed amount, and what remains to the one without an amount",
        payChain = confirmedChain twoCoinsTx,
        paySetup = synced,
        payOptions = [fixedTo (otherAddr 0) "0.0003", textOf (otherAddr 1), "--fee-rate", "10"],
        payTyped = [],
        payHanded = (bothCoins, [(30000, Right (otherAddr 0)), (46260, Right (otherAddr 1))])
      },
    -- 340 bytes × 20 = 6800 of fee, and 80000 - 6800 = 73200
    Payment
      { payRule = "pays the fee rate of --fee-rate for each byte",
        payChain = confirmedChain twoCoinsTx,
        paySetup = synced,
        payOptions = [everything, "--fee-rate", "20"],
        payTyped = [],
        payHanded = (bothCoins, [(73200, Right (otherAddr 0))])
      },
    -- The median rate of the chain of fees is 12, and its one coin is of
    -- 50000: 192 × 12 = 2304 of fee, and 50000 - 2304 = 47696. An empty
    -- line takes the median.
    Payment
      { payRule = "pays the median rate of recent blocks when nothing is typed at the question of the fee rate",
        payChain = feesChain,
        paySetup = synced,
        payOptions = [everything],
        payTyped = [""],
        payHanded = ([paidCoin], [(47696, Right (otherAddr 0))])
      },
    -- 340 × 101 = 34340 of fee, and 80000 - 34340 = 45660
    Payment
      { payRule = "pays a fee rate above 100 once the user answers yes",
        payChain = confirmedChain twoCoinsTx,
        paySetup = synced,
        payOptions = [everything, "--fee-rate", "101"],
        payTyped = ["y"],
        payHanded = (bothCoins, [(45660, Right (otherAddr 0))])
      },
    Payment
      { payRule = "pays a fee rate above 100 without a question, with --yes",
        payChain = confirmedChain twoCoinsTx,
        paySetup = synced,
        payOptions = [everything, "--fee-rate", "101", "--yes"],
        payTyped = [],
        payHanded = (bothCoins, [(45660, Right (otherAddr 0))])
      },
    -- The coin of 30000 has 1 confirmation and the one of 50000 has 3, so
    -- --min-conf 2 sends the one of 50000 alone
    Payment
      { payRule = "sends only the coins with the confirmations --min-conf asks",
        payChain = olderAndNewerChain,
        paySetup = synced,
        payOptions = [everything, "--min-conf", "2", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([paidCoin], [(48080, Right (otherAddr 0))])
      },
    -- The coin of 30000 at /0/1, output 1, named by its short id: 192
    -- bytes, a fee of 1920, and 30000 - 1920 = 28080
    Payment
      { payRule = "sends the coin --coin names, and no other",
        payChain = confirmedChain twoAddressesTx,
        paySetup = synced,
        payOptions = [everything, "--coin", shortCoinArg twoAddressesTx 1, "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf twoAddressesTx 1], [(28080, Right (otherAddr 0))])
      },
    -- The coin of 50000 at /0/0, output 0, and not the one at /0/1
    Payment
      { payRule = "sends the coins at the --spend-from address, and no other",
        payChain = confirmedChain twoAddressesTx,
        paySetup = synced,
        payOptions = [everything, "--spend-from", textOf (externalAddr 0), "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf twoAddressesTx 0], [(48080, Right (otherAddr 0))])
      },
    -- The bip84 coin of 30000, output 1: 10.5 + 68 + 34 = 112.5, so 113
    -- virtual bytes, a fee of 1130, and 30000 - 1130 = 28870
    Payment
      { payRule = "sends only the coins of the type --coin-type names",
        payChain = confirmedChain bothTypesTx,
        paySetup = syncedBothTypes,
        payOptions = [everything, "--coin-type", "bip84", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf bothTypesTx 1], [(28870, Right (otherAddr 0))])
      },
    -- Both coins, 80000: 10.5 + 148 + 68 + 34 = 260.5, so 261 virtual
    -- bytes, a fee of 2610, and 80000 - 2610 = 77390
    Payment
      { payRule = "sends bip44 and bip84 coins together, signed for both from the mnemonic typed once",
        payChain = confirmedChain bothTypesTx,
        paySetup = syncedBothTypes,
        payOptions = [everything, "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf bothTypesTx 0, coinOf bothTypesTx 1], [(77390, Right (otherAddr 0))])
      },
    -- The coins of 50000 and 30000, and three coins of 1200, 900 and 546,
    -- each worth less than the 1480 its input costs. All five make 10 +
    -- 5 × 148 + 34 = 784 bytes, a fee of 7840, and 82646 - 7840 = 74806.
    Payment
      { payRule = "sends the coins worth less than their fee too, once the user answers yes",
        payChain = confirmedChain dustCoinsTx,
        paySetup = synced,
        payOptions = [everything, "--fee-rate", "10"],
        payTyped = ["y"],
        payHanded = (allFiveCoins, [(74806, Right (otherAddr 0))])
      },
    -- The two coins worth their fee alone, as without the dust coins
    Payment
      { payRule = "leaves the coins worth less than their fee, and sends the others, once the user answers no",
        payChain = confirmedChain dustCoinsTx,
        paySetup = synced,
        payOptions = [everything, "--fee-rate", "10"],
        payTyped = ["n"],
        payHanded = ([coinOf dustCoinsTx 0, coinOf dustCoinsTx 1], [(76600, Right (otherAddr 0))])
      },
    Payment
      { payRule = "sends the coins worth less than their fee too without a question, with --spend-dust",
        payChain = confirmedChain dustCoinsTx,
        paySetup = synced,
        payOptions = [everything, "--spend-dust", "--fee-rate", "10"],
        payTyped = [],
        payHanded = (allFiveCoins, [(74806, Right (otherAddr 0))])
      },
    Payment
      { payRule = "leaves the coins worth less than their fee without a question, with --leave-dust",
        payChain = confirmedChain dustCoinsTx,
        paySetup = synced,
        payOptions = [everything, "--leave-dust", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf dustCoinsTx 0, coinOf dustCoinsTx 1], [(76600, Right (otherAddr 0))])
      },
    -- The coin of 50000 of the portfolio on Bitcoin Cash
    Payment
      { payRule = "sends the coins of the portfolio on the --network given",
        payChain = confirmedChain cashCoinTx,
        paySetup = cashSynced,
        payOptions = [everything, "--network", "bch", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf cashCoinTx 0], [(48080, Right (otherAddr 0))])
      }
  ]
  where
    -- The address that gets everything
    everything = textOf (otherAddr 0)
    coinOf tx = OutPoint (txHash tx)
    -- The coin of 50000 of the paid chain, and of the chain of fees
    paidCoin = coinOf payment 0
    bothCoins = [coinOf twoCoinsTx 0, coinOf twoCoinsTx 1]
    allFiveCoins = [coinOf dustCoinsTx i | i <- [0 .. 4]]

-- | A home with portfolio0, its first address on Bitcoin Cash handed out,
-- and synced
cashSynced :: FilePath -> IO ()
cashSynced home = do
  newTestPortfolio home
  _ <- answer home ["receive", "--network", "bch"] []
  void $ answer home ["sync"] []

-- | One of the two commands that prepare a payment, sign it and send it,
-- with the payment the tests make with it when they name none: the plain
-- payment, from the coin of 50000 of the paid chain to
-- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU at 10 satoshis a byte. The tests run
-- for both commands, and these are the few things that differ.
data InOne = InOne
  { -- | The command
    oneName :: String,
    -- | The command that prepares the same transaction, which signtx and
    -- broadcast then sign and send
    onePrepare :: String,
    -- | Make a payment with the command, or with the three it stands for
    onePaid :: PayWith -> FilePath -> [String] -> [Text] -> IO TxInfo,
    -- | The payments made both ways, one for each kind of option
    onePayments :: [Payment],
    -- | The recipient of the plain payment, as typed
    onePlainTo :: String,
    -- | What the network is handed of the plain payment
    onePlainHanded :: ([OutPoint], [(Word64, Either String Address)]),
    -- | The line of the plain transaction that shows the recipient
    onePlainLine :: Text,
    -- | What the portfolio can spend once the plain payment has a block
    onePlainLeaves :: Word64,
    -- | A payment the preparing command refuses, from the same coin, and
    -- its refusal
    oneRefused :: [String],
    oneRefusal :: Text,
    -- | A payment from the coins of 50000 at /0/0 and of 30000 at /0/1,
    -- which leaves the second one once the user locked it, and the line
    -- of its transaction that shows the recipient
    oneAroundLocked :: [String],
    oneAroundLockedLine :: Text
  }

-- | send and sendall, each with its plain payment.
--
-- send pays 20000 of the coin of 50000: 226 bytes, a fee of 2260, and
-- 27740 of change, which is what the portfolio can spend afterwards.
--
-- sendall sends the coin whole: 10 + 148 + 34 = 192 bytes, a fee of
-- 1920, and 50000 - 1920 = 48080 for the address. Nothing is left to
-- spend.
inOnes :: [InOne]
inOnes =
  [ InOne
      { oneName = "send",
        onePrepare = "preparetx",
        onePaid = paidWith,
        onePayments = payments,
        onePlainTo = fixedTo (otherAddr 0) "0.0002",
        onePlainHanded = paymentSent,
        onePlainLine = "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00020000 btc\n",
        onePlainLeaves = 27740,
        -- 100000 satoshis from a portfolio that holds 50000
        oneRefused = [fixedTo (otherAddr 0) "0.001", "--fee-rate", "10"],
        oneRefusal = "The coins do not cover the amount and the fee",
        -- Both coins are named with --coin, and the one of 50000 pays
        oneAroundLocked = [fixedTo (otherAddr 0) "0.0002", "--coin", coinArg twoAddressesTx 0, "--coin", coinArg twoAddressesTx 1, "--fee-rate", "10"],
        oneAroundLockedLine = "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00020000 btc\n"
      },
    InOne
      { oneName = "sendall",
        onePrepare = "preparesendall",
        onePaid = paidAllWith,
        onePayments = sendAlls,
        onePlainTo = textOf (otherAddr 0),
        onePlainHanded = ([OutPoint (txHash payment) 0], [(48080, Right (otherAddr 0))]),
        onePlainLine = "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00048080 btc\n",
        onePlainLeaves = 0,
        -- A fixed amount of the whole coin of 50000 leaves nothing for
        -- the fee of the two outputs, 226 bytes × 10 = 2260
        oneRefused = [fixedTo (otherAddr 0) "0.0005", textOf (otherAddr 1), "--fee-rate", "10"],
        oneRefusal = "The coins, 50000 satoshis, do not cover the fixed amounts, 50000, and the fee, 2260",
        -- The coin of 50000 is sent whole: 48080 for the address
        oneAroundLocked = [textOf (otherAddr 0), "--fee-rate", "10"],
        oneAroundLockedLine = "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00048080 btc\n"
      }
  ]

-- | The plain payment of a command, as typed after its name
plainOf :: InOne -> [String]
plainOf one = [one.onePlainTo, "--fee-rate", "10"]

-- | The two ways to pay with a command, each with how the tests name it
waysOf :: InOne -> [(PayWith, String)]
waysOf one = [(WithSend, one.oneName), (WithThreeCommands, one.onePrepare <> ", signtx and broadcast")]

-- Every test below runs for send and for sendall, and once for each way
-- to pay, with the same expectation. The payment of the tests that name
-- none is the plain payment of the command ('inOnes').
bothWaysSpec :: Spec
bothWaysSpec =
  forM_ inOnes $ \one ->
    forM_ (waysOf one) $ \(way, name) ->
      describe ("A payment made with " <> name) $ do
        let plain = plainOf one

        -- The inputs are compared sorted: their order is drawn at random
        forM_ one.onePayments $ \pay ->
          it pay.payRule $
            withChain pay.payChain $ \home chain -> do
              pay.paySetup home
              _ <- one.onePaid way home pay.payOptions pay.payTyped
              published <- inMempool <$> readIORef chain
              ((\(spent, paid) -> (sort spent, paid)) . spendsAndPays <$> published) `shouldBe` [pay.payHanded]

        it "is answered with the transaction signed and sent" $
          withChain paidChain $ \home _ -> do
            synced home
            sent <- one.onePaid way home plain []
            pendingState sent `shouldBe` Just (True, True)

        it "is held by the wallet as signed and sent" $
          withChain paidChain $ \home _ -> do
            synced home
            _ <- one.onePaid way home plain []
            onePendingState home `shouldReturn` Just (True, True)

        it "keeps the label of --label" $
          withChain paidChain $ \home _ -> do
            synced home
            _ <- one.onePaid way home (plain <> ["--label", "rent of October"]) []
            (txInfoLabel <$> onePendingTx home) `shouldReturn` "rent of October"

        -- The best block of the chain is 800002. One time in ten the lock
        -- time is below it, by up to 99 blocks.
        it "has the lock time of the best block the wallet knows, or one up to 99 blocks below it" $
          withChain paidChain $ \home _ -> do
            synced home
            sent <- one.onePaid way home plain []
            summaryLockTime (txInfoSummary sent) `shouldSatisfy` (\lockTime -> lockTime <= 800002 && lockTime >= 800002 - 99)

        -- The best block was mined nine hours before the test, so the wallet
        -- is behind the chain
        it "has a lock time of 0 from a wallet behind the chain, with --yes" $
          withChainMinedAgo [] (9 * 3600) paidChain $ \home _ -> do
            synced home
            sent <- one.onePaid way home (plain <> ["--yes"]) []
            summaryLockTime (txInfoSummary sent) `shouldBe` 0

        -- The sequence number of BIP 125, which bumpfee needs
        it "is marked replaceable on Bitcoin" $
          withChain paidChain $ \home chain -> do
            synced home
            _ <- one.onePaid way home plain []
            published <- inMempool <$> readIORef chain
            concatMap (map (.sequence) . (.inputs)) published `shouldBe` [0xfffffffd]

        -- The change of a payment, and nothing of a payment of every coin
        it "leaves the portfolio what it did not pay away, to spend once it has a block and a sync has seen it" $
          withChain paidChain $ \home chain -> do
            synced home
            _ <- one.onePaid way home plain []
            modifyIORef chain mine
            (spendableOn btc <$> hwJson home ["sync"] []) `shouldReturn` Right [(mainPortfolioRef, one.onePlainLeaves)]

        it "is no longer pending once a sync has seen it on the network" $
          withChain paidChain $ \home _ -> do
            synced home
            _ <- one.onePaid way home plain []
            _ <- answer home ["sync"] []
            (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

{- What the command shows -}

-- Each test runs the plain payment of the command, in a home synced with
-- the paid chain, with the mnemonic typed.
screenSpec :: Spec
screenSpec = do
  forM_ inOnes $ \one ->
    describe (one.oneName <> ", on the screen") $ do
      let run home = do
            synced home
            hw home (one.oneName : plainOf one) seedOnce

      it "shows the transaction before it asks for the mnemonic" $
        withChain paidChain $ \home _ -> do
          shown <- run home
          shownBeforeMnemonic shown `shouldSatisfy` Text.isInfixOf one.onePlainLine

      it "asks for the mnemonic after the transaction" $
        withChain paidChain $ \home _ -> do
          shown <- run home
          shown `shouldSatisfy` askedMnemonic

      -- The recipient is on one line of the transaction, and nowhere else
      it "shows the transaction once" $
        withChain paidChain $ \home _ -> do
          shown <- run home
          Text.count "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" (runOut shown <> runErr shown) `shouldBe` 1

      -- After a blank line, apart from the transaction above it
      it "says before the mnemonic that typing it signs and sends the transaction" $
        withChain paidChain $ \home _ -> do
          shown <- run home
          shownBeforeMnemonic shown
            `shouldSatisfy` Text.isSuffixOf "\n\nTyping the mnemonic signs this transaction and sends it to the network. This cannot be undone.\n"

      -- The mnemonic is the answer
      it "does not ask whether to send, as broadcast does" $
        withChain paidChain $ \home _ -> do
          shown <- run home
          shown `shouldNotSatisfy` printed sendQuestion

      it "answers with the txid of the transaction it sent, not with the transaction again" $
        withChain paidChain $ \home _ -> do
          shown <- run home
          txid <- txidOf =<< onePendingTx home
          take 1 (Text.lines (runOut shown)) `shouldBe` ["Sent, txid " <> Text.pack txid]

      it "answers with a txid of 64 hexadecimal digits" $
        withChain paidChain $ \home _ -> do
          shown <- run home
          (Text.stripPrefix "Sent, txid " =<< firstLine (runOut shown))
            `shouldSatisfy` maybe False (\txid -> Text.length txid == 64 && Text.all isHexDigit txid)

      it "says to sync next, as broadcast does" $
        withChain paidChain $ \home _ -> do
          shown <- run home
          runOut shown `shouldSatisfy` Text.isSuffixOf "\n\nNext, sync the wallet to follow the transaction:\n  hw sync\n"

      it "exits as a success" $
        withChain paidChain $ \home _ -> do
          shown <- run home
          runExit shown `shouldBe` ExitSuccess

      -- The coin of 30000 at /0/1 is locked, so the payment leaves it
      -- ('oneAroundLocked'). 30000 satoshis are 0.0003 bitcoins. The coin
      -- is 3 confirmations deep, its transaction seen at 1700000000, 23:13
      -- in Central Europe.
      it ("shows the coins it leaves in the portfolio below the transaction, before the mnemonic, as " <> one.onePrepare <> " lists them") $
        withChain (confirmedChain twoAddressesTx) $ \home _ -> do
          synced home
          _ <- answer home ["lockcoin", coinArg twoAddressesTx 1] []
          shown <- hwWithEnv [("TZ", centralEurope)] home (one.oneName : one.oneAroundLocked) seedOnce
          let (transaction, staying) = Text.breakOn "1 coin stays in the portfolio:" (shownBeforeMnemonic shown)
          transaction `shouldSatisfy` Text.isInfixOf one.oneAroundLockedLine
          staying
            `shouldSatisfy` Text.isInfixOf
              ( "\n" <> Text.pack (shortCoinArg twoAddressesTx 1) <> "  14 Nov 2023 23:13      0.00030000 btc  locked by you, 3 confirmations\n"
                  <> "  at 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg  bip44/0/1\n"
                  <> "  you locked it\n"
              )

  -- The coins of 50000 and 30000, and three worth less than their fee.
  -- sendall has one question more than send, which preparesendall asks.
  describe "sendall, with coins worth less than their fee" $ do
    let run home typed = do
          synced home
          hw home ["sendall", textOf (otherAddr 0), "--fee-rate", "10"] typed

    it "asks whether to spend them before it shows the transaction" $
      withChain (confirmedChain dustCoinsTx) $ \home _ -> do
        shown <- run home ("n" : seedOnce)
        fst (Text.breakOn "  To       : " (runErr shown)) `shouldSatisfy` Text.isInfixOf dustQuestion

    -- A no leaves them and sends the two others: 76600 for the address
    it "goes on to the mnemonic after a no, with the transaction that leaves them" $
      withChain (confirmedChain dustCoinsTx) $ \home _ -> do
        shown <- run home ("n" : seedOnce)
        shownBeforeMnemonic shown `shouldSatisfy` Text.isInfixOf "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00076600 btc\n"

    it "shows the coins it leaves below the transaction, before the mnemonic" $
      withChain (confirmedChain dustCoinsTx) $ \home _ -> do
        shown <- run home ("n" : seedOnce)
        shownBeforeMnemonic shown `shouldSatisfy` Text.isInfixOf "3 coins stay in the portfolio:"
  where
    firstLine text = case Text.lines text of
      line : _ -> Just line
      [] -> Nothing

{- A payment that was not signed -}

-- The payments are the plain one of each command, from the coin of 50000
-- of the paid chain, unless the test says otherwise. Each failure is
-- checked for what it says, and for what it leaves in the wallet, in
-- tests of their own. A wallet left as it was has no pending transaction,
-- and its one coin is as coins showed it before: 50000 satoshis, 3
-- confirmations, no lock, and free to spend.
notSignedSpec :: Spec
notSignedSpec =
  forM_ inOnes $ \one -> do
    let plain = one.oneName : plainOf one
        noPendingTx home = (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []
        coinIsFree home = (coinsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(OutPoint (txHash payment) 0, 50000, 3, Nothing, True)]

    describe (one.oneName <> " of a payment " <> one.onePrepare <> " refuses") $ do
      let refused = one.oneName : one.oneRefused
      it ("refuses it as " <> one.onePrepare <> " does") $
        withChain paidChain $ \home _ -> do
          synced home
          (errorOf <$> hwJson home refused seedOnce) `shouldReturn` Right one.oneRefusal

      it "does not ask for the mnemonic" $
        withChain paidChain $ \home _ -> do
          synced home
          run <- hw home refused seedOnce
          run `shouldNotSatisfy` askedMnemonic

      it "exits as a failure" $
        withChain paidChain $ \home _ -> do
          synced home
          run <- hw home refused seedOnce
          runExit run `shouldBe` ExitFailure 1

      it "keeps no pending transaction" $
        withChain paidChain $ \home _ -> do
          synced home
          _ <- hwJson home refused seedOnce
          noPendingTx home

      it "leaves the coin of the portfolio free to spend" $
        withChain paidChain $ \home _ -> do
          synced home
          _ <- hwJson home refused seedOnce
          coinIsFree home

    describe (one.oneName <> " at a fee rate above 100 that the user turns down") $ do
      let highFee = [one.oneName, one.onePlainTo, "--fee-rate", "101"]
      it ("asks about the rate, as " <> one.onePrepare <> " does") $
        withChain paidChain $ \home _ -> do
          synced home
          run <- hw home highFee ["n"]
          runErr run `shouldSatisfy` Text.isInfixOf highFeeQuestion

      it "says that nothing was prepared" $
        withChain paidChain $ \home _ -> do
          synced home
          (errorOf <$> hwJson home highFee ["n"]) `shouldReturn` Right highFeeRefused

      it "does not ask for the mnemonic" $
        withChain paidChain $ \home _ -> do
          synced home
          run <- hw home highFee ["n"]
          run `shouldNotSatisfy` askedMnemonic

      it "keeps no pending transaction" $
        withChain paidChain $ \home _ -> do
          synced home
          _ <- hwJson home highFee ["n"]
          noPendingTx home

      it "leaves the coin of the portfolio free to spend" $
        withChain paidChain $ \home _ -> do
          synced home
          _ <- hwJson home highFee ["n"]
          coinIsFree home

    -- The input ends where the mnemonic is asked, as when the user stops
    -- there
    describe (one.oneName <> " stopped at the mnemonic") $ do
      it "says it was cancelled" $
        withChain paidChain $ \home _ -> do
          synced home
          (errorOf <$> hwJson home plain []) `shouldReturn` Right "Cancelled: the input ended before the answer"

      it "sends nothing" $
        withChain paidChain $ \home chain -> do
          synced home
          _ <- hwJson home plain []
          (inMempool <$> readIORef chain) `shouldReturn` []

      it "keeps no pending transaction" $
        withChain paidChain $ \home _ -> do
          synced home
          _ <- hwJson home plain []
          noPendingTx home

      it "leaves the coin of the portfolio free to spend" $
        withChain paidChain $ \home _ -> do
          synced home
          _ <- hwJson home plain []
          coinIsFree home

    -- The second test mnemonic is of another wallet
    describe (one.oneName <> " given a mnemonic that is not the portfolio's") $ do
      let wrong = seedOnceOf testMnemonic2
      it "asks again, and sends once the right one is typed" $
        withChain paidChain $ \home chain -> do
          synced home
          _ <- answer home plain (wrong <> seedOnce)
          ((spendsAndPays <$>) . inMempool <$> readIORef chain) `shouldReturn` [one.onePlainHanded]

      it "gives up after three, and says the mnemonic does not match" $
        withChain paidChain $ \home _ -> do
          synced home
          (errorOf <$> hwJson home plain (wrong <> wrong <> wrong))
            `shouldReturn` Right "The mnemonic or the passphrase does not match the portfolio"

      it "sends nothing after three" $
        withChain paidChain $ \home chain -> do
          synced home
          _ <- hwJson home plain (wrong <> wrong <> wrong)
          (inMempool <$> readIORef chain) `shouldReturn` []

      it "keeps no pending transaction after three" $
        withChain paidChain $ \home _ -> do
          synced home
          _ <- hwJson home plain (wrong <> wrong <> wrong)
          noPendingTx home

      it "leaves the coin of the portfolio free to spend after three" $
        withChain paidChain $ \home _ -> do
          synced home
          _ <- hwJson home plain (wrong <> wrong <> wrong)
          coinIsFree home

    -- The home is synced, then its configuration names a store that
    -- refuses every connection
    describe (one.oneName <> " when the store cannot be reached") $ do
      let unreachable home = setConfigKey home "host" "http://127.0.0.1:1/"
      it "says so" $
        withChain paidChain $ \home _ -> do
          synced home
          unreachable home
          (errorOf <$> hwJson home plain seedOnce) `shouldReturn` Right "The store at http://127.0.0.1:1/ could not be reached: the connection failed"

      it "does not ask for the mnemonic, which would be typed for nothing" $
        withChain paidChain $ \home _ -> do
          synced home
          unreachable home
          run <- hw home plain seedOnce
          run `shouldNotSatisfy` askedMnemonic

      it "keeps no pending transaction" $
        withChain paidChain $ \home _ -> do
          synced home
          unreachable home
          _ <- hwJson home plain seedOnce
          noPendingTx home

      it "leaves the coin of the portfolio free to spend" $
        withChain paidChain $ \home _ -> do
          synced home
          unreachable home
          _ <- hwJson home plain seedOnce
          coinIsFree home

{- A payment that was signed and not sent -}

-- The store takes no transaction. The command has signed the payment by
-- then, and the wallet holds it as signtx leaves one: signed, and not
-- online. The payment is the plain one of each command.
notSentSpec :: Spec
notSentSpec =
  forM_ inOnes $ \one -> do
    let plain = one.oneName : plainOf one
        refusing = withChainNotPublishing (NotPublished "bad-txns-inputs-missingorspent") paidChain
        -- What the command adds to the failure of a sending: that the
        -- transaction is signed and kept, and the two commands that take
        -- it from there, each with the short id of its txid, its first 8
        -- digits. The txid of a payment with change differs from run to
        -- run, as the order of its outputs does.
        keptNote txid =
          ". The transaction is signed and stays pending: broadcast "
            <> Text.pack (shortOf txid)
            <> " sends it again, and deletetx "
            <> Text.pack (shortOf txid)
            <> " deletes it and frees its coins"

    describe (one.oneName <> " of a transaction the store says no node would take") $ do
      it "fails with what the store said, then with what to do with the signed transaction" $
        refusing $ \home _ -> do
          synced home
          failure <- errorOf <$> hwJson home plain seedOnce
          txid <- txidOf =<< onePendingTx home
          failure `shouldSatisfy` either (const False) (Text.isSuffixOf (" refused the transaction: bad-txns-inputs-missingorspent" <> keptNote txid))

      it "exits as a failure" $
        refusing $ \home _ -> do
          synced home
          run <- hw home plain seedOnce
          runExit run `shouldBe` ExitFailure 1

      it "keeps the transaction signed and not sent, as signtx leaves one" $
        refusing $ \home _ -> do
          synced home
          _ <- hwJson home plain seedOnce
          onePendingState home `shouldReturn` Just (True, False)

      -- The coin of 50000, 3 confirmations deep, is the one the signed
      -- transaction spends
      it "keeps the coin of the transaction locked" $
        refusing $ \home _ -> do
          synced home
          _ <- hwJson home plain seedOnce
          (coinsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(OutPoint (txHash payment) 0, 50000, 3, Just LockedByTransaction, False)]

      it "leaves a transaction that deletetx deletes, which frees its coin" $
        refusing $ \home _ -> do
          synced home
          _ <- hwJson home plain seedOnce
          txid <- txidOf =<< onePendingTx home
          _ <- answer home ["deletetx", shortOf txid] []
          (coinsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(OutPoint (txHash payment) 0, 50000, 3, Nothing, True)]

    -- A node says nothing of a transaction it refuses for its fee
    describe (one.oneName <> " of a transaction no node announced back to the store") $ do
      let silent = withChainNotPublishing (PublishFailed "no node announced it") paidChain
      it "fails as broadcast does, then with what to do with the signed transaction" $
        silent $ \home _ -> do
          synced home
          failure <- errorOf <$> hwJson home plain seedOnce
          txid <- txidOf =<< onePendingTx home
          failure
            `shouldSatisfy` either
              (const False)
              ( Text.isSuffixOf
                  ( " handed the transaction to a node, and no node announced it in the time the store waits. A node says nothing of a transaction it refuses, so its fee may be too low for the network"
                      <> keptNote txid
                  )
              )

      it "keeps the transaction signed and not sent" $
        silent $ \home _ -> do
          synced home
          _ <- hwJson home plain seedOnce
          onePendingState home `shouldReturn` Just (True, False)

    -- The store takes no transaction the first time it is handed one, and
    -- takes it the second time. broadcast names the transaction by the
    -- short id of its txid, as the failure of the command names it.
    describe ("A transaction " <> one.oneName <> " signed and could not send") $ do
      let awayAtFirst = withChainNotPublishingAtFirst (PublishFailed "no node announced it") paidChain
      it "is sent by broadcast once the store takes it" $
        awayAtFirst $ \home chain -> do
          synced home
          _ <- hwJson home plain seedOnce
          txid <- txidOf =<< onePendingTx home
          _ <- answer home ["broadcast", shortOf txid, "--yes"] []
          ((spendsAndPays <$>) . inMempool <$> readIORef chain) `shouldReturn` [one.onePlainHanded]

      it "is held as signed and sent once broadcast has sent it" $
        awayAtFirst $ \home _ -> do
          synced home
          _ <- hwJson home plain seedOnce
          txid <- txidOf =<< onePendingTx home
          _ <- answer home ["broadcast", shortOf txid, "--yes"] []
          onePendingState home `shouldReturn` Just (True, True)
