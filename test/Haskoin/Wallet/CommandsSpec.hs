{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields -fno-warn-orphans #-}

module Haskoin.Wallet.CommandsSpec where

import qualified Data.Aeson as Json
import Data.Default (def)
import Haskoin
import Haskoin.Util.Arbitrary
import Haskoin.Wallet.Arbitrary
import Haskoin.Wallet.Address (genExtAddress)
import Haskoin.Wallet.Coin (CoinLock (..), JsonCoin (..), refreshCoins)
import Haskoin.Wallet.Commands (Response (..), cmdCoins, cmdLockCoin, cmdSendTx, cmdUnlockCoin)
import Haskoin.Wallet.Config (Config (..))
import Haskoin.Wallet.Database (migrateAll)
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxSignData
import Haskoin.Wallet.Util (Page (Page))
import System.FilePath ((</>))
import Test.Hspec

identityTests :: Network -> Ctx -> IdentityTests
identityTests net ctx =
  def
    { jsonTests =
        [ JsonBox $ arbitraryDBAccount btc ctx,
          JsonBox $ arbitraryDBAddress btc,
          JsonBox arbitraryAddressBalance,
          JsonBox $ arbitraryTxSignData btc ctx,
          JsonBox arbitraryConfig,
          JsonBox $ arbitraryResponse btc ctx
        ],
      marshalJsonTests =
        [ MarshalJsonBox ((btc,) <$> arbitraryJsonCoin net),
          MarshalJsonBox ((ctx,) <$> arbitraryPubKeyDoc ctx),
          MarshalJsonBox ((btc,) <$> arbitraryTxInfo btc)
        ]
    }

spec :: Spec
spec = do
  prepareContext $ \ctx -> do
    describe "Sending a transaction from a file" $ do
      -- The checks before anything is uploaded: no network is reached, and
      -- the wallet is an empty database in a scratch directory
      let unsignedSweep = TxSignData (spendingTx ctx [(fakeTxHash 1, 0)] [(otherAddr 0, 1000)]) [] [] [] (Just 1) Nothing
          funding = fundingTx ctx 1 [(externalAddr 0, 50000)]
          signedPayment = signedByWallet ctx $ TxSignData (spendingTx ctx [(txHash funding, 0)] [(otherAddr 0, 40000)]) [funding] [Deriv :/ 0 :/ 0] [] Nothing Nothing
          walletIn dir = (def :: Config) {configDatabaseFile = Just (dir </> "accounts.sqlite")}
          -- The tables, which hw creates before it runs a command
          emptyWallet dir = inDatabaseFile (dir </> "accounts.sqlite") (createTables migrateAll)
      it "needs a nosigHash or a file" $
        cmdSendTx ctx def Nothing Nothing Nothing
          `shouldReturn` ResponseError "Give the NOSIGHASH of a pending transaction or an --input file"
      it "takes a nosigHash or a file, not both" $
        cmdSendTx ctx def (Just (fakeTxHash 1)) (Just "sweep.json") Nothing
          `shouldReturn` ResponseError "Can not specify both a NOSIGHASH and an --input file"
      it "refuses a missing file" $
        inScratchDirectory $ \dir -> do
          emptyWallet dir
          cmdSendTx ctx (walletIn dir) Nothing (Just (dir </> "missing.json")) (Just btc)
            `shouldReturn` ResponseError "Input file does not exist"
      it "refuses an unsigned transaction" $
        inScratchDirectory $ \dir -> do
          let file = dir </> "sweep.json"
          emptyWallet dir
          writeJsonFile file $ Json.toJSON unsignedSweep
          cmdSendTx ctx (walletIn dir) Nothing (Just file) (Just btc)
            `shouldReturn` ResponseError "The transaction is not signed"
      it "needs the network of a signed transaction the wallet does not hold" $
        inScratchDirectory $ \dir -> do
          let file = dir </> "payment.json"
          emptyWallet dir
          writeJsonFile file $ Json.toJSON signedPayment
          cmdSendTx ctx (walletIn dir) Nothing (Just file) Nothing
            `shouldReturn` ResponseError "Give the --network of a transaction the wallet does not hold"
    describe "Locking coins, as lockcoin, unlockcoin and coins --locked do" $ do
      -- A wallet with the account main and two coins at its first address
      let twoCoins dir =
            walletInFile (dir </> "wallet.sqlite") $ do
              (accId, _) <- testNewAcc ctx "main"
              _ <- genExtAddress ctx def accId ""
              refreshCoins
                btc
                ctx
                accId
                [externalAddr 0]
                [ storeCoin ctx (fakeTxHash 1, 0) (Just 800000) (externalAddr 0) 50000,
                  storeCoin ctx (fakeTxHash 2, 0) (Just 800001) (externalAddr 0) 20000
                ]
          firstCoin = OutPoint (fakeTxHash 1) 0
      it "answer with the coin locked by the user, then free again, and refuse a second unlock" $
        inScratchDirectory $ \dir -> do
          wallet <- twoCoins dir
          shownLocks <$> cmdLockCoin wallet firstCoin `shouldReturn` Right [(firstCoin, Just LockedByUser)]
          shownLocks <$> cmdUnlockCoin wallet firstCoin `shouldReturn` Right [(firstCoin, Nothing)]
          cmdUnlockCoin wallet firstCoin `shouldReturn` ResponseError "The coin is not locked"
      it "list only the locked coins with --locked" $
        inScratchDirectory $ \dir -> do
          wallet <- twoCoins dir
          _ <- cmdLockCoin wallet firstCoin
          shownLocks <$> cmdCoins wallet Nothing True (Page 5 0) `shouldReturn` Right [(firstCoin, Just LockedByUser)]
          (fmap length . shownLocks <$> cmdCoins wallet Nothing False (Page 5 0)) `shouldReturn` Right 2
    testIdentity $ identityTests btc ctx
  where
    -- The coins a response shows, with their locks
    shownLocks (ResponseCoins _ coins) = Right [(jsonCoinOutpoint coin, jsonCoinLock coin) | coin <- coins]
    shownLocks other = Left (show other)
