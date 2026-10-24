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
import Haskoin.Wallet.Commands (Response (..), cmdSendTx)
import Haskoin.Wallet.Config (Config (..))
import Haskoin.Wallet.Database (migrateAll)
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxSignData
import Haskoin.Wallet.Util
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
      let unsignedSweep = TxSignData (spendingTx ctx [(fakeTxHash 1, 0)] [(otherAddr 0, 1000)]) [] [] [] (Just 1)
          funding = fundingTx ctx 1 [(externalAddr 0, 50000)]
          signedPayment = signedByWallet ctx $ TxSignData (spendingTx ctx [(txHash funding, 0)] [(otherAddr 0, 40000)]) [funding] [Deriv :/ 0 :/ 0] [] Nothing
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
    testIdentity $ identityTests btc ctx
