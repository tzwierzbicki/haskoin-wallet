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
          JsonBox arbitraryConfig
        ],
      marshalJsonTests =
        [ MarshalJsonBox ((btc,) <$> arbitraryJsonCoin net),
          MarshalJsonBox ((ctx,) <$> arbitraryPubKeyDoc ctx),
          MarshalJsonBox (((btc, ctx),) <$> arbitraryTxInfo btc ctx),
          MarshalJsonBox ((ctx,) <$> arbitraryResponse btc ctx)
        ]
    }

spec :: Spec
spec = do
  prepareContext $ \ctx -> do
    describe "Sending a transaction from a file" $ do
      -- The checks before anything is uploaded: no network is reached
      let unsignedSweep = TxSignData (spendingTx ctx [(fakeTxHash 1, 0)] [(otherAddr 0, 1000)]) [] [] [] (Just 1) False
      it "needs a nosigHash or a file" $
        cmdSendTx ctx def Nothing Nothing Nothing
          `shouldReturn` ResponseError "Give the NOSIGHASH of a pending transaction or an --input file"
      it "takes a nosigHash or a file, not both" $
        cmdSendTx ctx def (Just (fakeTxHash 1)) (Just "sweep.json") Nothing
          `shouldReturn` ResponseError "Can not specify both a NOSIGHASH and an --input file"
      it "needs the network of the file" $
        cmdSendTx ctx def Nothing (Just "sweep.json") Nothing
          `shouldReturn` ResponseError "Give the --network of a transaction sent from a file"
      it "refuses a missing file" $
        inScratchDirectory $ \dir ->
          cmdSendTx ctx def Nothing (Just (dir </> "missing.json")) (Just btc)
            `shouldReturn` ResponseError "Input file does not exist"
      it "refuses an unsigned transaction" $
        inScratchDirectory $ \dir -> do
          let file = dir </> "sweep.json"
          writeJsonFile file $ Json.toJSON unsignedSweep
          cmdSendTx ctx def Nothing (Just file) (Just btc)
            `shouldReturn` ResponseError "The transaction is not signed"
    testIdentity $ identityTests btc ctx
