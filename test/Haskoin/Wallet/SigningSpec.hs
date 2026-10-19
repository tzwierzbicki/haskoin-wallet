{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}

module Haskoin.Wallet.SigningSpec where

import qualified Data.ByteString as BS
import Data.Either (fromRight)
import qualified Data.Map as Map
import qualified Data.Serialize as S
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Keys
import Haskoin.Wallet.TxBuilding
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.TxSigning (signWalletTx)
import System.Random (StdGen, mkStdGen)
import Test.Hspec

spec :: Spec
spec =
  prepareContext $ \ctx -> do
    buildWalletTxSpec ctx
    signWalletTxSpec ctx

buildWalletTxSpec :: Ctx -> Spec
buildWalletTxSpec ctx =
  describe "Transaction builder" $ do
    it "can build a transaction" $ do
      let coins =
            [ storeCoin ctx (fakeTxHash 1, 0) Nothing (externalAddr 0) 100000000,
              storeCoin ctx (fakeTxHash 1, 1) Nothing (externalAddr 1) 200000000,
              storeCoin ctx (fakeTxHash 1, 2) Nothing (externalAddr 1) 300000000,
              storeCoin ctx (fakeTxHash 1, 3) Nothing (externalAddr 2) 400000000
            ]
          change = internalAddr 0
          rcps = [(otherAddr 0, 200000000), (otherAddr 1, 200000000)]
          resE = buildWalletTx btc ctx gen (walletCoins coins) rcps 314 10000 False []
      ((.tx) <$> resE)
        `shouldBe` Right
          ( spendingTx
              ctx
              [(fakeTxHash 1, 2), (fakeTxHash 1, 1), (fakeTxHash 1, 0)] -- Greedy algorithm
              ((rcps !! 1) : (change, 199825416) : [head rcps])
          )
      ((.spentCoins) <$> resE)
        `shouldBe` Right [coins !! 2, coins !! 1, head coins]
    it "can fail to build a transaction if funds are insufficient" $ do
      let coins =
            [ storeCoin ctx (fakeTxHash 1, 0) Nothing (externalAddr 0) 100000000,
              storeCoin ctx (fakeTxHash 1, 1) Nothing (externalAddr 1) 200000000,
              storeCoin ctx (fakeTxHash 1, 2) Nothing (externalAddr 1) 300000000,
              storeCoin ctx (fakeTxHash 1, 3) Nothing (externalAddr 2) 400000000
            ]
          rcps = [(otherAddr 0, 500000000), (otherAddr 1, 500000000)]
          resE = buildWalletTx btc ctx gen (walletCoins coins) rcps 1 10000 False []
      resE `shouldBe` Left "chooseCoins: No solution found"
    it "will drop the change output if it is dust" $ do
      let coins =
            [ storeCoin ctx (fakeTxHash 1, 0) Nothing (externalAddr 0) 100000000,
              storeCoin ctx (fakeTxHash 1, 1) Nothing (externalAddr 1) 200000000,
              storeCoin ctx (fakeTxHash 1, 2) Nothing (externalAddr 1) 300000000,
              storeCoin ctx (fakeTxHash 1, 3) Nothing (externalAddr 2) 400000000
            ]
          change = internalAddr 0
          rcps = [(otherAddr 0, 500000000), (otherAddr 1, 499990000)]
          resE1 = buildWalletTx btc ctx gen (walletCoins coins) rcps 0 9999 False []
          resE2 = buildWalletTx btc ctx gen (walletCoins coins) rcps 0 10000 False []
          resE3 = buildWalletTx btc ctx gen (walletCoins coins) rcps 1 9999 False []
      ((.tx) <$> resE1)
        `shouldBe` Right
          ( spendingTx
              ctx
              [(fakeTxHash 1, 3), (fakeTxHash 1, 2), (fakeTxHash 1, 1), (fakeTxHash 1, 0)]
              ((rcps !! 1) : (change, 10000) : [head rcps])
          )
      ((.tx) <$> resE2)
        `shouldBe` Right
          ( spendingTx
              ctx
              [(fakeTxHash 1, 3), (fakeTxHash 1, 2), (fakeTxHash 1, 1), (fakeTxHash 1, 0)]
              ((rcps !! 1) : [head rcps])
          )
      ((.tx) <$> resE3)
        `shouldBe` Right
          ( spendingTx
              ctx
              [(fakeTxHash 1, 3), (fakeTxHash 1, 2), (fakeTxHash 1, 1), (fakeTxHash 1, 0)]
              ((rcps !! 1) : [head rcps])
          )
    it "will fail if sending dust" $ do
      let coins =
            [ storeCoin ctx (fakeTxHash 1, 0) Nothing (externalAddr 0) 100000000,
              storeCoin ctx (fakeTxHash 1, 1) Nothing (externalAddr 1) 200000000,
              storeCoin ctx (fakeTxHash 1, 2) Nothing (externalAddr 1) 300000000,
              storeCoin ctx (fakeTxHash 1, 3) Nothing (externalAddr 2) 400000000
            ]
          rcps = [(otherAddr 0, 500000000), (otherAddr 1, 10000)]
          resE = buildWalletTx btc ctx gen (walletCoins coins) rcps 1 10000 False []
      resE `shouldBe` Left "Recipient output is smaller than the dust value"
    it "can make the recipient pay for the fees" $ do
      let coins =
            [ storeCoin ctx (fakeTxHash 1, 0) Nothing (externalAddr 0) 100000000,
              storeCoin ctx (fakeTxHash 1, 1) Nothing (externalAddr 1) 200000000,
              storeCoin ctx (fakeTxHash 1, 2) Nothing (externalAddr 1) 300000000,
              storeCoin ctx (fakeTxHash 1, 3) Nothing (externalAddr 2) 400000000
            ]
          change = internalAddr 0
          rcps = [(otherAddr 0, 200000000), (otherAddr 1, 200000000)]
          resE = buildWalletTx btc ctx gen (walletCoins coins) rcps 314 10000 True []
      ((.tx) <$> resE)
        `shouldBe` Right
          ( spendingTx
              ctx
              [(fakeTxHash 1, 2), (fakeTxHash 1, 1), (fakeTxHash 1, 0)]
              ( (otherAddr 1, 199912708)
                  : (change, 200000000)
                  : [(otherAddr 0, 199912708)]
              )
          )
    it "fails when recipients cannot pay" $
      do
        let coins =
              [ storeCoin ctx (fakeTxHash 1, 0) Nothing (externalAddr 0) 100000000,
                storeCoin ctx (fakeTxHash 1, 1) Nothing (externalAddr 1) 200000000,
                storeCoin ctx (fakeTxHash 1, 2) Nothing (externalAddr 1) 300000000,
                storeCoin ctx (fakeTxHash 1, 3) Nothing (externalAddr 2) 400000000
              ]
            change = internalAddr 0
            rcps1 = [(otherAddr 0, 400000000), (otherAddr 1, 87291)] -- fee is 2*87292
            rcps2 = [(otherAddr 0, 400000000), (otherAddr 1, 87292)]
            rcps3 = [(otherAddr 0, 400000000), (otherAddr 1, 97293)]
            resE1 = buildWalletTx btc ctx gen (walletCoins coins) rcps1 314 10000 True []
            resE2 = buildWalletTx btc ctx gen (walletCoins coins) rcps2 314 10000 True []
            resE3 = buildWalletTx btc ctx gen (walletCoins coins) rcps3 314 10000 True []
        resE1 `shouldBe` Left "Recipients can't pay for the fee"
        resE2 `shouldBe` Left "Recipient output is smaller than the dust value"
        ((.tx) <$> resE3)
          `shouldBe` Right
            ( spendingTx
                ctx
                [(fakeTxHash 1, 2), (fakeTxHash 1, 1), (fakeTxHash 1, 0)]
                ((otherAddr 1, 10001) : (change, 199902707) : [(otherAddr 0, 399912708)])
            )
    it "can select coin control" $ do
      let coins =
            [ storeCoin ctx (fakeTxHash 1, 0) Nothing (externalAddr 0) 100000000,
              storeCoin ctx (fakeTxHash 1, 1) Nothing (externalAddr 1) 200000000,
              storeCoin ctx (fakeTxHash 1, 2) Nothing (externalAddr 2) 300000000,
              storeCoin ctx (fakeTxHash 1, 3) Nothing (externalAddr 3) 400000000,
              storeCoin ctx (fakeTxHash 1, 4) Nothing (externalAddr 4) 100000000,
              storeCoin ctx (fakeTxHash 1, 5) Nothing (externalAddr 5) 200000000,
              storeCoin ctx (fakeTxHash 1, 6) Nothing (externalAddr 6) 300000000,
              storeCoin ctx (fakeTxHash 1, 7) Nothing (externalAddr 7) 400000000,
              storeCoin ctx (fakeTxHash 1, 8) Nothing (externalAddr 8) 100000000,
              storeCoin ctx (fakeTxHash 1, 9) Nothing (externalAddr 9) 200000000
            ]
          control = [OutPoint (fakeTxHash 1) 6]
          control2 = [OutPoint (fakeTxHash 1) 3]
          control3 = [OutPoint (fakeTxHash 1) 4]
          control4 = [OutPoint (fakeTxHash 1) 2, OutPoint (fakeTxHash 1) 3]
          control5 = [OutPoint (fakeTxHash 1) 3, OutPoint (fakeTxHash 1) 8, OutPoint (fakeTxHash 1) 9]
          change = internalAddr 0
          rcps = [(otherAddr 0, 100000000)]
          rcps5 = [(otherAddr 0, 1300000000)]
          resE = buildWalletTx btc ctx gen (walletCoins coins) rcps 314 10000 False control
          resE2 = buildWalletTx btc ctx gen (walletCoins coins) rcps 314 10000 False control2
          resE3 = buildWalletTx btc ctx gen (walletCoins coins) rcps 314 10000 False control3
          resE4 = buildWalletTx btc ctx gen (walletCoins coins) rcps 314 10000 False control4
          resE5 = buildWalletTx btc ctx gen (walletCoins coins) rcps5 314 10000 False control5
      ((.tx) <$> resE)
        `shouldBe` Right
          ( spendingTx
              ctx
              [(fakeTxHash 1, 6)] -- Selected coins
              [head rcps, (change, 199929036)]
          )
      ((.spentCoins) <$> resE)
        `shouldBe` Right [coins !! 6]
      ((.tx) <$> resE2)
        `shouldBe` Right
          ( spendingTx
              ctx
              [(fakeTxHash 1, 3)] -- Selected coins
              [head rcps, (change, 299929036)]
          )
      ((.spentCoins) <$> resE2)
        `shouldBe` Right [coins !! 3]
      ((.tx) <$> resE3)
        `shouldBe` Right
          ( spendingTx
              ctx
              [(fakeTxHash 1, 9),(fakeTxHash 1, 4)] -- Selected coins
              [head rcps, (change, 199882564)]
          )
      ((.spentCoins) <$> resE3)
        `shouldBe` Right [coins !! 9, coins !! 4]
      ((.tx) <$> resE4)
        `shouldBe` Right
          ( spendingTx
              ctx
              [(fakeTxHash 1, 2)] -- Only one coin was selected
              [head rcps, (change, 199929036)]
          )
      ((.spentCoins) <$> resE4)
        `shouldBe` Right [coins !! 2]
      ((.tx) <$> resE5)
        `shouldBe` Right
          ( spendingTx
              ctx
              [ (fakeTxHash 1, 7),
                (fakeTxHash 1, 2),
                (fakeTxHash 1, 1),
                (fakeTxHash 1, 0),
                (fakeTxHash 1, 9),
                (fakeTxHash 1, 8),
                (fakeTxHash 1, 3) ] -- Selected coins
              [head rcps5, (change, 399650204)]
          )
      ((.spentCoins) <$> resE5)
        `shouldBe` Right [ coins !! 7,
                           coins !! 2,
                           coins !! 1,
                           coins !! 0,
                           coins !! 9,
                           coins !! 8,
                           coins !! 3 ]

signWalletTxSpec :: Ctx -> Spec
signWalletTxSpec ctx =
  describe "Transaction signer" $ do
    it "can derive private signing keys" $ do
      let xPrvE = signingKey btc ctx testMnemonic 0
          xPubE = deriveXPubKey ctx <$> xPrvE
      xPrvE `shouldBe` Right (testAccountKey)
      xPubE `shouldBe` Right (testAccountPub ctx)
    it "can sign a simple transaction" $ do
      let fundTx = spendingTx ctx [(fakeTxHash 1, 0)] [(externalAddr 0, 100000000)]
          newTx =
            spendingTx
              ctx
              [(txHash fundTx, 0)]
              [(otherAddr 0, 50000000), (internalAddr 0, 40000000)]
          dat =
            TxSignData
              newTx
              [fundTx]
              [extDeriv :/ 0]
              [intDeriv :/ 0]
              False
          xPrv = testAccountKey
      let resE = signWalletTx btc ctx dat xPrv
          (resDat, resTxInfo) = fromRight (error "fromRight") resE
          signedTx = txSignDataTx resDat
      txSignDataSigned resDat `shouldBe` True
      resTxInfo
        `shouldBe` TxInfo
          { txInfoHash =
              Just "e66b790c73d4e72fe13a07e247e4439bdea210cac2f947040e901f1e0ce59ac2",
            txInfoType = TxDebit,
            txInfoAmount = -60000000,
            txInfoMyOutputs =
              Map.fromList [(internalAddr 0, MyOutputs 40000000 (intDeriv :/ 0) "")],
            txInfoOtherOutputs = Map.fromList [(otherAddr 0, 50000000)],
            txInfoNonStdOutputs = [],
            txInfoMyInputs =
              Map.fromList
                [ ( externalAddr 0,
                    MyInputs
                      100000000
                      (extDeriv :/ 0)
                      ""
                      [ SigInput
                          (PayPKHash $ addrHash160 (externalAddr 0))
                          100000000
                          (OutPoint (txHash fundTx) 0)
                          sigHashAll
                          Nothing
                      ]
                  )
                ],
            txInfoOtherInputs = Map.empty,
            txInfoNonStdInputs = [],
            txInfoSize = fromIntegral $ BS.length $ S.encode signedTx,
            txInfoFee = 10000000,
            txInfoFeeByte = 44247,
            txInfoBlockRef = Store.MemRef 0,
            txInfoConfirmations = 0,
            txInfoPending =
              Just $
                TxInfoPending
                  "9b41b3cb7b10bfd1dc3f7f1885ce3623789b1b946d206fd59fb432ad4fdb6d70"
                  True
                  False
          }
    it "can set the correct TxInternal transaction types" $ do
      let fundTx =
            spendingTx ctx [(fakeTxHash 1, 0)] [(externalAddr 0, 100000000), (externalAddr 1, 200000000)]
          newTx =
            spendingTx
              ctx
              [(txHash fundTx, 0), (txHash fundTx, 1)]
              [(internalAddr 0, 50000000), (externalAddr 2, 200000000)]
          dat =
            TxSignData
              newTx
              [fundTx]
              [extDeriv :/ 0, extDeriv :/ 1]
              [intDeriv :/ 0, extDeriv :/ 2]
              False
          xPrv = testAccountKey
      let resE = signWalletTx btc ctx dat xPrv
      (txInfoType . snd <$> resE) `shouldBe` Right TxInternal
    it "fails when an input is not signed" $ do
      let fundTx =
            spendingTx ctx [(fakeTxHash 1, 0)] [(externalAddr 0, 100000000), (externalAddr 1, 100000000)]
          newTx =
            spendingTx
              ctx
              [(txHash fundTx, 0), (txHash fundTx, 1)]
              [(otherAddr 0, 50000000), (internalAddr 0, 40000000)]
          dat =
            TxSignData
              newTx
              [fundTx]
              [extDeriv :/ 0] -- We omit derivation 1
              [intDeriv :/ 0]
              False
          xPrv = testAccountKey
      let resE = signWalletTx btc ctx dat xPrv
      resE `shouldBe` Left "input 1 (1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg) does not belong to the wallet"
    it "fails when referenced input transactions are missing" $ do
      let fundTx = spendingTx ctx [(fakeTxHash 1, 0)] [(externalAddr 0, 100000000)]
          newTx =
            spendingTx
              ctx
              [(txHash fundTx, 0), (txHash fundTx, 1)]
              [(otherAddr 0, 50000000), (internalAddr 0, 40000000)]
          dat =
            TxSignData
              newTx
              [fundTx]
              [extDeriv :/ 0, extDeriv :/ 1] -- 1 is missing in fundTx
              [intDeriv :/ 0]
              False
          xPrv = testAccountKey
      let resE = signWalletTx btc ctx dat xPrv
      resE `shouldBe` Left "Referenced input transactions are missing"
    it "fails when private key derivations don't match the Tx inputs" $
      do
        let fundTx =
              spendingTx ctx [(fakeTxHash 1, 0)] [(externalAddr 0, 100000000), (externalAddr 1, 100000000)]
            newTx =
              spendingTx
                ctx
                [(txHash fundTx, 0), (txHash fundTx, 1)]
                [(otherAddr 0, 50000000), (internalAddr 0, 40000000)]
            dat =
              TxSignData
                newTx
                [fundTx]
                [extDeriv :/ 1, extDeriv :/ 2] -- 1 and 2 instead of 0 and 1
                [intDeriv :/ 0]
                False
            xPrv = testAccountKey
        let resE = signWalletTx btc ctx dat xPrv
        resE `shouldBe` Left "Input derivations don't match the transaction inputs"
    it "fails when output derivations don't match the Tx outputs" $
      do
        let fundTx = spendingTx ctx [(fakeTxHash 1, 0)] [(externalAddr 0, 100000000)]
            newTx =
              spendingTx
                ctx
                [(txHash fundTx, 0)]
                [(otherAddr 0, 50000000), (internalAddr 0, 40000000)]
            dat =
              TxSignData
                newTx
                [fundTx]
                [extDeriv :/ 0]
                [intDeriv :/ 1] -- 1 instead of 0
                False
            xPrv = testAccountKey
        let resE = signWalletTx btc ctx dat xPrv
        resE `shouldBe` Left "Output derivations don't match the transaction outputs"

-- Test Helpers --

addrHash160 :: Address -> Hash160
addrHash160 =
  \case
    PubKeyAddress h -> h
    ScriptAddress h -> h
    WitnessPubKeyAddress h -> h
    _ -> undefined

-- | The coins as the builder receives them: with the paths of their
-- addresses, the change address /1/0 and no recipient of the wallet
walletCoins :: [Store.Unspent] -> WalletCoins
walletCoins coins =
  WalletCoins
    { coins = coins,
      coinPaths = Map.fromList [(externalAddr i, Deriv :/ 0 :/ fromIntegral i) | i <- [0 .. 9]],
      change = (internalAddr 0, Deriv :/ 1 :/ 0),
      recipientPaths = Map.empty
    }

-- Test Constants

-- Use a predictable seed for tests
gen :: StdGen
gen = mkStdGen 0
