{-# LANGUAGE OverloadedRecordDot #-}

module Haskoin.Wallet.TxSigningSpec where

import Control.Monad (when)
import Data.Maybe (isJust)
import Haskoin.Wallet.TestUtils (accountSigningKey, prefixError)
import Haskoin.Wallet.TxSigning (signWalletTx)
import Haskoin.Wallet.TxVectors
import Test.Hspec

-- The test vectors live in JSON files in test/data/tx-signing (see the
-- README.md file there for the format and the origin of the vectors). Every
-- .json file is loaded, so new vectors can be added without changing this
-- module.
spec :: Spec
spec =
  txVectorSpec "test/data/tx-signing" "Transaction signing test vectors" walletSigner

-- | Sign with the account key, like signtx does. Vectors with sweep keys
-- belong in test/data/tx-sweep.
walletSigner :: VectorSigner
walletSigner ctx v = do
  when (isJust v.vecSweepKeys) $
    Left "this is a sweep vector: it belongs in test/data/tx-sweep"
  accountKey <- prefixError "account" $ accountSigningKey v.vecNetwork ctx v.vecAccount
  return $ \c -> signWalletTx v.vecNetwork c v.vecSignData accountKey
