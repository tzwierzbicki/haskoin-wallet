{-# LANGUAGE OverloadedRecordDot #-}

module Haskoin.Wallet.TxSweepSpec where

import Haskoin (deriveXPubKey)
import Haskoin.Wallet.TestUtils (accountSigningKey, prefixError, vectorKey)
import Haskoin.Wallet.TxSweep (signTxWithKeys)
import Haskoin.Wallet.TxVectors
import Test.Hspec

-- The test vectors live in JSON files in test/data/tx-sweep (see the
-- README.md file there). Every .json file is loaded, so new vectors can be
-- added without changing this module.
spec :: Spec
spec = txVectorSpec "test/data/tx-sweep" "Sweep signing test vectors" sweepSigner

-- | Sign with the sweep keys and the account public key, like signsweep does
sweepSigner :: VectorSigner
sweepSigner ctx v = do
  specs <- maybe (Left "not a sweep vector: it has no sweep keys") Right v.vecSweepKeys
  accountKey <- prefixError "account" $ accountSigningKey v.vecNetwork ctx v.vecAccount
  keys <- prefixError "sweep key" $ mapM (vectorKey ctx v.vecNetwork) specs
  let accountPub = deriveXPubKey ctx accountKey
  return $ \c -> signTxWithKeys v.vecNetwork c v.vecSignData accountPub keys
