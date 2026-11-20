{-# LANGUAGE OverloadedStrings #-}

-- | What hw reads of the tokens of Bitcoin Cash (CashTokens): whether the
-- script field of an output holds tokens, and the script that locks the
-- output, which stands after the token prefix
module Haskoin.Wallet.CashTokensSpec (spec) where

import qualified Data.ByteString as BS
import Haskoin (bch, btc)
import Haskoin.Wallet.CashTokens
import Haskoin.Wallet.TestUtils (tokenPrefix)
import Test.Hspec

-- | A script that pays to the hash 22…22 of a public key (P2PKH)
payToKeyHash :: BS.ByteString
payToKeyHash = BS.concat [BS.pack [0x76, 0xa9, 0x14], BS.replicate 20 0x22, BS.pack [0x88, 0xac]]

spec :: Spec
spec =
  describe "The script field of an output" $ do
    it "holds tokens on Bitcoin Cash when a token prefix stands before the script" $
      holdsTokens bch (tokenPrefix <> payToKeyHash) `shouldBe` True

    it "holds no tokens on Bitcoin Cash when it is a script alone" $
      holdsTokens bch payToKeyHash `shouldBe` False

    it "holds no tokens on Bitcoin, which has none, whatever its first byte" $
      holdsTokens btc (tokenPrefix <> payToKeyHash) `shouldBe` False

    it "is locked by the script after the token prefix, when it holds tokens" $
      lockingScript bch (tokenPrefix <> payToKeyHash) `shouldBe` Just payToKeyHash

    it "is locked by all of it, when it holds no tokens" $
      lockingScript bch payToKeyHash `shouldBe` Just payToKeyHash

    it "is locked by all of it on Bitcoin, where the same first byte starts no token prefix" $
      lockingScript btc (tokenPrefix <> payToKeyHash) `shouldBe` Just (tokenPrefix <> payToKeyHash)

    -- The prefix ends inside the 32 bytes of the category
    it "has no locking script when its token prefix is cut short" $
      lockingScript bch (BS.pack [0xef, 0x11, 0x11]) `shouldBe` Nothing
