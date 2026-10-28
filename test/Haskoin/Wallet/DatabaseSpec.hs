{-# LANGUAGE OverloadedStrings #-}

-- | The keys of the database, hashes and outpoints as the rows store them,
-- and the indexes that spare the queries of large wallets a read of a
-- whole table
module Haskoin.Wallet.DatabaseSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Sql (PersistValue (..))
import Haskoin
import Haskoin.Wallet.Database
import Haskoin.Wallet.TestUtils
import Data.Default (def)
import Haskoin.Util.Arbitrary (IdentityTests (..), JsonBox (..), testIdentity)
import Haskoin.Wallet.Arbitrary (arbitraryKeyBytes)
import Test.Hspec

spec :: Spec
spec = do
  keysSpec
  indexesSpec
  -- A key reads back as it was written, in its hexadecimal form
  testIdentity def {jsonTests = [JsonBox arbitraryKeyBytes]}

keysSpec :: Spec
keysSpec =
  describe "The keys of the database" $ do
    it "store a transaction hash as the bytes of its hexadecimal form, and read it back" $ do
      encodeHex (keyBytes (hashBytes (fakeTxHash 1))) `shouldBe` "0000000000000000000000000000000000000000000000000000000000000001"
      bytesToHash (hashBytes (fakeTxHash 1)) `shouldBe` Right (fakeTxHash 1)

    it "store an outpoint as its 36 serialized bytes, the hash first, and read it back" $ do
      let outpoint = OutPoint (fakeTxHash 1) 7
      BS.length (keyBytes (outpointBytes outpoint)) `shouldBe` 36
      BS.take 32 (keyBytes (outpointBytes outpoint)) `shouldBe` S.encode (fakeTxHash 1)
      bytesToOutpoint (outpointBytes outpoint) `shouldBe` Right outpoint

    it "put the outpoints of one transaction between its lowest and highest index" $ do
      let outpoint = OutPoint (fakeTxHash 1) 7
      outpointBytes outpoint `shouldSatisfy` (>= outpointBytes (OutPoint (fakeTxHash 1) minBound))
      outpointBytes outpoint `shouldSatisfy` (<= outpointBytes (OutPoint (fakeTxHash 1) maxBound))
      outpointBytes (OutPoint (fakeTxHash 2) 0) `shouldSatisfy` (> outpointBytes (OutPoint (fakeTxHash 1) maxBound))

indexesSpec :: Spec
indexesSpec =
  describe "The indexes of the database" $ do
    it "find the coins of an address of an account, which the sync refreshes" $
      inMemory $ do
        globalMigration
        plan <- queryPlan "select * from d_b_coin where account = 1 and address = '1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj'"
        liftIO $ plan `shouldBe` ["SEARCH d_b_coin USING INDEX d_b_coin_account_address (account=? AND address=?)"]

    it "find the coins of the addresses addrs shows, for their spendable balances" $
      inMemory $ do
        globalMigration
        plan <- queryPlan "select outpoint, address, value, block_ref from d_b_coin where account = 1 and address in ('1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj', '1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg')"
        liftIO $ plan `shouldBe` ["SEARCH d_b_coin USING INDEX d_b_coin_account_address (account=? AND address=?)"]

    -- The order of balances needs the total of every address of the account.
    -- SQLite walks the coins by account or by address, as its statistics
    -- tell, from the index either way.
    it "add up the coins of each address of an account without reading the coins, as balances orders them" $
      inMemory $ do
        globalMigration
        plan <-
          queryPlan
            "select a.*, sum(c.value) from d_b_coin c inner join d_b_address a on c.account = a.account and c.address = a.address \
            \where c.account = 1 group by a.account, a.internal, a.\"index\" \
            \order by sum(c.value) desc, a.internal, a.\"index\" limit 5 offset 0"
        liftIO $ plan `shouldSatisfy` any ("SEARCH c USING COVERING INDEX d_b_coin_account_address " `Text.isPrefixOf`)

    it "give the coins of an account in the order coins lists them, without sorting them" $
      inMemory $ do
        globalMigration
        plan <- queryPlan "select * from d_b_coin where account = 1 order by block_ref, outpoint limit 10 offset 20"
        liftIO $ plan `shouldBe` ["SEARCH d_b_coin USING INDEX d_b_coin_account_block_ref (account=?)"]

-- | The steps SQLite takes to answer a query
queryPlan :: Text -> DB IO [Text]
queryPlan query = do
  rows <- queryRows ("explain query plan " <> query)
  return [detail | [_, _, _, PersistText detail] <- rows]
