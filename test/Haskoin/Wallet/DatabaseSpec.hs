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
import Conduit (runConduit, sinkList, (.|))
import Database.Esqueleto.Legacy (renderQuerySelect)
import Database.Persist.Sql (PersistValue (..), rawQuery)
import Haskoin
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.TxInfo (liveSummariesQuery)
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

    it "find the coins of the addresses hw addresses shows, for their spendable balances" $
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
            \having sum(c.value) < 50000 or (sum(c.value) = 50000 and (a.internal > 0 or (a.internal = 0 and a.\"index\" > 1))) \
            \order by sum(c.value) desc, a.internal, a.\"index\" limit 100"
        liftIO $ plan `shouldSatisfy` any ("SEARCH c USING COVERING INDEX d_b_coin_account_address " `Text.isPrefixOf`)

    it "give the coins of an account in the order coins lists them, without sorting them" $
      inMemory $ do
        globalMigration
        plan <- queryPlan "select * from d_b_coin where account = 1 order by block_ref, outpoint limit 100"
        liftIO $ plan `shouldBe` ["SEARCH d_b_coin USING INDEX d_b_coin_account_block_ref (account=?)"]

    -- A chunk of coins starts where the one before ended: at its last block
    -- in the index, then past its last coin
    it "find the coins after a given one from the index, without sorting them" $
      inMemory $ do
        globalMigration
        plan <-
          queryPlan
            "select * from d_b_coin where account = 1 and block_ref >= x'0001' and (block_ref > x'0001' or outpoint > x'ff') \
            \order by block_ref, outpoint limit 100"
        liftIO $ plan `shouldBe` ["SEARCH d_b_coin USING INDEX d_b_coin_account_block_ref (account=? AND block_ref>?)"]

    -- The transactions belong to a portfolio on a network
    it "find the transactions of a portfolio on a network after a given one from the index, without sorting them" $
      inMemory $ do
        globalMigration
        plan <-
          queryPlan
            "select txid, block_ref, time, dead, blob from d_b_tx_info where portfolio = 1 and network = 'btc' and block_ref >= x'0001' and (block_ref > x'0001' or txid > x'ff') \
            \order by block_ref, txid limit 100"
        liftIO $ plan `shouldBe` ["SEARCH d_b_tx_info USING INDEX d_b_tx_info_pool_block_ref (portfolio=? AND network=? AND block_ref>?)"]

    it "add up the amounts down to a given transaction from the index alone, without reading the rows" $
      inMemory $ do
        globalMigration
        plan <-
          queryPlan
            "select sum(amount) from d_b_tx_info where portfolio = 1 and network = 'btc' and dead = 0 and block_ref <= x'0001' and (block_ref < x'0001' or txid <= x'ff')"
        liftIO $ plan `shouldBe` ["SEARCH d_b_tx_info USING COVERING INDEX d_b_tx_info_pool_block_ref (portfolio=? AND network=? AND block_ref<?)"]

    -- lockcoin and show name a coin without its account, and the key of a
    -- coin starts with its account
    it "find the rows of a coin named by its outpoint alone, in whatever account" $
      inMemory $ do
        globalMigration
        plan <- queryPlan "select * from d_b_coin where outpoint = x'ff'"
        liftIO $ plan `shouldBe` ["SEARCH d_b_coin USING INDEX d_b_coin_outpoint (outpoint=?)"]

    -- trustedTx looks up the summaries of a transaction for each coin it
    -- judges. The key of a summary starts with its portfolio and its
    -- network, and no index starts with the txid.
    it "find the live summaries of a transaction on a network by the key of each portfolio, without reading every summary" $
      inMemory $ do
        globalMigration
        plan <- queryPlanOf =<< renderQuerySelect (liveSummariesQuery btc (fakeTxHash 1))
        liftIO $
          plan
            `shouldBe` [ "SCAN d_b_portfolio USING COVERING INDEX sqlite_autoindex_d_b_portfolio_1",
                         "SEARCH d_b_tx_info USING INDEX sqlite_autoindex_d_b_tx_info_1 (portfolio=? AND network=? AND txid=?)"
                       ]

    -- The key of a pending transaction is its nosigHash alone
    it "find the pending transactions of a portfolio on a network" $
      inMemory $ do
        globalMigration
        plan <- queryPlan "select * from d_b_pending_tx where portfolio = 1 and network = 'btc'"
        liftIO $ plan `shouldBe` ["SEARCH d_b_pending_tx USING INDEX d_b_pending_tx_pool (portfolio=? AND network=?)"]

-- | The steps SQLite takes to answer a query
queryPlan :: Text -> DB IO [Text]
queryPlan query = do
  rows <- queryRows ("explain query plan " <> query)
  return [detail | [_, _, _, PersistText detail] <- rows]

-- | The steps SQLite takes to answer a query of hw, as esqueleto renders
-- it with its parameters, in their order
queryPlanOf :: (Text, [PersistValue]) -> DB IO [Text]
queryPlanOf (query, params) = do
  rows <- runConduit (rawQuery ("explain query plan " <> query) params .| sinkList)
  return [detail | [_, _, _, PersistText detail] <- rows]
