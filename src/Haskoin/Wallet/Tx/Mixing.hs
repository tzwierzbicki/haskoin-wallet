{-# LANGUAGE OverloadedRecordDot #-}

-- | The groups of coins a transaction joins, which hw warns about before
-- it prepares one.
--
-- A tag on an address names a group of coins, such as kyc for those
-- bought at an exchange that knows the user, and p2p for those bought
-- from a person. The user keeps such groups to keep them apart. One
-- transaction undoes that: whoever reads the chain takes the coins a
-- transaction spends for the coins of one owner, so a payment that spends
-- a kyc coin and a p2p coin ties the two groups together for good.
--
-- The coin selection knows nothing of tags, and picks such coins together
-- without a word. So the commands that choose the coins of a transaction
-- look at what was chosen, and ask before they go on: confirmMixing of
-- "Haskoin.Wallet.Commands".
--
-- = The rule
--
-- The coins a transaction spends are of one group when their addresses
-- all have the same tags. Otherwise it joins groups.
--
-- * A coin without a tag is a group too. A user who tags the coins of an
--   exchange kyc, and nothing else, means that the others are not: a
--   payment that spends both ties them to that exchange.
--
-- * The tags must be the same, not only shared. A coin tagged kyc and
--   cold and a coin tagged kyc alone are of two groups, since hw cannot
--   know which of the tags says where a coin comes from. This is also the
--   payment whose change --tag-change gives fewer tags than one of its
--   coins has ('Haskoin.Wallet.Database.PendingTx.changeTakesTags').
--
-- The other rule considered was to warn only when no tag is shared by
-- all the coins. It says nothing of a kyc coin and a p2p coin that are
-- both tagged savings, which is the case the warning is for.
module Haskoin.Wallet.Tx.Mixing
  ( SpentGroup (..),
    spentGroups,
    joinsGroups,
    addsGroup,
  )
where

import Data.List (sort, sortOn)
import qualified Data.Map.Strict as Map
import Haskoin
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Tags (ShownTag)
import Numeric.Natural (Natural)

-- | The coins of the portfolio a transaction spends whose addresses have
-- the same tags
data SpentGroup = SpentGroup
  { -- | The tags of the group, in the order of the alphabet: none for the
    -- coins at addresses without a tag
    spentGroupTags :: ![ShownTag],
    spentGroupCoins :: ![OutPoint],
    -- | What the coins are worth together, in satoshis
    spentGroupValue :: !Natural
  }
  deriving (Eq, Show)

-- | The groups of the coins of the portfolio a transaction spends, from
-- its summary with the tags of its addresses
-- ("Haskoin.Wallet.Database.TxInfo".'fillTxInfoTags'). They come in the
-- order of their tags, the coins without a tag last, and the coins of a
-- group in the order of their outpoints.
spentGroups :: TxInfo -> [SpentGroup]
spentGroups info =
  sortOn
    (\group -> (null group.spentGroupTags, group.spentGroupTags))
    [SpentGroup tags (sort coins) value | (tags, (coins, value)) <- Map.assocs byTags]
  where
    byTags =
      Map.fromListWith
        (\(coins, value) (others, valueOfOthers) -> (coins <> others, value + valueOfOthers))
        [ (sort (tagsAt addr), (myInputsOutpoints spent, myInputsValue spent))
          | (addr, spent) <- Map.assocs (summaryMyInputs (txInfoSummary info))
        ]
    tagsAt addr = Map.findWithDefault [] addr (txAddressTags (txInfoTags info))

-- | Whether a transaction that spends the coins of these groups joins
-- groups: there are several
joinsGroups :: [SpentGroup] -> Bool
joinsGroups groups = length groups > 1

-- | Whether the coins added to a transaction join a group to those it
-- spent already, given the groups it spends with them: one of several
-- groups is made of added coins alone. A replacement by bumpfee is asked
-- about by this rule. The groups the replaced transaction joined were
-- agreed to when it was prepared, and are not asked about again.
addsGroup :: [OutPoint] -> [SpentGroup] -> Bool
addsGroup added groups =
  joinsGroups groups && any (all (`elem` added) . (.spentGroupCoins)) groups
