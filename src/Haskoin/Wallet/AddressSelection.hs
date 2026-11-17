{-# LANGUAGE OverloadedRecordDot #-}

-- | The addresses a file keeps when tags narrow it down: the balance
-- statement of report, with --tag or --untagged.
--
-- A user groups coins with tags on their addresses: kyc on those bought
-- at an exchange, business on those of a shop. A declaration may be of
-- one group only, what the business held as the year ended, and its
-- reader is not to be handed the rest of the wallet. So a balance
-- statement can be of some addresses only.
--
-- = The addresses balances lists
--
-- The options are those of balances, and they keep the addresses
-- balances lists with them: those that have any one of the tags of
-- --tag, and with --untagged those that have none. The user reads the
-- list on the screen first, and the file then holds the same addresses.
-- The filter is the one of the listings
-- ("Haskoin.Wallet.Database.Tag".'TagFilter'), asked here of each
-- address ('keepsTagged').
--
-- It is the sibling of "Haskoin.Wallet.TxSelection", which keeps
-- transactions by their tags for the account statement and the journal.
--
-- = The tags of now, the balances of a day
--
-- hw keeps the tags an address has now, and no history of them. A
-- statement for a past day therefore lists the addresses that have the
-- tag when it is made, with what they held on that day. An address
-- tagged since is in it, and the statement says so.
--
-- = Only the tagged addresses are read
--
-- Most addresses have no tag. A selection holds the tags of the
-- addresses that have some, and an address it does not know has none.
-- So --untagged needs no reading of every address of the wallet.
module Haskoin.Wallet.AddressSelection
  ( AddressSelection (..),
    keepsAddress,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Haskoin
import Haskoin.Wallet.Database.Portfolio (PortfolioRef)
import Haskoin.Wallet.Database.Tag (TagFilter, keepsTagged)
import Haskoin.Wallet.Tags (Tag)

-- | Some of the addresses of the wallet, kept by their tags
data AddressSelection = AddressSelection
  { -- | What the addresses kept have in common, in the words that follow
    -- "addresses", as the first line of balances says it: "tagged kyc",
    -- "without a tag"
    addressesStated :: !Text,
    -- | The tags asked for, and whether the addresses without a tag are
    addressesFilter :: !TagFilter,
    -- | The tags of each address that has some: the portfolio that holds
    -- it, the name of its network, and the address as that network
    -- writes it. The network is part of it, since an address of Bitcoin
    -- may be written as one of a test network is.
    addressesTagged :: !(Map (PortfolioRef, String, Text) [Tag])
  }
  deriving (Eq, Show)

-- | Whether a selection keeps an address a portfolio holds on a network
keepsAddress :: AddressSelection -> PortfolioRef -> Network -> Text -> Bool
keepsAddress selection ref net address =
  keepsTagged (addressesFilter selection) (Map.findWithDefault [] (ref, net.name, address) (addressesTagged selection))
